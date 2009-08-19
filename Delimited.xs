#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "ppport.h"

#define DEBUG

#ifdef DEBUG
#define trace( format, args... ) fprintf( stderr, format, ##args )
#define debug( args... ) args
#else
#define trace(...)
#define debug(...)
#endif

#define MY_CXT_KEY "Continuation::Delimited::_guts" XS_VERSION

typedef struct delim {
	I32 stack;
	I32 marks;
	I32 tmps;
	I32 cxs;
	I32 scopes;
	I32 saves;


#define VAR(name, type) type name;
#include"state.h"
#undef VAR

	SV **curpad;
	AV *comppad;

	struct delim *prev;
} delim_t;

/* For Perl space exposure, flatten to single mixed stack of objects, [
 * SV(...), MARK, SCOPE(...), etc, merging the order of the items correctly.
 *
 * reconstructing a cont_t from this is a simple matter of walking that stack
 * and putting elements on the right stack */

typedef struct cont {
	AV *stack;

	I32 *marks;
	I32 marks_len;

	SV **tmps;
	I32 tmps_len;

	ANY *defer_saves;
	I32 defer_len;

	ANY *repeat_saves;
	I32 repeat_len;

	I32 *scopes;
	I32 scopes_len;

	PERL_CONTEXT *cxs;
	I32 cxs_len;

	AV *pads;
#ifdef DEBUG
	AV *cvs; /* used in an assertion */
#endif

	delim_t *start;
	delim_t *end;
} cont_t;

typedef struct {
	delim_t *last_mark;

	OP fakeop;
	UNOP trampoline;
	OP *trampoline_PL_op;

	CV *block;
	void (*hook)(pTHX);

	/* used in invoke */
	SV **args;
	I32 items;
	cont_t *cont;
	OP *retop;
} my_cxt_t;

START_MY_CXT

static void print_delim (delim_t *marker) {
	printf("prev:  %p\n"
	       "stack: %d\n"
	       "mark:  %d\n"
	       "tmps:  %d\n"
	       "scope: %d\n"
	       "save:  %d\n"
	       "cxs:   %d\n",
	       marker->prev,
		   marker->stack,
		   marker->marks,
		   marker->tmps,
		   marker->scopes,
		   marker->saves,
		   marker->cxs
	);
}

static void print_cont (cont_t *cont) {
	printf("stack %p (%d)\n"
	       "marks %p (%d)\n"
	       "tmps %p (%d)\n"
	       "cxs %p (%d)\n"
	       "pads %p (%d)\n",
	       cont->stack, av_len(cont->stack),
	       cont->marks, cont->marks_len,
	       cont->tmps, cont->tmps_len,
		   cont->cxs, cont->cxs_len,
		   cont->pads, av_len(cont->pads)
	);

}

static void push_block (pTHX) {
	dSP;

	assert(MY_CXT.block != NULL);

	XPUSHs((SV *)MY_CXT.block);
	PUTBACK;

	MY_CXT.block = NULL;
}

/* this is like an entersub ppaddr but lets us invoke a hook first. it's used
 * to inject a call to the blocks given to reset { } and shift { } without the
 * XSUB context on the stack */
static OP *entersub_wrapper (pTHX) {
	dSP;
	bool entersub = MY_CXT.block != NULL;

	trace("entersub stack pos %p (%d)\n", PL_stack_sp, PL_stack_sp - PL_stack_base);

	/* reset PL_op to what it should be (instead of the trampoline). this is
	 * the entersub that invoked the XSUB, and it's only used for its ->op_next
	 * by ENTERSUB below */
	PL_op = MY_CXT.trampoline_PL_op;

	if ( MY_CXT.hook ) {
		MY_CXT.hook(aTHX);
		MY_CXT.hook = NULL;
	}

	if ( MY_CXT.block ) {
		/* push the block SV if it hasn't been pushed by the hook */
		PUSHMARK(SP);
		push_block(aTHX);
	}

	/* FIXME refactor */
	if ( entersub ) {
		return PL_ppaddr[OP_ENTERSUB](aTHX);
	} else {
		return NORMAL;
	}
}

/* setup PL_op to trampoline into a block without an XSUB context */
static void setup_trampoline_cb (pTHX_ void *ptr) {
	int flags = GIMME_V;

	trace("trigered stack pos %p (%d)\n", PL_stack_sp, PL_stack_sp - PL_stack_base);

	Zero(&MY_CXT.trampoline, 1, UNOP);

	if (!(flags & G_NOARGS))
		MY_CXT.trampoline.op_flags |= OPf_STACKED;

	MY_CXT.trampoline.op_flags |= ((flags & G_VOID) ? OPf_WANT_VOID :
		(flags & G_ARRAY) ? OPf_WANT_LIST : OPf_WANT_SCALAR);

	MY_CXT.trampoline.op_type = OP_ENTERSUB;
	MY_CXT.trampoline.op_next = PL_op->op_next;
	MY_CXT.trampoline.op_ppaddr = entersub_wrapper;

	MY_CXT.trampoline_PL_op = PL_op;

	/* ENTERSUB will return PL_op->op_next causing execution of the trampoline */
	MY_CXT.fakeop.op_next = (OP *)&MY_CXT.trampoline;
	PL_op = &MY_CXT.fakeop;
}

static void setup_trampoline (pTHX_ void (*hook)(pTHX)) {
	MY_CXT.hook = hook;

	/* these get triggered on LEAVE inside ENTERSUB */
	trace("saving stack pos %p (%d)\n", PL_stack_sp, PL_stack_sp - PL_stack_base);
	SAVEDESTRUCTOR_X(setup_trampoline_cb, NULL);
	SAVESTACK_POS(); /* Enforce some insanity in scalar context. */
}

/* captures values from the interpreter state */
static void init_delim (pTHX_ delim_t *marker) {
	marker->prev = MY_CXT.last_mark;

	/* pointers have to be converted to relative indices */
	marker->stack  = PL_stack_sp      - PL_stack_base;
	marker->marks  = PL_markstack_ptr - PL_markstack;

	/* these relative indices are not reused when resetting the state on
	 * restoration, so they aren't in state.h */
	marker->tmps   = PL_tmps_ix;
	marker->cxs    = cxstack_ix;
	marker->scopes = PL_scopestack_ix;
	marker->saves  = PL_savestack_ix;

#define VAR(name, type) marker->name = PL_ ## name;
#include "state.h"
#undef VAR

	marker->curpad  = PL_curpad;
	marker->comppad = PL_comppad;

	trace("init delim, SP=%p\n", PL_stack_sp);
}

static delim_t* create_delim (pTHX) {
	delim_t *marker;

	Newx(marker, 1, delim_t);
	init_delim(aTHX_ marker);

	return marker;
}

static void destroy_delim (pTHX_ delim_t *marker) {
	Safefree(marker);
}

static void pop_delim (pTHX) {
	delim_t *prev = MY_CXT.last_mark->prev;
	destroy_delim(aTHX_ MY_CXT.last_mark);
	MY_CXT.last_mark = prev;
}

static void delim_destructor (pTHX_ void *ptr) {
	if ( ptr == MY_CXT.last_mark ) {
		/* unused delimiter, no shift { } captured it */
		pop_delim(aTHX);
	} else {
		/* captured delimiter in a fully reified continuation */
		destroy_delim(aTHX_ ptr);
	}
}

static void push_delim (pTHX) {
	MY_CXT.last_mark = create_delim();
	SAVEDESTRUCTOR_X(delim_destructor, (void *)MY_CXT.last_mark);
	/* FIXME does this unwind at the right time? i think reset { }; shift { }
	 * might cause a stale delimiter to be visible to shift { } */
}

/* move the values and marks off the main stack, increasing the reference count */
static void init_cont_stack (pTHX_ cont_t *cont) {
	delim_t *start = cont->start;
	delim_t *end   = cont->end;
	I32 i;
	I32 stack_len = ( end->stack - start->stack );
	AV *stack = newAV();

	//av_extend(stack, (end->stack - start->stack));
	cont->stack = stack;

	/* make sure all the values are refcounted in the */
	trace("stack len: %d\n", stack_len);
	for ( i = 0; i < stack_len; i++ ) {
		SV *sv = PL_stack_base[start->stack + i];
		SvREFCNT_inc(sv);
		av_push(stack, sv);
	}

	cont->marks_len = end->marks - start->marks;

	if ( cont->marks_len ) {
		Newx(cont->marks, cont->marks_len, I32);

		for ( i = 0; i < cont->marks_len; i++) {
			cont->marks[i] = PL_markstack[i + start->marks] - start->stack;
		}
	} else {
		cont->marks = NULL;
	}

	PL_stack_sp      = start->stack + PL_stack_base;
	PL_markstack_ptr = start->marks + PL_markstack;
}

static void init_cont_cxs (pTHX_ cont_t *cont) {
	delim_t *start = cont->start;
	delim_t *end   = cont->end;
	dVAR;
	I32 i;
	PERL_CONTEXT *cxs;
	PERL_CONTEXT *cx;
	I32 *scopes;

	cont->pads = newAV();
	debug(cont->cvs = newAV());
	/* save scope objects, and also pads */

	cont->cxs_len = end->cxs - start->cxs;

	/* relocate the contexts */
	if ( cont->cxs_len ) {
		Newx(cxs, cont->cxs_len, PERL_CONTEXT);
		Copy(&cxstack[start->cxs + 1], cxs, cont->cxs_len, PERL_CONTEXT);

		assert(cxstack_ix == start->cxs + cont->cxs_len);

		/* steal the pads */
		for ( i = cont->cxs_len - 1; i >= 0; i-- ) {
			PERL_CONTEXT *cx = &cxs[i];

			/* convert the various values into 0 based offsets,  */
			cx->blk_oldsp      -= cont->start->stack;
			cx->blk_oldmarksp  -= cont->start->marks;
			cx->blk_oldscopesp -= cont->start->scopes;

			switch (CxTYPE(cx)) {
				CV *cv;
				SV *pad;

				trace("saw oldcomppad %p\n", cx->blk_sub.oldcomppad);

				case CXt_SUB:
					cv = cx->blk_sub.cv;
					pad = av_pop(CvPADLIST(cv));

					trace("saw pad %p at depth %d of cv %p\n", pad, CvDEPTH(cv), cv);

					/* these are kept around only for reference counting purposes,
					 * they will be copied back from the cx stack on continuation
					 * restoration */
					av_push(cont->pads, pad);

					debug(av_push(cont->cvs, SvREFCNT_inc(cv)));

					/* POPSUB but without destroying @_ */
					if (cx->blk_sub.hasargs) {
						trace("@_=%p, pad0=%p, argarray=%p\n", GvAV(PL_defgv), AvARRAY(pad)[0], cx->blk_sub.argarray);
						// assert(GvAV(PL_defgv) == AvARRAY(pad)[0]);
						POP_SAVEARRAY();

						if (!AvREAL(cx->blk_sub.argarray)) {
							CLEAR_ARGARRAY(cx->blk_sub.argarray);
						}
					}
					CvDEPTH(cv) = cx->blk_sub.olddepth;

					break;

				case CXt_LOOP:
					/* FIXME check if ITERVAR and ITERARY are lexicals, and if
					 * so make sure to map them through a pointer table */
					break;

				default:
					break;

			}
		}
	} else {
		cxs = NULL;
	}

	cxstack_ix = start->cxs;

	cont->cxs = cxs;
}

static void init_cont_saves (pTHX_ cont_t *cont) {
	delim_t *start = cont->start;
	delim_t *end   = cont->end;

	I32 i;

	SV **tmps;

	ANY *saves_ptr;
	I32 *scopes_ptr;

	ANY *defer_ptr;
	ANY *repeat_ptr;
	I32 *scopes;

	I32 saves_len = end->saves - start->saves;

	cont->tmps_len   = end->tmps   - start->tmps;
	cont->scopes_len = end->scopes - start->scopes;

	/* wasteful I guess, but who cares */
	Newx(cont->defer_saves,   saves_len, ANY);
	Newx(cont->repeat_saves,  saves_len, ANY);
	Newx(cont->scopes,        cont->scopes_len, I32);

	/* source iterators */
	scopes_ptr = &PL_scopestack[PL_scopestack_ix - 1];
	saves_ptr  = &PL_savestack[PL_savestack_ix - 1];

	/* dest iterators, these are preincremented on assignment so they don't have a -1 */
	defer_ptr  = &cont->defer_saves[saves_len];
	repeat_ptr = &cont->repeat_saves[saves_len];
	scopes     = &cont->scopes[cont->scopes_len];

	trace("start_saves=%d\n", start->saves);
	trace("start_scopes=%d\n", start->scopes);
	
	/* we need to copy the save stack one by one from the end to the begining,
	 * because the last element is what denotes the type */

	while ( scopes_ptr >= &PL_scopestack[start->scopes] ) {
		trace("scopes_ptr=%p, base=%p (%d), end=%p\n", scopes_ptr, &PL_scopestack[start->scopes], scopes_ptr - &PL_scopestack[start->scopes], &PL_scopestack[PL_scopestack_ix]);
		/* iterate all the saves for a given scope
		 *
		 * the entries that involve properly unwinding are moved to the repeat
		 * saves, and the others are moved to the deferred saves
		 *
		 * the new scopestack matches only the repeat saves, since those are
		 * recreated during restore_cont
		 *
		 * all the other saves are recreated during continuation destruction
		 * (these are the entries that e.g. free storage space (DESTRUCTOR_X)
		 * etc.
		 * */
		while ( saves_ptr >= &PL_savestack[*scopes_ptr] ) {
			trace("scopes ptr=%p, index=%d, scope_base=%p, savestack=%p\n", scopes_ptr, *scopes_ptr, &PL_savestack[*scopes_ptr], PL_savestack);
			trace("repeat_ptr=%p, defer_ptr=%p, saves_ptr=%p\n", repeat_ptr, defer_ptr, saves_ptr);

			trace("save entry: %d\n",saves_ptr->any_i32); 
			switch (saves_ptr->any_i32) {
				/* repeated entries, recreated during restore_cont */
				case SAVEt_COMPPAD:
					/* SSCHECK(2) */
					trace("repeat comppad %p\n", *(saves_ptr -1));
				case SAVEt_CLEARSV:
					*--repeat_ptr = *saves_ptr--;
					*--repeat_ptr = *saves_ptr--;
					break;

				case SAVEt_STACK_POS:
					trace("repeat stack pos\n");
					/* SSCHECK(2) */
					*--repeat_ptr = *saves_ptr--;
					repeat_ptr--;
					repeat_ptr->any_i32 = saves_ptr->any_i32 - start->stack; /* 0 based */
					saves_ptr--;
					break;

				case SAVEt_STACK_CXPOS:
					/* SSCHECK(2) */
					trace("repeat cxspos\n");
					*--repeat_ptr = *saves_ptr--;
					repeat_ptr--;
					repeat_ptr->any_i32 = saves_ptr->any_i32 - start->cxs; /* 0 based */
					saves_ptr--;
					break;

				/* deferred entries, happen at destruction */
				case SAVEt_SET_SVFLAGS:
				case SAVEt_PADSV:
				case SAVEt_DELETE:
				case SAVEt_AELEM:
				case SAVEt_HELEM:
				case SAVEt_HINTS:
					/* SSCHECK(4) */
					trace("Size 4\n");
					*--defer_ptr = *saves_ptr--;
					/* fall through */
				case SAVEt_ITEM:
				case SAVEt_SV:
				case SAVEt_AV:
				case SAVEt_HV:
				case SAVEt_INT:
				case SAVEt_LONG:
				case SAVEt_I32:
				case SAVEt_IV:
				case SAVEt_SPTR:
				case SAVEt_APTR:
				case SAVEt_HPTR:
				case SAVEt_PPTR:
				case SAVEt_SVREF:
				case SAVEt_GP:
				case SAVEt_I16:
				case SAVEt_GENERIC_SVREF:
				case SAVEt_DESTRUCTOR_X:
				case SAVEt_VPTR:
				case SAVEt_I8:
				case SAVEt_GENERIC_PVREF:
				case SAVEt_SHARED_PVREF:
				case SAVEt_BOOL:
				case SAVEt_SAVESWITCHSTACK: /* FIXME should this be supported at all? */
				case SAVEt_COP_ARYBASE:
					/* SSCHECK(3) */
					trace("Size 3\n");
					*--defer_ptr = *saves_ptr--;
					/* FIXME skip SAVETMPS? if ( saves_ptr->any_ptr == &PL_tmps_floor ) */
					/* fall through */
				case SAVEt_NSTAB:
				case SAVEt_FREESV:
				case SAVEt_FREEOP:
				case SAVEt_FREEPV:
				case SAVEt_DESTRUCTOR:
				case SAVEt_REGCONTEXT:
				case SAVEt_OP:
				case SAVEt_MORTALIZESV:
				case SAVEt_COMPILE_WARNINGS:
				case SAVEt_PARSER:
					/* SSCHECK(2) */
					trace("Size 2\n");
					*--defer_ptr = *saves_ptr--;
					*--defer_ptr = *saves_ptr--;
					trace("deferred %p\n", saves_ptr);
					break;

				case SAVEt_ALLOC:
					/* SSCHECK(elems + 2) */
					*--defer_ptr = *saves_ptr--;
					i = saves_ptr->any_i32 + 1; /* this is elems */
					defer_ptr -= i;
					saves_ptr -= i;
					Copy(saves_ptr, defer_ptr, i, ANY);
					trace("Deferred alloc %p\n", saves_ptr);
					break;

				case SAVEt_RE_STATE:
					/* SSGROW(SAVESTACK_ALLOC_FOR_RE_SAVE_STATE + 1) */
					saves_ptr -= SAVESTACK_ALLOC_FOR_RE_SAVE_STATE + 1;
					defer_ptr -= SAVESTACK_ALLOC_FOR_RE_SAVE_STATE + 1;
					Copy(saves_ptr, defer_ptr, SAVESTACK_ALLOC_FOR_RE_SAVE_STATE + 1, ANY);
					trace("Deferred re state %p\n", saves_ptr);
					break;

				default:
					croak("unknown save type: %d\n", saves_ptr->any_i32);
			}
		}

		/* now create a new scopes entry for the repeat stack only (at
		 * destruction we unwind everything as a single scope) 
		 *
		 * the save entry is actually bogus, we need to convert it into a zero
		 * based offsets based on the differences at the end, but we don't yet
		 * know how many elements we'll end up with, so we keep a pointer for
		 * now */

		trace("scope end, %p, %d\n", repeat_ptr, (I32)repeat_ptr);
		*--scopes = (I32)repeat_ptr;
		scopes_ptr--;
	}

	trace("saves len=%d, repeat_ptr=%p, repeat_saves=%p (%d)\n", saves_len, repeat_ptr, cont->repeat_saves, repeat_ptr - cont->repeat_saves );
	cont->repeat_len = saves_len - ( repeat_ptr - cont->repeat_saves );
	cont->defer_len  = saves_len - ( defer_ptr  - cont->defer_saves  );

	trace("relocating\n");
	/* relocate the copied stacks to the begining of their buffers */
	// Move(repeat_ptr, cont->repeat_saves, cont->repeat_len, ANY);
	//Move(defer_ptr,  cont->defer_saves,  cont->defer_len,  ANY);

	// FIXME leaks, relocate the data or add another pointer
	cont->repeat_saves = repeat_ptr;
	cont->defer_saves  = defer_ptr;

	/* correct the savestack indices for the relative locations, now that we know where the begining is */
	for ( i = 0; i < cont->scopes_len; i++ ) {
		trace("scope %d = %d, %d\n", i, cont->scopes[i]);
		cont->scopes[i] -= (I32)repeat_ptr;
		trace("scope %d = %d, %d\n", i, cont->scopes[i]);
	}

	PL_tmps_ix       = start->tmps;
	PL_savestack_ix  = start->saves;
	PL_scopestack_ix = start->scopes;

	trace("top save: %d\n", PL_savestack[PL_savestack_ix - 1].any_i32);
	trace("top scope: %d, save ix=%d\n", PL_scopestack[PL_scopestack_ix - 1], PL_savestack_ix);

	Newx(tmps, cont->tmps_len, SV *);
	Copy(&PL_tmps_stack, tmps, cont->tmps_len, SV **);

	cont->tmps  = tmps;

	/* TODO:
	 *
	 * restore all localized values from the savestack at this point, and
	 * disable those savestack entries.  the localizations need to be replayed
	 * in restore_cont (and the save entries need to be recreated of course)
	 *
	 * all other SAVEt_* things should be saved in the continuation and added
	 * to the savestack during destroy_cont, to allow the correct leave_scope
	 * behavior to happen when all the data the continuation points to can
	 * really be properly destroyed
	 */
}

static void init_cont_state (pTHX_ cont_t *cont) {
	/* reset interpreter state */
#define VAR(name, type) PL_ ## name = cont->start->name;
#include "state.h"
#undef VAR

	PL_comppad = cont->start->comppad;
	PL_curpad  = cont->start->curpad;

	/* SAVEDESTRUCTOR_X is captured inside the continuation, so we detach this
	 * delimiter chain */
	MY_CXT.last_mark = cont->start->prev;
	cont->start->prev = NULL;
}

/* move everything (destructively) from the last delimiter into a reified
 * continuation structure, reverting the state into what it was at the time
 * that delimiter was taken
 *
 * all stack unwinding operations need to be deferred until the continuation
 * object is garbage collected */
static void init_cont (pTHX_ cont_t *cont) {
	assert(MY_CXT.last_mark != NULL); /* FIXME if reset() was not called do we
										 have an implicit reset for the top of
										 the program? */

	cont->start = MY_CXT.last_mark;
	cont->end = create_delim(aTHX);

	/* FIXME first verify we aren't in substitution context or any XSUB between
	 * the reset and the shift */

	/* trace("from\n"); */
	/* print_delim(start); */
	/* trace("to\n"); */
	/* print_delim(&end); */

	init_cont_stack(aTHX_ cont);
	init_cont_cxs(aTHX_ cont);
	init_cont_saves(aTHX_ cont);
	init_cont_state(aTHX_ cont);

	/* init_delim(aTHX_ &end); */
	/* trace("after\n"); */
	/* print_delim(&end); */
}

void save_delim (pTHX_ void *ptr) {
	MY_CXT.last_mark = (delim_t *)ptr;
}

/* copy values from the saved continuation into the live interpreter
 *
 * this operation should be invokable multiple times */
static void restore_cont (pTHX_ cont_t *cont, OP *retop) {
	I32 i, end;
	dSP;
	SV **stack = AvARRAY(cont->stack);
	I32 stack_len = av_len(cont->stack) + 1;
	PTR_TBL_t *cloned = ptr_table_new();
	I32 pads;

	trace("restoring\n");

	push_delim();

	trace("top save: %d\n", PL_savestack[PL_savestack_ix - 1].any_i32);
	trace("top scope: %d, save ix=%d\n", PL_scopestack[PL_scopestack_ix - 1], PL_savestack_ix);

	trace("current PL_comppad=%p, start comppad=%p, end comppad=%p\n", PL_comppad, cont->start->comppad, cont->end->comppad);

	/* restore all the interpreter variables to the state at the end of the  */
#define VAR(name, type) PL_ ## name = cont->end->name;
#include "state.h"
#undef VAR

	/* clone the pads, fixup the contexts */

	trace("top save: %d\n", PL_savestack[PL_savestack_ix - 1].any_i32);
	trace("top scope: %d, save ix=%d\n", PL_scopestack[PL_scopestack_ix - 1], PL_savestack_ix);


	end = cxstack_ix + cont->cxs_len;

	/* FIXME, this is horrible, but there's nothing in the Perl api for it */
	if ( cxstack_ix + cont->cxs_len > cxstack_max ) {
		cxstack_max = GROW(end);
		Renew(cxstack, cxstack_max, PERL_CONTEXT);
	}

	Copy(cont->cxs, &cxstack[cxstack_ix+1], cont->cxs_len, PERL_CONTEXT);

	/* setup top context */
	cxstack[cxstack_ix+1].blk_sub.retop     = retop;
	cxstack[cxstack_ix+1].blk_sub.savearray = GvAV(PL_defgv);
	// cxstack[cxstack_ix+1].blk_gimme = GIMME_V; /* FIXME won't propagate */

	pads = av_len(cont->pads);

	ptr_table_store(cloned, cont->end->comppad, PL_comppad);

	while ( cxstack_ix < end ) {
		PERL_CONTEXT *cx = &cxstack[++cxstack_ix];
		AV *args;

		/* convert the 0 based offsets to match the current interpreter state */
		cx->blk_oldsp      += PL_stack_sp      - PL_stack_base;
		cx->blk_oldmarksp  += PL_markstack_ptr - PL_markstack;
		cx->blk_oldscopesp += PL_scopestack_ix;

		trace("cx %d type=%d\n", cxstack_ix, CxTYPE(cx));
		debug(cx_dump(cx));

		switch ( CxTYPE(cx) ) {
			case CXt_SUB:
				{
					CV *cv  = cx->blk_sub.cv;
					AV **pad_av_elem = (AV **)av_fetch(cont->pads, pads--, 0);
					assert(pad_av_elem);
					AV *pad_av = *pad_av_elem;
					AV *names_av = (AV *)AvARRAY(CvPADLIST(cv))[0];
					SV **pad   = AvARRAY(pad_av);
					SV **names = AvARRAY(names_av);
					I32 pad_fill   = AvFILLp(pad_av);
					I32 names_fill = AvFILLp(names_av);

					trace("BLK_SUB.OLDCOMPPAD = %p\n", cx->blk_sub.oldcomppad);
					trace("pad[%d] = %p\n", pads-1, pad_av);
					trace("cv=%p, cvs[pads]=%p\n", cv, AvARRAY(cont->cvs)[pads+1]);
					debug(assert( (SV *)cv == *av_fetch(cont->cvs, pads+1, 0) ));


					trace("names fill: %d, pad_fill: %d\n", names_fill, pad_fill);

					/* create a new comppad AV, with the same SVs as the previous one */
					AV *copy = newAV();
					for ( i = 1; i <= pad_fill; i++ ) {
						SV *sv = pad[i];
						SV *name = &PL_sv_undef;
						SV *new  = &PL_sv_undef;

						if ( i <= names_fill ) {
							name = names[i];
						}

						if ( name != &PL_sv_undef ) {
							const char sigil = SvPVX_const(name)[0];

							if ( SvFLAGS(name) & SVf_FAKE || sigil == '&' ) {
								new = SvREFCNT_inc(sv);
							} else {
								/* FIXME clone sv */

								if ( sigil == '@' ) {
									trace("faking clone of AV\n");
									new = (SV *)newAV();
								} else if ( sigil == '%' ) {
									trace("faking clone of HV\n");
									new = (SV *)newHV();
								} else {
									trace("faking clone of SV\n");
									new = newSVsv(sv);
								}
							}
						} else {
							switch (SvTYPE(sv)) {
								case SVt_NULL:
								case SVt_PV:
								case SVt_IV:
								case SVt_NV:
								case SVt_PVIV:
								case SVt_PVNV:
								case SVt_RV:
									trace("clone tmp sv\n");
									new = newSVsv(sv);
									break;
								case SVt_PVAV:
									trace("clone tmp array\n");
									new = (SV *)newAV();
									break;
								case SVt_PVHV:
									trace("clone tmp hash\n");
									new = (SV *)newHV();
									break;
								default:
									trace("WTF\n");
									new = newSV(0);
							}
						}

						ptr_table_store(cloned, sv, new);
						av_store(copy, i, new);
					}

					/* recreate @_ FIXME clone the proto pad's 0 slot */
					args = newAV();
					/* FIXME AvREAL shit */
					av_store(copy, 0, (SV *)args); /* FIXME what does pad_push do? */

					if ( cx->blk_sub.hasargs ) {
						cx->blk_sub.savearray = GvAV(PL_defgv);
						GvAV(PL_defgv) = (AV *)SvREFCNT_inc_simple(args);
						cx->blk_sub.argarray = args;
					}

					/* FIXME need to work out sp/mark from oldsp and friends to count args and copy */

					cx->blk_sub.olddepth = CvDEPTH(cv);

					CvDEPTH(cv)++;
					av_push(CvPADLIST(cv), (SV *)copy);

					trace("pad %p -> %p\n", pad_av, copy);
					ptr_table_store(cloned, pad_av, copy);

					trace("oldcomppad: %p\n", cx->blk_sub.oldcomppad);
					if ( ptr_table_fetch(cloned, cx->blk_sub.oldcomppad) ) {
						cx->blk_sub.oldcomppad = ptr_table_fetch(cloned, cx->blk_sub.oldcomppad);
						trace("mapped: %p\n", cx->blk_sub.oldcomppad);
					} else {
						trace("BAAAA\n");
						sv_dump(cx->blk_sub.oldcomppad);
						sv_dump(cv);
					}

					if ( pad_av == cont->end->comppad ) {
						trace("pad_av=%p == end->comppad=%p\n", pad_av, cont->end->comppad);
					}
				}

				break;
			case CXt_LOOP:
				trace("loop\n");
				/* FIXME ITERVAR and ITERARRAY through pointertable for clones */
#ifndef USE_ITHREADS
				if ( CxITERVAR(cx) ) {
					/* no need with ithreads, it's just a pad offset */
					SV *sv = *CxITERVAR(cx);
					SV *new;

					if ( ptr_table_fetch(cloned, sv) ) {
						new = ptr_table_fetch(cloned, sv);
					} else {
						new = newSVsv(sv);
						ptr_table_store(cloned, sv, new);
					}

					*CxITERVAR(cx) = new;

					// cx->blk_loop.ITERVAR
				}
#endif

				break;

			default:
				trace("some other CX type\n");
				cx_dump(cx);
				break;
		}
	}

	trace("top save: %d\n", PL_savestack[PL_savestack_ix - 1].any_i32);
	trace("top scope: %d, save ix=%d\n", PL_scopestack[PL_scopestack_ix - 1], PL_savestack_ix);


	/* FIXME, this is horrible, but there's nothing in the Perl api for it */
	if ( PL_scopestack_ix + cont->scopes_len > PL_scopestack_max ) {
		PL_scopestack_max = GROW(PL_scopestack_max + cont->scopes_len);
		Renew(PL_scopestack, PL_scopestack_max, I32);
	}

	for ( i = 0; i < cont->scopes_len; i++ ) {
		PL_scopestack[PL_scopestack_ix++] = PL_savestack_ix + cont->scopes[i];
	}
	trace("scopescack_ix=%d\n", PL_scopestack_ix);
	trace("top save: %d\n", PL_savestack[PL_savestack_ix - 1].any_i32);
	trace("top scope: %d, save ix=%d\n", PL_scopestack[PL_scopestack_ix - 1], PL_savestack_ix);


	if ( cont->repeat_len ) {
		/* FIXME fixup SAVECOMPPAD entries, fix 0 based offsets */
		SSGROW(PL_savestack_ix + cont->repeat_len);
		trace("Savestack %d from %p to %p\n", cont->repeat_len, cont->repeat_saves, &PL_savestack[PL_savestack_ix]);
		Copy(cont->repeat_saves, &PL_savestack[PL_savestack_ix], cont->repeat_len, ANY *);
		trace("blech\n");

		i = PL_savestack_ix + cont->repeat_len - 1;

		while ( i >= PL_savestack_ix ) {
			trace("i=%d ix=%d len=%d\n", i, PL_savestack_ix, cont->repeat_len);
			switch ( PL_savestack[i--].any_i32 ) {
				case SAVEt_COMPPAD:
					if ( i == PL_savestack_ix ) {
						/* last save entry */
						assert( PL_savestack[i].any_ptr == cont->start->comppad );
						trace("top savet comppad %p\n", PL_comppad);
						PL_savestack[i].any_ptr = PL_comppad;
					} else {
						trace("savet comppad %p -> %p\n", PL_savestack[i].any_ptr, ptr_table_fetch(cloned, PL_savestack[i].any_ptr));
						PL_savestack[i].any_ptr = ptr_table_fetch(cloned, PL_savestack[i].any_ptr);// || PL_savestack[i].any_ptr; /* FIXME is this wrong? */
						assert(PL_savestack[i].any_ptr);
					}

					i--;
					break;

				case SAVEt_STACK_POS:
					PL_savestack[i--].any_i32 += PL_stack_sp - PL_stack_base;
					break;

				case SAVEt_STACK_CXPOS:
					PL_savestack[i--].any_i32 += cxstack_ix - cont->cxs_len;
					break;

				case SAVEt_CLEARSV:
					i--;
					break;

				default:
					croak("unknown save entry type %d\n", PL_savestack[i+1].any_i32);
			}
		}

		PL_savestack_ix += cont->repeat_len;
	}


	/* fixup PL_comppad to point to the cloned pad corresponding to the top of the stack */
	trace("overwriting PL_comppad=%p from end comppad=%p to %p\n", PL_comppad, cont->end->comppad, ptr_table_fetch(cloned, cont->end->comppad));
	PL_comppad = ptr_table_fetch(cloned, cont->end->comppad);
	assert(PL_comppad);
	PL_curpad = AvARRAY(PL_comppad);

	ptr_table_free(cloned);

	for ( i = 0; i < cont->marks_len; i++ ) {
		PUSHMARK( SP + cont->marks[i] );
	}

	if ( stack_len ) {
		EXTEND(SP, stack_len);
		Copy(stack, SP + 1, stack_len, SV *);
		SP += stack_len;

		PUTBACK;
	}
}

static cont_t *create_cont (pTHX) {
	cont_t *cont;

	Newx(cont, 1, cont_t);
	init_cont(cont);

	return cont;
}

static void destroy_cont (pTHX_ cont_t *cont) {
	/* restore the state including the savestack, with a DESTRUCTOR_X for
	 * Safefree(cont) at the top, and then leave_scope() to destroy everything */

	/* all the localizations should be neutralized in the savestack, and the
	 * tmps could just be an AV free */

	/* make sure to redecrement the refcount for stacked values properly */

	ENTER;

	SSGROW(PL_savestack_ix + cont->defer_len);
	Copy(cont->defer_saves, &PL_savestack[PL_savestack_ix], cont->defer_len, ANY *);
	PL_savestack_ix += cont->defer_len;

	LEAVE;

}

static void invoke_hook (pTHX) {
	dSP;
	I32 i;

	trace("invoking\n");

	trace("SP=%p, MARK=%d\n", SP, TOPMARK);

	restore_cont(MY_CXT.cont, MY_CXT.retop);
	MY_CXT.cont = NULL;
	MY_CXT.retop = NULL;

	SPAGAIN;

	trace("SP=%p, MARK=%d\n", SP, TOPMARK);

	trace("appending extra args\n");

	EXTEND(SP, MY_CXT.items);

	PUSHMARK(SP);
	for ( i = 0; i < MY_CXT.items; i++ ) {
		SV *sv = MY_CXT.args[i];

		SvREFCNT_inc(sv);
		mPUSHs(MY_CXT.args[i]);
	}

	PUTBACK;

	trace("SP=%p, MARK=%d\n", SP, TOPMARK);

	Safefree(MY_CXT.args);
	MY_CXT.items = 0;
}

XS(XS_Continuation__Delimited_cont_invoke); /* prototype to pass -Wmissing-prototypes */
XS(XS_Continuation__Delimited_cont_invoke)
{
	trace("stashing args, SP=%p\n", PL_stack_sp);
#ifdef dVAR
	dVAR; dXSARGS;
#else
	dXSARGS;
#endif
	/* stash the arguments */
	trace("stashing args, SP=%p, items=%d\n", SP, items);
	SP -= items;
	Newx(MY_CXT.args, items, SV *);
	Copy(SP + 1, MY_CXT.args, items, SV *);
	MY_CXT.items = items;
	trace("stashed args, SP=%p, items=%d\n", SP, items);

	PUTBACK;

	MY_CXT.block = NULL;
	MY_CXT.cont = (cont_t *)XSANY.any_ptr;
	MY_CXT.retop = PL_op->op_next;

	setup_trampoline(aTHX_ invoke_hook);
}

static CV *cont_to_cv (pTHX_ cont_t *cont) {
	CV *cv = newXS(NULL, XS_Continuation__Delimited_cont_invoke, "Delimited.xs");
	sv_2mortal((SV *)cv);

	XSANY.any_ptr = (ANY *)cont;

	/* FIXME add free magic to call destroy_cont */

	return cv;
}

static void stackdump (pTHX) {
	delim_t delim;
	//trace("====orz\n");
	init_delim(&delim);
	print_delim(&delim);
	trace("curpad=%p comppad=%p\n", PL_curpad, PL_comppad);
	//sv_dump((SV *)PL_comppad);
	//debstack();
}

static void cont_shift_hook (pTHX) {
	dSP;

	cont_t *cont = create_cont(aTHX);
	CV *cont_cv = cont_to_cv(aTHX_ cont);

	/* create_cont modifies the stacks */
	SPAGAIN;
	PUSHMARK(SP);
	XPUSHs(sv_2mortal(newRV_inc((SV *)cont_cv)));
	PUTBACK;
	push_block(aTHX);
}

#define TRAMPOLINE(hook) PUTBACK, setup_trampoline(aTHX_ hook);

MODULE = Continuation::Delimited		PACKAGE = Continuation::Delimited


BOOT:
{
	MY_CXT_INIT;

	MY_CXT.last_mark = NULL;
}

void
cont_shift (CV *block)
	PROTOTYPE: &
	PPCODE:
		/* these operations should execute without the XSUB scope, and unwinding it is
		 * tricky so we use SAVEDESTRUCTOR_x to override PL_op. This lets us execute
		 * code in the context of an opcode instead of in the context of an XSUB
		 *
		 * we could also rewrite the reset { } and shift { } calls when statically
		 * bound to have a different op_ppaddr but this seems more reliable */

		MY_CXT.block = block;
		SvREFCNT_inc(block);

		TRAMPOLINE(cont_shift_hook);

		XSRETURN(0);

void
cont_reset (CV *block)
	PROTOTYPE: &
	PPCODE:
		MY_CXT.block = block;
		SvREFCNT_inc(block);

		TRAMPOLINE(push_delim);

		XSRETURN(0);

void stk ()
	PPCODE:
		//TRAMPOLINE(stackdump);
		XSRETURN(0);
