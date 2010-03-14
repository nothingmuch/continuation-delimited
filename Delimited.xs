#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "ppport.h"

#include "hook_xsub_callasop.h"
#include "magical_hooker_decorate.h"
#include "xs_object_magic.h"

//#define DEBUG

#ifdef DEBUG
#define trace( format, args... ) fprintf( stderr, format, ##args )
#define debug( args... ) args
#else
#define trace(...)
#define debug(...)
#endif

#ifndef GROW
#define GROW(old) ((old) * 3 / 2)
#endif


#define PERL_VERSION_ATLEAST(a,b,c)				\
  (PERL_REVISION > (a)						\
   || (PERL_REVISION == (a)					\
       && (PERL_VERSION > (b)					\
           || (PERL_VERSION == (b) && PERL_SUBVERSION >= (c)))))




#define MY_CXT_KEY "Continuation::Delimited::_guts" XS_VERSION

static char private = 0;

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


/* FIXME
 *
 * For Perl space exposure, flatten to single mixed stack of objects, [
 * SV(...), MARK, SCOPE(...), etc, merging the order of the items correctly.
 *
 * reconstructing a cont_t from this is a simple matter of walking that stack
 * and putting elements on the right stack */



/* misc structures, used for trampolining etc. should mostly go away once code
 * stabilizes a bit */

typedef struct {
	delim_t *last_mark;

	/* used by invoke, like trampoline_save_* */
	CV     *saved_block;
	cont_t *saved_cont;
	OP     *saved_retop;
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


/* stashes a block for future invocation. Since the trampoline can't pass
 * values using the stack, we have to save it temporarily. This is used by
 * delimit { } and suspend { }'s calling convention */
static void trampoline_save_block (pTHX_ CV *block) {
	dMY_CXT;

	assert(MY_CXT.saved_block == NULL);

	assert(block);
	assert(SvTYPE((SV *)block) == SVt_PVCV);

	MY_CXT.saved_block = block;
	SvREFCNT_inc(block);
}


/* pushes the stashed CV from MY_CXT for the entersub trampoline */
static void push_saved_block (pTHX) {
	dSP;
	dMY_CXT;

	assert(MY_CXT.saved_block != NULL);

	XPUSHs((SV *)MY_CXT.saved_block);
	PUTBACK;

	MY_CXT.saved_block = NULL;
}

/* this is like an entersub ppaddr but lets us invoke a hook first. it's used
 * to inject a call to the blocks given to reset { } and shift { } without the
 * XSUB context on the stack */
static OP *invoke_saved_block (pTHX_ SV *arg) {
	dSP;
	dMY_CXT;

	PUSHMARK(SP);

	if ( arg != NULL ) {
		XPUSHs(arg);
		PUTBACK;
	}

	push_saved_block(aTHX);

	return PL_ppaddr[OP_ENTERSUB](aTHX);
}

static OP *invoke_saved_block_no_args (pTHX) {
	return invoke_saved_block(aTHX_ NULL);
}

/* FIXME this should move into say B::Hooks::XSUB::CallAsOp or somesuch */

/* setup PL_op to trampoline into a block without an XSUB context */



/* captures values from the interpreter state */
static void init_delim (pTHX_ delim_t *delim) {
	dMY_CXT;

	delim->prev = MY_CXT.last_mark;

	/* pointers have to be converted to relative indices */
	delim->stack  = PL_stack_sp      - PL_stack_base;
	delim->marks  = PL_markstack_ptr - PL_markstack;

	/* these relative indices are not reused when resetting the state on
	 * restoration, so they aren't in state.h */
	delim->tmps   = PL_tmps_ix;
	delim->cxs    = cxstack_ix;
	delim->scopes = PL_scopestack_ix;
	delim->saves  = PL_savestack_ix;

#define VAR(name, type) delim->name = PL_ ## name;
#include "state.h"
#undef VAR

	delim->curpad  = PL_curpad;
	delim->comppad = PL_comppad;

	trace("init delim, SP=%p\n", PL_stack_sp);
}

static delim_t* create_delim (pTHX) {
	delim_t *delim;

	Newx(delim, 1, delim_t);
	init_delim(aTHX_ delim);

	return delim;
}

static void destroy_delim (pTHX_ delim_t *delim) {
	Safefree(delim);
}



/* these functions manage delimiters in the dynamic scopes */

static void pop_delim (pTHX) {
	dMY_CXT;

	delim_t *prev = MY_CXT.last_mark->prev;
	destroy_delim(aTHX_ MY_CXT.last_mark);
	MY_CXT.last_mark = prev;
}

static void delim_destructor (pTHX_ void *ptr) {
	dMY_CXT;

	if ( ptr == MY_CXT.last_mark ) {
		/* unused delimiter, no shift { } captured it */
		pop_delim(aTHX);
	} else {
		/* captured delimiter in a fully reified continuation */
		destroy_delim(aTHX_ ptr);
	}
}

static void push_delim (pTHX) {
	dMY_CXT;

	MY_CXT.last_mark = create_delim(aTHX);
	SAVEDESTRUCTOR_X(delim_destructor, (void *)MY_CXT.last_mark);
	/* FIXME does this unwind at the right time? i think reset { }; shift { }
	 * might cause a stale delimiter to be visible to shift { } */
}




/* moves the values and marks off the main stack, increasing the reference
 * count of the SVs */
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
		trace("i=%d=%p sp=%p\n", i, &PL_stack_base[start->stack + i + 1], PL_stack_sp);
		SV *sv = PL_stack_base[start->stack + i + 1];
		SvREFCNT_inc(sv);
		av_push(stack, sv);
	}

	cont->marks_len = end->marks - start->marks;

	if ( cont->marks_len ) {
		Newx(cont->marks, cont->marks_len, I32);

		for ( i = 0; i < cont->marks_len; i++) {
			trace("i=%d=%p mark ptr=%p\n", i, &PL_markstack[start->stack + i + 1], PL_markstack_ptr);

			cont->marks[i] = PL_markstack[1 + i + start->marks] - start->stack;
			trace("mark: %d (sp=%p, start=%d, orig=%d),\n", cont->marks[i], PL_stack_sp, start->stack, PL_markstack[1+i+start->marks]);
		}
	} else {
		cont->marks = NULL;
	}

	PL_stack_sp      = start->stack + PL_stack_base;
	PL_markstack_ptr = start->marks + PL_markstack;
}

/* moves the PERL_CONTEXTs from the cxstack into the cont
 *
 * offsets are converted to be relative to the start delimiter, instead of the
 * the begining of runtime */
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

			assert(cx->blk_oldsp >= 0);
			assert(cx->blk_oldmarksp >= 0);
			assert(cx->blk_oldscopesp >= 0);

			switch (CxTYPE(cx)) {
				CV *cv;
				AV *padlist;
				SV *pad;

				trace("saw oldcomppad %p\n", cx->blk_sub.oldcomppad);

				case CXt_SUB:
					cv = cx->blk_sub.cv;
					padlist = CvPADLIST(cv);

					assert(AvFILLp(padlist) >= 1); /* don't pop the names */
					assert(CvDEPTH(cv) >= 1); /* don't pop the names */
					assert(CvDEPTH(cv) == 1 + cx->blk_sub.olddepth);
					assert(CvDEPTH(cv) == AvFILLp(padlist));

					pad = AvARRAY(padlist)[AvFILLp(padlist)];
					AvFILLp(padlist)--;

					/* we must always have at least one allocated pad storage array */
					if ( AvFILLp(padlist) == 0 ) {
#if PERL_VERSION_ATLEAST (5,10,0)
						Perl_pad_push(aTHX_ padlist, AvFILLp(padlist) + 1);
#else
						Perl_pad_push(aTHX_ padlist, AvFILLp(padlist) + 1, 1);
#endif
					}

					assert((CvDEPTH(cv) == 1 + AvFILLp(padlist)) || (cx->blk_sub.olddepth == 0 && AvFILLp(padlist) == 1));

					assert(pad);
					assert(SvTYPE(pad) == SVt_PVAV);

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

					assert( CvDEPTH(cv) == 1 + cx->blk_sub.olddepth );

					CvDEPTH(cv) = cx->blk_sub.olddepth;

					/* make sure we didn't stomp the names */
					assert((CvDEPTH(cv) == AvFILLp(padlist)) || (CvDEPTH(cv) == 0 && AvFILLp(padlist) == 1));
					assert(AvARRAY(CvPADLIST(cv))[0]);
					assert(SvTYPE(AvARRAY(CvPADLIST(cv))[0]) == SVt_PVAV);

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


/* moves a single frame (the enum and all associated data) from the save stack
 * to the appropriate target
 *
 * certain values go to the repeat stack (for instance lexical pad
 * manipulation), to be recreated every time the continuation is invoked, and
 * certain values are deferred to be recreated once and unwound when the
 * reified continuation is destroyed */
static void copy_save_frame (pTHX_ delim_t *start, ANY **saves_ptr_ptr, ANY **repeat_ptr_ptr, ANY **defer_ptr_ptr) {
	I32 i;
	ANY *saves_ptr = *saves_ptr_ptr;
	ANY *repeat_ptr = *repeat_ptr_ptr;
	ANY *defer_ptr = *defer_ptr_ptr;

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

	assert((*saves_ptr_ptr - saves_ptr) == (*defer_ptr_ptr - defer_ptr) + (*repeat_ptr_ptr - repeat_ptr));

	*saves_ptr_ptr = saves_ptr;
	*repeat_ptr_ptr = repeat_ptr;
	*defer_ptr_ptr = defer_ptr;
}

/* partition the savestack into the defer_saves and repeat_saves buffers
 *
 * also handles the scopestack, which delimits the savestack */
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

	assert(saves_len <= PL_savestack_ix );

	cont->tmps_len   = end->tmps   - start->tmps;
	cont->scopes_len = end->scopes - start->scopes;

	assert(cont->scopes_len <= PL_scopestack_ix );

	/* wasteful I guess, but who cares */
	Newx(cont->defer_saves,   saves_len, ANY);
	Newx(cont->repeat_saves,  saves_len, ANY);
	Newx(cont->scopes,        cont->scopes_len, I32);

	/* source iterators */
	scopes_ptr = &PL_scopestack[end->scopes - 1];
	saves_ptr  = &PL_savestack[end->saves - 1];

	/* dest iterators, these are preincremented on assignment so they don't have a -1 */
	defer_ptr  = &cont->defer_saves[saves_len];
	repeat_ptr = &cont->repeat_saves[saves_len];
	scopes     = &cont->scopes[cont->scopes_len];

	trace("start_saves=%d end=%d\n", start->saves, end->saves);
	trace("start_scopes=%d end=%d\n", start->scopes, end->scopes);
	trace("first save=%d last=%d\n", PL_scopestack[start->scopes], PL_scopestack[end->scopes]);

	/* we need to copy the save stack one by one from the end to the begining,
	 * because the last element is what denotes the type */

	while ( scopes_ptr >= &PL_scopestack[start->scopes] ) {
		trace("scopes_ptr=%p (%d), base=%p (%d), end=%p\n", scopes_ptr, *scopes_ptr, &PL_scopestack[start->scopes], scopes_ptr - &PL_scopestack[start->scopes], &PL_scopestack[PL_scopestack_ix]);
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

		assert(saves_ptr >= &PL_savestack[PL_scopestack[start->scopes]]);
		assert(saves_ptr >= &PL_savestack[*scopes_ptr - 1]);

		while ( saves_ptr > &PL_savestack[*scopes_ptr] ) {
			trace("scopes ptr=%p, index=%d, scope_base=%p, savestack=%p\n", scopes_ptr, *scopes_ptr, &PL_savestack[*scopes_ptr], PL_savestack);
			trace("repeat_ptr=%p, defer_ptr=%p, saves_ptr=%p\n", repeat_ptr, defer_ptr, saves_ptr);

			trace("save entry: %d\n",saves_ptr->any_i32);

			copy_save_frame(aTHX_ start, &saves_ptr, &repeat_ptr, &defer_ptr);
		}

		assert(saves_ptr == &PL_savestack[*scopes_ptr - 1]);

		/* now create a new scopes entry for the repeat stack only (at
		 * destruction we unwind everything in the defers as a single scope)
		 *
		 * the save entry is actually bogus, we need to convert it into a zero
		 * based offsets based on the differences at the end, but we don't yet
		 * know how many elements we'll end up with, so we keep the pointer
		 * value for now */

		trace("scope end, %p, %d, orig=%d (%p)\n", repeat_ptr, (I32)repeat_ptr, *scopes_ptr - start->saves, &PL_savestack[*scopes_ptr]);
		*--scopes = repeat_ptr - cont->repeat_saves;
		scopes_ptr--;
	}

	/* all scopes were copied */
	assert(scopes == cont->scopes);
	assert(scopes_ptr == &PL_scopestack[start->scopes - 1]);

	/* all saves of all scopes were copied */
	assert(saves_ptr == &PL_savestack[PL_scopestack[start->scopes] - 1]);

	/* copy any remaining SAVEs that were created between reset { and ENTER */
	while ( saves_ptr > &PL_savestack[start->saves] ) {
		copy_save_frame(aTHX_ start, &saves_ptr, &repeat_ptr, &defer_ptr);
	}

	/* all saves were copied */
	assert(saves_ptr == &PL_savestack[start->saves - 1]);

	/* no overflows */
	assert(repeat_ptr >= cont->repeat_saves);
	assert(defer_ptr  >= cont->defer_saves);

	/* correct number of SAVEt's + associated data */
	trace("ptrs: %d + %d == %d\n", (&cont->repeat_saves[saves_len] - repeat_ptr), (&cont->defer_saves[saves_len] - defer_ptr), saves_len);
	assert((&cont->repeat_saves[saves_len] - repeat_ptr) + (&cont->defer_saves[saves_len] - defer_ptr) == saves_len);

	trace("saves len=%d, repeat_ptr=%p, repeat_saves=%p (%d)\n", saves_len, repeat_ptr, cont->repeat_saves, repeat_ptr - cont->repeat_saves );
	cont->repeat_len = saves_len - ( repeat_ptr - cont->repeat_saves );
	cont->defer_len  = saves_len - ( defer_ptr  - cont->defer_saves  );

	assert(cont->repeat_len + cont->defer_len == saves_len);

	/* correct the savestack indices for the relative locations, now that we know where the begining is */
	for ( i = 0; i < cont->scopes_len; i++ ) {
		cont->scopes[i] -= repeat_ptr - cont->repeat_saves;
		trace("scope %d = %d, orig = %d\n", i, cont->scopes[i], PL_scopestack[start->scopes + i] - start->saves);
		assert(cont->scopes[i] <= PL_scopestack[start->scopes + i] - start->saves);

		/* ensure there are fewer SAVEt entries per frame in the filtered savestack than there were in the original */
		debug(if (i) trace("delta=%d, orig delta=%d\n", (cont->scopes[i] - cont->scopes[i-1]), (PL_scopestack[start->scopes + i] - PL_scopestack[start->scopes + i - 1])));
		assert(i == 0 || ( (cont->scopes[i] - cont->scopes[i-1]) <= (PL_scopestack[start->scopes + i] - PL_scopestack[start->scopes + i - 1]) ));
	}

	trace("relocating\n");
	/* relocate the copied stacks to the begining of their buffers */
	//memmove(repeat_ptr, cont->repeat_saves, cont->repeat_len * sizeof(ANY));
	//memmove(defer_ptr,  cont->defer_saves,  cont->defer_len  * sizeof(ANY));
	cont->repeat_saves = repeat_ptr; /* FIXME leaks */
	cont->defer_saves = defer_ptr; /* FIXME leaks */

	PL_tmps_ix       = start->tmps;
	PL_savestack_ix  = start->saves;
	PL_scopestack_ix = start->scopes;

	trace("top save: %d\n", PL_savestack[PL_savestack_ix - 1].any_i32);
	trace("top scope: %d, save ix=%d\n", PL_scopestack[PL_scopestack_ix - 1], PL_savestack_ix);

	Newx(tmps, cont->tmps_len, SV *);
	Copy(&PL_tmps_stack[PL_tmps_ix], tmps, cont->tmps_len, SV **);

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

/* save various state variables, including the delimiter pointers, and reset
 * the interpreter state vars to what they were in the start delimiter */
static void init_cont_state (pTHX_ cont_t *cont) {
	/* reset interpreter state */
#define VAR(name, type) PL_ ## name = cont->start->name;
#include "state.h"
#undef VAR

	assert(PL_op);
	assert(PL_op->op_next);

	assert(PL_comppad);
	assert(SvTYPE(PL_comppad) == SVt_PVAV);

	dMY_CXT;

	PL_comppad = cont->start->comppad;
	PL_curpad  = cont->start->curpad;

	assert(PL_comppad);
	assert(SvTYPE(PL_comppad) == SVt_PVAV);
	assert(PL_curpad == AvARRAY(PL_comppad));

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
	dMY_CXT;

	assert(MY_CXT.last_mark != NULL); /* FIXME if reset() was not called do we
										 have an implicit reset for the top of
										 the program? */

	cont->start = MY_CXT.last_mark;
	cont->end = create_delim(aTHX);

	/* FIXME first verify we aren't in substitution context or any XSUB,
	 * overloading, tie, or whatever between the reset and the shift
	 *
	 * we need to steal from Coro for this */

	init_cont_stack(aTHX_ cont); /* stacked SVs */
	init_cont_cxs(aTHX_ cont); /* PERL_CONTEXT frames */
	init_cont_saves(aTHX_ cont); /* partitions the savestack into defer and repeat, and also does scope */
	init_cont_state(aTHX_ cont); /* state variables rolled back to cont->start values */
}







static void restore_cont_cxs (pTHX_ cont_t *cont, OP *retop, PTR_TBL_t *cloned) {
	I32 i, end, pads;

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
		//debug(cx_dump(cx));

		switch ( CxTYPE(cx) ) {
			case CXt_SUB:
				{
					CV *cv  = cx->blk_sub.cv;
					AV **pad_av_elem = (AV **)av_fetch(cont->pads, pads--, 0);
					assert(pad_av_elem);
					AV *padlist = CvPADLIST(cv);
					AV *pad_av = *pad_av_elem;
					AV *names_av = (AV *)AvARRAY(padlist)[0];
					SV **pad   = AvARRAY(pad_av);
					SV **names = AvARRAY(names_av);
					I32 pad_fill   = AvFILLp(pad_av);
					I32 names_fill = AvFILLp(names_av);

					SvREFCNT_inc(cv);

					trace("BLK_SUB.OLDCOMPPAD = %p\n", cx->blk_sub.oldcomppad);
					trace("pad[%d] = %p\n", pads-1, pad_av);
					trace("cv=%p, cvs[pads]=%p\n", cv, AvARRAY(cont->cvs)[pads+1]);
					debug(assert( (SV *)cv == *av_fetch(cont->cvs, pads+1, 0) ));


					assert(names_fill <= pad_fill);
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

					assert(CvDEPTH(cv) >= 0);

					cx->blk_sub.olddepth = CvDEPTH(cv);

					trace("padfill: %d, depth: %d\n", AvFILLp(padlist), CvDEPTH(cv));
					assert((CvDEPTH(cv) == AvFILLp(padlist)) || (CvDEPTH(cv) == 0 && AvFILLp(padlist) == 1));

					CvDEPTH(cv)++;
					av_store(padlist, CvDEPTH(cv), (SV *)copy);

					assert(AvFILLp(padlist) == CvDEPTH(cv));

					trace("pad %p -> %p\n", pad_av, copy);
					ptr_table_store(cloned, pad_av, copy);

					trace("oldcomppad: %p\n", cx->blk_sub.oldcomppad);
					if ( ptr_table_fetch(cloned, cx->blk_sub.oldcomppad) ) {
						cx->blk_sub.oldcomppad = ptr_table_fetch(cloned, cx->blk_sub.oldcomppad);
						trace("mapped: %p\n", cx->blk_sub.oldcomppad);
					} else {
						trace("BAAAA\n");
						//sv_dump(cx->blk_sub.oldcomppad);
						//sv_dump(cv);
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
		trace("i=%d, savestack-ix=%d, scope=%d\n", i, PL_savestack_ix, cont->scopes[i]);
		PL_scopestack[PL_scopestack_ix++] = PL_savestack_ix + cont->scopes[i];
	}
}


/* recreate all the savestack frames that should be recreated on each
 * invocation, namely pad management (and in the future, localizations) */

static void restore_cont_saves (pTHX_ cont_t *cont, PTR_TBL_t *cloned) {
	I32 i;

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
						SV *mapped = ptr_table_fetch(cloned, PL_savestack[i].any_ptr);
						assert(mapped);
						assert(SvTYPE(mapped) == SvTYPE((SV *)PL_savestack[i].any_ptr));
						assert(SvTYPE(mapped) == SVt_PVAV);
						assert(av_len((AV *)mapped) == av_len((AV *)PL_savestack[i].any_ptr));

						trace("savet comppad %p -> %p\n", PL_savestack[i].any_ptr, mapped);

						PL_savestack[i].any_ptr = mapped;
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

	trace("scopescack_ix=%d\n", PL_scopestack_ix);
	trace("top save: %d\n", PL_savestack[PL_savestack_ix - 1].any_i32);
	trace("top scope: %d, save ix=%d\n", PL_scopestack[PL_scopestack_ix - 1], PL_savestack_ix);
}


/* this is like the other havlf of restore_cont_state, it must be run *after*
 * we've cloned everything in the pads */
static void restore_cont_state (pTHX_ cont_t *cont, PTR_TBL_t *cloned) {
	AV *comppad;

	/* restore all the interpreter variables to the state at the end of the  */
#define VAR(name, type) PL_ ## name = cont->end->name;
#include "state.h"
#undef VAR

	assert(PL_op);
	assert(PL_op->op_next);

	/* fixup PL_comppad to point to the cloned pad corresponding to the top of the stack */
	trace("overwriting PL_comppad=%p from end comppad=%p to %p\n", PL_comppad, cont->end->comppad, comppad);

	assert(PL_comppad);
	assert(SvTYPE(PL_comppad) == SVt_PVAV);

	comppad = (AV *)ptr_table_fetch(cloned, cont->end->comppad);
	PL_comppad = comppad;

	assert(PL_comppad);
	assert(SvTYPE(PL_comppad) == SVt_PVAV);

	PL_curpad = AvARRAY(PL_comppad);
}

/* restore SV values. arguments that have been cloned (appear in the pointer
 * table) are translated into the clones */
void restore_cont_stack (pTHX_ cont_t *cont, PTR_TBL_t *cloned) {
	I32 i;

	for ( i = 0; i < cont->marks_len; i++ ) {
		PUSHMARK( PL_stack_sp + cont->marks[i] );
	}

	if ( av_len(cont->stack) > -1 ) {
		dSP;
		SV **stack = AvARRAY(cont->stack);
		I32 stack_len = av_len(cont->stack) + 1;

		EXTEND(SP, stack_len);

		for ( i = 0; i < stack_len; i++ ) {
			SV *sv = ptr_table_fetch(cloned, *stack);
			if ( !sv ) sv = *stack;
			trace("%p -> %p\n", *stack, sv);
			SP++;
			stack++;
			*SP = sv;
		}

		PUTBACK;
	}
}


/* copy values from the saved continuation into the live interpreter
 *
 * this operation should be invokable multiple times */
static void restore_cont (pTHX_ cont_t *cont, OP *retop) {
	PTR_TBL_t *cloned = ptr_table_new(); /* tracks pointers from proto pads to cloned values */
	dMY_CXT;

	trace("restoring\n");

	push_delim(aTHX); /* create something akin to the start delim_t, but in the new context */

	restore_cont_cxs(aTHX_ cont, retop, cloned); /* clones pad storage and recreates the context stack */
	restore_cont_saves(aTHX_ cont, cloned); /* recreates the save frames from the repeat list */
	restore_cont_state(aTHX_ cont, cloned); /* resets state variables, including comppad from the loned list */
	restore_cont_stack(aTHX_ cont, cloned); /* recreates argument values */

	ptr_table_free(cloned);
}


/* allocates a continuation and captures it destructively */
static cont_t *create_cont (pTHX) {
	cont_t *cont;

	Newx(cont, 1, cont_t);
	init_cont(aTHX_ cont);

	return cont;
}

static void destroy_cont (pTHX_ cont_t *cont) {
	/* restore the state including the savestack, with a DESTRUCTOR_X for
	 * Safefree(cont) at the top, and then leave_scope() to destroy everything */

	/* all the localizations should be neutralized in the savestack, and the
	 * tmps could just be an AV free */

	/* make sure to redecrement the refcount for stacked values properly */

	ENTER;
	SAVETMPS;

	SSGROW(PL_savestack_ix + cont->defer_len);
	Copy(cont->defer_saves, &PL_savestack[PL_savestack_ix], cont->defer_len, ANY *);
	PL_savestack_ix += cont->defer_len;

	/* push the stashed tmps onto the tmps stack */
	EXTEND_MORTAL(cont->tmps_len);
	Copy(cont->tmps, &PL_tmps_stack[PL_tmps_ix + 1], cont->tmps_len, SV **);
	PL_tmps_ix += cont->tmps_len;

	FREETMPS;
	LEAVE;


	/* FIXME should these be destroyed in reverse by popping? */
	sv_2mortal((SV *)cont->stack);
	sv_2mortal((SV *)cont->pads);

	debug(sv_2mortal((SV *)cont->cvs)); /* only present in debug mode */


	Safefree(cont);
}








/* this is the hook used to invoke continuations, it's fired using the
 * trampoline code above to avoid needing to mop up the extra XSUB context */

static TRAMPOLINE_HOOK(resume_hook) {
	dSP;
	I32 i;
	dMY_CXT;

	TRAMPOLINE_RESTORE_OP;

	trace("invoking\n");

	trace("SP=%p, MARK=%d\n", SP, TOPMARK);

	assert(MY_CXT.saved_cont != NULL);
	assert(MY_CXT.saved_retop != NULL);
	restore_cont(aTHX_ MY_CXT.saved_cont, MY_CXT.saved_retop);
	MY_CXT.saved_cont = NULL;
	MY_CXT.saved_retop = NULL;

	SPAGAIN;

	trace("SP=%p, MARK=%d\n", SP, TOPMARK);

	trace("appending extra args\n");

	TRAMPOLINE_RESTORE_ARGS;

	trace("SP=%p, MARK=%d\n", SP, TOPMARK);

	return NORMAL;
}



/* this is a manually declared XSUB because we call newXS to generate anonymous
 * CVs based on it, it's not a static function anywhere
 *
 * note that it is only glorified argument handling and trampoline setup, the
 * actual invocation is done outisde of the XSUB context inside resume_hook
 * (called like an opcode) */

XS(XS_Continuation__Delimited_cont_resume); /* prototype to pass -Wmissing-prototypes */
XS(XS_Continuation__Delimited_cont_resume)
{
#ifdef dVAR
    dVAR; dXSARGS;
#else
    dXSARGS;
#endif
	dMY_CXT;
	cont_t *cont;

	/* stash the continuation stored in the CV */
	assert(PL_op != NULL);
	assert(PL_op->op_next != NULL);

	assert(MY_CXT.saved_cont == NULL);
	assert(MY_CXT.saved_retop == NULL);

	cont = (cont_t *)XSANY.any_ptr;

	assert(cont != NULL);
	MY_CXT.saved_cont = cont;
	MY_CXT.saved_retop = PL_op->op_next;

	/* stash additional things */
	TRAMPOLINE_SAVE_ARGS;
	TRAMPOLINE_SAVE_OP;

	TRAMPOLINE(resume_hook);
}


static SV *cont_to_obj (pTHX_ cont_t *cont) {
	return sv_2mortal(xs_object_magic_create((void *)cont, gv_stashpv("Continuation::Delimited::State", 0)));
}


/* reify a cont_t into a CV */

static CV *cont_to_cv (pTHX_ cont_t *cont) {
	/* create an anonymous CV from the cont_resume hook */
	CV *cv = newXS(NULL, XS_Continuation__Delimited_cont_resume, "Delimited.xs");
	sv_2mortal((SV *)cv);

	/* put the cont_t in the CV's extra pointer */
	XSANY.any_ptr = (ANY *)cont;

	magical_hooker_decoration_set(aTHX_ (SV *)cv, cont_to_obj(cont), (void *)&private);

	return cv;
}





/* debugging aid to print the state of various stack items, invoked using the
 * trampoline hooks */
static TRAMPOLINE_HOOK(stackdump) {
	delim_t delim;
	//trace("====orz\n");
	init_delim(aTHX_ &delim);
	print_delim(&delim);
	trace("curpad=%p comppad=%p\n", PL_curpad, PL_comppad);
	sv_dump((SV *)PL_comppad);
	//sv_dump((SV *)PL_comppad);
	//debstack();

	return NORMAL;
}



/* captures a continuation
 *
 * invoked from the trampoline hook so that the XSUB scope structures do not
 * have to be unwound (effectively called as an opcode in the scope that called
 * suspend */
static TRAMPOLINE_HOOK(suspend_hook) {
	cont_t *cont;
	CV *cont_cv;
	SV *coderef;

	/* restore PL_op so that the delimiter contains the right value, the
	 * current value is set by the trampoline hack */
	TRAMPOLINE_RESTORE_OP;

	cont = create_cont(aTHX);
	cont_cv = cont_to_cv(aTHX_ cont);
	coderef = newRV_inc((SV *)cont_cv);

	sv_2mortal(coderef);

	/* invoke the block with the reified continuation as the only arg */
	invoke_saved_block(aTHX_ coderef);
}

static TRAMPOLINE_HOOK(delimit_hook) {
	/* restore PL_op so that the delimiter contains the right value, the
	 * current value is set by the trampoline hack */
	TRAMPOLINE_RESTORE_OP;

	push_delim(aTHX);

	/* after pushing the delimiter just invoke the saved block normally */
	return invoke_saved_block_no_args(aTHX);
}

static TRAMPOLINE_HOOK(destroy_cont_hook) {
	cont_t *cont;

	assert(MY_CXT.saved_cont != NULL);
	cont = MY_CXT.saved_cont;
	MY_CXT.saved_cont = NULL;

	destroy_cont(cont);

	return NORMAL;
}


MODULE = Continuation::Delimited		PACKAGE = Continuation::Delimited


BOOT:
{
	MY_CXT_INIT;

	MY_CXT.saved_block = NULL;
	MY_CXT.last_mark   = NULL;
}


void
suspend (CV *block)
	PROTOTYPE: &
	PREINIT:
		dMY_CXT;
	PPCODE:
		/* these operations should execute without the XSUB scope, and unwinding it is
		 * tricky so we use SAVEDESTRUCTOR_x to override PL_op. This lets us execute
		 * code in the context of an opcode instead of in the context of an XSUB
		 *
		 * we could also rewrite the reset { } and shift { } calls when statically
		 * bound to have a different op_ppaddr but this seems more reliable */

		trampoline_save_block(aTHX_ block);
		TRAMPOLINE_SAVE_OP;
		TRAMPOLINE(suspend_hook);

void
delimit (CV *block)
	PROTOTYPE: &
	PREINIT:
		dMY_CXT;
	PPCODE:
		trampoline_save_block(aTHX_ block);
		TRAMPOLINE_SAVE_OP;
		TRAMPOLINE(delimit_hook);

void stk ()
	PPCODE:
		TRAMPOLINE(stackdump);


MODULE = Continuation::Delimited		PACKAGE = Continuation::Delimited::State

void
DESTROY (cont_t *cont)
	CODE:
		printf("destroy\n");

		assert(MY_CXT.saved_cont == NULL);
		MY_CXT.saved_cont = cont;

		TRAMPOLINE(destroy_cont_hook);

