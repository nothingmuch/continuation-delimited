#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "ppport.h"


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

	ANY *saves;
	I32 saves_len;

	I32 *scopes;
	I32 scopes_len;

	PERL_CONTEXT *cxs;
	I32 cxs_len;

	AV *pads;
	AV *cvs;

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
	       marker->prev, marker->stack, marker->marks, marker->tmps, marker->scopes, marker->saves, marker->cxs
	);
}

static void print_cont (cont_t *cont) {
	printf("stack %p (%d)\n"
	       "marks %p (%d)\n"
	       "tmps %p (%d)\n"
	       "saves %p (%d)\n"
	       "scopes %p (%d)\n"
	       "cxs %p (%d)\n"
	       "pads %p (%d)\n"
	       "cvs %p (%d)\n",
	       cont->stack, av_len(cont->stack),
	       cont->marks, cont->marks_len,
	       cont->tmps, cont->tmps_len
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
	SAVEDESTRUCTOR_X(setup_trampoline_cb, NULL);
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

	marker->curpad = PL_curpad;
	marker->comppad = PL_comppad;

	printf("init delim, SP=%p\n", PL_stack_sp);
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
	printf("stack len: %d\n", stack_len);
	for ( i = 0; i < stack_len; i++ ) {
		SV *sv = PL_stack_base[start->stack + i];
		SvREFCNT_inc(sv);
		av_push(stack, sv);
		sv_dump(sv);
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

static void init_cont_scopes (pTHX_ cont_t *cont) {
	delim_t *start = cont->start;
	delim_t *end   = cont->end;
	dVAR;
	I32 i;
	PERL_CONTEXT *cxs;
	PERL_CONTEXT *cx;
	I32 *scopes;
	AV *pads = newAV();
	AV *cvs = newAV();
	/* save scope objects, and also pads */

	cont->cxs_len    = end->cxs - start->cxs;
	cont->scopes_len = end->scopes - start->scopes;

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

			if ( CxTYPE(cx) == CXt_SUB ) {
				CV *cv = cx->blk_sub.cv;

				SV *pad = av_pop(CvPADLIST(cv));

				SvREFCNT_inc((SV *)cv);
				av_push(cvs, (SV *)cv);

				av_push(pads, pad);

				/* unwind the ctx */
				CvDEPTH(cv) = cx->blk_sub.olddepth;

				/* 0 based offset logic for non reified @_ */
				if ( !AvREAL(cx->blk_sub.argarray) ) {
					printf("non reified @_\n");
					CLEAR_ARGARRAY(cx->blk_sub.argarray);
				}
			}
		}
	} else {
		cxs = NULL;
	}

	if ( cont->scopes_len ) {
		Newx(scopes, cont->scopes_len, I32);

		for ( i = 0; i < cont->scopes_len; i++ ) {
			scopes[i] = PL_scopestack[i + start->scopes] - start->saves;
		}
	} else {
		scopes = NULL;
	}

	cxstack_ix       = start->cxs;
	PL_scopestack_ix = start->scopes;

	cont->cxs = cxs;
	cont->scopes = scopes;
	cont->pads = pads;
	cont->cvs = cvs;
}

static void init_cont_saves (pTHX_ cont_t *cont) {
	delim_t *start = cont->start;
	delim_t *end   = cont->end;
	I32 i;
	SV **tmps;
	ANY *saves;

	cont->tmps_len  = end->tmps  - start->tmps;
	cont->saves_len = end->saves - start->saves;

	Newx(tmps, cont->tmps_len, SV *);
	Copy(&PL_tmps_stack, tmps, cont->tmps_len, SV **);

	Newx(saves, cont->saves_len, ANY);
	Copy(&PL_savestack[start->saves], saves, cont->saves_len, SV **);

	PL_tmps_ix      = start->tmps;
	PL_savestack_ix = start->saves;

	cont->tmps  = tmps;
	cont->saves = saves;

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

	/* printf("from\n"); */
	/* print_delim(start); */
	/* printf("to\n"); */
	/* print_delim(&end); */

	init_cont_stack(aTHX_ cont);
	init_cont_scopes(aTHX_ cont);
	init_cont_saves(aTHX_ cont);
	init_cont_state(aTHX_ cont);

	/* init_delim(aTHX_ &end); */
	/* printf("after\n"); */
	/* print_delim(&end); */
}

/* copy values from the saved continuation into the live interpreter
 *
 * this operation should be invokable multiple times */
static void restore_cont (pTHX_ cont_t *cont, OP *retop) {
	I32 i, end;
	dSP;
	SV **stack = AvARRAY(cont->stack);
	I32 stack_len = av_len(cont->stack) + 1;
	I32 cvs_len = av_len(cont->cvs) + 1;

	/* no need for tmps, the refcnt is at least 1 so still live, and they will
	 * be decremented when the continuation is destroyed  */

	for ( i = 0; i < cont->scopes_len; i++ ) {
		PL_scopestack[++PL_scopestack_ix] = cont->scopes[i] + PL_savestack_ix;
	}

	/* restore all the interpreter variables to the state at the end of the  */
#define VAR(name, type) PL_ ## name = cont->end->name;
#include "state.h"
#undef VAR

	/* clone the pads, fixup the contexts */

	end = cxstack_ix + cont->cxs_len;

	/* FIXME, this is horrible, but there's nothing in the Perl api for it */
	if ( cxstack_ix + cont->cxs_len > cxstack_max )
		Renew(cxstack, end, PERL_CONTEXT);

	Copy(cont->cxs, &cxstack[cxstack_ix+1], cont->cxs_len, PERL_CONTEXT);

	/* setup top context */
	cxstack[cxstack_ix+1].blk_sub.retop     = retop;
	cxstack[cxstack_ix+1].blk_sub.savearray = GvAV(PL_defgv);
	// cxstack[cxstack_ix+1].blk_gimme = GIMME_V; /* FIXME won't propagate */

	while ( cxstack_ix < end ) {
		PERL_CONTEXT *cx = &cxstack[++cxstack_ix];
		AV *args;

		/* convert the 0 based offsets to match the current interpreter state */
		cx->blk_oldsp      += PL_stack_sp      - PL_stack_base;
		cx->blk_oldmarksp  += PL_markstack_ptr - PL_markstack;
		cx->blk_oldscopesp += PL_scopestack_ix;

		if ( CxTYPE(cx) == CXt_SUB ) { /* FIXME fallthrough for CXt_BLOCK comppad handling */
			printf("copying pad\n");
			CV *cv  = cx->blk_sub.cv;
			AV *pad_av = cx->blk_sub.oldcomppad;
			AV *names_av = (AV *)AvARRAY(CvPADLIST(cv))[0];
			SV **pad   = AvARRAY(pad_av);
			SV **names = AvARRAY(names_av);
			I32 pad_fill   = AvFILLp(pad_av);
			I32 names_fill = AvFILLp(names_av);

			printf("names fill: %d, pad_fill: %d\n", names_fill, pad_fill);

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
							new = (SV *)newAV();
						} else if ( sigil == '%' ) {
							new = (SV *)newHV();
						} else {
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
							new = newSVsv(sv);
							break;
						case SVt_PVAV:
							new = (SV *)newAV();
							break;
						case SVt_PVHV:
							new = (SV *)newHV();
							break;
						default:
							printf("WTF\n");
							new = newSV(0);
					}
				}

				av_store(copy, i, new);
			}

			args = newAV();
			av_store(copy, 0, (SV *)args); /* FIXME what does pad_push do? */
			cx->blk_sub.savearray = GvAV(PL_defgv);
			GvAV(PL_defgv) = SvREFCNT_inc_simple(args);
			cx->blk_sub.argarray = args;

			/* FIXME need to work out sp/mark from oldsp and friends to count args and copy */

			cx->blk_sub.olddepth = CvDEPTH(cv);

			CvDEPTH(cv)++;
			av_push(CvPADLIST(cv), (SV *)copy);

			cx->blk_sub.oldcomppad = copy;

			if ( pad_av == cont->end->comppad ) {
				printf("stashing comppad %p, replacing with %p\n", PL_comppad, copy);
				SAVECOMPPAD(); /* FIXME horrible kludge, need to fixup savestack like we will for local() */
				PL_comppad = copy;
			}
		}
	}

	PL_curpad = AvARRAY(PL_comppad);

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

	SSGROW(PL_savestack_ix + cont->saves_len);
	Copy(cont->saves, &PL_savestack[PL_savestack_ix], cont->saves_len, ANY *);
	PL_savestack_ix += cont->saves_len;
}

static void invoke_hook (pTHX) {
	dSP;
	I32 i;

	printf("invoking\n");

	printf("SP=%p, MARK=%d\n", SP, TOPMARK);

	restore_cont(MY_CXT.cont, MY_CXT.retop);
	MY_CXT.cont = NULL;
	MY_CXT.retop = NULL;

	SPAGAIN;

	printf("SP=%p, MARK=%d\n", SP, TOPMARK);

	printf("appending extra args\n");

	EXTEND(SP, MY_CXT.items);

	PUSHMARK(SP);
	for ( i = 0; i < MY_CXT.items; i++ ) {
		SV *sv = MY_CXT.args[i];

		SvREFCNT_inc(sv);
		mPUSHs(MY_CXT.args[i]);
	}

	PUTBACK;

	printf("SP=%p, MARK=%d\n", SP, TOPMARK);

	Safefree(MY_CXT.args);
	MY_CXT.items = 0;
}

XS(XS_Continuation__Delimited_cont_invoke); /* prototype to pass -Wmissing-prototypes */
XS(XS_Continuation__Delimited_cont_invoke)
{
	printf("stashing args, SP=%p\n", PL_stack_sp);
#ifdef dVAR
	dVAR; dXSARGS;
#else
	dXSARGS;
#endif
	/* stash the arguments */
	printf("stashing args, SP=%p, items=%d\n", SP, items);
	sv_dump(*SP);
	SP -= items;
	Newx(MY_CXT.args, items, SV *);
	Copy(SP + 1, MY_CXT.args, items, SV *);
	MY_CXT.items = items;
	printf("stashed args, SP=%p, items=%d\n", SP, items);

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

static void cont_shift_hook (pTHX) {
	dSP;

	PL_stack_sp--; /* Enforce some insanity in scalar context. */

	cont_t *cont = create_cont(aTHX);
	CV *cont_cv = cont_to_cv(aTHX_ cont);

	/* create_cont modifies the stacks */
	SPAGAIN;
	PUSHMARK(SP);
	XPUSHs(sv_2mortal(newRV_inc((SV *)cont_cv)));
	PUTBACK;
	push_block(aTHX);
}

static void cont_reset_hook (pTHX) {
	PL_stack_sp--; /* Enforce some insanity in scalar context. */
	push_delim(aTHX);
}

static void stackdump (pTHX) {
	printf("SP=%p mark=%p\n", PL_stack_sp, TOPMARK);
	printf("curpad=%p comppad=%p\n", PL_curpad, PL_comppad);
	sv_dump(PL_comppad);
	debstack();
}

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

		setup_trampoline(aTHX_ cont_shift_hook);

		XSRETURN(0);

void
cont_reset (CV *block)
	PROTOTYPE: &
	PPCODE:
		MY_CXT.block = block;
		SvREFCNT_inc(block);

		setup_trampoline(aTHX_ push_delim);

		XSRETURN(0);

void stk ()
	PPCODE:
		setup_trampoline(aTHX_ stackdump);
		XSRETURN(0);
