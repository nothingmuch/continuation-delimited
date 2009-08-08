/* copypasted from Coro/state.h
 *
 * these are just the state variables, not the stacks which are handled
 * differently */

VAR(op,                    OP *)           /* currently executing op */
VAR(curpm,                 PMOP *)         /* what to do \ interps in REs from */
VAR(curcop,                COP *)
VAR(in_eval,               int)            /* trap "fatal" errors? */
VAR(comppad_name,          AV *)           /* variable names for "my" variables */
VAR(comppad_name_fill,     I32)            /* last "introduced" variable offset */
VAR(comppad_name_floor,    I32)            /* start of vars in innermost block */
VAR(hints,                 U32)            /* pragma-tic compile-time flags */
