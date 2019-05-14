/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 | File:           C.all/lib/lib/random_states.p
 | Purpose:        Random number generator record class (as used by Clisp)
 | Author:         John Williams, Oct  2 1986 (see revisions)
 | Documentation:  HELP * RANDOM_STATES
 | Related Files:
 */

compile_mode :pop11 +strict;

section;

defclass random_state { random_seed };

global vars sys_random_state = consrandom_state(false);


define global apply_random_state(num, state) -> num;
    dlocal ranseed;
    state or sys_random_state -> state;
    random_seed(state) -> ranseed;
    random0(num) -> num;
    ranseed -> random_seed(state)
enddefine;

apply_random_state -> class_apply(random_state_key);


define global new_random_state(state);
    consrandom_state(if state == true then
                        false
                     elseif isnumber(state) then
                        state
                     else
                        random_seed(state or sys_random_state)
                     endif)
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jul  9 1993
        Uses defclass instead of recordclass
--- John Williams, Mar 16 1988
        Added -sys_random_state- and -new_random_state-
 */
