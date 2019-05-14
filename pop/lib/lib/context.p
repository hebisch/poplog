/*  --- Copyright University of Sussex 1995.  All rights reserved. ---------
 >  File:           C.all/lib/lib/context.p
 >  Purpose:        Efficient context switching
 >  Author:         Allan Ramsay, Oct 11 1985 (see revisions)
 >  Documentation:  HELP * CONTEXT
 >  Related Files:
 */

/*  Please see HELP CONTEXT for how to use it, and
 *  "On Efficient Context Switching" (Computer Journal 1985, or
 *  CogStudsResearchPaper 023) for a detailed description of how it works
 */

recordclass constant register reg_index reg_value ;
    /* A register contains an index which says when it was set, and a pointer
     * to the binding stack entry containing its most recently set value */

recordclass constant state s_index s_context ;
    /* A state has an index which says when it was created, and a list of
     * register:value pairs. The first element of this list is the
     * register:value list for the parent context */

section $-lib consregister reg_index reg_value consstate s_index s_context
        => register value_in restart registers new_states @ fork
                            reinstate split all_registers ;

section context consregister reg_index reg_value consstate s_index s_context
        => register value_in restart registers new_states @ fork
                            reinstate split all_registers ;

vars all_registers states current_context current_index ;

nil -> all_registers;

/* Association list lookup - entries are pairs, first element doesn't count */
define constant lookup (key,alist) -> val ;
    lvars val, alist, key ;
    for val in tl(alist)
    do  if front(val) == key then return; endif;
    endfor;
    "undef" -> val;
enddefine;

/* Is i a direct predecessor of index ? */
define constant check_index (i, index) -> t;
    lvars index, i, t ;
    until   (index = i ->> t) or (index < i)
    do      index >> 1 -> index;
    enduntil;
enddefine;

/* Walk down chain of binding stacks looking for entry for register */
define constant find_val (reg, index, current_context) -> res;
    lvars reg, current_context, res ;
    while   (lookup(reg, current_context) ->> res) == "undef"
    do      unless  hd(current_context) ->> current_context
            then    return;
            endunless;
    endwhile;
enddefine;

/* If register's index is valid, return associated value, o.w. search chain
 * of binding stacks, starting with current one */
define constant register (reg) -> r_value;
    lvars r_index, reg, r_index, r_value ;
    reg_index(reg) -> r_index;
    if      check_index(r_index, current_index)
    then    reg_value(reg)
    else    find_val(reg,r_index,current_context)
    endif   -> r_value;
    if ispair(r_value) then fast_back(r_value) -> r_value endif;
enddefine;

/* Update register's value. 3 cases
    - (i)   register has already been updated in current context - just update
            binding stack entry
    - (ii)  register's current value is still valid and is same as new value -
            do nothing
    - (iii) add new entry to binding stack, set register's value field to point
            to this new entry
*/
define updaterof register (new, reg);
    lvars r_index, x, reg, new ;
    reg_index(reg) -> r_index;
    if      current_index = r_index
    then    new -> back(reg_value(reg))
    elseif  new == reg_value(reg) and check_index(r_index, current_index)
    then
    else    current_index -> reg_index(reg);
            conspair(reg,new) ->> x -> reg_value(reg);
            hd(current_context) :: (x:: tl(current_context)) -> current_context;
    endif;
enddefine;

/* A register identifier is a 0-ary operator, whose value is REGISTER
 * partially applied to a new register cell
 */
define macro registers ;
    lvars x, y ;
    until   (readitem() ->> x) == ";"
    do      sysVARS(x,1);
            register(%consregister(0, "undef")%) ->> y -> valof(x);
            unless  member(y, all_registers)
            then    y :: all_registers -> all_registers
            endunless;
    enduntil;
enddefine;

/* Specified state becomes current state */
define constant restart (state);
    lvars state ;
    s_index(state) -> current_index;
    s_context(state) -> current_context;
enddefine;

/* Split the current state, return one version to the user and continue with
 * the other */
define 1 split ;
    [% current_context %] -> current_context;
    current_index << 1 -> current_index;
    consstate(current_index+1, current_context);
enddefine;

/* Split the current state, put one version on global state stack and
 * continue with the other */
define constant 1 fork ;
    [% current_context %] -> current_context;
    current_index << 1 -> current_index;
    consstate(current_index, current_context) :: states -> states;
    current_index + 1 -> current_index;
enddefine;

/* Restart top state from global state stack */
define 1 reinstate ;
    restart(dest(states)->states);
enddefine;

/* Look up the value of a register in some INACTIVE state (useful for things
 * like looking at values in parents or children */
define constant 1 state value_in reg ;
    dlocal current_context, current_index ;
    restart(state);
    popval([%reg%]);
enddefine;

/* Macro packaging up the above */
define macro @ (reg);
    "value_in", """, reg, """;
enddefine;

/* Pretend you've never assigned a value to this register */
define constant reset_register (reg);
    lvars reg ;
    frozval(1, reg) -> reg;
    0 -> reg_index(reg); "undef" -> reg_value(reg);
enddefine;

/* Pretend you've never assigned a value to any registers */
define constant 1 new_states ;
    applist(all_registers,reset_register);
    nil -> states; [% false %] -> current_context; 1 -> current_index;
enddefine;

endsection;

endsection;

vars context; true -> context; /* So "uses" will work properly */



/*  --- Revision History ---------------------------------------------------
--- Allan Ramsay, Oct 29 1985
    Removed calls of EXIT.
--- Allan Ramsay, Oct 29 1985
    Removed a serious stupidity about creation of new contexts
--- Allan Ramsay, Oct 11 1985
    Small changes to take account of the fact that once you've shifted the
    index far enough to the left, it will become a "biginteger". At this point
    it is inappropriate to use == for equality testing.
 */
