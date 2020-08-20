/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/lr_parser/lib/lr_parse.p
 > Purpose:         LALR(1) Parser Generator: the LR(1) parser
 > Author:          Robert John Duncan, Nov 27 1992 (see revisions)
 > Documentation:   HELP * LR_PARSER, REF * LR_PARSER
 > Related Files:   LIB * LR_TRACE
 */

compile_mode:pop11 +strict;

section $-LR =>
    lr_parse,
    lr_parse_error,
;

uses lr_core;

;;; =========================================================================

define global vars lr_parse_error(item, token_n, state);
    lvars item, token_n, state;
    mishap(item, 1, 'PARSE ERROR');
enddefine;

define global lr_parse(/*input_p, reduce_p, parser*/) with_nargs 3;
    lconstant STACK_INCR = 128;
    lvars   action, i, state, stack_ptr, stack, token_n, item,
            procedure (input_p, reduce_p), defaults, actions, gotos;
    ;;; extract the compressed tables from the parser
    explode(parser_tables(/*parser*/)) -> (defaults, actions, gotos);
    ;;; pop remaining args
    () -> (input_p, reduce_p);
    ;;; initialise the state stack
    initv(STACK_INCR) -> stack;
    STACK_INCR fi_+ 1 -> stack_ptr;
    ;;; initialise the state
    1 -> state;
    ;;; initialise the input
    false -> token_n;
    repeat
        ;;; push the current state
        stack_ptr fi_- 1 -> stack_ptr;
        if stack_ptr == 0 then
            ;;; stack overflow: expand by another chunk
            datalength(stack) -> i;
            move_subvector(
                1, stack,
                STACK_INCR fi_+ 1, initv(i fi_+ STACK_INCR) ->> stack,
                i);
            STACK_INCR -> stack_ptr;
        endif;
        state -> fast_subscrv(stack_ptr, stack);
        ;;; determine the action
        if fast_subscrv(state, actions) ->> action then
            ;;; -action- is a vector of possibilities: use the next input
            ;;; token to select which
            unless token_n then input_p() -> (item, token_n) endunless;
            ;;; action vectors are short, so this loop can be
            ;;; non-interruptable
            compile_mode:vm -bjmpch;
            datalength(action) -> i;
            repeat
                if fast_subscrv(i, action) == token_n then
                    ;;; found an action
                    fast_subscrv(i fi_- 1, action) -> action;
                    ;;; a SHIFT action can only ever come out of an action
                    ;;; vector, so check for that here;
                    ;;; otherwise continue with tests after the loop
                    quitunless(isinteger(action));
                    ;;; SHIFT:
                    ;;; push token value
                    item;
                    ;;; mark it used
                    false -> token_n;
                    ;;; make the new state current
                    action -> state;
                    ;;; continue main loop
                    nextloop(2);
                elseif i == 2 then
                    ;;; nothing found: use the default
                    fast_subscrv(state, defaults) -> action;
                    quitloop;
                else
                    i fi_- 2 -> i;
                endif;
            endrepeat;
            compile_mode:vm +bjmpch;
        else
            ;;; use the default action
            fast_subscrv(state, defaults) -> action;
        endif;
        ;;; do the action (can't be SHIFT)
        if action == "accept" then
            ;;; ACCEPT:
            ;;; discard the terminator symbol
            erase();
            ;;; finish
            quitloop;
        elseif action then
            ;;; REDUCE:
            ;;; action must be a 3-vector of the form
            ;;; {^rule-number ^rhs-length ^lhs-symbol}
            ;;; do the semantic action
            reduce_p(fast_subscrv(1, action));
            ;;; remove a corresponding number of states from the stack
            stack_ptr fi_+ fast_subscrv(2, action) -> stack_ptr;
            ;;; use the goto table of the uncovered state to find the next
            ;;; state to go to
            fast_subscrv(3, action) -> state;   ;;; LHS symbol
            fast_subscrv(fast_subscrv(stack_ptr, stack), gotos) -> action;
            ;;; goto tables are short too, so make this loop non-interruptable
            compile_mode:vm -bjmpch;
            datalength(action) -> i;
            repeat
                if fast_subscrv(i, action) == state then
                    ;;; new state found
                    fast_subscrv(i fi_- 1, action) -> state;
                    quitloop;
                elseif i == 2 then
                    ;;; nothing found: impossible
                    mishap(0, 'IMPOSSIBLE CASE IN PARSER');
                else
                    i fi_- 2 -> i;
                endif;
            endrepeat;
            compile_mode:vm +bjmpch;
        else
            ;;; ERROR:
            lr_parse_error(item, token_n, state);
        endif;
    endrepeat;
enddefine;

endsection;     /* $-LR */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul  1 1993
        No need to check for the parser tables.
        Class apply procedure now defined in "lr_core.p".
 */
