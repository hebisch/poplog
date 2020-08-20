/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/lr_parser/lib/lr_trace.p
 > Purpose:         LALR(1) Parser Generator: tracing version of the parser
 > Author:          Robert John Duncan, Nov 27 1992 (see revisions)
 > Documentation:   HELP * LR_PARSER, REF * LR_PARSER
 > Related Files:   LIB * LR_PARSER
 */

compile_mode:pop11 +strict;

section $-LR =>
    lr_trace,
    lr_trace_constree,
    lr_trace_file,
    lr_trace_file_writeable,
    lr_trace_input_width,
    lr_trace_stack_width,
    lr_trace_state_width,
    lr_trace_tracing,
;

uses lr_core;
uses lr_info;
uses lr_parse;  ;;; for -lr_parse_error-

;;; =======================================================================


/*
 *  Trace Output
 */

global vars
    lr_trace_file           = 'lr_trace',
    lr_trace_file_writeable = false,
    lr_trace_input_width    = false,
    lr_trace_stack_width    = false,
    lr_trace_state_width    = false,
    lr_trace_tracing        = true,
;

lvars
    trace_|_char    = `|`,
    trace_+_char    = `+`,
    trace_-_char    = `-`,
;

lconstant
    ;;; unique "tokens"
    ERR_TOKEN   = '$error$',
    NONE        = '$none$',
;

;;; trace_apply:
;;;     establishes the context for trace output.

define lconstant trace_apply();
    dlocal poplinewidth = false;
    if lr_trace_file then
        procedure();
            dlocal  % ved_current_file, vedselect() %;
            dlocal  trace_|_char, trace_+_char, trace_-_char, cucharout,
                    vedbreak, vedautowrite;
            vedselect(lr_trace_file, false);
            vedendfile();
            false ->> vedbreak -> vedautowrite;
            vedcharinsert -> cucharout;
#_IF DEFV pop_internal_version >= 141100
            `\G|` -> trace_|_char;
            `\G+` -> trace_+_char;
            `\G-` -> trace_-_char;
#_ENDIF
            apply();
        endprocedure();
    else
        apply();
    endif;
enddefine;

;;; display:
;;;     writes a line of trace output

define lconstant display(state, stack, input, action, pr_action);
    lvars state, stack, input, action, pr_action;
    if lr_trace_state_width > 0 then
        pr_field(state, lr_trace_state_width, `\s`, false);
        cucharout(trace_|_char);
    endif;
    if lr_trace_stack_width > 0 then
        pr_field(stack, lr_trace_stack_width-1, `\s`, false);
        cucharout(`\s`);
        cucharout(trace_|_char);
    endif;
    if lr_trace_input_width > 0 then
        cucharout(`\s`);
        pr_field(input, lr_trace_input_width-1, false, `\s`);
        cucharout(trace_|_char);
    endif;
    cucharout(`\s`);
    pr_action(action);
    cucharout(`\n`);
enddefine;

;;; title:
;;;     prints and underlines the output field names

define lconstant title();
    dlocal pr = syspr, pop_pr_quotes = false;
    cucharout(`\n`);
    display('State', 'Stack', 'Input', 'Action', pr);
    if lr_trace_state_width > 0 then
        repeat lr_trace_state_width times
            cucharout(trace_-_char);
        endrepeat;
        cucharout(trace_+_char);
    endif;
    if lr_trace_stack_width > 0 then
        repeat lr_trace_stack_width times
            cucharout(trace_-_char);
        endrepeat;
        cucharout(trace_+_char);
    endif;
    if lr_trace_input_width > 0 then
        repeat lr_trace_input_width times
            cucharout(trace_-_char);
        endrepeat;
        cucharout(trace_+_char);
    endif;
    ;;; 7 = datalength('\sAction')
    repeat 7 times cucharout(trace_-_char) endrepeat;
    cucharout(`\n`);
enddefine;

;;; init_trace:
;;;     calculates output field widths and prints the title

define lconstant init_trace(n_states);
    lvars n_states, width;
    returnunless(lr_trace_tracing);
    if lr_trace_file and vedinvedprocess then
        ;;; output to -lr_trace_file-
        trace_apply(
            procedure;
                lr_trace_file_writeable -> vedwriteable;
                vedscreenwidth;
            endprocedure
        ) -> width;
    else
        ;;; output to the screen
        false -> lr_trace_file;
        poplinemax -> width;
    endif;
    unless lr_trace_state_width then
        ;;; max spaces needed for state number
        max(5, datalength(n_states sys_>< nullstring)) -> lr_trace_state_width;
    endunless;
    unless lr_trace_stack_width then
        ;;; 30 columns in an 80-column window, adjusted appropriately
        max(6, (30 * width) div 80) -> lr_trace_stack_width;
    endunless;
    unless lr_trace_input_width then
        ;;; 10 columns in an 80-column window, adjusted appropriately
        max(6, (10 * width) div 80) -> lr_trace_input_width;
    endunless;
    trace_apply(title);
enddefine;

;;; show_state:
;;;     displays the current state of the parser

define lconstant show_state(state, stack, item, action);
    lvars   symbol, symbols, state, stack, item, action;
    dlocal  pr = syspr, pop_pr_quotes = false;

    define lconstant pr_action(action);
        lvars action;
        if isstate(action) then
            printf(state_number(action), 'SHIFT\s\s%p');
        elseif isrule(action) then
            printf(nonterminal_name(rule_lhs(action)), 'REDUCE %p -->');
            rule_rhs(action) -> action;
            while item_symbol(action) do
                printf(symbol_name(item_symbol(action)), ' %p');
                item_next(action) -> action;
            endwhile;
        elseif action == "accept" then
            printf('ACCEPT');
        elseif ispair(action) then
            printf('CONFLICT [');
            pr_action(front(action));
            printf(']');
        else
            printf('ERROR');
        endif;
    enddefine;

    returnunless(lr_trace_tracing);
    state_number(state) -> state;
    (stack, nullstring) -> (symbols, stack);
    until symbols == [] or datalength(stack) >= lr_trace_stack_width do
        destpair(symbols) -> (symbol, symbols);
        ('\s' >< symbol) <> stack -> stack;
    enduntil;
    if item == NONE then nullstring -> item endif;
    trace_apply(state, stack, item, action, pr_action, display);
enddefine;


/*
 *  Tracing version of the LR(1) parser
 */

;;; associating input items with tokens
defclass lconstant tokenitem {
    ti_token,
    ti_item,
};
;;;
procedure(tv);
    lvars tv;
    ;;; e.g. IDENT(foo)
    printf('%p(%p)', [% desttokenitem(tv) %]);
endprocedure -> class_print(tokenitem_key);

;;; default error procedure just returns
define lvars lr_trace_error(item, state);
    lvars item, state;
enddefine;

;;; default constree procedure builds a list
define global vars lr_trace_constree(lhs, n) -> tree;
    lvars lhs, n, tree = conslist(n);
    lhs :: tree -> tree;
enddefine;

define lconstant LR_1(input, reduce_p, state);
    lvars   i, action, state, states, token, stack, item, lhs, sl,
            input, procedure reduce_p, parser;
    stacklength() -> sl;
    [^state] -> states;
    [] -> stack;
    NONE -> item;
    repeat
        ;;; get the next action
        state_default_action(state) -> action;
        unless state_actions(state) == [] then
            ;;; get next input item
            if item == NONE then
                if null(input) then
                    END_TOKEN -> item;
                else
                    fast_destpair(input) -> (item, input);
                endif;
                if istokenitem(item) then
                    ti_token(item)
                else
                    item
                endif -> token;
            endif;
            ;;; search for matching action
            for i in state_actions(state) do
                if terminal_name(front(i)) == token then
                    back(i) -> action;
                    quitloop;
                endif;
            endfor;
        endunless;
        ;;; show where we are
        show_state(state, stack, item, action);
        ;;; is there a valid action?
        unless action then
            ;;; no -- the parse has failed
            ;;; raise the error (may not return)
            lr_trace_error(item, state);
            ;;; erase everything from the stack
            erasenum(stacklength() - sl);
            ;;; return <false>
            return(false);
        endunless;
        ;;; if there's a conflict, select the first choice
        if ispair(action) then
            front(action) -> action;
        endif;
        ;;; action switch
        if action == "accept" then
            ;;; ok -- the parse has succeeded
            ;;; erase the end token and return
            erase();
            return;
        elseif isstate(action) then
            ;;; shift:
            ;;; move to the new state
            action -> state;
            state :: states -> states;
            ;;; consume the item, leaving its value for a later reduction
            token :: stack -> stack;
            if istokenitem(item) then ti_item(item) else item endif;
            NONE -> item;
        elseif isrule(action) then
            ;;; reduce:
            ;;; pop one state for each symbol on the RHS
            allbutfirst(rule_rhs_length(action), states) -> states;
            allbutfirst(rule_rhs_length(action), stack) -> stack;
            ;;; do the reduce action
            reduce_p(rule_number(action));
            ;;; the uncovered state tells us where to go next
            front(states) -> state;
            nonterminal_name(rule_lhs(action)) -> lhs;
            for i in state_gotos(state) do
                if nonterminal_name(front(i)) == lhs then
                    back(i) -> action;
                    quitloop;
                endif;
            endfor;
            unless isstate(action) then
                mishap(state, 1, 'lr_trace: ERROR IN PARSER GOTOS');
            endunless;
            action -> state;
            state :: states -> states;
            lhs :: stack -> stack;
        else
            mishap(state, 1, 'lr_trace: ERROR IN PARSER ACTIONS');
        endif;
    endrepeat;
enddefine;

define global lr_trace(input, parser);
    lvars input, parser, info;
    dlocal
        lr_trace_file,
        lr_trace_input_width,
        lr_trace_stack_width,
        lr_trace_state_width,
        lr_trace_tracing,
    ;
    Checkr_parser_info(parser) -> info;
    if isprocedure(input) then
        ;;; lr_trace(INPUT_P, REDUCE_P, PARSER):
        ;;;     simulates the behaviour of -lr_parse-
        lvars
            (procedure (input_p, reduce_p)) = ((), input),
            tokens = parser_terminal_symbols(Checkr_parser_symbols(parser)),
            n_tokens = datalength(tokens);
        ;;; convert -input_p- to a dynamic list
        pdtolist(
            procedure() -> item;
                lvars (item, item_n) = input_p();
                if item_n == 0 then
                    termin -> item;
                elseif item_n > 0 and item_n <= n_tokens then
                    lvars token = fast_subscrv(item_n, tokens);
                    unless token == item then
                        constokenitem(token, item) -> item;
                    endunless;
                else
                    constokenitem(ERR_TOKEN, item) -> item;
                endif;
            endprocedure
        ) -> input;
        ;;; redefine error procedure to call -lr_parse_error-
        define dlocal lr_trace_error(item, state);
            lvars item, state, token, token_n;
            if istokenitem(item) then
                desttokenitem(item) -> (token, item);
            else
                item -> token;
            endif;
            for token_n to n_tokens do
                quitif(tokens(token_n) == token);
            endfor;
            if token_n > n_tokens then
                if token == END_TOKEN then 0 else -1 endif -> token_n;
            endif;
            lr_parse_error(item, token_n, state_number(state));
            ;;; in case that returns ...
            mishap(item, 1, 'PARSE ERROR');
        enddefine;
    else
        ;;; lr_trace(INPUT, PARSER) -> PARSE_TREE
        ;;;     builds a parse tree from a list of tokens
        lvars rules = parser_rules(info);
        ;;; input must be a list
        unless islist(input) then
            mishap(input, 1, 'LIST NEEDED');
        endunless;
        ;;; default REDUCE_P builds a parse tree
        define lvars reduce_p(i);
            lvars i, rule = subscrv(i, rules);
            lr_trace_constree(rule(1), length(rule) - 1);
        enddefine;
    endif;
    ;;; set up the output
    init_trace(datalength(parser_states(info)));
    ;;; parse
    LR_1(input, reduce_p, parser_states(info)(1));
enddefine;

;;; make the tracer be the -class_apply- of parser structures
procedure(parser);
    lvars   parser, symbols;
    dlocal  lr_trace_tracing = false;
    if (parser_symbols(parser) ->> symbols) and parser_info(symbols) then
        lr_trace(parser);
    else
        lr_parse(parser);
    endif;
endprocedure -> class_apply(parser_key);

endsection;     /* $-LR */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jan  4 1993
        Changed trace output to use -syspr- with -pop_pr_quotes- set false.
 */
