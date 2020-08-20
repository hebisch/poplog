/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/lr_parser/lib/lr_state.p
 > Purpose:         LALR(1) Parser Generator: accessing the parser states
 > Author:          Robert John Duncan, Nov 27 1992 (see revisions)
 > Documentation:   HELP * LR_PARSER, REF * LR_PARSER
 > Related Files:   LIB * LR_CORE, * LR_INFO
 */

compile_mode:pop11 +strict;

section $-LR =>
    lr_state_end_token,
    lr_state_max,
    lr_state_tokens,
    lr_state_actions,
    lr_state_shift,
    lr_state_reduce,
    lr_state_symbols,
    lr_state_goto,
    lr_state_items,
    lr_state,
;

uses lr_core;
uses lr_info;

;;;=======================================================================


global vars
    lr_state_end_token = termin,
        ;;; special token indicating end of input
;

;;; lr_state_max:
;;;     returns the number of states in the parser

define global lr_state_max(parser) -> n;
    lvars parser, n, tables;
    if parser_tables(Checkr_parser(parser) ->> parser) ->> tables then
        datalength(parser_defaults(tables)) -> n;
    else
        datalength(parser_states(Checkr_parser_info(parser))) -> n;
    endif;
enddefine;

;;; lr_state_tokens:
;;;     returns a list of tokens which have actions in state -n- of the
;;;     parser, plus a flag to indicate whether there's a default action
;;;     for other tokens not listed

define global lr_state_tokens(n, parser) -> (token_list, default);
    lvars n, parser, token_list, default, symbols, info;
    Checkr_parser_symbols(parser) -> symbols;
    if parser_info(symbols) ->> info then
        lvars action, state = subscrv(n, parser_states(info));
        [%  for action in state_actions(state) do
                lvars token = terminal_name(front(action));
                if token == END_TOKEN then lr_state_end_token else token endif;
            endfor;
        %] -> token_list;
        state_default_action(state) /== false -> default;
    else
        lvars tables = parser_tables(parser), actions;
        [%  if subscrv(n, parser_actions(tables)) ->> actions then
                lvars i, tokens = parser_terminal_symbols(symbols);
                for i from 2 by 2 to datalength(actions) do
                    lvars token_n = fast_subscrv(i, actions);
                    if token_n == 0 then
                        lr_state_end_token;
                    else
                        subscrv(token_n, tokens);
                    endif;
                endfor;
            endif;
        %] -> token_list;
        subscrv(n, parser_defaults(tables)) /== false -> default;
    endif;
enddefine;

;;; lr_state_actions:
;;;     returns the action(s) associated with a token in state -n- of the
;;;     parser: -shift- will be a state number if the token has a shift
;;;     action, or <false> otherwise; -reductions- will be a list of
;;;     rule numbers for all possible reduce actions (NB: multiple actions
;;;     are possible in the presence of conflicts, but only where the
;;;     input is a parser -- parse procedures have all conflicts stripped
;;;     out)

define global lr_state_actions(token, n, parser) -> (shift, reductions);
    lvars token, n, parser, (shift, reductions) = (false, []), symbols, info;
    Checkr_parser_symbols(parser) -> symbols;
    if parser_info(symbols) ->> info then
        lvars i, state = subscrv(n, parser_states(info)),
            action = state_default_action(state);
        if token == lr_state_end_token then END_TOKEN -> token endif;
        for i in state_actions(state) do
            if terminal_name(front(i)) == token then
                fast_back(i) -> action;
                quitloop;
            endif;
        endfor;
        if action == "accept" then
            [0] -> reductions;
        elseif action then
            unless islist(action) then [^action] -> action endunless;
            if isstate(front(action)) then
                state_number(fast_front(action)) -> shift;
                fast_back(action) -> action;
            endif;
            maplist(action, rule_number) -> reductions;
        endif;
    else
        lvars tables = parser_tables(parser),
            action = subscrv(n, parser_defaults(tables)), actions;
        if subscrv(n, parser_actions(tables)) ->> actions then
            lvars i, tokens = parser_terminal_symbols(symbols);
            for i from 2 by 2 to datalength(actions) do
                lvars token_n = fast_subscrv(i, actions);
                if token_n == 0 and token == lr_state_end_token
                or subscrv(token_n, tokens) == token
                then
                    fast_subscrv(i fi_- 1, actions) -> action;
                    quitloop;
                endif;
            endfor;
        endif;
        if action == "accept" then
            [0] -> reductions;
        elseif isinteger(action) then
            action -> shift;
        elseif isvector(action) then
            [^(action(1))] -> reductions;
        endif;
    endif;
enddefine;

;;; lr_state_shift:
;;;     if token has a shift action in state n of the parser, returns its
;;;     state number or <false> otherwise

define global lr_state_shift(token, n, parser) -> shift;
    lvars token, n, parser, (shift,) = lr_state_actions(token, n, parser);
enddefine;

;;; lr_state_reduce:
;;;     if token has a reduce action in state n of the parser, returns its
;;;     rule number, or <false> otherwise

define global lr_state_reduce(token, n, parser) -> rule_n;
    lvars token, n, parser, (, rules) = lr_state_actions(token, n, parser),
        rule_n = if rules == [] then false else fast_front(rules) endif;
enddefine;

;;; lr_state_symbols:
;;;     returns a list of symbols which have goto transitions in state -n-
;;;     of the parser

define global lr_state_symbols(n, parser) -> symbol_list;
    lvars n, parser, symbol_list, symbols, info;
    Checkr_parser_symbols(parser) -> symbols;
    if parser_info(symbols) ->> info then
        lvars i, state = subscrv(n, parser_states(info));
        [%  for i in state_gotos(state) do
                nonterminal_name(front(i));
            endfor;
        %] -> symbol_list;
    else
        lvars tables = parser_tables(parser), gotos;
        [%  if subscrv(n, parser_gotos(tables)) ->> gotos then
                parser_nonterminal_symbols(symbols) -> symbols;
                lvars i;
                for i from 2 by 2 to datalength(gotos) do
                    subscrv(fast_subscrv(i, gotos), symbols);
                endfor;
            endif;
        %] -> symbol_list;
    endif;
enddefine;

;;; lr_state_goto:
;;;     if symbol has a goto transition in state -n- of the parser, then
;;;     return the target state number

define global lr_state_goto(symbol, n, parser) -> m;
    lvars symbol, n, parser, m = false, symbols, info;
    Checkr_parser_symbols(parser) -> symbols;
    if parser_info(symbols) ->> info then
        lvars i, state = subscrv(n, parser_states(info));
        for i in state_gotos(state) do
            if symbol == nonterminal_name(front(i)) then
                state_number(fast_back(i)) -> m;
            endif;
            return;
        endfor;
    else
        lvars tables = parser_tables(parser), gotos;
        if subscrv(n, parser_gotos(tables)) ->> gotos then
            parser_nonterminal_symbols(symbols) -> symbols;
            lvars i;
            for i from 2 by 2 to datalength(gotos) do
                if symbol == subscrv(fast_subscrv(i, gotos), symbols) then
                    fast_subscrv(i fi_- 1, gotos) -> m;
                endif;
                return;
            endfor;
        endif;
    endif;
enddefine;

;;; lr_state_items:
;;;     returns the list of LR(0) items from which parser state n is
;;;     constructed. An item is a pair of rule number and dot position.
;;;     (NB: doesn't work on parsers which have been stripped)

define global lr_state_items(n, parser) -> item_list;
    lvars n, parser, item_list, state;
    subscrv(n, parser_states(Checkr_parser_info(parser))) -> state;
    [%  lvars item;
        for item in state_kernel_items(state) do
            lvars i = 0;
            while item_symbol(item) do
                item_next(item) -> item;
                i + 1 -> i;
            endwhile;
            item_next(item) -> item;
            ;;; item is now the rule
            conspair(rule_number(item), rule_rhs_length(item) + 1 - i);
        endfor;
    %] -> item_list;
enddefine;

global vars lr_state = true;    ;;; for USES

endsection;     /* $-LR */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul  1 1993
        No need to check for the parser tables.
 */
