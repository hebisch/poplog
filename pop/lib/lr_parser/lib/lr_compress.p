/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/lr_parser/lib/lr_compress.p
 > Purpose:         LALR(1) Parser Generator: table compression
 > Author:          Robert John Duncan, Nov 27 1992 (see revisions)
 > Documentation:   HELP * LR_PARSER, REF * LR_PARSER
 > Related Files:   LIB * LR_BUILD, * LR_GEN
 */

compile_mode:pop11 +strict;

section $-LR;

uses lr_core;
uses lr_info;

/***********************************************************************

lr_compress(rules, states) -> (defaults, actions, gotos)
    Generates a compact form of the LR(1) parsing tables suitable for
    use by lr_parse. The argument rules is a vector of the original
    grammar rules (excluding rule 0) and states is a vector of the
    parser states as produced by lr_gen.

    The resulting defaults, actions and gotos tables are vectors indexed
    by state number.

    The actions and defaults vectors together determine the possible
    actions for each state. The true action table is a (state X token)
    matrix. Here, the most common reduce action for each state is
    extracted and placed in the defaults table; the remaining actions
    (if any) are encoded as an association list keyed on the token
    numbers. For compactness, each association "list" is actually an
    even-length vector of the form:

        {^action ^key ... ^action ^key}

    A value of <false> in the actions vector indicates that all tokens
    in that state map to the default action. A value of <false> in the
    defaults vector indicates an error.

    An action in the actions or defaults tables can be any of:

        a)  an integer N, indicating a shift to state N (actions table
            only);

        b)  a 3-vector {^N ^L ^S} indicating a reduction by rule N: L is
            the right-hand-side length of the rule and S is the symbol
            number of the left-hand-side;

        c)  the word "accept" meaning that the parse is complete
            (defaults table only);

        d)  <false> meaning error (defaults table only).

    The gotos vector is organised similarly to the actions table, except
    that there is no default goto for any state. The association lists
    in the table map from non-terminal symbol numbers to state numbers.
    The parser should never try to perform a goto which is not in the
    table.

    The space taken up by the tables is minimised by sharing structures
    wherever possible. So each rule has a unique reduce action
    constructed for it, and rows of the actions and gotos tables are
    shared whenever they are the same. The tables should, of course, be
    read-only.

***********************************************************************/


define lr_compress(rules, states) -> (defaults, actions, gotos);
    lvars rules, states, defaults, actions, gotos, reductions;

        ;;; construct a reduce action from a rule
    define lconstant compress_rule(rule);
        lvars rule, rule_n = rule_number(rule);
        consvector(
            rule_n,
            rule_rhs_length(rule),
            nonterminal_number(rule_lhs(rule)),
        3) -> fast_subscrv(rule_n, reductions);
    enddefine;

        ;;; hash a row (vector) from the action or goto tables
    define lconstant row_hash(row) -> code;
        lvars i, row, code = 0;
        fast_for i to datalength(row) do
            lvars n = fast_subscrv(i, row);
            if isinteger(n) then
                code fi_+ n -> code;
            elseif isvector(n) then
                ;;; reduction: use the rule number
                code fi_+ fast_subscrv(1, n) -> code;
            ;;; ignore accept/error entries
            endif;
        endfor;
    enddefine;

        ;;; compare two rows (vector or <false>) from the action or goto
        ;;; tables
    define lconstant row_=(row1, row2);
        lvars i, row1, row2;
        returnunless(row1 and row2)(row1 == row2);
        returnunless(datalength(row1) == datalength(row2))(false);
        fast_for i to datalength(row1) do
            returnunless(fast_subscrv(i,row1) == fast_subscrv(i,row2))(false);
        endfor;
        true;
    enddefine;

        ;;; add a row to the cache
    define lconstant row_insert(row, cache) -> row;
        lvars row, cache;
        row -> cache(row);
    enddefine;

        ;;; cache rows in the action and goto tables to maximise sharing
    define lvars row_cache =
        newanyproperty([], 2 fi_* datalength(states), false, false,
            row_hash, row_=, "perm", false, row_insert);
    enddefine;

        ;;; compact encoding for actions
    define lconstant compress_action(action) -> action;
        lvars action, n, rule;
        if ispair(action) then
            ;;; conflict: take the first alternative
            fast_front(action) -> action;
        endif;
        if isstate(action) then
            ;;; shift: replace with the state number
            state_number(action) -> action;
        elseif isrule(action) then
            ;;; reduce: replace with the corresponding reduction
            fast_subscrv(rule_number(action), reductions) -> action;
        ;;; accept or error stays unchanged
        endif;
    enddefine;

        ;;; compress the actions for a state into a vector of pairs
        ;;;     (action, token-n)
        ;;; where action is a compressed action code and token-n a
        ;;; terminal symbol number. Returns <false> if there are no
        ;;; actions.
    define lconstant compress_actions(actions);
        lvars action, actions;
        returnif(actions == [])(false);
        row_cache({%
            fast_for action in actions do
                compress_action(fast_back(action)),
                terminal_number(fast_front(action));
            endfor;
        %});
    enddefine;

        ;;; compress the goto transitions for a state into a vector of pairs
        ;;;     (state-n, symbol-n)
        ;;; (both integers) indicating a transition to state-n on symbol-n
    define lconstant compress_gotos(gotos);
        lvars action, gotos;
        returnif(gotos == [])(false);
        row_cache({%
            fast_for action in gotos do
                state_number(fast_back(action)),
                nonterminal_number(fast_front(action));
            endfor;
        %});
    enddefine;

        ;;; add entries for state to the defaults, actions and gotos
        ;;; vectors
    define lconstant compress_state(state);
        lvars state, state_n = state_number(state);
        compress_action(state_default_action(state))
            -> fast_subscrv(state_n, defaults);
        compress_actions(state_actions(state))
            -> fast_subscrv(state_n, actions);
        compress_gotos(state_gotos(state))
            -> fast_subscrv(state_n, gotos);
    enddefine;

    initv(datalength(rules)) -> reductions; ;;; work space
    initv(datalength(states)) -> defaults;
    initv(datalength(states)) -> actions;
    initv(datalength(states)) -> gotos;
    appdata(rules, compress_rule);
    appdata(states, compress_state);
enddefine;

endsection;     /* $-LR */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul  1 1993
        Simplified by compressing all the rules first
 */
