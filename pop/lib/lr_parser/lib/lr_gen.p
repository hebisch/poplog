/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/lr_parser/lib/lr_gen.p
 > Purpose:         LALR(1) Parser Generator: generating the parser tables
 > Author:          Robert John Duncan, Nov 27 1992 (see revisions)
 > Documentation:   HELP * LR_PARSER, REF * LR_PARSER
 > Related Files:   LIB * LR_BUILD, * LR_COMPRESS
 */

compile_mode:pop11 +strict;

#_IF not(DEF pop_max_int)
loadlib("int_parameters");
#_ENDIF

section $-LR;

uses lr_core;
uses lr_info;

/***********************************************************************

lr_gen(tokens, symbols, rule0, rules, resolve_p) -> (states, n_sr, n_rr)
    Computes the LALR(1) parsing states and transitions for a grammar
    with the given tokens, symbols and rules. The extra rule0 has the
    form:

        $start --> <start> $end

    where <start> is the start symbol of the original grammar. resolve_p
    is a procedure for resolving conflicts.

    The result states is a vector of the parser states; n_sr and n_rr
    are the numbers of shift/reduce and reduce/reduce conflicts not
    resolved.

    The algorithm for the computation of the LALR(1) lookahead sets
    is taken from:

        Frank DeRemer and Thomas Pennello
        Efficient Computation of LALR(1) Look-Ahead Sets
        ACM TOPLAS, Vol. 4, No. 4, Oct 1982, pp. 615-649

***********************************************************************/

/*
 *  Local Class Definitions
 */

;;; Non-terminal transitions:
defclass lconstant transition {
    transition_symbol,
        ;;; non-terminal symbol labelling this transition
    transition_state,
        ;;; the successor state
    transition_reads,
        ;;; the -reads- relation: a list of transitions
    transition_includes,
        ;;; the -includes- relation: a list of transitions
    transition_set,
        ;;; the -Read- and -Follow- sets
    transition_depth,
        ;;; marker used by -digraph-
};
;;;
procedure(trans);
    lvars trans;
    printf('<transition %p %p>', [%
        transition_symbol(trans), transition_state(trans)
    %]);
endprocedure -> class_print(transition_key);

;;; Final items:
defclass lconstant finalitem {
    finalitem_item,
        ;;; item to reduce
    finalitem_lookaheads,
        ;;; lookahead set
    finalitem_lookbacks,
        ;;; -lookback- relation: a list of transitions
};
;;;
procedure(item);
    lvars item;
    printf('<finalitem %p %p>', [%
        item_next(finalitem_item(item)), finalitem_lookaheads(item)
    %]);
endprocedure -> class_print(finalitem_key);


/*
 *  Lookahead Sets
 */

lconstant
    DIVISOR = integer_length(pop_max_int) + 1,
        ;;; the number of usable bits in a pop integer
;

lvars
    setsize = 0,
        ;;; number of words needed for a lookahead set
        ;;; (varies with the number of terminal symbols)
;

;;; init_sets:
;;;     determine the size of vector needed to represent a set of up to
;;;     -max_size- symbols

define lconstant init_sets(max_size);
    lvars max_size;
    (max_size + DIVISOR - 1) div DIVISOR -> setsize;
enddefine;

;;; new_set:
;;;     returns an empty lookahead set

define lconstant new_set();
    consvector(fast_repeat setsize times 0 endrepeat, setsize);
enddefine;

;;; insert:
;;;     add terminal symbol number -i- into a lookahead set.
;;;     Symbols are numbered from 0.

define lconstant insert(i, set);
    lvars i, set;
    1 fi_<< (i fi_// DIVISOR fi_+ 1 -> i) fi_|| fast_subscrv(i, set)
        -> fast_subscrv(i, set);
enddefine;

;;; union:
;;;     merge two lookahead sets (updating -set2-)

define lconstant union(set1, set2);
    lvars i, set2, set1;
    fast_for i to setsize do
        fast_subscrv(i, set1) fi_|| fast_subscrv(i, set2)
            -> fast_subscrv(i, set2);
    endfor;
enddefine;

;;; set_contents:
;;;     converts a lookahead set to a list of terminal numbers

define lconstant set_contents(set);
    lvars mask, n, j, w, i, set;
    0 -> n;
    [%  fast_for i to setsize do
            fast_subscrv(i, set) -> w;
            1 -> mask;
            fast_for j to DIVISOR do
                if (w fi_&& mask) /== 0 then n endif;
                mask fi_<< 1 -> mask;
                n fi_+ 1 -> n;
            endfor;
        endfor;
    %];
enddefine;


/*
 *  Computing the LR(0) Parser States
 */

;;; closure:
;;;     returns the LR(0) closure of a set of kernel items

define lconstant closure(items) -> items;
    lvars item, symbol, items, included;
    popstackmark, dl(items);
    [] ->> included -> items;
    until (->> item) == popstackmark do
        ;;; include the item in the closure
        conspair(item, items) -> items;
        ;;; if the symbol at the dot position is a non-terminal which
        ;;; we haven't yet seen, include all its productions
        if (item_symbol(item) ->> symbol)
        and isnonterminal(symbol)
        and not(lmember(symbol, included))
        then
            conspair(symbol, included) -> included;
            fast_for item in nonterminal_items(symbol) do
                ;;; leave for later processing
                item;
            endfor;
        endif;
    enduntil;
enddefine;

;;; compute_states:
;;;     compute the LR(0) states and the transitions between them

define lconstant compute_states(start_item) -> states;
    lvars start_item, states;

    ;;; order items such that items with the same symbol at the dot
    ;;; position are adjacent
    define lconstant item_<(item1, item2);
        lvars n1, n2, item1, item2;
        item_symbol_number(item1) -> n1;
        item_symbol_number(item2) -> n2;
        ;;; order by symbol number first
        if n1 fi_< n2 then
            true;
        elseif n1 == n2 then
            ;;; sort by item number
            item_number(item1) fi_< item_number(item2);
        else
            false;
        endif;
    enddefine;

    ;;; hash a set of kernel items
    define lconstant state_hash(items) -> code;
        lvars item, code = 0, items;
        fast_for item in items do
            item_number(item) fi_+ code -> code;
        endfor;
    enddefine;

    ;;; compare two ordered sets of kernel items
    define lconstant state_=(items1, items2);
        lvars items1, items2;
        until items1 == [] or items2 == [] do
            fast_destpair(items1) -> items1;
            fast_destpair(items2) -> items2;
            returnunless(() == ())(false);
        enduntil;
        ;;; both nil?
        items1 == items2;
    enddefine;

    ;;; mapping from kernel sets to states
    define lvars state_table =
        newanyproperty([], 256, 1, 240, state_hash, state_=, "perm",
            false, false);
    enddefine;

    ;;; create a new state
    lvars n_states = 0;
    define lconstant new_state(kernel_items) -> state;
        lvars kernel_items, state;
        n_states fi_+ 1 -> n_states;
        consstate(n_states, kernel_items, [], [], []) -> state;
        state -> state_table(kernel_items);
    enddefine;

    ;;; create a new non-terminal transition
    define lconstant new_transition =
        constransition(% [], [], false, 0 %);
    enddefine;

    ;;; create a new final item
    define lconstant new_final_item =
        consfinalitem(% [], [] %);
    enddefine;

    ;;; compute the states given the starting item
    define lconstant Compute_states(item);
        lvars item, items, symbol, state, next_state, kernel_items;
        popstackmark, new_state([^item]);
        until (->> state) == popstackmark do
            ;;; compute the full set of items from the kernel
            closure(state_kernel_items(state)) -> items;
            ;;; sort into standard order
            syssort(items, false, item_<) -> items;
            ;;; final items are at the front of the list:
            ;;; these will generate reductions from the state
            [%  until items == []
                or item_symbol(front(items))
                do
                    new_final_item(fast_destpair(items) -> items);
                enduntil;
            %] -> state_final_items(state);
            ;;; remaining items are grouped according to the item at the
            ;;; dot position: each group forms the kernel of a new state
            until items == [] do
                fast_destpair(items) -> (item, items);
                item_symbol(item) -> symbol;
                [%  item_next(item);
                    until items == []
                    or item_symbol(front(items)) /== symbol
                    do
                        fast_destpair(items) -> (item, items);
                        item_next(item);
                    enduntil;
                %] -> kernel_items;
                ;;; find the next state
                unless state_table(kernel_items) ->> next_state then
                    new_state(kernel_items) -> next_state;
                    ;;; leave for further processing
                    next_state;
                endunless;
                ;;; add a transition to it from this state
                if isterminal(symbol) then
                    conspair(symbol, next_state),
                    conspair((), state_actions(state)) -> state_actions(state);
                else
                    new_transition(symbol, next_state),
                    conspair((), state_gotos(state)) -> state_gotos(state);
                endif;
            enduntil;
        enduntil;
    enddefine;

    Compute_states(start_item);
    ;;; return a vector of all states
    initv(n_states) -> states;
    appproperty(
        state_table,
        procedure(items, state);
            lvars items, state;
            state -> fast_subscrv(state_number(state), states);
        endprocedure);
enddefine;


/*
 *  Computing the LALR(1) Look-Aheads
 */

;;; digraph:
;;;     traverse the state transition graph

define lconstant digraph(states, R);
    lvars states, procedure R;

    ;;; mark all the transitions from a state as unvisited
    define lconstant init(state);
        lvars trans, state;
        fast_for trans in state_gotos(state) do
            0 -> transition_depth(trans);
        endfor;
    enddefine;

    ;;; explore all the transitions from a state
    define lconstant traverse(state);
        lvars trans, state;

        define lconstant Traverse(trans, depth);
            lvars t, trans, set, depth;
            ;;; push the transition
            trans;
            ;;; mark it as visited
            depth fi_+ 1 ->> depth -> transition_depth(trans);
            transition_set(trans) -> set;
            ;;; explore children
            fast_for t in R(trans) do
                if transition_depth(t) == 0 then
                    Traverse(t, depth);
                endif;
                if transition_depth(t) fi_< transition_depth(trans) then
                    ;;; we've found a cycle rooted at an ancestor node:
                    ;;; mark -trans- as a part of it
                    transition_depth(t) -> transition_depth(trans);
                endif;
                union(transition_set(t), set);
            endfor;
            ;;; collapse any strongly-connected components rooted here
            if transition_depth(trans) == depth then
                until (->> t) == trans do
                    ;;; mark as finished
                    pop_max_int -> transition_depth(t);
                    set -> transition_set(t);
                enduntil;
                ;;; mark as finished
                pop_max_int -> transition_depth(trans);
            endif;
        enddefine;

        fast_for trans in state_gotos(state) do
            if transition_depth(trans) == 0 then
                ;;; not yet visited
                Traverse(trans, 0);
            endif;
        endfor;
    enddefine;

    appdata(states, init);
    appdata(states, traverse);
enddefine;

;;; compute_nulls:
;;;     determine which non-terminal symbols can produce the null string

define lconstant compute_nulls(symbols);
    lvars symbols, changed;

    ;;; is the symbol -lhs- nullable?
    define lconstant check_nullable(lhs);
        lvars symbol, item, lhs;
        unless nonterminal_isnullable(lhs) then
            fast_for item in nonterminal_items(lhs) do
                while (item_symbol(item) ->> symbol)
                and isnonterminal(symbol)
                and nonterminal_isnullable(symbol)
                do
                    item_next(item) -> item;
                endwhile;
                unless symbol then
                    true ->> changed -> nonterminal_isnullable(lhs);
                    return;
                endunless;
            endfor;
        endunless;
    enddefine;

    true -> changed;
    while changed do
        false -> changed;
        appdata(symbols, check_nullable);
    endwhile;
enddefine;

;;; compute_reads:
;;;     initialise transitions with their -DR- and -reads- attributes

define lconstant compute_reads(states);
    lvars work, states;

    ;;; summarise the transitions from each state: these are shared by
    ;;; all states which access this one
    define lconstant init_state(state);
        lvars trans, tset, ntset, state;
        ;;; summarise all terminal transitions
        new_set() -> tset;
        fast_for trans in state_actions(state) do
            insert(terminal_number(front(trans)), tset);
        endfor;
        ;;; ... and nullable non-terminal transitions
        [] -> ntset;
        fast_for trans in state_gotos(state) do
            if nonterminal_isnullable(transition_symbol(trans)) then
                conspair(trans, ntset) -> ntset;
            endif;
        endfor;
        ;;; save for later
        conspair(tset, ntset) -> fast_subscrv(state_number(state), work);
    enddefine;

    ;;; get the precomputed -DirectReads- and -reads- attributes from
    ;;; the successor states
    define lconstant do_transitions(state);
        lvars trans, w, state;
        fast_for trans in state_gotos(state) do
            fast_subscrv(state_number(transition_state(trans)), work) -> w;
            ;;; copy the lookahead set for future updates
            copy(fast_front(w)) -> transition_set(trans);
            fast_back(w) -> transition_reads(trans);
        endfor;
    enddefine;

    initv(datalength(states)) -> work;
    appdata(states, init_state);
    appdata(states, do_transitions);
enddefine;

;;; compute_Reads:
;;;     compute the -Reads- set for each transition

define lconstant compute_Reads =
    digraph(% transition_reads %);
enddefine;

;;; compute_includes:
;;;     compute the -includes- relation for each transition, and the
;;;     -lookback- set for each final item

define lconstant compute_includes(states);
    lvars states;

    ;;; follows the path from -state- spelled out by -item-, adding to
    ;;; the -includes- and -lookbacks-; returns <true> if the item is
    ;;; nullable
    define lconstant do_item(trans, state, item);
        lvars i, symbol, item, state, trans;
        if item_symbol(item) ->> symbol then
            item_next(item) -> item;
            if isterminal(symbol) then
                fast_for i in state_actions(state) do
                    if fast_front(i) == symbol then
                        do_item(trans, fast_back(i), item) -> ;
                        ;;; can't be null
                        return(false);
                    endif;
                endfor;
            else
                fast_for i in state_gotos(state) do
                    if transition_symbol(i) == symbol then
                        if do_item(trans, transition_state(i), item) then
                            ;;; rest of item is nullable
                            conspair(trans, transition_includes(i))
                                -> transition_includes(i);
                            return(nonterminal_isnullable(symbol));
                        else
                            return(false);
                        endif;
                    endif;
                endfor;
            endif;
        else
            ;;; should be a corresponding final item in -state-
            fast_for i in state_final_items(state) do
                if finalitem_item(i) == item then
                    conspair(trans, finalitem_lookbacks(i))
                        -> finalitem_lookbacks(i);
                    return(true);
                endif;
            endfor;
        endif;
        mishap(0, 'lr_gen: ERROR IN do_production');
    enddefine;

    define lconstant do_transitions(state);
        lvars item, symbol, trans, state;
        fast_for trans in state_gotos(state) do
            transition_symbol(trans) -> symbol;
            fast_for item in nonterminal_items(symbol) do
                do_item(trans, state, item) -> ;
            endfor;
        endfor;
    enddefine;

    appdata(states, do_transitions);
enddefine;

;;; compute_Follow:
;;;     compute the -Follow- set for each transition

define lconstant compute_Follow =
    digraph(% transition_includes %);
enddefine;

;;; compute_lookaheads:
;;;     compute the lookahead set for each final item

define lconstant compute_lookaheads(states);
    lvars states;

    define lconstant do_final_items(state);
        lvars trans, set, item, state;
        fast_for item in state_final_items(state) do
            new_set() ->> set -> finalitem_lookaheads(item);
            fast_for trans in finalitem_lookbacks(item) do
                union(transition_set(trans), set);
            endfor;
        endfor;
    enddefine;

    appdata(states, do_final_items);
enddefine;


/*
 *  Computing the Parser Tables
 */

;;; compute_tables:
;;;     determine the action, goto and default tables for each state
;;;     and return the total numbers of conflicts

define lconstant compute_tables(tokens, rule0, states, resolve_p)
                                                            -> (n_sr, n_rr);
    lvars tokens, rule0, states, resolve_p, (n_sr, n_rr) = (0, 0);

        ;;; get terminal symbol number -i-
    lvars token0 = item_symbol(item_next(rule_rhs(rule0)));
    define lconstant get_token(i);
        lvars i;
        if i == 0 then token0 else fast_subscrv(i, tokens) endif;
    enddefine;

        ;;; merge final items and kernel items
    define lconstant merge_items(state);
        lvars f_item, k_item, final_items, kernel_items, state;

        define lconstant item_<(item1, item2);
            lvars item1, item2;
            item_number(item1) fi_< item_number(item2);
        enddefine;

        state_final_items(state) -> final_items;    ;;; already sorted
        syssort(state_kernel_items(state), false, item_<) -> kernel_items;
        [%  until final_items == [] do
                finalitem_item(fast_front(final_items)) -> f_item;
                if kernel_items == []
                or item_<(f_item, fast_front(kernel_items) ->> k_item)
                then
                    f_item;
                    fast_back(final_items) -> final_items;
                else
                    k_item;
                    fast_back(kernel_items) -> kernel_items;
                    if f_item == k_item then
                        fast_back(final_items) -> final_items;
                    endif;
                endif;
            enduntil
         %  ^^kernel_items] -> state_kernel_items(state);
    enddefine;

        ;;; add a reduction into an existing list of actions
    define lconstant resolve_conflict(reduction, actions);
        lvars reduction, actions, action, choice, save_actions = actions;

        define lconstant try_resolve_p(action1, action2);
            lvars item, item1, action1, item2, action2;
            if isstate(action1) then
                terminal_name(fast_front(save_actions)), rule_name(action2)
            else
                if rule_number(action1) fi_< rule_number(action2) then
                    (action1, action2) -> (action2, action1);
                endif;
                rule_name(action1), rule_name(action2)
            endif -> (item1, item2);
            resolve_p(item1, item2) -> item;
            if item == item1 then
                action1;
            elseif item == item2 then
                action2;
            elseif item then
                mishap(item, 1, 'lr_gen: ILLEGAL RESULT FROM CONFLICT RESOLUTION');
            else
                false;
            endif;
        enddefine;

        back(reduction) -> reduction;
        back(actions) -> actions;
        if ispair(actions) then
            fast_destpair(actions)
        else
            (actions, [])
        endif -> (action, actions);
        if resolve_p
        and (try_resolve_p(action, reduction) ->> choice)
        then
            ;;; conflict resolved
            if choice == action then
                ;;; no change
            elseif actions == [] then
                ;;; straight replacement
                reduction -> back(save_actions);
            else
                if isstate(action) then
                    ;;; deleting a shift action: any remaining conflicts
                    ;;; previously counted as shift/reduce are now
                    ;;; reclassified as reduce/reduce
                    n_sr fi_- listlength(actions) -> n_sr;
                    n_rr fi_+ listlength(actions) -> n_rr;
                endif;
                [^reduction ^^actions] -> back(save_actions);
            endif;
        else
            if isstate(action) then
                ;;; shift/reduce conflict
                n_sr fi_+ 1 -> n_sr;
            else
                ;;; reduce/reduce
                n_rr fi_+ 1 -> n_rr;
                if rule_number(reduction) fi_< rule_number(action) then
                    ;;; lower numbered rule takes precedence
                    (action, reduction) -> (reduction, action);
                endif;
            endif;
            [^action ^reduction ^^actions] -> back(save_actions);
        endif;
    enddefine;

    lvars work = initv(datalength(tokens) fi_+ 1);

        ;;; compute specific actions and any default action for state
    define lconstant complete_actions(state);
        lvars i, n, rule, n_reductions, max_reductions, default_action, state;
        ;;; shifts first
        fast_for i in state_actions(state) do
            terminal_number(front(i)) fi_+ 1 -> n;
            if fast_subscrv(n, work) then
                ;;; duplicate shift -- impossible
                mishap(0, 'lr_gen: ERROR IN complete_actions');
            endif;
            i -> fast_subscrv(n, work);
        endfor;
        ;;; then reductions
        false -> default_action;
        0 -> max_reductions;
        fast_for i in state_final_items(state) do
            ;;; item_next points to the rule to be reduced
            item_next(finalitem_item(i)) -> rule;
            ;;; add a reduction by this rule for each token in the
            ;;; associated lookahead set
            0 -> n_reductions;
            fast_for n in set_contents(finalitem_lookaheads(i)) do
                conspair(get_token(n), rule) -> i;
                n fi_+ 1 -> n;
                if fast_subscrv(n, work) then
                    ;;; existing action -- conflict
                    resolve_conflict(i, fast_subscrv(n, work));
                else
                    i -> fast_subscrv(n, work);
                    n_reductions fi_+ 1 -> n_reductions;
                endif;
            endfor;
            if n_reductions == 0 and rule_number(rule) == 0 then
                ;;; reduction on the starting rule -- accept
                "accept" -> default_action;
            elseif n_reductions fi_> max_reductions then
                ;;; candidate for the default action
                n_reductions -> max_reductions;
                rule -> default_action;
            endif;
        endfor;
        ;;; collect results
        [%  appdata(work,
                procedure(x);
                    lvars x;
                    if x and fast_back(x) /== default_action then x endif;
                endprocedure)
        %] -> state_actions(state);
        ;;; set the default action -- this replaces the final_items field
        default_action -> state_default_action(state);
    enddefine;

        ;;; trim and sort the goto transitions
    define lconstant complete_gotos(state);
        lvars i, state;
        syssort(
            [%  fast_for i in state_gotos(state) do
                    ;;; most of the transition information can be discarded
                    conspair(transition_symbol(i), transition_state(i));
                endfor;
            %],
            false,
            procedure(x, y);
                lvars x, y;
                nonterminal_number(front(x)) -> x;
                nonterminal_number(front(y)) -> y;
                if x == y then
                    ;;; duplicate goto -- impossible
                    mishap(0, 'lr_gen: ERROR IN complete_gotos');
                endif;
                x fi_< y;
            endprocedure
        ) -> state_gotos(state);
    enddefine;

    define lconstant complete_state(state);
        lvars state;
        fill(fast_repeat datalength(work) times false endrepeat, work) -> ;
        merge_items(state);
        complete_actions(state);
        complete_gotos(state);
    enddefine;

    appdata(states, complete_state);
enddefine;

;;; lr_gen:
;;;     computes the LALR(1) parser states

define lr_gen(tokens, symbols, rule0, rules, resolve_p) -> (states, n_sr, n_rr);
    lvars tokens, symbols, rule0, rules, resolve_p, states, n_sr, n_rr;
    init_sets(datalength(tokens) fi_+ 1);
    compute_states(rule_rhs(rule0)) -> states;
    compute_nulls(symbols);
    compute_reads(states);
    compute_Reads(states);
    compute_includes(states);
    compute_Follow(states);
    compute_lookaheads(states);
    compute_tables(tokens, rule0, states, resolve_p) -> (n_sr, n_rr);
enddefine;

endsection;     /* $-LR */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul  1 1993
        Cosmetic changes.
 */
