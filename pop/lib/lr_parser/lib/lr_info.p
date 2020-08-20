/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/lr_parser/lib/lr_info.p
 > Purpose:         LALR(1) Parser Generator: additional class definitions
 > Author:          Robert John Duncan, Nov 27 1992
 > Documentation:   HELP * LR_PARSER, REF * LR_PARSER
 > Related Files:   LIB * LR_CORE
 */

compile_mode:pop11 +strict;

section $-LR;

;;; =========================================================================


;;; Special symbol names:
constant
    START_SYMBOL    = '$begin$',
    END_TOKEN       = '$end$',
;


;;; Terminal symbols (tokens):
defclass terminal {
    terminal_name,
        ;;; token name
    terminal_number,
        ;;; token number (>= 0)
};
;;;
procedure(token);
    lvars token;
    printf('<terminal %p>', [% terminal_name(token) %]);
endprocedure -> class_print(terminal_key);

;;; Non-terminal symbols:
defclass nonterminal {
    nonterminal_name,
        ;;; symbol name
    nonterminal_number,
        ;;; symbol number (>= 0)
    nonterminal_items,
        ;;; list of RHSs of rules with this symbol on the LHS
    nonterminal_isnullable,
        ;;; <true> if this symbol can produce the empty string
};
;;;
procedure(symbol);
    lvars symbol;
    printf('<nonterminal %p>', [% nonterminal_name(symbol) %]);
endprocedure -> class_print(nonterminal_key);

;;; get the name of any symbol
define symbol_name(symbol);
    lvars symbol;
    if isterminal(symbol) then
        terminal_name(symbol);
    else
        nonterminal_name(symbol);
    endif;
enddefine;


;;; LR(0) items:
;;; an item is a rule with a "dot" somewhere on the RHS
defclass item {
    item_number,
        ;;; item number (>= 0)
    item_symbol,
        ;;; symbol at the dot position, or <false> for a final item
    item_symbol_number,
        ;;; symbol number of item_symbol, or -1 for a final item
    item_next,
        ;;; next item in the rule (obtained by moving the dot to the right)
        ;;; or the rule itself for a final item (NB: this makes items
        ;;; cyclic)
};
;;;
procedure(item);
    lvars item;
    printf('<item %p %p>', [% item_number(item), item_symbol(item) %]);
endprocedure -> class_print(item_key);


;;; Productions (rules):
defclass rule {
    rule_name,
        ;;; original text for this rule
    rule_number,
        ;;; the number of this rule (>= 0)
    rule_lhs,
        ;;; LHS of this rule
    rule_rhs_length,
        ;;; length of the RHS
    rule_rhs,
        ;;; first item for the RHS
};
;;;
procedure(rule);
    lvars rule, symbols = datalist(rule_name(rule));
    printf('<rule %p -->', symbols);
    until null(tl(symbols) ->> symbols) do
        printf(' %p', symbols);
    enduntil;
    printf('>');
endprocedure -> class_print(rule_key);


;;; Parser states:
defclass state {
    state_number,
        ;;; state number (>= 1)
    state_kernel_items,
        ;;; sorted list of kernel items
    state_final_items,
        ;;; sorted list of final items (then reused for default action)
    state_actions,
        ;;; shifts and reductions (terminal transitions) from this state:
        ;;; a list of pairs [<terminal>|<action>] where <action> may be
        ;;; a state (shift) or a rule (reduce)
    state_gotos,
        ;;; gotos (non-terminal transitions) from this state:
        ;;; a list of pairs [<nonterminal>|<state>]
};
;;;
procedure(state);
    lvars state;
    printf('<state %p>', [% state_number(state) %]);
endprocedure -> class_print(state_key);

;;; Re-use the state final_items field to hold the default action
constant procedure state_default_action = state_final_items;

;;; Symbolic information:
defclass parser_info {
    parser_rules,
        ;;; vector of rules
    parser_states,
        ;;; vector of states
    parser_sr_conflicts,
        ;;; number of shift/reduce conflicts
    parser_rr_conflicts,
        ;;; number of reduce/reduce conflicts
};
;;;
procedure(p);
    lvars p;
    printf('<parser info>');
endprocedure -> class_print(parser_info_key);

define Checkr_parser_info(parser) -> info with_props false;
    lvars parser, info;
    unless parser_info(Checkr_parser_symbols(parser)) ->> info then
        mishap(parser, 1, 'INSUFFICIENT SYMBOLIC INFORMATION FOR PARSER');
    endunless;
enddefine;

vars lr_info = true;    ;;; for USES

endsection;     /* $-LR */
