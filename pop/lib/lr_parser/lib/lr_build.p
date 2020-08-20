/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/lr_parser/lib/lr_build.p
 > Purpose:         LALR(1) Parser Generator: building the parser
 > Author:          Robert John Duncan, Nov 27 1992 (see revisions)
 > Documentation:   HELP * LR_PARSER, REF * LR_PARSER
 > Related Files:   LIB * LR_GEN, * LR_COMPRESS
 */

compile_mode:pop11 +strict;

section $-LR =>
    lr_parser_name,
    lr_parser_terminal_symbols,
    lr_parser_nonterminal_symbols,
    lr_parser_start_symbol,
    lr_parser_rules,
    lr_parser_sr_conflicts,
    lr_parser_rr_conflicts,
    lr_strip,
    lr_build,
;

uses lr_core;
uses lr_info;
uses lr_gen;
uses lr_compress;


;;; lr_parser_*:
;;;     extract fields from a parser record

define lconstant from_symbols(parser, access_p) -> item;
    lvars parser, access_p, item = false, symbols;
    if parser_symbols(Checkr_parser(parser)) ->> symbols then
        access_p(symbols) -> item;
    endif;
enddefine;

define lconstant from_info(parser, access_p, default) -> item;
    lvars parser, access_p, default, item = default, info;
    if from_symbols(parser, parser_info) ->> info then
        access_p(info) -> item;
    endif;
enddefine;

define global lr_parser_name =
    Checkr_parser <> parser_name;
enddefine;

define global lr_parser_terminal_symbols =
    from_symbols(% parser_terminal_symbols %);
enddefine;

define global lr_parser_nonterminal_symbols =
    from_symbols(% parser_nonterminal_symbols %);
enddefine;

define global lr_parser_start_symbol =
    from_symbols(% parser_start_symbol %);
enddefine;

define global lr_parser_rules =
    from_info(% parser_rules, false %);
enddefine;

define global lr_parser_sr_conflicts =
    from_info(% parser_sr_conflicts, 0 %);
enddefine;

define global lr_parser_rr_conflicts =
    from_info(% parser_rr_conflicts, 0 %);
enddefine;

;;; lr_strip:
;;;     strips symbolic information from a parser

define global lr_strip(parser);
    lvars parser, full = false;
    ;;; option boolean full
    if isboolean(parser) then
        ((), parser) -> (parser, full);
    endif;
    Checkr_parser(parser) -> parser;
    lvars symbols = parser_symbols(parser);
    if symbols then
        ;;; not yet stripped:
        if full then
            false -> parser_symbols(parser);
        else
            ;;; strip just the expanded information
            false -> parser_info(symbols);
        endif;
    endif;
enddefine;

;;; encode:
;;;     transforms a grammar into a form suitable for lr_gen

define lconstant encode(tokens, symbols, start, rules)
                                        -> (tokens, symbols, rule0, rules);
    lvars tokens, symbols, start, rules, rule0;

        ;;; map user names to terminal/nonterminal symbols
    define lvars symbol_table =
        newproperty([], 256, false, "perm");
    enddefine;

        ;;; add a new terminal symbol
    lvars n_tokens = 0;
    define lconstant add_token(name) -> symbol;
        lvars symbol, name;
        unless symbol_table(name) ->> symbol then
            consterminal(name, n_tokens) -> symbol;
            symbol -> symbol_table(name);
        endunless;
        n_tokens fi_+ 1 -> n_tokens;
    enddefine;

        ;;; add a new nonterminal symbol
    lvars n_symbols = 0;
    define lconstant add_symbol(name) -> symbol;
        lvars symbol, name;
        if symbol_table(name) ->> symbol then
            unless isnonterminal(symbol) then
                mishap(name, 1, 'REDECLARATION OF SYMBOL');
            endunless;
        else
            consnonterminal(name, n_symbols, [], false) -> symbol;
            symbol -> symbol_table(name);
        endif;
        n_symbols fi_+ 1 -> n_symbols;
    enddefine;

        ;;; the start symbol must have been declared as a nonterminal
    define lconstant check_start_symbol(name);
        lvars symbol, name;
        unless symbol_table(name) ->> symbol then
            mishap(name, 1, 'UNDECLARED START SYMBOL');
        elseunless isnonterminal(symbol) then
            mishap(name, 1, 'ILLEGAL START SYMBOL');
        endunless;
    enddefine;

        ;;; convert a rule from list or vector to a rule+items structure
    lvars n_rules = 0, n_items = 0, undeclared = [];
    define lconstant add_rule(rule) -> rule;
        lvars rule;

            ;;; returns a number for a symbol occurring in a rule s.t.
            ;;; each symbol has a unique number and non-terminals score
            ;;; higher than terminals (for sorting)
        define lconstant symbol_number(symbol);
            lvars symbol;
            if isterminal(symbol) then
                terminal_number(symbol);
            else
                nonterminal_number(symbol) fi_+ n_tokens;
            endif;
        enddefine;

        lvars len = (#| explode(rule) |#);
        ;;; a rule must have at least one symbol for the left-hand-side
        if len == 0 then mishap(rule, 1, 'NO LHS FOR RULE') endif;
        ;;; remaining symbols make up the right-hand-side
        lvars rhs_len = len fi_- 1;
        ;;; the RHS generates one item for each symbol plus a final item
        n_items fi_+ len -> n_items;
        ;;; the items are constructed last first, but their numbering must
        ;;; go from left to right, with the very first item numbered 0
        lvars item_n = n_items fi_- 1;
        lvars final_item = consitem(item_n, false, -1, "dummy");
        lvars name, symbol, item = final_item;
        repeat
            ;;; get next symbol from the stack
            () -> name;
            unless symbol_table(name) ->> symbol then
                add_symbol(name) -> symbol; ;;; dummy
                name :: undeclared -> undeclared;
            endunless;
        quitif(len == 1);
            len fi_- 1 -> len;
            item_n fi_- 1 -> item_n;
            consitem(item_n, symbol, symbol_number(symbol), item) -> item;
        endrepeat;
        ;;; symbol is now the LHS symbol: must be a nonterminal
        unless isnonterminal(symbol) then
            mishap(name, rule, 2, 'ILLEGAL LHS FOR RULE');
        endunless;
        conspair(item, nonterminal_items(symbol)) -> nonterminal_items(symbol);
        consrule(rule, n_rules, symbol, rhs_len, item) -> rule;
        n_rules fi_+ 1 -> n_rules;
        ;;; the final item of a rule points back to the rule itself
        rule -> item_next(final_item);
    enddefine;

        ;;; apply a procedure to every element of a list or vector
    define lconstant applist_or_v(seq, p);
        lvars seq, p;
        if isvector(seq) then
            appdata(seq, p);
        else
            applist(seq, p);
        endif;
    enddefine;

    ;;; make the special end-of-input token be terminal 0
    add_token(END_TOKEN) -> ;
    ;;; encode user tokens
    {% applist_or_v(tokens, add_token) %} -> tokens;
    ;;; make the special start symbol be nonterminal 0
    add_symbol(START_SYMBOL) -> ;
    ;;; encode user symbols
    {% applist_or_v(symbols, add_symbol) %} -> symbols;
    ;;; check validity of the nominated start symbol
    check_start_symbol(start);
    ;;; add the initial rule
    add_rule({^START_SYMBOL ^start ^END_TOKEN}) -> rule0;
    ;;; encode user rules
    {% applist_or_v(rules, add_rule) %} -> rules;
    ;;; check for undeclared symbols
    unless undeclared == [] then
        mishap('UNDECLARED SYMBOL(S) IN GRAMMAR RULES', undeclared);
    endunless;
enddefine;

;;; decode:
;;;     restores an encoded grammar back to its original form

define lconstant decode(tokens, symbols, rules) -> (tokens, symbols, rules);
    lvars tokens, symbols, rules;

    define lconstant trim_nonterminal(symbol);
        lvars symbol;
        [] -> nonterminal_items(symbol);    ;;; save space
        nonterminal_name(symbol);
    enddefine;

    ncmapdata(tokens, terminal_name) -> tokens;
    ncmapdata(symbols, trim_nonterminal) -> symbols;
    ncmapdata(rules, rule_name) -> rules;
enddefine;

;;; lr_build:
;;;     constructs a parser record from the symbolic description of a
;;;     grammar

define global lr_build(/* name, tokens, symbols, start, rules, resolve_p,
                          keep */) -> parser with_nargs 5;
    lvars parser;
    ;;; optional boolean keep
    lvars keep;
    unless isboolean(->> keep) then
        (keep, false) -> keep;
    endunless;
    ;;; optional procedure resolve_p
    lvars resolve_p;
    unless isprocedure(->> resolve_p) then
        (resolve_p, false) -> resolve_p;
    endunless;
    ;;; compulsory arguments
    lvars (name, tokens, symbols, start, rules) = ();
    ;;; zap any existing parser to save space
    false -> lr_parser(name);
    ;;; encode the grammar
    lvars rule0;
    encode(tokens, symbols, start, rules) -> (tokens, symbols, rule0, rules);
    ;;; compute the parser states
    lvars (states, n_sr, n_rr) = lr_gen(tokens,symbols,rule0,rules,resolve_p);
    ;;; generate the compressed tables
    lvars tables = consparser_tables(lr_compress(rules, states));
    ;;; restore the grammar
    decode(tokens, symbols, rules) -> (tokens, symbols, rules);
    ;;; make the parser record
    lvars info = consparser_info(rules, states, n_sr, n_rr);
    lvars symbols = writeable consparser_symbols(tokens,symbols,start,info);
    writeable consparser(name, tables, symbols) -> parser;
    ;;; add it back to the lr_parser table if required
    if keep then parser -> lr_parser(name) endif;
enddefine;

endsection;     /* $-LR */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul  1 1993
        Simplified always to do table compression rather than on first use
        and reorganised the file.
 */
