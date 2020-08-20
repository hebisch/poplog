/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/lr_parser/lib/lr_core.p
 > Purpose:         LALR(1) Parser Generator: basic class definitions
 > Author:          Robert John Duncan, Nov 27 1992 (see revisions)
 > Documentation:   HELP * LR_PARSER, REF * LR_PARSER
 > Related Files:   LIB * LR_INFO
 */

compile_mode:pop11 +strict;

section $-LR =>
    lr_parser,
;

constant
    procedure lr_parse,
;

weak constant
    procedure lr_trace,
;

weak vars
    lr_trace_tracing,
;

;;; =========================================================================


;;; compressed parser tables:
defclass parser_tables {
    parser_defaults,
        ;;; vector of default actions
    parser_actions,
        ;;; vector of specific actions
    parser_gotos,
        ;;; vector of gotos
};
;;;
procedure(p);
    lvars p;
    printf('<parser tables>');
endprocedure -> class_print(parser_tables_key);

;;; symbolic information:
defclass parser_symbols {
    parser_terminal_symbols,
        ;;; vector of terminal symbols
    parser_nonterminal_symbols,
        ;;; vector of nonterminal symbols
    parser_start_symbol,
        ;;; start symbol
    parser_info,
        ;;; additional symbolic information (structure defined in
        ;;; "lr_info.p") or <false> if stripped
};
;;;
procedure(p);
    lvars p;
    printf('<parser symbols>');
endprocedure -> class_print(parser_symbols_key);

;;; LR parser structure returned by lr_build
defclass parser {
    parser_name,
        ;;; identifier for this parser (any item)
    parser_tables,
        ;;; parser_tables structure
    parser_symbols,
        ;;; parser_symbols structure (or <false> if fully stripped)
};
;;;
procedure(p);
    lvars p;
    printf('<parser %p>', [^(parser_name(p))]);
endprocedure -> class_print(parser_key);
;;;
procedure(p);
    lvars p;
    if testdef lr_trace then
        dlocal weakref lr_trace_tracing = false;
        weakref lr_trace(p);
    else
        chain(p, lr_parse);
    endif;
endprocedure -> class_apply(parser_key);

;;; mapping from parser names to parsers
define lconstant parser_of =
    newproperty([], 16, false, "perm");
enddefine;

define global lr_parser(item) -> item;
    lvars item;
    unless isparser(item) then
        parser_of(item) -> item;
    endunless;
enddefine;
;;;
define updaterof lr_parser(parser, item);
    lvars parser, item;
    if not(parser) or isparser(parser) then
        parser -> parser_of(item);
    else
        mishap(parser, 1, 'PARSER NEEDED');
    endif;
enddefine;

define Checkr_parser(item) -> parser with_props false;
    lvars item, parser;
    unless lr_parser(item) ->> parser then
        mishap(item, 1, 'PARSER NEEDED');
    endunless;
enddefine;

define Checkr_parser_symbols(parser) -> symbols with_props false;
    lvars parser, info, symbols;
    unless parser_symbols(Checkr_parser(parser)) ->> symbols then
        mishap(parser, 1, 'NO SYMBOLIC INFORMATION FOR PARSER');
    endunless;
enddefine;

vars lr_core = true;    ;;; for USES

endsection;     /* $-LR */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul  1 1993
        Simplified with the assumption that the parser tables will always
        be present. Added the class_apply definition.
 */
