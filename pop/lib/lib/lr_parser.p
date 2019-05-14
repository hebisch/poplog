/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/lib/lr_parser.p
 > Purpose:         LALR(1) parser generator and utilities
 > Author:          Robert John Duncan, Nov 27 1992
 > Documentation:   HELP * LR_PARSER, REF * LR_PARSER
 > Related Files:   C.all/lib/lr_parser/...
 */
compile_mode:pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

#_IF not(DEF LR_PARSER_DIR)
global constant
    LR_PARSER_DIR = '$usepop/pop/lib/lr_parser/' dir_>< '',
;
#_ENDIF

lconstant
    LR_PARSER_LIB   = LR_PARSER_DIR dir_>< 'lib/',
    LR_PARSER_AUTO  = LR_PARSER_DIR dir_>< 'auto/',
;

extend_searchlist(LR_PARSER_LIB, popuseslist, true) -> popuseslist;
extend_searchlist(LR_PARSER_AUTO, popautolist, true) -> popautolist;

section $-LR;

pop11_compile(LR_PARSER_LIB dir_>< 'lr_core.p');
    ;;; essential definitions (including -lr_parser-)
pop11_compile(LR_PARSER_LIB dir_>< 'lr_parse.p');
    ;;; avoids the need to load this from generated programs

endsection;     /* $-LR */

endsection;     /* $- */
