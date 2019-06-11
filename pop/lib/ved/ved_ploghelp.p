/* --- Copyright University of Sussex 1992.  All rights reserved. ---------
 > File:           C.all/lib/ved/ved_ploghelp.p
 > Purpose:        looks for prolog documentation files
 > Author:         Jonathan Laventhol, Jun   9 1983 (see revisions)
 > Documentation:  HELP * PLOGHELPHELP * PLOGHELP
 > Related Files:  LIB * PLOGSHOWLIB, LIB * PLOGTEACH
 */
compile_mode :pop11 +strict;

section;

uses-by_name (prolog_compile);

;;; perhaps the 2nd items in these lists should be "ploghelp" and "plogteach"
vars ploghelplist = [
    ['$poplocal/local/plog/help/'   help  prolog_compile]
    ['$poplocal/local/plog/teach/'  teach prolog_compile]
    ['$usepop/pop/plog/help/'       help  prolog_compile]
    ['$usepop/pop/plog/teach/'      teach prolog_compile]
    ^(ident vedhelplist)
];

define ploghelp_symbol_list = newproperty([
        [;          SEMICOLON]
        [,          COMMA]
        [->         CONDITIONAL]
        [%"'\\+'"%  NOT]
        [%"'=..'"%  UNIV]
        [<          LESSTHAN]
        [>          GREATERTHAN]
        [>=         GREATERTHAN]
        [=<         LESSTHAN]
        [-          MINUS]
        [+          PLUS]
        [*          TIMES]
        [/          DIVIDE]
        [@<         COMPARE]
        [@>         COMPARE]
        [@=<        COMPARE]
        [@>=        COMPARE]
        [=:=        EQUAL]
        [%"'=\\='"% EQUAL]
        [=          EQUAL]
        [%"'\\='"%  EQUAL]
        [==         EQUAL]
        [%"'\\=='"% EQUAL]
        [%"'\\/'"%  BITWISE]
        [%"'/\\'"%  BITWISE]
        [%"'\\'"%   BITWISE]
        [>>         BITWISE]
        [<<         BITWISE]
        [%"^"%      SETOF]
    ], 30, false, false)
enddefine;

define vars ved_ploghelp();
    lvars real_help;
    vedsetup();  ;;; ensure vedinit.p compiled
    if      ploghelp_symbol_list(consword(vedargument)) ->> real_help
    then    vedputmessage('relevant documentation in HELP ' >< real_help);
            real_help -> vedargument;
    endif;
    vedsysfile("vedhelpname", ploghelplist, false)
enddefine;

"ved_ploghelp" -> vedgetsysfilepdr("PLOGHELP");

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Dec  2 1992
        Uses ident vedhelplist not list itself, and name of prolog_compile
        not procedure.
--- John Gibson, Oct 31 1990
        Renamed as ved_ploghelp.p from ploghelp.p
--- Aaron Sloman, Jan  8 1989
    Made sure vedsetup called
--- John Williams, Apr 24 1987 - changed $usepop/pop to $poplocal
--- Aaron Sloman, Nov  5 1986 -fixed to work with <ESC> H
--- Kathryn Seifert, Oct 17 1986 - Symbol table revised to be in accord with
    new HELP files;
--- Kathryn Seifert, Sep  9 1986 -  added facility for locating help files
    for symbols
--- Mark Rubinstein, Feb 12 1986 - made local files dominant.
--- Mark Rubinstein, Sep 16 1985 - altered the library specification for new
    format.
--- Aaron Sloman, Jul 19 1983 - (unspecified modifications).
 */
