/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/lib/lisp_subsystem.p
 > Purpose:         Define subsystem record for Poplog Common Lisp
 > Author:          John Williams, Jan 13 1993 (see revisions)
 > Documentation:
 > Related Files:   C.all/lisp/src/subsystem_procedures.p
 */

compile_mode :pop11 +strict;

section;

define lconstant OS(dir);
    dir dir_>< nullstring
enddefine;


global constant
    lisptitle       =   'Common Lisp',
    lispversion     =   '2.0',
    ;


global vars
    lispprompt
        =   '== ',
    lisphelplist
        =   [[^(OS('$poplocal/local/lisp/help/')) help "lisp]
             [^(OS('$usepop/pop/lisp/help/')) help "lisp]],
    lispreflist
        =   [[^(OS('$usepop/pop/lisp/ref/')) ref "lisp]],
    lispsrcdirlist
        =   [^(OS('$usepop/pop/lisp/src/'))],
    lispsrclist
        =   maplist(lispsrcdirlist, procedure(i); lvars i; [^i src] endprocedure),
    lispteachlist
        =   [lisphelplist],
    lisp_modules_list
        =   [^(OS('$poplocal/local/lisp/modules/'))
             ^(OS('$usepop/pop/lisp/modules/'))],
    lisp_??_list
        =   [lispreflist],
    ;


lconstant Lisp_search_lists
    =   [vedhelpname   [% ident lisphelplist %]
         vedlibname    [% ident lisp_modules_list %]
         vedrefname    [% ident lispreflist %]
         vedsrcname    [% ident lispsrclist %]
         vedteachname  [% ident lispteachlist %]
         ved_??_name   [% ident lisp_??_list %]
        ];


subsystem_add_new
    ("lisp",
     "lisp_subsystem_procedures",
     '.lsp',
     lispprompt,
     Lisp_search_lists,
     lisptitle);


constant lisp_subsystem = "lisp";       ;;; for uses


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jul 27 1995
        lispversion now 2.0 (for release of Poplog 15)
--- John Williams, Aug 24 1994
        lispversion now 1.6 (upgrades to compiler more or less finished).
--- John Williams, Aug 11 1993
        lispversion now 1.5 (upgrade to Steele 1990 started).
--- John Gibson, Apr 26 1993
        Uses subsystem_add_new
--- John Gibson, Jan 15 1993
        o Removed declaration for lisp_subsystem_procedures
        o Replaced lisp_compile in searchlists with "lisp
        o Moved lisp/clisp macros and setlisp to lisp/src/clisp.p
          (clisp mustn't be defined in lisp_subsystem otherwise 'uses clisp'
          won't work).
--- John Williams, Jan 15 1993
        Removed accidental reference to current_directory in lispsrcdirlist
 */
