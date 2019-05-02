/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lisp/src/packages.p
 > Purpose:         Set up packages for system compilation
 > Author:          John Williams, Jun 22 1988 (see revisions)
 > Documentation:
 > Related Files:   C.all/lisp/src/lispcore.p
 */

lisp_compile_mode;

section $-lisp;

    /* At time of writing (Nov 1st 1993), Clisp starts up with
        123 symbols in the keyword package,
        813 symbols in the Lisp package,
        161 symbols in the system package,
        0 symbols in the user package,
        3 symbols in the Pop-11 package.
    */


constant
    keyword_package
        =   make_package(['KEYWORD'], [], 256),
    system_package
        =   make_package(['SYSTEM' 'SYS'], [], 256),
    pop11_package
        =   make_package(['POP11'], [], 128),
    poplog_package
        =   make_package(['POPLOG'], [], 128),
    lisp_package
        =   make_package(['COMMON-LISP' 'CL' 'LISP'],
                         [^poplog_package], 1024),
    user_package
        =   make_package(['COMMON-LISP-USER' 'CL-USER' 'USER'],
                         [^lisp_package ^poplog_package], 1024),
    ;

use_package(lisp_package, poplog_package);

constant
    default_package_use_list    =   [^lisp_package ^poplog_package],
    default_package_size        =   128,
    symbol_status_internal      =   sysintern('INTERNAL', keyword_package),
    symbol_status_external      =   sysintern('EXTERNAL', keyword_package),
    symbol_status_inherited     =   sysintern('INHERITED', keyword_package),
    symbol_status_new           =   nil,
    ;


define active package;
    current_package
enddefine;


define updaterof active package(p);
    lvars pkg;
    if (find_package(p) ->> pkg) then
        pkg -> current_package
    else
        mishap(p, 1, 'Attempt to assign non-package to *package*')
    endif
enddefine;

#_IF DEF propsheet_ident_class
"active" -> propsheet_ident_class(ident package);
#_ENDIF

lisp_package -> package;

import([^nil ^true], lisp_package);
export([^nil ^true], lisp_package);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jun  8 1994
        poplog_package now uses lisp_package.
--- John Williams, Jun  7 1994
        Changes for Steele 1990. Also, added poplog_package,
        default_package_use_list and default_package_size.
--- John Williams, Nov  2 1993
        package now defined as an active variable (based on current_package
        defined in C.all/lisp/src/lispcore.p).
        Also moved defintion of apropos_list to lispcore.p
 */
