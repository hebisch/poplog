/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/lib/edinsynt/edinsynt.pl
 > Purpose:         Edinburgh syntax compatibility library
 > Author:          Simon Nichols, Mar 14 1990 (see revisions)
 > Related Files:   C.all/plog/lib/edinsynt/ (directory)
 */

:- prolog_language(pop11).

subsystem_compile('tokens.p',    false);
subsystem_compile('readtoken.p', false);
subsystem_compile('readterm.p',  false);


:- prolog_language(prolog).

:- library(dec10_ops).
:- library(prolog_syntax).
:- $-prolog$-prolog_val('DOT_SPACE', DotSpace),
   $-prolog$-assert(
        prolog_syntax_table(edinburgh, edinburgh_readterm, false, DotSpace)
   ).

/* --- Revision History ---------------------------------------------------
--- Simon Nichols, May 20 1993
        Changed to use subsystem_compile.
--- Simon Nichols, Oct 24 1991
        Changed library(syntax) to library(prolog_syntax).
--- Simon Nichols, Jun 26 1990
        Changed terminator token to special token DOT_SPACE.
 */
