/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:           C.unix/lib/auto/csh_compile.p
 > Purpose:        compile by sending characters to a CSH subprocess
 > Author:         Roger Evans, (adapted from DCL_COMPILE) Aug 1983 (see revisions)
 > Documentation:  HELP * CSH_COMPILE  HELP * IMCSH
 > Related Files:  LIB * CSH_SEND
 */
uses csh_subsystem;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Apr 26 1993
        Moved everything to csh_subsystem
--- John Gibson, Dec  1 1992
        Code commoned with sh_compile into lib shell_compile
--- John Williams, Aug 30 1992
        Changed occurences of >< to <>, and -pr- to -prstring-, to avoid
        problems with -pop_pr_quotes- and other print control variables.
        (c.f. BR johnw.1039)
--- John Gibson, Jun 24 1991
        Undid last change -- not needed now since ved output devices no
        longer cause unnecessary sideways scrolling.
--- Aaron Sloman, May 28 1990
    Replaced calls of "pr" with new procedure csh_pr designed to prevent
    scrolling on long lines.
--- Aaron Sloman, Dec 28 1989
    Made prompt constant, though user assignable via csh_fixed_prompt
    Changed to use lvars and dlocal, where appropriate
 */
