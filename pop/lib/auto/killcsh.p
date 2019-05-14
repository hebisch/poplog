/*  --- Copyright University of Sussex 1991. All rights reserved. ----------
 >  File:           C.unix/lib/auto/killcsh.p
 >  Purpose:        kill comunicating CSH process
 >  Author:         Roger Evans, Aug 1983 (see revisions)
 >  Documentation:  HELP * KILLCSH
 >  Related Files:  LIB * VED_KILLCSH
 */

section;

uses ved_killcsh;

global vars macro killcsh = [pr(ved_killcsh()); pr(newline);];

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov  5 1991
        Re-installed in C.unix
 */
