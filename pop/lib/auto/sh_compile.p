/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:           C.unix/lib/auto/sh_compile.p
 > Purpose:        compile by sending characters to a SH subprocess
 > Author:         A.Sloman(adapted from CSH_COMPILE) Aug 1986 (see revisions)
 > Documentation:  HELP * IMSH   HELP * CSH_COMPILE  HELP * VED_IMCSH
 > Related Files:  LIB * CSH_SEND LIB * VED_IMSH.P
 */

uses sh_subsystem;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 26 1993
        Moved everything to sh_subsystem
--- John Gibson, Dec  1 1992
        Code commoned with csh_compile into lib shell_compile
--- Aaron Sloman, Feb  1 1991
        Changed default prompt to be fixed string "imsh$ ". Should prevent
        problems causedby user's .profile redefining prompt.
--- Aaron Sloman, Jan 31 1991
        Fixed (I hope) double prompt that causes output to get out of
        phase. See Bugreport isl.42
--- Aaron Sloman, May 28 1990
        Altered so that long lines don't cause sideways scrolling
*/
