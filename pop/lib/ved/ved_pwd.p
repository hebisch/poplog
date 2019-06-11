/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 |  File:           $usepop/master/C.all/lib/ved/ved_pwd.p
 |  Purpose:        Print working directory
 |  Author:         A.Sloman many years ago (see revisions)
 |  Documentation:  HELP * CURRRENT_DIRECTORY, HELP *VEDCOMMS
 |  Related Files:  LIB * PWD
 */

section;

define global ved_pwd;
    vedputmessage(current_directory)                  
enddefine;

endsection;

/* --- Revision History
--- Aaron Sloman, Nov  9 1986 - use current_directory
*/
