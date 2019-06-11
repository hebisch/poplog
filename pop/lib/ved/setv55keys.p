/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/ved/setv55keys.p
 >  Purpose:        reset Function keys keys on Visual 55. (needs doing twice)
 >  Author:         Aaron Sloman, Aug 1984 (see revisions)
 >  Documentation:  HELP * SETV55KEYS
 >  Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

;;; for some reason it doesn't work unless done twice.

sysflush(popdevout);
rawoutflush();
    vedoutascii('\^[@1\^[P\^[|\^[@2\^[Q\^[|');
    vedoutascii('\^[@3\^[R\^[|\^[@4\^[ \^[|');
    vedoutascii('\^[@5\^[!\^[|\^[@6\^["\^[|');
    vedoutascii('\^[@7\^[#\^[|\^[@8\^[$\^[|');
    vedoutascii('\^[@9\^[%\^[|\^[@A\^[&\^[|');
    vedoutascii('\^[@B\^[\'\^[|\^[@C\^[(\^[|');
    rawoutflush();
    vedoutascii('\^[@1\^[P\^[|\^[@2\^[Q\^[|');
    vedoutascii('\^[@3\^[R\^[|\^[@4\^[ \^[|');
    vedoutascii('\^[@5\^[!\^[|\^[@6\^["\^[|');
    vedoutascii('\^[@7\^[#\^[|\^[@8\^[$\^[|');
    vedoutascii('\^[@9\^[%\^[|\^[@A\^[&\^[|');
    vedoutascii('\^[@B\^[\'\^[|\^[@C\^[(\^[|');
    rawoutflush();
