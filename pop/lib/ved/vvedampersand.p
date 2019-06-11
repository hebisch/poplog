/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/vvedampersand.p
 > Purpose:         Backward compatability for ved search vars
 > Author:          Jonathan Meyer, Sep 28 1993
 > Documentation:   REF *OBSOLETE
 */
#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +strict;
section;

vars
    vvedampersand       = 16:9A,    ;;; replaces "@&"
    vvedpercent         = 16:99,    ;;; replaces "@%"
    vvedquery           = 16:98,    ;;; replaces "@?"
    vvedsrchendline     = 16:9B,
    vvedsrchstartline   = `\n`,     ;;; codes to match start and end of line
;

endsection;
