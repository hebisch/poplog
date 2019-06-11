/* --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/term/vedsun.p
 >  Purpose:        Configure ved for Sun Workstation
 >  Author:         Ben Rubinstein, July 16 1986 (see revisions)
 >  Documentation:  HELP *VEDSUN *VEDWINSUN
 >  Related Files:  LIB *VEDSUN *VEDWINSUN
 */
compile_mode :pop11 +strict;

uses-by_name vedsunkeys, vedsunscreen;

section;

define vars vedsun();
    veduseterm("sun") -> ;
    identfn -> vedsun;
enddefine;

if iscaller(vedsetup) then vedsun() endif;

endsection;


/* --- Revision History ---------------------------------------------------
--- Rob Duncan & Jason Handby, Sept/Oct 1989
        Moved most code into "vedsunscreen.p" and "vedsunkeys.p" files
        and defined -vedsun- just to call -veduseterm-
*/
