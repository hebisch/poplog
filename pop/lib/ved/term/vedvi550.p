/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/term/vedvi550.p
 >  Purpose:        lib v500 actually for Visual 550.- liable to change
 >  Author:         Aaron Sloman, Nov 1983 (see revisions)
 >  Documentation:  HELP * V550
 >  Related Files:
 */
compile_mode :pop11 +strict;

uses-by_name vedvi550keys, vedvi550screen;

section;

define vars vedvi550();
    veduseterm("vi550") -> ;
    identfn -> vedvi550;
enddefine;

if iscaller(vedsetup) then vedvi550() endif;

endsection;


/* --- Revision History ---------------------------------------------------
--- Jason Handby, Rob Duncan, Oct 23 1989
        Separated into "vedvi550screen.p" and "vedvi550keys.p" files.
--- Ben Rubinstein, Oct 12 1986 - vedenter, vedredo indirected through ..key
*/
