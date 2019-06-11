/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/term/vedtvi.p
 >  Purpose:        For TELEVIDEO MODEL TVI 920C Emulating VT52
 >  Author:         A Sloman, 1982 (see revisions)
 >  Documentation:  HELP * TVI
 >  Related Files:  LIB * TVI925
 */
compile_mode :pop11 +strict;

uses-by_name vedtvikeys, vedtviscreen;

section;

define vars vedtvi();
    veduseterm("tvi") -> ;
    identfn -> vedtvi;
enddefine;

if iscaller(vedsetup) then vedtvi() endif;

endsection;


/* --- Revision History ---------------------------------------------------
--- Jason Handby, Jul 11 1989
        Seperated into "vedtviscreen.p" and "vedtvikeys.p"
*/
