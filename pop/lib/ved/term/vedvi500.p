/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/term/vedvi500.p
 >  Purpose:        Conversion of VED for Visual 500 with VT52 emulation.
 >  Author:         A Sloman, Jan 1984 (see revisions)
 >  Documentation:  HELP * V500
 >  Related Files:
 */
compile_mode :pop11 +strict;

uses-by_name vedvi500keys, vedvi500screen;

section;

define vars vedvi500();
    veduseterm("vi500") -> ;
    identfn -> vedvi500;
enddefine;

if iscaller(vedsetup) then vedvi500() endif;

endsection;


/* --- Revision History ---------------------------------------------------
--- Jason Handby, Rob Duncan, Oct 23 1989
        Separated out into "vedvi500screen.p" and "vedvi500keys.p" files
*/
