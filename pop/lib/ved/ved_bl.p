/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_bl.p
 >  Purpose:        move marked block specified number of places to the left
 >  Author:         Chris Slymon (after David Roberts), June 1983 (see revisions)
 >  Documentation:  HELP * FORMAT/BL
 >  Related Files:  SHOWLIB * VED_BR, *AL
 */
compile_mode :pop11 +strict;

section;

define ved_bl;
    lvars _offset, oldchanged=vedchanged;
    dlocal vedstatic, vedbreak, vedautowrite;
    vedpositionpush();
    false ->> vedstatic ->> vedbreak -> vedautowrite;
    vedargint(vedargument) -> _offset;
    ;;; prevent attempt to push text left of screen
    vedmarkfind();
    until vedline > vvedmarkhi do
        if vvedlinesize > 0 then
            vedtextleft();
            if vedcolumn <= _offset then
                vedcolumn - 1 -> _offset
            endif
        endif;
        vednextline()
    enduntil;
    ;;; remove spaces
    vedmarkfind();
    unless _offset > 0 then
        vederror('Line(s) already at left margin');
    endunless;
    _offset + 1 -> _offset;
    until vedline > vvedmarkhi do
        if vvedlinesize > 0 then
            _offset -> vedcolumn;
            vedclearhead();
        endif;
    vednextline()
    enduntil;
    vedpositionpop();
    if oldchanged then oldchanged + 1 else 1 endif -> vedchanged;
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Aaron Sloman, Nov 11 1989
    changed vedargnum to vedargint
--- Simon Nichols, Nov 18 1986
        Removed lvars declaration of VEDSTATIC.
*/
