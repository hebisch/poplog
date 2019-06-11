/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_ac.p
 >  Purpose:        Move each line or range to midway between margins
 >  Author:         Chris Slymon (after David Roberts), June 1983
 >  Documentation:  HELP * FORMAT
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define ved_ac;
    lvars oldchanged, _marginwidth, _textwidth, _offset;
    dlocal vedleftmargin, vedstatic;
    vedpositionpush();
    vedchanged -> oldchanged;
    false ->> vedchanged -> vedstatic;
    if isprocedure( vedleftmargin) then
        vedleftmargin() -> vedleftmargin
    endif;
    ;;; find marginwidth; it it's even, add 1
    (vedlinemax fi_- vedleftmargin) || 1 -> _marginwidth;
    vedmarkfind();
    until vedline fi_> vvedmarkhi do
        if vvedlinesize fi_> 0 then
            vedtextleft();
            vvedlinesize fi_- vedcolumn fi_+ 1 -> _textwidth;
            ((_marginwidth fi_- _textwidth) >> 1 )
                fi_+ vedleftmargin fi_+ 1 fi_- vedcolumn -> _offset;
            if _offset fi_< 0 then
                repeat negate(_offset) times
                    unless vedcolumn = 1 then
                        vedchardelete()
                    else
                        vedputmessage('Line(s) too long');
                    endunless
                endrepeat
            else
                repeat _offset times
                    vedcharinsert(` `)
                endrepeat
            endif
        endif;
        vednextline()
    enduntil;
    vedpositionpop();
    if oldchanged then oldchanged fi_+ 1 else 1 endif -> vedchanged;
enddefine;

endsection;
