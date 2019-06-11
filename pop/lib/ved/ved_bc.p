/*  --- Copyright University of Sussex 1995.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_bc.p
 >  Purpose:        centre range as a block
 >  Author:         Chris Slymon (after David Roberts), June 1983
 >  Documentation:  HELP * FORMAT/BC
 >  Related Files:  LIB * VED_BL, *VED_BR, *VED_AC
 */
compile_mode :pop11 +strict;

section ;

define vars ved_bc();
    dlocal vedstatic=false, vedleftmargin;
    lvars oldchanged=vedchanged,
        _leftcol _rightcol _textwidth _marginwidth _offset;
    false -> vedchanged;
    vedpositionpush();
    if isprocedure( vedleftmargin) then
        vedleftmargin() -> vedleftmargin
    endif;
;;; find width of marked text
    9999 -> _leftcol; 0 -> _rightcol;
    vedmarkfind();
    until vedline fi_> vvedmarkhi do
        if vvedlinesize fi_> 0 then
            if vvedlinesize fi_> _rightcol then
                vvedlinesize -> _rightcol
            endif;
            vedtextleft();
            if vedcolumn fi_< _leftcol then
                vedcolumn -> _leftcol
            endif
        endif;
        vednextline()
    enduntil;
    _rightcol fi_- _leftcol fi_+ 1 -> _textwidth;
    (vedlinemax fi_- vedleftmargin) || 1 -> _marginwidth;
    ;;; i.e. if _marginwidth is even, add 1 to it

;;; find how far and which way to move text
    (_marginwidth fi_- _textwidth) div 2 fi_+ vedleftmargin fi_+ 1 fi_- _leftcol
        -> _offset;
;;; move marked text
    if _textwidth fi_<= _marginwidth then
        vedmarkfind();
        until vedline fi_> vvedmarkhi do
            if vvedlinesize fi_> 0 then
                vedtextleft();
                if _offset fi_< 0 then
                    repeat negate(_offset) times
                        vedchardelete()
                    endrepeat
                else
                    repeat _offset times
                        vedcharinsert(` `)
                    endrepeat
                endif
            endif;
            vednextline()
        enduntil
    else
        vedputmessage('Text too wide')
    endif;
    vedpositionpop();
    if oldchanged then oldchanged fi_+ 1 else 1 endif -> vedchanged;
enddefine;

endsection;
