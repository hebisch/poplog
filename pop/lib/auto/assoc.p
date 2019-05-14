/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/auto/assoc.p
 >  Purpose:        provide associate tables
 >  Author:         S.Hardy,1981,A.Sloman, April 1986(see revisions)
 >  Documentation:  HELP * ASSOC
 >  Related Files:  LIB * APPASSOC, NEWASSOC, NEWPROPERTY and NEWANYPROPERTY
 */
compile_mode:pop11 +strict;

section;

;;; uses lists of two element lists. Wasteful but convenient
define lconstant sysassoc(key,list);
    lvars key,list;
    ;;; assume it is always a list on entry
    until (null(fast_back(list) ->> list)) do
        if key = hd(fast_front(list)) then
            return(hd(fast_back(fast_front(list))))
        endif
    enduntil;
    return(false);
enddefine;

define updaterof sysassoc(value,key,list);
    lvars list,key,l=list,value,temp=list;
    tl(list) -> list;
    repeat
    quitif(null(list));
        if key = hd(fast_front(list)) then
            if value then
                value -> hd(fast_back(fast_front(list)));
            else
                fast_back(list) -> fast_back(l)     ;;; remove record
            endif;
            return
        else
            fast_back(list ->>l) -> list;
        endif;
    endrepeat;
    if value then
        conspair([%key,value%], fast_back(temp)) -> fast_back(temp)
    endif
enddefine;

define global assoc(list) -> result;
    lvars list,result;
    sysassoc(%"assoc"::list%) -> result;
    frozval(1,result) -> pdprops(result);
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- Aaron Sloman, May  4 1986
    Substantially rewritten to improve efficiency, using 'fast' procedures
    when safe. lvarsed, and made to remove records of items given value false.
 */
