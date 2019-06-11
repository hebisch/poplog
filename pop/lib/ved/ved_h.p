/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_h.p
 >  Purpose:        alternative help command
 >  Author:         Jonathan Cunningham, Jul  8 1985 (see revisions)
 >  Documentation:  HELP *VEDCOMMS/ved_h
 >  Related Files:  hh.p
 */

section $-library => helpfiles ved_h;

#_IF hd(sys_os_type) == "vms"

define veddirtofile (_s) -> _s;
    lvars _n _d ;
    if strmember(`.`,_s) ->> _n then
        ;;; get last '.' in path name
        _n -> _d;
        while locchar(`.`,_n + 1,_s) ->> _n do _n -> _d endwhile;
        ;;; turn [foo **** .baz] into [foo ****]baz.dir
        substring(1, _d - 1, _s)
             sys_><  ']'
             sys_><  substring(_d + 1, length(_s) - _d - 1, _s)
             sys_><  '.dir'
            -> _s
    else
        ;;; no '.' in directory name
        '[000000]'  sys_><  substring(2, length(_s) - 2, _s)  sys_><  '.DIR' -> _s
    endif;
enddefine;

#_ELSE

vars procedure veddirtofile; identfn -> veddirtofile;

#_ENDIF

vars helplist; false -> helplist;
define helpfiles();
    lvars dir dev;
    unless helplist then
        vedputmessage('Initialising \'h\' command for name search');
        [%
;;; sys_matchin_dir not implemented for vms yet, so ...
#_IF hd(sys_os_type) == "vms"
        lvars _s; inits(512) -> _s;
        lvars _rl _len;
            for dir in flatten_searchlist(vedhelplist) do
                if isprocedure(dir) then
                    nextloop
                elseif islist(dir) then
                    hd(dir) -> dir
                endif;
                sysopen(veddirtofile(dir),0,"record") -> dev;
                if dev then
                    until (sysread(dev,_s,512) ->> _rl) == 0 do
                        _s(4) fi_- 1 -> _len;
                        substring(5, _len, _s)
                    enduntil
                endif
            endfor
#_ELSE
            for dir in flatten_searchlist(vedhelplist) do
                if isprocedure(dir) then
                    nextloop
                elseif islist(dir) then
                    hd(dir) -> dir
                endif;
                if readable(dir) ->> dev then
                    sysclose(dev);
                    dl(sys_matchin_dir(sysfileok(dir),[4],3))
                endif
            endfor
#_ENDIF
        %] -> helplist
    endunless;
    vedputmessage('Searching through ' sys_>< length(helplist) sys_>< ' file names');
    return(helplist)
enddefine;

define string_match_score(s1,s2) -> _n;
    lvars _k _l;
    0 -> _n;
    length(s1) -> _k;
    length(s2) -> _l;
    if _k < _l then
        _k,_l -> _k -> _l;
        s1,s2 -> s1 -> s2
    endif;
    if issubstring(s2,1,s1) then
        10 * _l -> _n;
        return
    endif;
    for _k from 1 to _l do
        if s1(_k) == s2(_k) then
            10 + _n -> _n
        elseif _k > 1 then
            if s1(_k - 1) == s2(_k) then
                4 + _n -> _n
            endif;
            if s1(_k) == s2(_k - 1) then
                4 + _n -> _n
            endif
        endif
    endfor
enddefine;

define best_match(target_string,choices) -> best;
    lvars _k _l _word single;
    0 -> _k;
    false ->> single -> best;
        for _word in choices do
            if (string_match_score(_word,target_string) ->> _l) > _k then
                _l -> _k;
                [^ _word] -> best;
                true -> single;
            elseif _l == 0 then
            elseif _l == _k and not(member(_word,best)) then
                _word :: best -> best;
                false -> single;
            endif
        endfor;
    if _k < length(target_string) * 6 then
        false -> best
    elseif single then front(best) -> best
    endif
enddefine;

vars ved_h;
define best_help(_word) -> _word;
dlocal pop_pr_quotes = false;
lvars _k;
    if _word.isword then _word sys_>< '' -> _word endif;
#_IF hd(sys_os_type) == "vms"
   for _k from 1 to length(_word) do
   if _word(_k).islowercode then _word(_k)-32 -> _word(_k)
       /* elseif _word(_k) == `_` then `9` -> _word(_k) ;;; vms 3. */
       endif
   endfor;
#_ENDIF
    best_match(_word,helpfiles()) -> _word;
    unless _word then
        pr('NO REASONABLE MATCH\n');
        ['helpfiles' 'index'] -> _word
    endunless;
    if _word.islist then
        pr('Which of the following?\n');
        for _k from 1 to length(_word) do
            tabs(1);
            pr(_k);
            tabs(1);
            pr(_word(_k));
            nl(1)
        endfor;
        dlocal popnewline; true -> popnewline;
        sys_clear_input(pop_charin_device);
        pr('Type number <return> ');
        incharitem(charin)()-> _k;
        if vedediting then vedrefresh(); false -> vedprintingdone endif;
        if _k.isinteger and _k > 0 and _k <= length(_word) then
            _word(_k) -> _word
        else
            'Abandoned H ....' -> vedmessage;
            vedputmessage(vedmessage);
            exitfrom(ved_h)
        endif
    endif
enddefine;

define global ved_h();
lvars temp;
    unless vedgetlibfilename(vedhelplist,"vedhelpname",vedargument) then
        best_help(vedargument) -> vedargument;
    endunless;
    ved_help()
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- Adrian Howard, Sep  3 1992
        Now works ok when pop_pr_quotes is true
--- John Gibson, Jun 11 1991
        Replaced popdevin with pop_charin_device
--- Jonathan Cunningham, 17 May 1991
    fixed for v14 searchlist
--- John Gibson, Nov 11 1987
        Replaced -sys_purge_terminal- with -sys_clear_input-
--- Mark Rubinstein, Sep 17 1985 - fixed to work with new vedhelplist format.
--- Jonathan Cunningham, Jul  8 1985
    fixed so that unreadable directories in vedhelplist don't bomb out
    also modified to work with vms 4.0
    also helpfiles() exported so that saved images can be ready-initialised
 */
