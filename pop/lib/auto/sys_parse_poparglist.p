/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/sys_parse_poparglist.p
 > Purpose:         Parsing flag/argument pairs in -poparglist-
 > Author:          John Williams, Apr  6 1992
 > Documentation:   REF * sys_parse_poparglist
 > Related Files:   LIB * NEWMAPPING
 */
compile_mode :pop11 +strict;

section;

define global sys_parse_poparglist(argspec, arglist) -> (prop, arglist);
    lvars arg, want_arg;

    define lconstant Isflag(arg, argspec);
        lvars spec;
        for spec in argspec do
            unless length(spec) == 2 do
                mishap(spec, 1, 'MALFORMED ARGUMENT SPEC')
            endunless;
            if spec(1) = arg then
                return(spec(2))
            endif
        endfor;
        false
    enddefine;

    newmapping([], 8, false, false) -> prop;
    until null(arglist) do
        fast_front(arglist) -> arg;
        if (Isflag(arg, argspec) ->> want_arg) then
            if prop(arg) then
                mishap(arg, 1, 'DUPLICATE FLAGS IN ARGLIST')
            endif;
            fast_back(arglist) -> arglist;
            if want_arg == 0 then
                true -> prop(arg)
            else
                if null(arglist)
                or Isflag(fast_front(arglist), argspec) then
                    mishap(arg, 1, 'NO ARGUMENT SUPPLIED WITH FLAG')
                else
                    fast_destpair(arglist) -> arglist -> prop(arg)
                endif
            endif
        else
            quitloop
        endif
    enduntil
enddefine;

endsection;
