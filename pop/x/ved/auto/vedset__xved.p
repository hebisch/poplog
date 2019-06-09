/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.x/x/ved/auto/vedset__xved.p
 > Purpose:         Defines action of vedset xved ...
 > Author:          John Gibson, Nov 12 1996
 > Documentation:   REF * XVED
 */
compile_mode :pop11 +strict;

section;

define vedset__xved;
    lvars item, context = false, names;
    dlocal pop_autoload = false;
    lconstant contexts = [application currentWindow nextWindow defaultWindow
                            %"["% endvedset];
    repeat
        if lmember(itemread() ->> item, contexts) then
            if context then
                -> sysCALL(sysPUSHQ(context), sysPUSHQ(ncrev(names)),
                                                        "xved_value")
            endif;
            quitif(item == "endvedset");
            if item == "[" then
                item :: proglist -> proglist;
                listread()
            else
                item
            endif -> context;
            [] -> names;
            itemread() -> item
        endif;
        pop11_need_nextreaditem("=") -> ;
        item :: names -> names;
        if (itemread() ->> item) == "%" then
            procedure;
                dlocal pop_autoload = true;
                pop11_comp_expr_to("%") ->
            endprocedure()
        else
            if item = "true" then
                true -> item
            elseif item = "false" then
                false -> item
            elseif isword(item) then
                fast_word_string(item) -> item
            endif;
            sysPUSHQ(item)
        endif
    endrepeat
enddefine;

endsection;
