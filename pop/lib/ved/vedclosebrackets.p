/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/ved/vedclosebrackets.p
 > Purpose:         Close brackets at the end of Lisp function, list, etc
 > Author:          John Williams, Apr  9 1992
 > Documentation:   REF * vedclosebrackets
 > Related Files:   C.all/lisp/src/lispved.p, LIB * VED_WMP
 */
compile_mode :pop11 +strict;

section;

define vedclosebrackets(ket_char);
    dlocal vedbreak = false, vedwiggletimes = vedwiggletimes div 2;

    define dlocal vederror(s);
        if iscaller(ved_wmp) then
            chainfrom(vedclosebrackets, vedchardelete)
        else
            chainfrom(s, vedclosebrackets, vedputmessage)
        endif
    enddefine;

    repeat
        vedtextright();
        vedcharinsert(ket_char);
        vedcharleft();
        ved_wmp();
    endrepeat
enddefine;

endsection;
