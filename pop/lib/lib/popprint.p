/*  --- Copyright University of Sussex 1995.  All rights reserved. ---------
 >  File:           C.all/lib/lib/popprint.p
 >  Purpose:        A pretty printer for lists of pop program
 >  Author:         Jonathan Cunningham, July 1982 (see revisions)
 >  Documentation:  HELP * POPPRINT
 >  Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +oldvar;


section popprint => popprint spacetable breaklinebefore breaklineafter
    popprintout startpopprint;

define global prstring(s);
    cucharout(`'`); pr(s); cucharout(`'`)
enddefine;

global vars spacetable;
{%
{% true, true, true, false, true, false, false, false, true%},
{% true, false, false, false, false, true, false, true, true%},
{% true, false, true, true, true, true, false, true, true%},
{% false, false, false, false, false, false, false, false, false%},
{% true, true, true, false, false, true, false, true, true%},
{% false, false, false, false, false, false, false, false, false%},
{% false, false, false, false, false, false, false, true, true%},
{% false, false, false, false, false, false, false, true, true%},
{% true, false, false, true, true, true, false, true, true %} %} -> spacetable;

vars types pcent bra ket oth;
"%" -> pcent;
"[" -> bra;
"]" -> ket;
"other" -> oth;
[ . , ; "%pcent,bra,ket,oth%] -> types;

define type(item);
    if item == "." then 1
    elseif item == "," then 2
    elseif item == ";" or item == "=>" or item == "==>" then 3
    elseif item == ["].hd then 4
    elseif item == pcent then 5
    elseif member(item,[(%"[","{"%]) then 6
    elseif member(item,[)%"]","}"%]) then 7
    elseif item.isword and identprops(item) == "syntax" then 9
    else 8
    endif
enddefine;

;;; stuff to do pretty-printing

global vars breaklinebefore;
    [else elseif elseunless if define repeat while
        until unless procedure lambda forevery foreach
        enddefine end endif endwhile enduntil endunless
        endfor endforevery endforeach endrepeat endprocedure close]
        -> breaklinebefore;

global vars breaklineafter;
    [; => then else times do ^newline ==>] -> breaklineafter;

global vars popprintout; false -> popprintout;

define global startpopprint(filename);
vars terminate;
    define terminate();
        vedline -> vvedmarkhi;
        1 -> vvedmarklo;
        ved_j();
        false -> popprintout
    enddefine;
    popval([uses vedout;]);
    12 -> vedwindowlength;
    unless filename.isstring then
        filename><'.p' -> filename
    endunless;
    vedout(filename,false,false,ved_clear,terminate) -> popprintout;
enddefine;

define global popprint(textlist);
vars item startnew cucharout;
vars x y;
    unless popprintout then
        startpopprint('program.p')
    endunless;
    popprintout -> cucharout;
    false -> startnew;
    6 -> x;
    for item in textlist do
        if startnew or member(item,breaklinebefore) then
            nl(1)
        elseif not(startnew) and subscrv(type(item),subscrv(x,spacetable)) then
            sp(1)
        endif;
        if item.isstring then
            prstring(item)
        elseunless item == newline then
            pr(item)
        endif;
        if member(item,breaklineafter) then
            true -> startnew
        else
            type(item) -> x;
            false -> startnew
        endif
    endfor
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul 31 1995
        Added +oldvar at top
 */
