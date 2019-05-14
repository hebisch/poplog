/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/lib/lib/readarray.p
 > Purpose:         Read text pictures into arrays
 > Author:          David Young, Jan 31 1988
 > Documentation:   HELP * READARRAY
 > Related Files:   LIB * SHOWARRAY, LIB TURTLE
 */

;;; For reading tables of characters into arrays. A bit like
;;; READPICTURE in the TURTLE library, on which this is based, but
;;; is entirely independent of LIB TURTLE.

section;

global vars sa_yup; ;;; Funny name from interaction with LIB * SHOWARRAY

if isundef(sa_yup) then false -> sa_yup endif;

define lconstant picline -> s -> n;
    ;;; Read a line and make a string. Return it, and its length, or termin
    lvars s c, n = 0;
    until (cucharin() ->> c) == `\n` or c == termin do
        c; n + 1 -> n;
    enduntil;
    consstring(n) -> s;
    if c == termin then termin -> n endif;
enddefine;

define global procedure readarray() -> picarray;
    ;;; Input terminated by end of file, end of marked range, or a blank
    ;;; line.
    lvars picarray;
    lvars x y xmax ymax y0 y1 ydir c line linelen linelist;

    0 ->> linelen ->> xmax -> ymax;
    [% repeat
             picline() -> line -> linelen;
         quitif (linelen == termin or linelen == 0);
             max(linelen,xmax) -> xmax;
             ymax + 1 -> ymax;
             line
         endrepeat %] -> linelist;

    ;;; Make the array now the size is known
    newarray([1 ^xmax 1 ^ymax],' ') -> picarray;
    if sa_yup then
        ymax -> y0, 1 -> y1; -1 -> ydir
    else
        1 -> y0; ymax -> y1; 1 -> ydir
    endif;

    for y from y0 by ydir to y1 do
        dest(linelist) -> linelist -> line;
        for x from 1 to datalength(line) do
            line(x) -> c;
            if isnumbercode(c) then
                c - `0`
            else
                consstring(c,1)
            endif -> picarray(x,y)
        endfor
    endfor;

    if linelen == termin then [] -> proglist endif;
enddefine;

endsection;
