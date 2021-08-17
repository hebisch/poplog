/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           C.all/lib/turtle/readpicture.p
 |  Purpose:        read in a turtle picture from a file or termianl
 |  Author:         Unknown, ??? (see revisions)
 |  Documentation:  HELP * READPICTURE
 |  Related Files:  LIB * TURTLE.
 */

;;; readpicture() called in a file or from a terminal, reads in a picture
;;; up to the end of the file or when CTRL-Z is typed.
;;; The picture is stored in the turtle picture

section $-turtle => readpicture;

vars _fin;

define readpicline();
    ;;; read a line and make a string. Return it, or termin
    vars _c _n;
    0 -> _n;
    until (cucharin() ->> _c) == `\n` or _c == termin do
        _c; _n + 1 -> _n;
    enduntil;
    if _c == termin then true -> _fin endif;
    consstring(_n)
enddefine;

define global readpicture();
    vars _fin _pic _s _xmax _ymax,_x,_y;
    0 -> _xmax;
    false -> _fin;
    {%
         until _fin or ((datalength(readpicline()->> _s) ->> _x) == 0 and _fin) do
             _s;
             max(_x,_xmax) -> _xmax;
         enduntil
         %} -> _pic;
    datalength(_pic) -> _ymax;
    newpicture(_xmax,_ymax);
    for _y from 1 to _ymax do
        _pic(_ymax - _y + 1) -> _s;
        for _x from 1 to datalength(_s) do
            subscrs(_x,_s) -> _fin;
            if isnumbercode(_fin) then _fin - `0` else consword(_fin,1)
            endif -> picture(_x,_y);
        endfor
    endfor;
    unless cucharin == charin then [] -> proglist endunless;
enddefine;

endsection;
