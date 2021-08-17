/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           C.all/lib/turtle/lturtle.p
 |  Purpose:        Turtle variation which uses lists of list instead of array
 |  Author:         Aaron Sloman, May 1982 (see revisions)
 |  Documentation:  HELP * LTURTLE
 |  Related Files:
 */

section;

vars cposition;
vars heading;
vars paint;
vars picture;
vars rposition;

define newposition(n);
    vars dr, dc, r, c;
    if     heading = 0   then    0 -> dr;   1 -> dc
    elseif heading = 45  then    1 -> dr;   1 -> dc
    elseif heading = 90  then    1 -> dr;   0 -> dc
    elseif heading = 135 then    1 -> dr;  -1 -> dc
    elseif heading = 180 then    0 -> dr;  -1 -> dc
    elseif heading = 225 then   -1 -> dr;  -1 -> dc
    elseif heading = 270 then   -1 -> dr;   0 -> dc
    elseif heading = 315 then   -1 -> dr;   1 -> dc
    else                mishap('Unknown heading', [%heading%])
    endif;
    rposition -> r;
    cposition -> c;
    while n > 0 do
        n - 1 -> n; r + dr -> r; c + dc -> c;
    endwhile;
    while n < 0 do
        n + 1 -> n; r - dr -> r; c - dc -> c
    endwhile;
    return(r, c)
enddefine;

define drawto(r, c);
    until rposition = r and cposition = c do
        paint -> picture(rposition)(cposition);
        if r > rposition then rposition + 1 -> rposition endif;
        if r < rposition then rposition - 1 -> rposition endif;
        if c > cposition then cposition + 1 -> cposition endif;
        if c < cposition then cposition - 1 -> cposition endif
    enduntil;
    paint -> picture(rposition)(cposition);
enddefine;

define draw(n); drawto(newposition(n)) enddefine;

define jumpto(r, c);
    r -> rposition;
    c -> cposition;
enddefine;

define jump(n); jumpto(newposition(n)) enddefine;

define newpicture(r, c);
    [%repeat r times [%repeat c times "." endrepeat%] endrepeat%]
        -> picture;
    0 -> heading;
    1 -> rposition;
    1 -> cposition;
    "*" -> paint;
enddefine;

define turn(n);
    heading + n -> n;
    while n < 0 do n + 360 -> n endwhile;
    while n >= 360 do n - 360 -> n endwhile;
    unless member(n, [0 45 90 135 180 225 270 315]) then
        mishap('Unknown heading', [%n%])
    endunless;
    n -> heading;
enddefine;

define turtle();
    newpicture(length(picture(1)),length(picture));
    'turtle ready' =>
enddefine;

newpicture(10, 10);
endsection;
