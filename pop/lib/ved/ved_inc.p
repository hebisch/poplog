/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/ved/ved_inc.p
 > Purpose:        insert character into vedbuffer (number or variable name)
 > Author:         Mark Rubinstein, May 13 1986
 > Documentation:
 > Related Files:
 */

section;

define global ved_inc();
lvars name value;
    define lconstant vcharinsert(c);
        if  strmember(c, '\n\r\t ') then
            vedcharinsert(`*`);
            vedcharleft();
            c -> vedthisline()(vedcolumn);
            vedcharright();
            vedrefreshrange(vedline, vedline, undef);
        else
            vedcharinsert(c)
        endif
    enddefine;

    if strnumber(vedargument) then
        strnumber(vedargument) -> value
    else
        consword(vedargument) -> name;
        if identprops(name) == undef or isundef(valof(name)) then
            vederror(name >< ' is not a variable name')
        endif;
        valof(name) -> value;
    endif;
    unless isinteger(value) and value >= 0 and value < 256 then
        vederror(value >< ' is inappropriate character code')
    endunless;
    vcharinsert(value);
enddefine;

endsection;
