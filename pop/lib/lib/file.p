/*  --- Copyright University of Sussex 1996. All rights reserved. ----------
 >  File:           C.all/lib/lib/file.p
 >  Purpose:        yet another program for storing data in files.
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:  HELP * FILE
 >  Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

define lconstant storevar(word, filename);
    dlocal cucharout, pop_pr_quotes = true;

    define storesize(x) -> r;
        define dlocal cucharout(c);
            unless r > 100 then
                r + 1 -> r;
            endunless;
        enddefine;
        0 -> r;
        pr(x);
    enddefine;

    define storeprint(i, x);
        lvars r;
        if atom(x) or storesize(x) < 70 - i then pr(x), return endif;
        pr("[");
        until x = [] do
            storeprint(i + 1, dest(x) -> x ->> r);
            if x = [] then
            elseif atom(r) then pr(space)
            else pr(newline); sp(i + 1)
            endif
        enduntil;
        pr("]")
    enddefine;

    discout(filename) -> cucharout;
    spr("vars"); pr(word); npr(";");
    storeprint(0, valof(word));
    pr(newline); pr(tab); spr("->"); pr(word); npr(";");
    cucharout(termin);
enddefine;


define global macro file(var, In);
    dlocal popnewline = true;
    unless isword(var) and identprops(var) = 0 then
        mishap('Variable name expected after FILE', [^var])
    endunless;
    unless In == "in" then
        mishap('IN needed after variable name in FILE command', [^In])
    endunless;
    storevar(var, sysfileok(rdstringto([^newline ^termin ;])))
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Apr 25 1996
        storevar now sets pop_pr_quotes true.
        No longer creates section $-file.
 */
