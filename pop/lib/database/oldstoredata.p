/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/database/storedata.p
 *  Purpose:        storing database in a file
 *  Author:         Aaron Sloman, ??? (see revisions)
 *  Documentation:  HELP * storedata
 *  Related Files:
 */

section;

define storedata(filename);
lvars r stdpr filename;
    define storesize(x) -> r;
    lvars x;
        define cucharout(c);
        lvars c;
            unless r > 100 then
                r + 1 -> r;
            endunless;
        enddefine;
        0 -> r;
        pr(x);
    enddefine;

    define storeprint(i, x);
    lvars i x;
    vars r;
        define pr(x);
        lvars x;
            if isstring(x) then cucharout(`'`); stdpr(x); cucharout(`'`)
            else stdpr(x)
            endif
        enddefine;

        if atom(x) or storesize(x) < 70 - i then pr(x)
        else
            pr("[");
            until x = [] then
                storeprint(i + 1, dest(x) -> x ->>r);
                if x = [] then
                elseif  atom(r) then pr(space)
                else pr(newline); sp(i + 1) endif;
            enduntil;
            pr("]");
        endif;
    enddefine;

    vars cucharout;
    discout(filename) -> cucharout;
    pr('vars database;\n');
    pr -> stdpr;
    storeprint(0, database);
    pr('\n    -> database;\n');
    cucharout(termin);
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Jul 29 1985 - added lvars and moved into database library.
--- Aaron Sloman, Jul 18 1981 - altered to reduce newlines after atoms
 */
