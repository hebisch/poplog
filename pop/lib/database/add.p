/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/database/add.p
 *  Purpose:        add an item to the database
 *  Author:         Unknown, ???
 *  Documentation:  HELP * ADD
 *  Related Files:
 */

section $-database => add;

define sysvaries(X);
lvars X;
    until atom(X) do
        if sysvaries(dest(X) -> X) then return(true) endif
    enduntil;
    X == "?" or X == "??" or X == "=" or X == "=="
enddefine;

define global add(X);
lvars X;
    if sysvaries(X) then
        mishap(X,1,'ATTEMPT TO ADD UNDER SPECIFIED ITEM')
    endif;
    X -> it;
    X :: database -> database;
enddefine;

endsection;
