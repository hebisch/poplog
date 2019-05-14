/* --- Copyright University of Birmingham 1996. All rights reserved. ------
 > File:            $poplocal/local/auto/which_values.p
 > Purpose:         Version of "which" that works with "!"
 > Author:          Aaron Sloman, Oct 13 1996
 > Documentation:   HELP * READPATTERN, HELP * WHICH
 > Related Files:   LIB * READPATTERN, LIB * !
 */

/*

The following procedure is a replacement for "which", as explained in
HELP * READPATTERN/which_values


   [[joe isa man]
    [jill isa woman]
    [joe lives_in london]
    [jill lives_in brighton]
    [bill isa man]
    [sue isa woman]
    [bill lives_in london]
    [sue lives_in paris]] -> database;
    database ==>

;;; test that
which_values(! [?x ?y], ! [[?x lives_in ?y]])==>
** [[joe london] [jill brighton] [bill london] [sue paris]]

which_values(![?person], ! [[?person isa woman]])==>
** [[jill] [sue]]


*/
compile_mode :pop11 +strict;
section;


define global vars which_values(Vars, Patternlist) -> List;
    ;;; Vars should be a list of variables prefixed by "?" transformed by "!"
    ;;; Patternlist should be a list of patterns, the whole having been transformed
    ;;; by "!"

    lconstant err_string = 'LIST NEEDED FOR "WHICH_VALUES"';

    if ispair(Vars) then
        if ispair(Patternlist) then
            [%forevery Patternlist do
                lvars list;
                [%for list on Vars do
                    if hd(list) == "?" or hd(list) == "??" then
                        valof(hd(tl(list)))
                    endif
                  endfor%]
              endforevery%] -> List
        else mishap(err_string, [^Patternlist])
        endif;
    else
        mishap(err_string, [^Vars] )
    endif;
enddefine;

endsection;
