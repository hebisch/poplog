/* --- Copyright University of Birmingham 1997. All rights reserved. ------
 > File:            $poplocal/local/auto/hasstartstringin.p
 > Purpose:         like hasstartstring but takes a list of alternatives
 > Author:          Aaron Sloman, May 22 1997
 > Documentation:
 > Related Files:
 */

/*

;;; tests

vars sss = ['abc' 'de' '12345'];

hasstartstringin('asdasdf', sss)=>
hasstartstringin('abcdef', sss)=>
hasstartstringin('123456789',sss)=>
hasstartstringin('de456789',sss)=>

*/

section;
compile_mode :pop11 +strict;

define hasstartstringin(string, strings) -> result;
    ;;; if string starts with one of strings returns the string
    ;;; it starts with otherwise false
    lvars item;
    for item in strings do
        if isstartstring(item, string) then
            return(item -> result)
        endif
    endfor;
    false -> result;
enddefine;

endsection;
