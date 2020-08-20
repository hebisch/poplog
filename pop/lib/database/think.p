/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/database/think.p
 *  Purpose:        demonstration of use of database
 *  Author:         Steven Hardy, ???
 *  Documentation:  TEACH * DATATHINK
 *  Related Files:
 */

section;
uses allpresent;
uses present;
uses instance;
uses shuffle;
vars x,y;
vars rules;
[
    [[[?x has flu]] implies  [?x feels bad]]
    [[[?x is poor]] implies  [?x feels bad]]
    [[[?x has flu] [?x kisses ?y]] implies [?y has flu]]
]
    -> rules;

define setup();
    [
        [john kisses mary]
        [john has flu]
        [ethel kisses albert]
        [bill has flu]
        [bill kisses ethel]
        [mary kisses albert]
        [carol kisses george]
    ] -> database;
enddefine;

define cycle(rules);
    vars rule ;
    for rule in rules do
        if allpresent(rule(1)) and not(present(instance(rule(3)))) then
            add(instance(rule(3)));
            return
        endif
    endfor;
    [nothing to add] =>
enddefine;

define think(amount);
    repeat amount times
        shuffle(database) -> database;
        cycle(shuffle(rules));
    endrepeat
enddefine;

endsection;
