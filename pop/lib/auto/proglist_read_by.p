/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/auto/proglist_read_by.p
 > Purpose:         Return list of items read from proglist by procedure
 > Author:          John Gibson, Sep  3 1991
 > Documentation:   REF *PROGLIST
 */
compile_mode:pop11 +strict;

section;

define global proglist_read_by(read_p);
    lvars svproglist = proglist, procedure read_p;
    dlocal pop_syntax_only = true;
    read_p();
    [%  until svproglist == proglist do
            if back(svproglist) == back(proglist) then
                front(svproglist) -> front(proglist);
                quitloop;
             endif;
             destpair(svproglist) -> svproglist
         enduntil
    %]
enddefine;

endsection;
