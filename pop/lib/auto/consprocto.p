/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/auto/consprocto.p
 > Purpose:         Old procedure replaced by -consproc_to- + -suspend_chain-
 > Author:          John Gibson, Sep 15 1989
 > Documentation:   REF *OBSOLETE
 */
compile_mode:pop11 +strict;

section;

define global consprocto() with_nargs 2;
    lvars proc;
    lconstant mark = 'mark';
    consproc_to(/* args */) -> proc;
    suspend_chain(proc, 1, proc,
                        procedure(proc);
                            lvars proc;
                            chain(proc, mark, 2, copy(proc), runproc)
                        endprocedure);
    if stacklength() /== 0 and dup() == mark then
        ->  ;;; leaving proc on stack
    else
        false
    endif
enddefine;

endsection;
