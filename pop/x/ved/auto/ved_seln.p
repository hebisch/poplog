/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/ved/auto/ved_seln.p
 > Purpose:         Operates on selections.
 > Author:          Jonathan Meyer, Aug  2 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define vars ved_seln;
    lvars arg = vedargument;
    dlocal ved_on_status = false;
    if member(arg, ['cut' 'copy' 'paste' 'help' 'undo' 'compile']) then
        valof(consword('vedselection_' sys_>< arg))();
    elseif arg = 't' then vedselection_copy()
    elseif arg = 'h' then vedselection_help()
    elseif arg = 'c' then vedselection_copy()
    elseif arg = 'd' then vedselection_cut()
    elseif arg = 't' then vedselection_paste()
    elseif arg = 'u' then vedselection_undo();
    else
        vederror('seln: cut(d), copy(c), paste(t), help(h), undo(u) or compile(c)?')
    endif;
enddefine;

endsection;
