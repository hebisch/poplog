/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/ved/auto/vedselection_new_class.p
 > Purpose:         Define a new XVed selection class
 > Author:          John Gibson, Jun  4 1993
 > Documentation:   REF * XVED
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define vedselection_new_class(name, find_start_p, find_end_p);
    lvars name, find_start_p, find_end_p, idname;
    dlocal current_section = pop_section;
    name :: ( {%find_start_p, find_end_p%}
        :: sys_current_val("ident $-xved$-xvedselectionclasses"))
                    -> sys_current_val("ident $-xved$-xvedselectionclasses");
    "vedmouse__select_" <> name -> idname;
    pop11_define_declare(idname, sysGLOBAL, sysCONSTANT, "procedure");
    sysPASSIGN(sys_current_val("ident $-xved$-make_class_selection")(%name%),
                                                    idname);
enddefine;

endsection;
