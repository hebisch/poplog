/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/src/pop11_sections.p
 > Purpose:
 > Author:          John Gibson, Mar  7 1988 (see revisions)
 > Documentation:   REF *SYNTAX, REF *POPSYNTAX
 */

;;; -------------------- SECTION SYNTAX ---------------------------------

#_INCLUDE 'declare.ph'

constant
        procedure (pop11_comp_stmnt_seq_to, pop11_exec_stmnt_seq_to,
        pop11_EMPTY, sys_read_path, section_import, section_export
        ),
        pop_section
    ;

vars
        procedure (pop_expr_inst), pop_expr_item, popclosebracket_exec
    ;

;;; -----------------------------------------------------------------

constant
    Sys$-Pop11$-Sections = true;    ;;; used to force inclusion of this file

constant syntax endsection = pop_undef;
;;;
define syntax section;
    lvars item, exports, imports, sect_list;
    dlocal current_section;
    if (readitem() ->> item) == "=>" then
        mishap(0, 'MISSING SECTION NAME')
    elseunless item == ";" then
        ;;; not the top-level section
        sys_read_path(item, false, []) -> sect_list;
        false -> imports;
        [] -> exports;
        until (readitem() ->> item) == ";" do
            if item == "=>" then
                exports -> imports, [] -> exports;
            elseunless isword(item) then
                mishap(item, 1, 'IMPERMISSIBLE ITEM IN section STATEMENT')
            elseunless item == "," then
                item :: exports -> exports
            endif
        enduntil;
        unless imports then exports -> imports, [] -> exports endunless;
        fast_for item in sect_list do
            item -> current_section;
            if item /== pop_section then
                applist(imports, section_import);
                applist(exports, section_export)
            endif
        endfast_for
    else
        ;;; the top-level
        pop_section -> current_section
    endif;
    if popclosebracket == popclosebracket_exec then
        pop11_exec_stmnt_seq_to
    else
        pop11_comp_stmnt_seq_to
    endif("endsection") ->
enddefine;

define syntax -1 $-;
    lvars item;
    if pop_expr_inst == pop11_EMPTY then
        ;;; began expression - start from top-level
        "$-" -> item
    elseif pop_expr_inst == sysPUSH then
        ;;; preceded by word - start from current_section
        "$-" :: proglist -> proglist;
        pop_expr_item -> item
    else
        mishap(0, 'INVALID SYNTAX FOR $-')
    endif;
    sys_read_path(item, false, false) -> pop_expr_item;
    sysPUSH -> pop_expr_inst
enddefine;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  1 1992
        endsection initialised to pop_undef
 */
