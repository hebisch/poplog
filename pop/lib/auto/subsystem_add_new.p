/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/lib/auto/subsystem_add_new.p
 > Purpose:         Make an (outer) subsystem record and add it to
 >                  sys_subsystem_table
 > Author:          John Gibson, Apr 26 1993 (see revisions)
 > Documentation:   REF * SUBSYSTEM
 */
compile_mode :pop11 +strict;

section;

define subsystem_add_new(ssname, pdrs, extn, prompt, searchlists, title);
    lvars   ssname, pdrs, extn, prompt, searchlists, title, ss,
            curr_table = sys_current_val("sys_subsystem_table");
    check_word(ssname);

    procedure;
        lvars ss;
        ;;; this little wrinkle is necessary for POPC ....
        dlocal sys_subsystem_table = curr_table;

        returnunless(is_subsystem_loaded(ssname, true));
        ;;; exists
        if pop_debugging then
            ;;; remove existing entry
            [%  for ss in curr_table do
                    unless ss(1) == ssname then ss endunless
                endfor
            %] -> curr_table
        else
            mishap(ssname, 1, 'SUBSYSTEM ALREADY EXISTS')
        endif
    endprocedure();

    check_string(extn); check_string(prompt); check_string(title);
    unless islist(searchlists) then
        mishap(searchlists, 1, 'LIST NEEDED')
    endunless;

    if isprocedure(pdrs) then
        consvector(pdrs, 1) -> pdrs
    elseunless isvector(pdrs) or isword(pdrs) then
        mishap(pdrs, 1, 'PROCEDURE, VECTOR, OR WORD NEEDED')
    endif;

    writeable
    [   ^ssname         ;;; SS_NAME
        ^pdrs           ;;; SS_PROCEDURES
        ^extn           ;;; SS_FILE_EXTN
        ^prompt         ;;; SS_PROMPT
        ^searchlists    ;;; SS_SEARCH_LISTS
        ^title          ;;; SS_TITLE
    ] -> ss;

    [% dl(curr_table), ss %] -> sys_current_val("sys_subsystem_table")
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 28 1997
        Made it replace an existing subsystem entry if pop_debugging is true
 */
