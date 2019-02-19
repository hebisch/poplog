/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/src/sys_read_path.p
 > Purpose:
 > Author:          John Gibson, Mar  8 1988 (see revisions)
 > Documentation:   REF *SECTIONS
 */

;;; ----------------- READING IDENTIFIER PATHNAMES -------------------------

#_INCLUDE 'declare.ph'

constant
        procedure (section_subsect, word_identifier),
        pop_section
    ;

;;; ----------------------------------------------------------------------

define sys_read_path(item, use_itemread, needsect);
    lvars   item, sect, procedure nxtitem, needsect, use_itemread,
            abs_path = true;
    lconstant terminators = [; , %termin, "\n"%];

    if use_itemread then nextitem else nextreaditem endif -> nxtitem;
    if item == "$-" then
        ;;; pathname starts from top-level section
        returnif(fast_lmember(nxtitem() ->> item, terminators))
                        (if needsect == [] then [^pop_section]
                         elseif needsect then pop_section
                         else "$-"
                         endif);
        pop_section -> sect;
        Sys$-Prglst$-Chop()
    elseif fast_lmember(item, terminators) then
        returnunless(needsect) (item);
        mishap(item, 1, 'EXPECTING SECTION NAME')
    elseunless nxtitem() == "$-" or needsect then
        return(item)
    else
        ;;; starts from current section
        false -> abs_path;
        current_section -> sect
    endif;

    ;;; while the current item is a section name, go down to
    ;;; next subsection
    define lconstant next_sect();
        if not(isword(item)) or fast_lmember(item, terminators) then
            mishap(item, 1, 'EXPECTING SECTION/IDENTIFIER NAME AFTER $-')
        endif;
        returnunless(nxtitem() == "$-") (false);
        Sys$-Prglst$-Chop();
        section_subsect(item, sect, true) -> sect;
        nxtitem() -> item;
        Sys$-Prglst$-Chop();
        true
    enddefine;

    if needsect == [] then
        [%  if abs_path then sect endif,
            while next_sect() do sect endwhile,
            section_subsect(item, sect, true)
        %]
    else
        while next_sect() do endwhile;
        if needsect then
            section_subsect(item, sect, true)
        else
            word_identifier(item, sect, "undef")
        endif
    endif
enddefine;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  4 1992
        Improved error checking
 */
