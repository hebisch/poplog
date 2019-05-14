/* --- Copyright University of Birmingham 1992. All rights reserved. ------
 > File:            $poplocal/local/auto/concat_strings.p
 > Purpose:         Build a string from a list or vector of strings
 > Author:          Aaron Sloman, Mar  3 1992 (see revisions)
 > Documentation:   HELP * CONCAT_STRINGS
 > Related Files:   REF * ><, REF * sys_><, LIB * SPRINTF
 */

compile_mode: pop11 +strict;

section;

define lconstant procedure get_chars_from(item);
    ;;; If it is a string, vector or list, explode it, otherwise
    ;;; stack the characters from its printed representation
    lvars item;
    if isstring(item) or isword(item) then
        explode(item)
    else
        dlocal cucharout = identfn; ;;; just stack characters

        syspr(item)
    endif
enddefine;

define procedure concat_strings(struct);
    lvars struct;
    consstring
       (#|
            if isvector(struct) then appdata
            elseif islist(struct) then applist
            else
                mishap(struct, 1, 'LIST OR VECTOR NEEDED')
            endif( struct, get_chars_from )
        |#)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Nov  2 1992
    Changed to use appdata and applist. Far more compact
--- Aaron Sloman, Nov  1 1992
    Extended so that the vector can contain arbitrarily printable
    objects as well as words and strings, e.g.
        concat_strings({'cat' ^space 66.5 ^space ^(10/3) ^space ^hd}) =>

    Updated the help file.
 */
