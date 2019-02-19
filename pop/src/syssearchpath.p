/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/syssearchpath.p
 > Purpose:         Find file using directory search list
 > Author:          John Williams, Dec  6 1988 (see revisions)
 > Documentation:
 > Related Files:
 */

#_INCLUDE 'declare.ph'

constant
        procedure (sys_file_stat sys_search_doc_index)
    ;

weak vars
    vvedgotoplace
    ;

section;

define syssearchpath(dirs, names);
    lvars multi, dir, file, dirlist, name, n;

    if isboolean(names) then
        if names then 2:01 else 2:11 endif -> multi;
        dirs -> names;
        -> dirs
    else
        2:11 -> multi
    endif;

    if not(islist(names)) then
        ;;; Normal case, unless trying multiple extensions, plurals, etc
        [^names] -> names
    endif;

    if not(islist(dirs)) then
        ;;; Unlikely, unless a recursive call via an ident
        [^dirs] -> dirs
    endif;

    define lconstant File_exists() with_nargs 1;
        sys_file_stat(#_< {} >_#)
    enddefine;

    define lconstant Return(file, dirlist);
        if dirlist then
            file :: tl(dirlist)
        else
            file
        endif
    enddefine;

    lconstant macro RETURN = [ fast_chain(file, dirlist, Return) ];

    for dir in dirs do
        false -> file;
        if islist(dir) then
            dir -> dirlist;
            hd(dirlist) -> dir
        else
            false -> dirlist
        endif;
        if isstring(dir) then
            for name in names do
                sysfileok(dir) dir_>< name -> file;
                if File_exists(file) then
                    RETURN
                elseif (sys_search_doc_index(name, dir, multi) ->> n) /== 0
                then
                    if n == 1 then
                        -> file;
                        fast_subscrv(3, file) -> weakref vvedgotoplace;
                        sysfileok(fast_subscrv(2, file)) -> file
                    else
                        consvector(n) -> file
                    endif;
                    RETURN
                endif
            endfor
        elseif isprocedure(dir) then
            for name in names do
                if (fast_apply(name, dir) ->> file) then
                    RETURN
                endif
            endfor
        elseif isword(dir) then
            if (syssearchpath(valof(dir), names) ->> file) then
                RETURN
            endif
        elseif isident(dir) then
            if (syssearchpath(idval(dir), names) ->> file) then
                RETURN
            endif
        else
            mishap(dir, 1, 'ILLEGAL DIRECTORY SPECIFIER')
        endif
    endfor;
    false
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jan  5 1996
        Now takes optional third boolean argument, which if true, indicates
        that duplicate REF entries (in the same directory) should be
        returned as a vector.
--- John Gibson, Apr  2 1992
        Made vvedgotoplace weak
--- John Williams, May 17 1990
        ident in search list can now refer to a directory or procedure
        as well as another search list.
--- John Williams, May 15 1990
        -syssearchpath- now allows an ident in the search list
--- John Williams, Mar  2 1990
        Use of -sys_file_stat- accidentally fixes FR 4301!
--- John Williams, May  5 1989
        Now tries searching documentation index too.
--- John Williams, Dec  6 1988
        Uses -sys_file_stat- instead of -readable- to test file existence
 */
