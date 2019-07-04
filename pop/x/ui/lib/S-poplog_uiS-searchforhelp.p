/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-searchforhelp.p
 > Purpose:         Some procedures for scanning documentation directories (based on ved_helpfor)
 > Author:          Julian Clinton, May 1991 (see revisions)
 > Documentation:
 > Related Files:   LIB * VED_HELPFOR, HELP * HELPFOR
 */
compile_mode :pop11 +strict;

section $-poplog_ui;

include pop_uiP.ph;
include sysdefs.ph;

lconstant vedname_of = newassoc([[help vedhelpname]
                                 [ref vedrefname]
                                 [teach vedteachname]
                                 [doc veddocname]]);

;;; remove_duplicates assumes that list has been sorted
define lconstant remove_duplicates(list) -> newlist;
lvars list newlist current_item;

    unless islist(list) then
        mishap(list, 1, 'List needed');
    endunless;

    [%
        until null(list) do
            fast_destpair(list) -> list -> current_item;
            until null(list) or current_item /= hd(list) then
                fast_destpair(list) -> list -> current_item;
            enduntil;
            current_item;
        enduntil;
    %] -> newlist;
enddefine;


;;; searchfor help takes a list of subsystem names, a string pattern to
;;; search for, a list of filetypes to be looked for and a boolean
;;; to define whether to search through the ref index files (although
;;; the refindex files are only searched if "ref" is somewhere in the
;;; filetypes list)
define searchforhelp(ss_list, patt, filetypes, use_indexes) -> flist;
    lvars ss_list dir file files filetypes item list flist patt type
        use_indexes temp_list ss searchlist repeater;
    dlocal pop_pr_quotes = false;

    define lconstant lists_of_subsystem(ss) -> s_list;
    lvars ss type s_list;
        unless ss == "pop11" then
            useslib(ss sys_>< '_subsystem')
        endunless;
        [%
            fast_for type in filetypes do
                if vedname_of(type) then
                    if ss == "pop11" then
                        dl(vedhelplist);    ;;; contains everything
                        quitloop();
                    else
                        dl(subsystem_searchlist(vedname_of(type), ss));
                    endif;
                endif;
            endfast_for;
        %] -> s_list;
    enddefine;

    unless isstring(patt) then
        patt sys_>< nullstring -> patt;
    endunless;

    [%
        fast_for ss in ss_list do
            if ss then  ;;; need to check since the GUI replaces unwanted
                        ;;; filetypes with <false> in the filetypes list
                lists_of_subsystem(ss) -> temp_list;
                unless temp_list == [] then
                    temp_list, true.flatten_searchlist -> list;
                    sys_grbg_list(temp_list);

                    VED_SS_CHECK(ss);

                    if use_indexes then
                        ved_??_search_doc_index('*' sys_>< patt sys_>< '*', list) -> files;
                        fast_for item in files do
                            dest(item) -> item -> type;
                            if type and lmember(type, filetypes) then
                                fast_for file in item do
                                    sprintf(file(1),
                                            sys_fname_name(file(2)), type,ss,'%p %p %p/%p');
                                endfast_for;
                            endif;
                        endfast_for;
                    endif;

                    fast_for item in list do
                        if islist(item) then
                            if ispair(tl(item)) then
                                hd(item), hd(tl(item))
                            else
                                undef, undef        ;;; to prevent library directories
                                                    ;;; being searched
                            endif;
                        else
                            item, "help"
                        endif -> (dir, type);

                        if lmember(type, filetypes) then
#_IF DEF UNIX
                            sys_matchin_dir(sysfileok(dir), [4 2 ^patt 4], 2:10) -> files;
#_ELSE
                            '*' <> patt <> '*' -> patt;
                            if sys_file_match(dir dir_>< patt, '*', false, false)
                                    ->> repeater then
                                expandlist(pdtolist(repeater))
                            else
                                false
                            endif -> files;
#_ENDIF
                            nextunless(files);
                            fast_for file in files do
                                sprintf(sys_fname_name(file), type, ss, '%p %p %p');
                            endfast_for;
                            sys_grbg_list(files);
                        endif;
                    endfast_for;
                endunless;
            endif;
        endfast_for %] -> flist;

    syssort(flist, false, alphabefore) -> list;
    remove_duplicates(list) -> flist;
    sys_grbg_list(list);
enddefine;

endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 28 1993
        Changes for POPC
--- John Gibson, Jan 19 1993
        Made lists_of_subsystem compile subsystem libs at run-time
--- Adrian Howard, Sep  4 1992
        Now works properly if pop_pr_quotes is true
--- John Gibson, Nov 21 1991
        Changed sys_file_match 2nd arg to '*' and added expandlist()
        around pdtolist. Also added sys_grbg_list after fast_for loop.
--- John Gibson, Oct 31 1991
        VMS mod to searchforhelp
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    Now in section $-poplog_ui.
Julian Clinton, 21/8/91
    Removed defs for ved_help and ved_ref (now in x*helptool.p).
    Only produces index lists for selected filetypes (except for Pop-11).
Julian Clinton, 19/8/91
    Searches for entries at the start of lines only (to ensure the
    call finds the definition of the identifier).
    Removed code which only searches index files if "ref" files have
    been included.
    Added new ved_help.
Julian Clinton, 2/8/91
    Now removes duplicate entries.
Julian Clinton, 20/6/91
    Added checking for each type of file.
Julian Clinton, 19/6/91
    Changed to use subsystem_searchlist.
    Added call to sys_grbg_list.
Julian Clinton, 18/6/91
    Changed first argument to -searchforhelp- to be a subsystem name.
    Added to check to ensure library search paths are not searched.
Julian Clinton, 11/6/91
    Changed >< to sys_>< in call of ved_??_search_doc_index in
    searchforhelp.
Jonathan Meyer, 7/6/91
    Added -use_indexes- type. Made the 'help' type a "help".
    Reinstated sorting of list.
 */
