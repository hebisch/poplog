/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_helpfor.p
 > Purpose:         List documentation files matching a given string
 > Author:          Aaron Sloman, June 1982 (see revisions)
 > Documentation:   HELP * HELPFOR
 > Related Files:   LIB * HELPFOR
 */
compile_mode :pop11 +strict;

section;

include sysdefs.ph;


define lconstant Match_in_dir(dir, patt);
#_IF DEF UNIX

    define lconstant Parse(patt);
        lvars l patt;
        datalength(patt) -> l;
        [% if patt(1) == `*` then
            4, 2, allbutfirst(1, patt)
        else
            2, patt
        endif;
        if patt(l) == `*` then
            -> patt;
            allbutlast(1, patt), 4
        endif %]
    enddefine;

    sys_matchin_dir(sysfileok(dir, false), Parse(patt), 2:10) or nil
#_ELSE
    lvars pdr;
    sys_file_match(dir, patt, false, false) -> pdr;
    if pdr then pdtolist(pdr) else [] endif
#_ENDIF
enddefine;


define lconstant Create_helpfor_vedbuffer(setting_on_screen, list);
    lvars item;
    /* list contains 2-element vectors of form {doctype name} */

    {% for item in list do
        consvedcrossref(item(1), item(2), false)
    endfor %}
enddefine;


define vedhelpfor(name, search_list);
    lvars item, dir, type, patt, file, list;

    [% for item in flatten_searchlist(search_list, true) do
        if islist(item) then
            hd(item), hd(tl(item))
        else
            item, 'help'
        endif -> (dir, type);
        if strmember(`*`, name) then
            name
        else
            '*' <> name <> '*'
        endif -> patt;
        for file in Match_in_dir(dir, patt) do
            {% type, sys_fname_name(file) %}
        endfor
    endfor %] -> list;

    if list == [] then
        vedsysfileerror('HELP', name)
    endif;

    syssort(list,
            false,
            procedure(x, y) -> bool;
                if (alphabefore(x(1), y(1)) ->> bool) == 1 then
                    alphabefore(x(2), y(2)) -> bool
                endif
            endprocedure)
        -> list;

    vededit(
        consref({% [% 'helpfor ' <> name, "helpfor" %],
                   Create_helpfor_vedbuffer(% list %),
                   'Use ESC-N and ESC-H to select file'
                %}),
        vedhelpdefaults)
enddefine;


define vars ved_helpfor();
    lvars list;
    if vedargument = nullstring then
        'help' -> vedargument
    endif;
    subsystem_searchlist("vedhelpname", veddocsubsystem) -> list;
    if null(list) then
        vedhelplist -> list
    endif;
    vedhelpfor(vedargument, list)
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jan  9 1996
        Now outputs `active' cross references.
--- John Williams, Apr  8 1994
        Now uses "create buffer" option to vededit.
--- John Williams, Apr 19 1993
        Now uses \Ss when inserting cross references
--- John Williams, Jan 15 1993
        subsystem_searchlist and veddocsubsystem no longer need to be
        weakref'd (they are now in the core system)
--- Adrian Howard, Nov 11 1992
        Added option arg to sysfileok to prevent silly mishap if vedhelplist
        corrupt
--- John Williams, Nov  5 1992
        Now copes with sys_file_match returning <false> (see BR isl-fr.4193)
--- Adrian Howard, Sep  8 1992
        Added weak declarations of subsystem_searchlist and veddocsubsystem
--- John Williams, Oct 22 1991
        Now copes with -sys_matchin_dir- returning <false>
--- John Williams, Oct 10 1991
        Now uses subsystem-specific searchlist if appropriate
--- John Williams, Mar 13 1991
        Fixed call to -sys_file_match- for VMS
--- John Williams, Feb 22 1991
        Now allows explicit patterns (e.g. *word, word*, *word*).
--- John Williams, Feb 14 1991
        Changed 3rd arg to -sys_matchin_dir- from 3 to 2
        (to avoid listing sub-directories)
--- John Williams, Nov 13 1990
        Completely re-written.
 */
