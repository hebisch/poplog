/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/lib/buildindex.p
 > Purpose:         Create index files for use with popindex and ved_sourcefile
 > Author:          Aaron Sloman, Apr 10 1989 (see revisions)
 > Documentation:   HELP POPINDEX
 > Related Files:   LIB * POPINDEX, * VED_SOURCEFILE, $POPCOM/MKIND*
 */

compile_mode:pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

/*

Run this as
    buildindex(popindexlist, popindex_filename, boolean);

where
    popindexlist is a list of patterns to match to get files to analyse,
    popindex_filename is a file name to which suffixes indicating contents
        should be added for the different index entries
    boolean is true if logical names (e.g. $usepop, $popvedlib) should
        be preserved at the front of file names in index entries.

Currently uses -popindex_suffixlist- and -choose-index-, as defined
in LIB * POPINDEX.

Called from LIB * MKIND with standard Poplog source and library
directories in -popindexlist- and -popindex_filename- set to
'$usepop/pop/ref/popindex.'.

*/

uses popindex;      ;;; contains global definitions e.g. choose_index
                    ;;; and popindex_suffixlist

section $-popindex => buildindex;

include sysdefs;    ;;; for Unix/VMS

define lconstant isterminator =
    lmember(% "=" :: define_terminators %)
enddefine;

define lconstant isspecial =
    lmember(% "%" :: define_headers %)
enddefine;

define lconstant splitstring(string);
    lconstant OPEN_BC = "'/*'", CLOSE_BC = "'*/'";
    lvars item, level = 0;

    ;;; Parse string after 'define '
    [% for item from_repeater incharitem(stringin(string)) do
        if item == OPEN_BC then
            level + 1 -> level
        elseif item == CLOSE_BC then
            level - 1 -> level
        elseif level == 0 then
            item
        endif
    endfor %]
enddefine;

define lconstant find_name(def_line) -> name;
    ;;; given a procedure header string, find the name
    lvars items = splitstring(def_line);
    lvars (name, form, isupdater) = (false, false, false);

    define First(list);
        list /== [] and fast_front(list);
    enddefine;
    ;;;
    define Second(list);
        list /== [] and First(fast_back(list));
    enddefine;

    ;;; check for 'define :<form> ...'
    if First(items) == ":" and isword(Second(items)) then
        fast_destpair(fast_back(items)) -> (form, items);
    endif;
    ;;; check for 'updaterof'
    if First(items) == "updaterof" then
        fast_destpair(items) -> (isupdater, items);
    endif;
    ;;; check for specials
    while isspecial(First(items)) do
        if (fast_destpair(items) -> items) == "active" then
            if First(items) == ":" and isinteger(Second(items)) then
                ;;; active multiplicity
                fast_back(fast_back(items)) -> items;
            endif;
        endif;
    endwhile;
    ;;; get stuff before terminator
    [%  while First(items) and not(isterminator(First(items))) do
            fast_destpair(items) -> items;
        endwhile;
    %] -> items;
    ;;; isolate name
    if isnumber(First(items)) and length(items) == 4 and isword(items(3)) then
        ;;; assume infix; doesn't account for section pathnames
        items(3) -> name;
    else
        if isnumber(First(items)) then
            fast_back(items) -> items;
        endif;
        ;;; get rid of section pathnames
        if First(items) == "$-" then
            fast_back(items) -> items;
        endif;
        while Second(items) == "$-" do
            fast_back(fast_back(items)) -> items;
        endwhile;
        ;;; at last!
        if isword(First(items)) then
            First(items) -> name;
        endif;
    endif;
    if name then
        /* this could be a useful extra bit of information...
        if form then
            consstring(#| explode(name), `\s`, `:`, explode(form) |#) -> name;
        endif;
        */
        if isupdater then
            consstring(#| explode(name), `\s`, explode("updaterof") |#) -> name;
        endif;
    endif;
enddefine;

define lconstant make_entry(dir_pattern, file_name, entryprop, preserve_environ);
    ;;; Get procedure definitions from the file and store information about
    ;;; them in the appropriate index list in entryprop
    ;;; Use dir_pattern to truncate the path name using logical names
    lconstant
        ;;; root directory for poplog
        usepopstring = '$usepop',
        ;;; length of expanded version
        usepoplength = datalength(sysfileok(usepopstring)),
        buffer = writeable inits(512),
    ;

    lvars fd, _len, dir_pattern, full_dir,
        file_name, logical, preserve_environ, procedure entryprop;

    dlocal popfilename = file_name, poplinenum = 1; ;;; for debugging

    lvars stklen = stacklength();
    define dlocal prmishap() with_nargs 2;
        warning();
        erasenum(stacklength() - stklen);
        exitfrom(make_entry)
    enddefine;

    sysopen(file_name, 0, "record", `N`) -> fd;

    if preserve_environ and isstartstring(usepopstring, dir_pattern) then
        ;;; Remove actual path name. Put back logical one
        usepopstring dir_>< allbutfirst(usepoplength, file_name)
    else
        if preserve_environ
        and (locchar(`$`,1,dir_pattern) or locchar(`~`,1,dir_pattern)) then
            ;;; handle other logical names and (on Unix) user names.
            if (locchar(`/`,2,dir_pattern) ->> _len) then
                ;;; get logical name starting dir_pattern
                substring(1, _len, dir_pattern) -> logical;
                sysfileok(logical) -> full_dir;
                logical dir_>< allbutfirst(datalength(full_dir), file_name)
            else goto USE_FILE_NAME
            endif
        else
            USE_FILE_NAME:
            sys_fname(file_name, 1,5)   ;;; strips version
        endif
    endif -> file_name;

    until (fast_sysread(fd, 1, buffer, 512) ->> _len) == 0 do
        if  (fast_subscrs(1, buffer) == `d`)
        and isstartstring('define', buffer)
        and _len /== 6
        ;;; deal with 'define ' and 'define:'
        and strmember(fast_subscrs(7, buffer), '\s:')
        then
            lvars p_name = find_name(substring(7, _len fi_- 6, buffer));
            if p_name then
                lvars index_name = consword(choose_index(p_name));
                conspair(p_name sys_>< ' ' <> file_name, entryprop(index_name))
                            -> entryprop(index_name);
            endif;
        endif;
        poplinenum + 1 -> poplinenum;
    enduntil;

    sysclose(fd);
enddefine;

;;; return file name generator for a directory
define lconstant file_repeater(dir);
#_IF DEF UNIX
    sys_file_match(dir, '*.p#', false, false)
#_ELSEIF DEF VMS
    sys_file_match(dir, '*.p;0', false, false)
    ;;; may return <false> if dir is wrong
    or identfn(%termin%);
#_ELSE
    mishap('UNKNOWN SYSTEM FOR sys_file_match', []);
#_ENDIF
enddefine;

define lconstant get_entries(index_list, entryprop, preserve_environ);
    dlocal
        % item_chartype(`*`) % = 3,
        % item_chartype(`/`) % = 3,
        % item_chartype(`'`) % = 1,
        ;
    ;;; keep a record of files seen to avoid duplicate entries
    define lvars seen =
        newanyproperty([], 64, 1, 56, syshash, nonop=, "perm", false, false);
    enddefine;
    lvars dir, file_name;
    for dir in index_list do
        for file_name from_repeater file_repeater(dir) do
            unless seen(file_name) then
                true -> seen(file_name);
                make_entry(dir, file_name, entryprop, preserve_environ);
            endunless;
        endfor;
    endfor;
enddefine;

define lconstant sort_and_write(suffixlist, entryprop, fileprop);
    lvars suffix;
    fast_for suffix in suffixlist do
        lvars i_name = consword(suffix);
        lvars dev = fileprop(i_name);
        lvars entry, list = syssort(entryprop(i_name), alphabefore);
        fast_for entry in list do
            fast_syswrite(dev, 1, entry, datalength(entry));
            fast_syswrite(dev, 1, '\n', 1);
        endfor;
        sysclose(dev);
    endfor;
enddefine;

define vars buildindex(index_list, index_filename, preserve_environ);
    lvars prop_size = (length(popindex_suffixlist) * 3) div 2;
    ;;; create the output devices for index files
    define lvars index_file =
        newproperty([], prop_size, false, "perm");
    enddefine;
    lvars suffix;
    for suffix in popindex_suffixlist do
        syscreate(index_filename sys_>< suffix, 1, false)
            -> index_file(consword(suffix));
    endfor;
    ;;; initialise the index database
    [get the entries] =>
    define lvars list_of_entries =
        newproperty([], prop_size, [], "perm");
    enddefine;
    get_entries(index_list, list_of_entries, preserve_environ);
    [sort the lists] =>
    sort_and_write(popindex_suffixlist, list_of_entries, index_file);
    [all done] =>
enddefine

endsection;     /* $-popindex */


/*  --- Revision History ---------------------------------------------------
--- John Williams, Jan 23 1996
        Now uses define_headers and define_terminators.
--- John Williams, Jan 22 1996
        splitstring now ignores items between /* and */.
--- Robert John Duncan, Dec  6 1995
        Fixed calls to sys_file_match to distinguish properly between Unix
        and VMS; this also makes it possible to supply just a directory name
        for indexing and the '*.p' gets added automatically.
        Added check for duplicate files.
        Changed prmishap to remove anything left on the stack.
        Changed find_name not to use the matcher to avoid having to declare
        redundant permanent identifiers.
        Removed call to sys*exit -- this could be run interactively!
        Sectionised, added +strict, tidied up.
--- John Williams, Apr 30 1993
        get_entries now dlocal's the item_chartype of / * and '
        (instead of changing them globally)
--- John Williams, Apr 30 1993
        make_entry now dlocal's prmishap to warning
--- John Williams, Apr 30 1993
        make_entry now dlocal's popfilename and poplinenum, in case errors
        happen while parsing procedure definitions
--- Aaron Sloman, Jul 30 1990
        Slightly altered comments
--- Aaron Sloman, May 20 1990
        Altered to allow define:form... as well as define :form...
--- John Gibson, Aug  4 1989
        Replaced use of -sysfiledir- and -sysfullfilename- with -sys_fname-
--- Aaron Sloman, May  1 1989 removed uses of pdr_valof
--- Aaron Sloman, Apr 22 1989
    Changed to allow define ... foo = ....; enddefine;

    Also gave -buildindex- (and other procedures) an extra argument to
    specify whether environment variables (e.g. $usepop) should be preserved
    at the beginning of file names in index entries. (Saves space and can
    make the index files more portable)

--- Aaron Sloman, Apr 11 1989
    Fixed to cope with active identifiers and their updaters
--- Aaron Sloman, Apr 10 1989
    Much modularised. Created top level procedure buildindex, invoked
    from $popcom/mkind file
*/
