/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/lib/lib/make_indexes.p
 > Purpose:         Index a Poplog directory tree
 > Author:          Adrian Howard, Feb  6 1992 (see revisions)
 > Documentation:   HELP *MAKE_INDEXES
 > Related Files:   LIB *MKREFINDEX, HELP *MKREFINDEX
 */

#_TERMIN_IF DEF POPC_COMPILING

compile_mode: pop11 +strict;
section;


include vedscreendefs.ph;


;;; Unless -false-, print messages when creating indexes
global vars mi_verbose = true;


;;; Max number of columns in the main index body
global vars mi_num_columns = 72;


;;; Access mode argument to -syscreate-
lconstant WRITE = 1;


;;; OS dependent procedures
lconstant OS = hd(sys_os_type);
#_IF OS == "unix"


    ;;; Characters used to seperate Unix path components, see -dest_path-
    lconstant PATH_COMPONENT_SEPERATORS = '/.';

    ;;; Wildcard that will match any directory tree
    lconstant ANY_DIR = '...';

    ;;; Wildcard that will match any file
    lconstant ANY_FILE = '*';

    ;;; Name of index directories, see -has_index-
    lconstant INDEX_DIR = 'doc_index';

    ;;; Given the path of an file, returns its name. See -mi_pr_index_body-
    define lconstant index_path2name =
        sys_fname_name(%%);
    enddefine;

    ;;; Convart a path into a directory specification (ie, add trailing /)
    ;;; See -locate_doc_dirs-
    define lconstant path2directory(path);
        lvars path;
        path dir_>< '';
    enddefine;

    ;;; Returns the current year as a 4 digit string.
    define lconstant year() /* -> YEAR_STRING */;
        lvars time_string = sysdaytime();
        substring(datalength(time_string)-3, 4, time_string);
    enddefine;

    ;;; Fully expand a directory name
    define lconstant expand_dir(dir) -> dir;
        lvars dir;
        sysfileok(dir dir_>< nullstring) -> dir;
        unless dir(1) == `/` then
            current_directory dir_>< dir -> dir;
        endunless;
        lvars pos;
        while issubstring('/.', 1, dir) or issubstring('//', 1, dir) ->> pos do
            allbutlast(datalength(dir)-pos+1, dir)
                sys_>< allbutfirst(pos+1, dir) -> dir
        endwhile
    enddefine;

    ;;; Appropriate Unix commands for creating index directories
    global vars mi_indexing_commands = [
        ['lisp'   'pop11 $usepop/pop/lisp/mklisprefindex']
        ['pml'    'pop11 +$popsavelib/pml $usepop/pop/pml/src/mkindex']
        ['ref'    'pop11 mkrefindex']
    ];


#_ELSEIF OS == "vms"


    ;;; Characters used to seperate VMS path components, see -dest_path-
    lconstant PATH_COMPONENT_SEPERATORS = '[]:.';

    ;;; Wildcard that will match any directory tree
    lconstant ANY_DIR = '[...]';

    ;;; Wildcard that will match any file
    lconstant ANY_FILE = '*.*;*';

    ;;; Name of index directories, see -has_index-
    lconstant INDEX_DIR = 'doc_index.dir';

    ;;; Given the path of an file, returns its name. See -mi_pr_index_body-
    define lconstant index_path2name =
        sys_fname_nam(%%);
    enddefine;

    ;;; Convart a path into a directory specification (ie move trailing
    ;;;; XXX.dir path component between the [] brackets). See -locate_doc_dirs-
    define lconstant path2directory(path) -> path;
        lvars path;
        if sys_fname_extn(path) = '.dir' then
            sys_fname_path(path) dir_><
                consstring(#| `[`, explode(sys_fname_nam(path)), `]` |#)
                -> path;
        endif;
    enddefine;

    ;;; Returns the current year as a 4 digit string
    define lconstant year() /* -> YEAR_STRING */;
        lvars time_string = sysdaytime();
        substring(
            locchar_back(`-`, datalength(time_string), time_string)+1,
            4,
            time_string
        );
    enddefine;

    ;;; Fully expand a directory name
    define lconstant expand_dir(dir) -> dir;
        lvars dir;
        sysfileok(dir dir_>< nullstring) -> dir;
        unless locchar(`:`, 1, dir) then
            sysfileok(if issubstring('[-', 1, dir) then
                        current_directory
                      else
                        'sys$disk:'
                      endif <> dir) -> dir
        endunless;
    enddefine;

    ;;; Appropriate DCL commands for creating index directories
    global vars mi_indexing_commands = [
        ['lisp'   'pop11 usepop:[pop.lisp]mklisprefindex.p']
        ['pml'    'pop11/popsavelib:pml usepop:[pop.pml.src]mkindex.ml']
        ['ref'    'pop11 mkrefindex']
    ];


#_ELSE
    mishap(0, 'CANNOT COMPILE --- UNSUPPORTED OPERATING SYSTEM');
#_ENDIF

/*
 * Takes the (d)string and converts it into a dstring ORing in the specified
 * VED attribute fields
 */
define lconstant Vedaddattrs(string, attrs);
    lvars string, attrs;
    consdstring(#|
        lvars n;
        for n from 1 to datalength(string) do string(n) || attrs endfor;
    |#);
enddefine;


/*
 * Display a line with dstrings in the external VED file format
 */
lvars procedure Ved_pr_line;


/*
 * Checks that the key of -item- is a member of -keys-
 */
define :inline lconstant CheckType(item, keys);
    unless member(datakey(item), keys) do;
        mishap(
            item, 1,
            'EXPECTING ' sys_>< struppercase(class_dataword(hd(keys)))
        );
    endunless;
enddefine;


/*
 * Checks -list- is a list, and that the key of each element is a member of
 * -keys-
 */
define check_list(list, keys);
    lvars list, keys;
    islist(list) or mishap(list, 1, 'LIST EXPECTED') ->;
    applist(
        list,
        procedure(item);
            lvars item;
            CheckType(item, keys);
        endprocedure
    );
enddefine;


/*
 * Returns a list of the individual disk, directory, file, and extension
 * components of the directory string -path-.
 * eg, '$usepop/pop/lib/foo.p' --> ['$usepop' 'pop' 'lib' 'foo' 'p']
 * NOTE: -path- should have been run through -expand_dir-
 */
define lconstant dest_path(path) /* -> STRING_LIST */;
    lvars path;
    lvars char, n=0;
    [%
        fast_for char in_string path do;
            if locchar(char, 1, PATH_COMPONENT_SEPERATORS) then
                unless n==0 do
                    consstring(n),
                    0 -> n;
                endunless;
            else
                char;
                n+1 -> n;
            endif;
        endfast_for;
        unless n==0 do
            consstring(n);
        endunless;
    %];
enddefine;


/*
 * Do a printf but lose the output if -mi_verbose- is -false-
 */
define lconstant mi_printf(/*args*/);
    dlocal cucharout;
    unless mi_verbose then erase -> cucharout endunless;
    printf(/*args*/);
    if mi_verbose then sysflush(pop_charout_device) endif
enddefine;


/*
 * Returns a list of directory strings -paths- containing all directories
 * named -doc_type- with -root- as their root, with the exception of any
 * directories contained in the list -skip_paths-.
 * NOTE: the directories in -skip_paths- should be full directory
 * specifications
 */
define lconstant locate_doc_dirs(root, skip_paths, doc_type) -> paths;
    lvars root, doc_type, paths=[];
    dlvars skip_paths;
    lvars path_repeater =
        sys_file_match(
            root dir_>< ANY_DIR dir_>< doc_type, ANY_FILE,
            false, false
        );
    lvars path;

    ;;; RETURNS -true- IF PATH SHOULD BE SKIPPED OVER
    define lconstant is_skipped(path);
        lvars path;
        lvars skip_path;
        for skip_path in skip_paths do;
            returnif(isstartstring(skip_path, path))(true);
        endfor;
        false;
    enddefine;

    unless path_repeater do;
        mishap(root, 1, 'COULD NOT LOCATE DOCUMENTATION DIRECTORIES');
    endunless;

    mi_printf('Locating directories .');

    until (path_repeater() ->> path) == termin do;
        mi_printf('.');
        path2directory(path) -> path;
        if sysisdirectory(path) and not(is_skipped(path)) then
            conspair(path, paths) -> paths;
        endif;
    enduntil;

    mi_printf('\n');

enddefine;


/*
 * Creates a list of index names for the list of directory strings -paths-.
 * Index names are formed by removing the root directory string -root- from
 * each path, concatanating the remaining directory components, and added
 * the specified -prefix- & -suffix-.
 * NOTE: -root-, and the directories in -paths-, should have been passed
 * through -expand_dir-
 */
define lconstant
    calculate_index_names(root, paths, prefix, suffix) /* -> NAME_LIST */
;
    lvars root, paths, prefix, suffix;
    lvars root_len = listlength(dest_path(root));
    lvars path, component;

    mi_printf('Calculating index names ...');

    [%
        fast_for path in_list paths do;
            consstring(#|
                explode(prefix);
                applist(
                    allbutlast(1, allbutfirst(root_len, dest_path(path))),
                    explode
                );
                explode(suffix);
            |#);
        endfast_for;
    %];

    mi_printf('\n');

enddefine;


/*
 * Returns -true- if the given directory contains an index sub-directory
 */
define lconstant has_index(path) /* -> BOOL */;
    lvars path;
    sysisdirectory(path dir_>< INDEX_DIR);
enddefine;


/*
 * Update the index sub-directory of the specified directory -path- using
 * the appropriate command from -mi_indexing_commands-
 */
define lconstant update_index(path);
    lvars path;
    lvars path_components = dest_path(path);
    lvars pair, command, component;

    ;;; CHECKING -for- SINCE -mi_indexing_commands- IS USER DEFINED
    for pair in_list mi_indexing_commands do;
        hd(dest(pair)) -> command -> component;
        if member(component, path_components) then
            mi_printf(path, component, 'Indexing %p directory %p\n');
            if isprocedure(command) then
                command(path);
            elseif isstring(command) then
                sysobey(command sys_>< space sys_>< path);
            else
                mishap(command, 1, 'PROCEDURE OR STRING EXPECTED');
            endif;
            return();   /** EARLY EXIT **/
        endif;
    endfor;

    mishap(path, 1,  'DO NOT KNOW HOW TO MAKE INDEX FOR DIRECTORY');

enddefine;


lconstant Line =
    consdstring(#| repeat 71 times; `\G-` || VEDCMODE_BOLD; endrepeat |#);

/*
 * Print out an index file header for the index named -index_name- of
 * documentation type -doc_type-, where -index_path- is the full pathname of
 * the index file and -other_indexes- is a list containing the names of
 * any other indexes of the same type.
 */
define global vars
    mi_pr_index_header(doc_type, index_name, index_path, other_indexes)
;
    lvars doc_type, index_name, index_path, other_indexes;
    dlocal pop_pr_quotes = false, pop_pr_level = 1;

    Ved_pr_line(
        Vedaddattrs(
            lowertoupper(doc_type sys_>< space sys_>< index_name),
            VEDCMODE_BOLD
        )
    );

    if doc_type == "ref" or doc_type = 'ref' then
        nl(1);
        Ved_pr_line(Vedaddattrs(
            '       COPYRIGHT University of Sussex '
                        sys_>< year()
                        sys_>< '. All Rights Reserved.',
            VEDCMODE_ALTFONT
        ));
        nl(1);
        npr('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>');
        npr('<<<<<<<<<<<<<<<<<<<<<                             >>>>>>>>>>>>>>>>>>>>>>');
        Ved_pr_line('<<<<<<<<<<<<<<<<<<<<<          '
            sys_>< Vedaddattrs('I N D E X', VEDCMODE_BOLD)
            sys_>< '          >>>>>>>>>>>>>>>>>>>>>>'
        );
        npr('<<<<<<<<<<<<<<<<<<<<<                             >>>>>>>>>>>>>>>>>>>>>>');
        npr('<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<');
    endif;

    nl(2);

    if doc_type == "ref" or doc_type = 'ref' then
        Ved_pr_line(Vedaddattrs('\G-\G-\G-\G-\G-\G-\G-\G-', VEDCMODE_BOLD));
        Ved_pr_line(Vedaddattrs('1  Files', VEDCMODE_BOLD));
        Ved_pr_line(Vedaddattrs('\G-\G-\G-\G-\G-\G-\G-\G-', VEDCMODE_BOLD));
    else
        Ved_pr_line(Line);
        Ved_pr_line(Vedaddattrs('1  Files', VEDCMODE_BOLD));
        Ved_pr_line(Line);
    endif;
    nl(1);

enddefine;


/*
 * Prints out the bottom of an index file, arguments are as for a call to
 * -mi_pr_index_header-
 */
define global vars
    mi_pr_index_footer(doc_type, index_name, index_path, other_indexes)
;
    lvars doc_type, index_name, index_path, other_indexes;
    dlocal pop_pr_quotes = false, pop_pr_level = 1;

    unless other_indexes == [] do;

        nl(2);
        if doc_type == "ref" or doc_type = 'ref' then
            nl(1);
            Ved_pr_line(Vedaddattrs(
                '\G-\G-\G-\G-\G-\G-\G-\G-\G-\G-\G-\G-\G-\G-\G-\G-',
                VEDCMODE_BOLD
            ));
            Ved_pr_line(Vedaddattrs('2  Other Indexes', VEDCMODE_BOLD));
            Ved_pr_line(Vedaddattrs(
                '\G-\G-\G-\G-\G-\G-\G-\G-\G-\G-\G-\G-\G-\G-\G-\G-',
                VEDCMODE_BOLD
            ));
        else
            Ved_pr_line(Line);
            Ved_pr_line(Vedaddattrs('2  Other Indexes', VEDCMODE_BOLD));
            Ved_pr_line(Line);
        endif;
        nl(1);

        npr('NOTE: Some indexes may not be available until appropriate libraries have');
        npr('been loaded');

        nl(1);
        for index_name in_list other_indexes do;
            Ved_pr_line(consvedcrossref(doc_type, index_name, false))
        endfor;

    endunless;

    nl(2);
    Ved_pr_line(
        '\Gtl\G-\Gtr '
            sys_>< Vedaddattrs(index_path, VEDCMODE_BOLD || VEDCMODE_ALTFONT)
    );
    Ved_pr_line(
        '\Gbl\G-\Gbr '
            sys_>< Vedaddattrs('Copyright University of Sussex ',
                        VEDCMODE_ALTFONT)
            sys_>< Vedaddattrs(year(), VEDCMODE_ALTFONT)
            sys_>< Vedaddattrs('. All rights reserved.', VEDCMODE_ALTFONT)
    );

enddefine;


/*
 * List the files in the currrent directory (-path-). Files are printed in
 * alphabetical order in as many columns as will fit in -mi_num_columns-
 */
define lconstant mi_pr_index_body(path) -> index_files;
    lvars path;
    lvars path_repeater =
            sys_file_match(ANY_FILE, ANY_FILE, false, false),
        index_files = [],
        max_file_width=0,
        n=0;
    lvars index_file, column_num, column_width, num_files;
    dlocal pop_pr_quotes = false, pop_pr_level = 1;

    unless path_repeater do;
        mishap(path, 1, 'COULD NOT LOCATE DOCUMENTATION FILES');
    endunless;

    until (path_repeater() ->> index_file) == termin do;
        unless sysisdirectory(index_file) do;
            index_path2name(index_file) -> index_file;
            conspair(index_file, index_files) -> index_files;
            max(datalength(index_file), max_file_width) -> max_file_width
        endunless;
    enduntil;

    syssort(index_files, alphabefore) -> index_files;
    mi_num_columns div (max_file_width+1) -> column_num;
    mi_num_columns div column_num -> column_width;
    listlength(index_files) -> num_files;

    fast_for index_file in index_files do;
        ;;; DO NEWLINE AT LAST FILE AND END OF COLUMN
        if ((n+1 ->> n) mod column_num == 0) or (n == num_files) then
            npr(index_file);
        else
            pr_field(index_file, column_width, false, ` `);
        endif
    endfast_for;


enddefine;


/*
 * Strip out char attributes and graphics chars in all doc files -files-
 * in the current directory (-path-)
 */
define lconstant strip_doc_files(path, files);
    lvars   path, files, file,
            tmpname = systmpfile('.', 'MKINDXTMP', nullstring),
            procedure (repeater, consumer);
    dlocal  vednotabs = false, vedindentstep = 1;

    mi_printf(path, 'Stripping doc files in %p\n');

    fast_for file in files do

        vedfile_line_repeater(file) -> repeater;
        vedfile_line_consumer(tmpname, 1) -> consumer;  ;;; 1 = plain ASCII

        until consumer(dup(repeater())) == termin do enduntil;

        sys_file_move(tmpname, file);
    endfor
enddefine;

/*
 * Create an index file named -index_name- of the documentation type -doc_type-
 * for the directory -path- where -other_indexes- is a list of any other
 * index files of the same documentation type
 */
define lconstant create_index_file(path, doc_type, index_name, other_indexes,
                                                                    strip);
    lvars path, doc_type, index_name, other_indexes, strip;
    lvars index_path = path dir_>< index_name, index_files;
    dlocal pop_file_versions = 1, current_directory = path;

    mi_printf(index_path, 'Creating index file %p\n');

    procedure;
        dlocal  pop_charout_device = syscreate(index_path, WRITE, false),
                poplinewidth = false,
                Ved_pr_line = vedfile_line_consumer(pop_charout_device);

        mi_pr_index_header(doc_type, index_name, index_path, other_indexes);
        mi_pr_index_body(path) -> index_files;
        mi_pr_index_footer(doc_type, index_name, index_path, other_indexes);

        sysclose(pop_charout_device);
    endprocedure();

    if strip then strip_doc_files(path, index_files) endif
enddefine;


/*
 * Creates index files and index directories for a given directory tree:
 *  -root- specifies the root of the directory tree to index
 *  -skip_paths- is a list of directories which should be ignored
 *  -doc_types- is a list of the documentation directory names
 *  -prefix- and -suffix- are added the start and end of all index files
 *  Takes optional last argument, a boolean -strip- saying whether to
 *  strip all doc files of VED char attributes and graphics chars.
 */
define global make_indexes(root, skip_paths, prefix, suffix, doc_types);
    lvars root, skip_paths, doc_types, prefix, suffix, strip = false;
    lconstant legal_keys = #_< [%string_key, word_key%] >_#;
    lvars doc_type, paths, index_names, path, index_name;

    ;;; CHECK FOR OPTIONAL strip ARGUMENT
    if isboolean(doc_types) then
        (root, skip_paths, prefix, suffix, doc_types)
            -> (root, skip_paths, prefix, suffix, doc_types, strip)
    endif;

    ;;; CHECK TYPES OF ARGUMENTS
    CheckType(root, legal_keys);
    check_list(skip_paths, legal_keys);
    check_list(doc_types, legal_keys);
    CheckType(prefix, legal_keys);
    CheckType(suffix, legal_keys);

    ;;; MAKE SURE ALL DIRECTORY NAMES ARE EXPANDED
    expand_dir(root) -> root;
    skip_paths -> paths;
    until paths == [] do;
        expand_dir(hd(paths)) -> hd(paths);
        tl(paths) -> paths;
    enduntil;

    for doc_type in doc_types do;

        mi_printf(root, doc_type, '\nMaking %p indexes in %p ...\n');

        locate_doc_dirs(root, skip_paths, doc_type) -> paths;
        calculate_index_names(root, paths, prefix, suffix) -> index_names;

        fast_for path, index_name in paths, index_names do;

            if has_index(path) then
                update_index(path);
            elseif doc_type == "ref" or doc_type = 'ref' then
                mi_printf(path, 'WARNING: %p has no doc_index dir\n');
            endif;

            create_index_file(
                path,
                doc_type,
                index_name,
                delete(index_name, index_names),
                strip
            );

        endfor;

    endfor;

    mi_printf('\nFinished.\n');

enddefine;

endsection;

#_TERMIN_IF not(pop_runtime)


;;; Automatic startup if loaded from the command like
;;;
;;;  pop11 make_indexes [-strip] DIR [SKIP_PATHS] [PREFIX] [SUFFIX] [DOC_TYPES]
;;;
;;; SKIP_PATHS & DOC_TYPES will need to be enclosed in "(" & ")"
section;

lblock

    lconstant stripflag = '-strip', default_prefix = nullstring;
    lvars args, strip = false, path, skip_paths, doc_types, prefix, suffix;

    if sys_fname_nam(poparg1) = 'make_indexes' then
        5e5 -> popmemlim;
        poparglist -> args;

        if args == [] then
            mishap(0, 'NEED ROOT DIRECTORY');
        else
            dest(args) -> (path, args);
        endif;

        if args /== [] and hd(args) = stripflag then
            tl(args) -> args;
            true -> strip
        endif;

        (args == [] and []) or (dest(args) -> args)
            -> skip_paths;
        (args == [] and default_prefix) or (dest(args) -> args)
            -> prefix;
        (args == [] and 'index') or (dest(args) -> args)
            -> suffix;
        (args == [] and [ref help teach doc]) or (dest(args) -> args)
            -> doc_types;

        [%"make_indexes", path, if strip then stripflag endif,
                            skip_paths, prefix, suffix, doc_types%] =>
        make_indexes(path, skip_paths, prefix, suffix, doc_types, strip);

    endif;

endlblock;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct  2 1998
        Got rid of silly and unnecessary code in Ved_pr_line, and made it a
        variable procedure initialised to a line consumer in create_index_file.
--- John Gibson, May 15 1998
        Added assignment to popmemlim
--- John Williams, Feb 21 1996
        Uses consvedcrossref.
--- John Gibson, Dec  6 1993
        Split expand_dir up into unix/vms versions
--- Adrian Howard, May  5 1993
        Now produces REF files to the new format using VED character attributes
--- Adrian Howard, Dec  8 1992
        The skip option now works from the command line
--- John Gibson, Oct 21 1992
        Added #_TERMIN_IF
--- John Gibson, Oct  5 1992
        Fixed strip_doc_files to set vednotabs false, etc.
--- John Gibson, Sep 17 1992
        Added optional strip arg to make_indexes, and corresponding
        -strip option to command line startup
--- John Williams, Apr 23 1992
        Now uses -poparg1- to decide if loaded on startup
--- Adrian Howard, Mar 23 1992
        Fixed bug in -mi_pr_index_body- which stuck extra spaces at the end of lines.
--- Robert John Duncan, Mar 19 1992
        Removed test for popusername='pop' as inappropriate for non-local
        use.
 */
