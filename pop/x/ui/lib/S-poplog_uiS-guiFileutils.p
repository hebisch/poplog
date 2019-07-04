/* --- Copyright University of Sussex 1996.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-guiFileutils.p
 > Purpose:         utilities for checking files, expanding pathnames etc.
 > Author:          Julian Clinton, May 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section $-poplog_ui;

include sysdefs.ph;

uses $-poplog_ui$-guiUtils;

#_IF DEF WIN32

    ;;; hack to cope with NuTCRACKER pathnames:
    ;;;     /X=/path --> X:/Path
    ;;; this was needed at one time for pathnames returned from dialogs
    ;;; but may not be now; kept just in case...
define NutConvert(file) -> file;
    sysfileok(file, false) -> file;
    if length(file) >= 4
    and file(1) == `\\` and file(3) == `=` and file(4) == `\\`
    and isalphacode(file(2))
    then
        consstring(#| file(2), `:`, explode(allbutfirst(3, file)) |#) -> file;
    endif;
enddefine;

define isfile =
    NutConvert <> sys_file_exists;
enddefine;

#_ELSE

define isfile =
    sys_file_exists(%%);
enddefine;

#_ENDIF

;;; isabsolute_pathname takes a string which is meant to
;;; represent a directory path and returns true if the path is
;;; absolute or false if relative
define isabsolute_pathname(path) /* -> boole */ ;
    lvars path;
    sysfileok(path) -> path;
#_IF DEF UNIX
    (datalength(path) fi_> 0) and fast_subscrs(1, path) == `/`;
#_ELSEIF DEF VMS
    not(issubstring('[.', path))
#_ELSEIF DEF WIN32
    ;;; if it starts with a \ it's clearly absolute, even if it's
    ;;; \\share\file but we should also count as absolute anything with
    ;;; a drive spec like A:file
    isstartstring('\\', path) or sys_fname(path, 2) /= nullstring;
#_ELSE_ERROR
#_ENDIF
enddefine;

;;; sys_dir_name takes a string of a directory pathname
;;; and expands it to the full pathname (i.e. no ~, ../, ./ etc.)
;;; If the value is not a real pathname then false is returned
define sys_dir_name(name) -> newname;
    lvars name, newname;
    dlocal current_directory;
#_IF DEF WIN32
    returnunless(isstring(name))(false -> newname);
    NutConvert(name) -> name;
#_ENDIF
    if isstring(name) and sysisdirectory(name) then
        name -> current_directory;
        current_directory -> newname;
    else
        false -> newname;
    endif;
enddefine;

#_IF DEF UNIX

define sys_get_files(path, filter) -> (filelist, nfiles, ndirs);
lvars path nextfile file filter filelist nfiles = 0,  ndirs = 0;

    define lconstant get_dirs(dir_filter);
    lvars nextdir dir_filter;
        sys_file_match(dir_filter, path, false, true) -> nextdir;
        if nextdir() /== termin then
            erase();
            until (nextdir() ->> file) == termin do
                if sysisdirectory(path dir_>< file) then
                    file dir_>< '/'
                endif;
            enduntil;
        endif;
    enddefine;

    if (sys_dir_name(path) ->> path) then
        ;;; build up a list of directories which should always include directories
        ;;; starting with '.'
        [%  get_dirs('.*');
            get_dirs('*');
            %] -> filelist;     ;;; this is now a list of directories
        length(filelist) -> ndirs;

        sys_file_match(filter, path, false, true) -> nextfile;

        if nextfile() /== termin then
            erase();
            filelist <>
                [%
                    until (nextfile() ->> file) == termin do
                        if file and not(sysisdirectory(path dir_>< file)) then
                            file
                        endif;
                    enduntil;
                %] -> filelist;
        endif;
        length(filelist) - ndirs -> nfiles;
    else
        false -> filelist;      ;;; invalid pathname
    endif;
enddefine;

#_ELSEIF DEF VMS

define sys_get_files(path, filter) -> (filelist, nfiles, ndirs);
lvars path nextfile nextdir file filter filelist nfiles = 0,  ndirs = 0,
    arg1 arg2;

    if (sys_dir_name(path) ->> path) then

        sys_file_match(path, '*', false, true) -> nextdir;
        if nextdir() /== termin then
            erase();
            [%
                until (nextdir() ->> file) == termin do
                    if sysisdirectory(path dir_>< file) then
                        file dir_>< '/'
                    endif;
                enduntil;
            %]
        else
            [-]
        endif -> filelist;     ;;; this is now a list of directories
        length(filelist) -> ndirs;

        sys_file_match(path, filter, false, true) -> nextfile;

        if nextfile() /== termin then
            erase();
            filelist <>
                [%
                    until (nextfile() ->> file) == termin do
                        if file and not(sysisdirectory(path dir_>< file)) then
                            file
                        endif;
                    enduntil;
                %] -> filelist;
        endif;
        length(filelist) - ndirs -> nfiles;
    else
        false -> filelist;      ;;; invalid pathname
    endif;
enddefine;

#_ELSEIF DEF WIN32

define sys_get_files(path, filter) -> (filelist, nfiles, ndirs);
    lvars filelist = [], nfiles = 0, ndirs = 0;
    returnunless(sysisdirectory(path));
    dlocal current_directory = path;
    [   '.\\' '..\\'
    %   2 -> ndirs;
        lvars dir;
        for dir in sys_files_matching('*', 1) do
            dir <> '\\';
            ndirs + 1 -> ndirs;
        endfor;
        lvars file;
        for file in sys_files_matching(filter) do
            unless sysisdirectory(file) then
                file;
                nfiles + 1 -> nfiles;
            endunless;
        endfor;
    %] -> filelist;
enddefine;

#_ENDIF



;;; sys_merge_filename takes a directory and an additional
define sys_merge_filename(directory, addition) -> newpath;
lvars directory addition newpath;

    unless directory = nullstring then
        if subscrs(1, addition) == `/` then
            sys_dir_name(addition) -> newpath;
        elseif subscrs(1, addition) == `~` then
            sys_dir_name(addition) -> newpath;
        else
            sys_dir_name(directory dir_>< addition) -> newpath;
        endif;
    endunless;

    unless newpath then
        directory -> newpath;
    endunless;
enddefine;

constant guiFileutils = true;

endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, May 29 1996
        Added cases for Win32 (for compilation in the NuTCRACKER
        environment)
--- Robert John Duncan, Apr 21 1995
        Changed isabsolute_pathname to do sysfileok first and simplified
        isfile
--- John Gibson, Jul 22 1993
        Added missing uses for $-poplog_ui$-guiUtils
--- John Gibson, Jun 28 1993
        Changes for POPC
--- Julian Clinton, Jun 2  1992
    -   Re-wrote sys_get_files so directories starting with '.' are included (see
        isl-fr.4446). Also split out a separate VMS version.
    -   added -isabsolute_pathname-
--- John Gibson, Nov 25 1991
        -isdirectory- no longer necessary since -sysisdirectory- is now in
        the core system and works properly for pathnames in VMS.
--- John Gibson, Nov 22 1991
        New version of -isdirectory- for VMS
--- Integral Solutions Ltd, Sep 12 1991 (Julian Clinton)
    Changed sys_get_files:
        - fixed bug which caused empty directories not to return '../'.
        - now returns false if given an invalid pathname.
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    Adds '../' to directory list unless path is '/'.
Julian Clinton, 20/8/91
    Altered checking to prevent ATTEMPT TO RUN DEAD PROCESS error.
Julian Clinton, 7/8/91
    Changed sys_get_files to return number of directories and files.
 */
