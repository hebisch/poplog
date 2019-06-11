/* --- Copyright University of Birmingham 1996. All rights reserved. ------
 > File:            $poplocal/local/auto/ved_getit.p
 > Purpose:         Get line of text in output from ved_grep
 > Author:          Aaron Sloman, Apr 29 1994 (see revisions)
 > Documentation:
 > Related Files:
 */

;;; lib ved_getit
;;; Aaron Sloman Fri Apr 29 19:43:00 BST 1994
/*
HELP VED_GETIT                                    Aaron Sloman July 1994
ENTER getit
ENTER getit q

-- Introduction

The output of grep can be a file showing many occurrences of the search
string given to grep in one or more files. The "ENTER getit" command
makes it easy to go to a specific occurrence in a specific file.

This command is intended for use with the output of a file of the grep
family (e.g. egrep, ngrep, fgrep) or the ved command.


         CONTENTS - (Use <ENTER> g to access required sections)

 -- Introduction
 -- When it works
 -- How to use ENTER getit
 -- How it works
 -- Quiting the current file

-- When it works

It works only when several files are searched, as only in that case is
the name of the file containing the search pattern given in each line
of output, e.g.

    ENTER grep setenv .login*
might produce
    .login:    setenv OPENWINHOME /usr/openwin
    .login: setenv PRINTER lw-staff
    .login-:    setenv OPENWINHOME /usr/openwin
    .login-: setenv PRINTER lw-staff

The output file produced by ved_grep then has the file name followed by
a colon at the beginning of each line.

If the grep command is given only one file to search (e.g. because the
final pattern matches only one file name) it does not show the file name
at the beginning of each line, and ved_getit will not work.

-- How to use ENTER getit

Select a line containing the name of the file and the line you wish
to go to. The command will then read in the file, and go to the first
occurrence of the line shown in the output of grep. If there is more
than one such line and you wish to find it, repeated search will be
required (e.g. using ESC /)

-- How it works
It extracts the file name (preceding the colon), and then uses the rest
of the line as a search string, assigned to vvedgotoplace, so that the
line containing that string will be found when VED starts up.
(See REF * VVEDGOTOPLACE)

In order to make the search mechanism work, occurrences of `@` and
of `/` in the string have to be preceded by an extra `@`, as described
in HELP VEDSEARCH

-- Quitting the current file
If you wish to quit the file produced by grep when you go to the chosen
file, use the command

    ENTER getit q

This is implemented using vedqget (See * vedqget)

*/

section;

define ved_getit();
    lvars
        line = vedthisline(),
        colon_loc = locchar(`:`, 1, line);

    unless colon_loc then
        vederror('No colon found in line. Cannot get file name')
    endunless;

    dlocal ved_search_state;

    lvars
        string = veddecodetabs(allbutfirst(colon_loc, line)),
        file = substring(1, colon_loc-1, line);

    ;;; replace occurrences of "@" or "/" in string.
    if strmember(`@`, string) or strmember(`/`, string) then
        lvars char;
        consstring(#|
                for char in string using_subscriptor subscrs do
                    if char == `@` or char == `/` then
                        `@`,
                    endif, char
                endfor |#) -> string;
    endif;
    dlocal vvedgotoplace = '@a' <> string;

    ;;; get full path name for file
    unless subscrs(1,file) == `/` then
        if isstartstring('/tmp_mnt', current_directory) then
            ;;; cope with automounted files (UGH!)
            allbutfirst(8,current_directory)
        else
            current_directory
        endif dir_>< file -> file
    endunless;

    ;;; localise search state variables (see HELP VEDSEARCH)
    dlocal vvedanywhere, vvedoldsrchdisplay, vvedsrchstring, vvedsrchsize;

    ;;; now get the file
    if vedargument = nullstring then
        edit(file);
    elseif vedargument = 'q' then
        ;;; quit current file and re-use its window for the new file
        vedqget(edit(%file%))
    else
        vederror ('GETIT: unrecognised argument:- ' <> vedargument)
    endif;

    ;;;; Now show some context
    repeat 4 times
        vedscrolldown();
    endrepeat;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Dec 21 1996
    Dlocalised ved_search_s*tate
 */
