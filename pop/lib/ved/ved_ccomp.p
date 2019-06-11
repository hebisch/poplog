/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.unix/lib/ved/ved_ccomp.p
 > Purpose:         Compile and run a C file
 > Author:          Aaron Sloman, May  2 1988 (see revisions)
 > Documentation:   HELP * CCOMP, HELP * PIPEUTILS
 > Related Files:   LIB * VEDPIPEIN, * VED_FCOMP, * PIPEIN
 */

/*
<ENTER> ccomp
Is used to compile and execute C programs from inside VED, reading the
output back into a VED file. See HELP * CCOMP for details.

For some reason this does not work with /bin/sh. Must use /bin/csh.
*/

section;

global vars ccomp_command;          ;;; name of C compiler to use

if isundef(ccomp_command) then
    'cc' -> ccomp_command
endif;

global vars ccomp_chatty;           ;;; controls warning about renaming file

unless isboolean(ccomp_chatty) then
    true -> ccomp_chatty
endunless;

global vars
    ccomp_pre,      ;;; string to be inserted after 'cc ' before file names
    ccomp_post;     ;;; string to be inserted after 'cc ' after file names

unless isstring(ccomp_pre) then false -> ccomp_pre endunless;

unless isstring(ccomp_post) then false -> ccomp_post endunless;


define global ved_ccomp();
    lvars
         bakfile,
         command,
         arg,
         name,
         namecount = 0,
         args = sysparse_string(vedargument),
         interact = false,
         filename = false,
         object = false,
         ;

    lconstant
        arglist = ['/bin/csh' '-ce' 0];

    [% until args == [] do
        fast_destpair(args) -> (arg, args);
        if arg = '-i' then
            true -> interact
        elseif arg = '-o' and args /== [] then
            fast_destpair(args) -> (filename, args)
        else
            if arg = '-c' or arg = '-S' then
                true -> object
            endif;
            arg
        endif
    enduntil %] -> args;

    if interact and object then
        vederror('INCOMPATIBLE FLAGS: -i and -c')
    elseif filename and object then
        vederror('INCOMPATIBLE FLAGS -o and -c')
    endif;
    nullstring -> vedargument;  ;;; for ved_w1 and ved_w

    for arg in args do
        if isendstring('.c', arg) or isendstring('.o', arg)  then
            namecount + 1 -> namecount;
            arg -> name
        endif
    endfor;
    if namecount == 0 then
        ;;; no filename so use current file
        if isendstring('.c', vedcurrent) then
            [^vedpathname ^^args] -> args;
            unless object or filename then
                allbutlast(2, vedpathname) -> filename
            endunless;
            if vedchanged and vedchanged /== 0  then ved_w1() endif
        else
            vederror('NO .o or .c FILE SPECIFIED')
        endif
    else
        ;;; write everything, in case some files in VED
        ved_w()
    endif;
    if not(object) and not(filename) and namecount == 1 then
        allbutlast(2, name) -> filename
    elseif not(object) and not(filename) then
        'a.out' -> filename
    endif;

    if filename and sys_file_exists(filename) then
        if ccomp_chatty then
            vedputmessage('RENAMING \'' sys_>< filename sys_>< '\' appending "-"');
            syssleep(200)
        endif;
        filename sys_>< '-' -> bakfile;
        sysunlink(bakfile) ->;
        syslink(filename, bakfile) ->;
        sysunlink(filename) ->
    endif;

    ;;; create a multi-command argument for csh
    cons_with consstring
    {%
         explode(ccomp_command), `\s`,
         if ccomp_pre then explode(ccomp_pre), `\s` endif,
         applist(args, procedure(); explode(), `\s` endprocedure),
         if filename then explode('-o '), explode(filename) endif,
         if ccomp_post then `\s`, explode(ccomp_post) endif
         %} -> command;

    ;;; create a string of csh commands
    cons_with consstring
    {% `(`,
         ;;; compile the file
         explode(command), `;`,
         unless interact or object or not(filename) then
             explode('echo ""; echo "Running the program produced:-";');
             ;;; run the file
             explode(filename)      ;;; run
         endunless;
        `)`
    %}     -> arglist(3);

    vedpipein('/bin/csh',
        arglist,
        if filename then
            filename sys_>< '.out'  ;;; name of output file
        else
            systmpfile(false, ccomp_command, 'out')
        endif,
        vedveddefaults <> procedure(); 1 -> vedchanged endprocedure,
        true,                       ;;; display the file
        command);                   ;;; header for file

    ;;; handle interaction
    if interact and filename and sys_file_exists(filename) then
        vedscreenreset();
        true -> vedprintingdone;
        sysobey(filename)
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Oct 28 1994
        Added variable ccomp_command for specifying the name of the C
        compiler to use. Default is 'cc'. Also, no longer uses matcher
        to parse arguments (and hence no longer creates top level
        permanent identifiers l1, l2, and filename !).
--- Aaron Sloman, Jan 14 1989
        Removed vednullstring
--- Aaron Sloman, May  8 1988
        Removed -f68881
*/
