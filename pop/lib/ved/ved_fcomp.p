/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.unix/lib/ved/ved_fcomp.p
 > Purpose:         Compile and run a fortran file
 > Author:          Aaron Sloman, May 10 1988 (see revisions)
 > Documentation:   HELP * FCOMP  * CCOMP  * PIPEUTILS
 > Related Files:   LIB * VEDPIPEIN * VED_CCOMP
 */
compile_mode :pop11 +strict;

/*
<ENTER> fcomp
Can be used to compile and run a fortran program, using the same
methods as <ENTER> ccomp (ved_ccomp)


For some reason this does not work with /bin/sh. Must use /bin/csh.
*/

section;

global vars fcomp_chatty;   ;;; controls warning about renaming file
unless isboolean(fcomp_chatty) then true -> fcomp_chatty
endunless;

global vars
    fcomp_pre,      ;;; string to be inserted after 'f77 ' before file names
    fcomp_post;     ;;; string to be inserted after 'f77 ' after file names

unless isstring(fcomp_pre) then false -> fcomp_pre endunless;
unless isstring(fcomp_post) then false -> fcomp_post endunless;


define vars ved_fcomp;
    lvars
         dev,
         bakfile,
         command,
         arg,
         name,
         namecount=0,
         args = sysparse_string(vedargument),
         interact = false,
         object = false,
         ;

    dlvars l1, l2, filename;

    lconstant
         arglist=['/bin/csh' '-ce' 0];
    if args = [=??l1 '-i' =??l2] then
        true -> interact;
        l1 nc_<> l2 -> args
    endif;
    if args = [=??l1 '-o' =?filename =??l2] then
        l1 nc_<> l2 -> args
    else
        false -> filename;
    endif;
    member('-c',args) or member('-S',args) -> object;
    if interact and object then vederror('INCOMPATIBLE FLAGS: -i and -c')
    elseif filename and object then vederror('INCOMPATIBLE FLAGS -o and -c')
    endif;
    nullstring -> vedargument;  ;;; for ved_w1 and ved_w


    for arg in args do
        if isendstring('.f',arg) or isendstring('.F',arg)
        or isendstring('.r',arg)  or isendstring('.o',arg)  then
            namecount + 1 -> namecount;
            arg ->name;
        endif
    endfor;
    if namecount == 0 then
        ;;; no filename so use current file
        if isendstring('.f',vedcurrent) or isendstring('.F',vedcurrent)
        or isendstring('.r',vedcurrent)
        then
            [^vedpathname ^^args] -> args;
            unless object or filename then
                allbutlast(2,vedpathname) -> filename
            endunless;
            if vedchanged and vedchanged /==0  then ved_w1() endif;
        else vederror('NO .o or .f FILE SPECIFIED')
        endif
    else
        ;;; write everything, in case some files in VED
        ved_w()
    endif;
    if not(object) and not(filename) and namecount == 1 then
        allbutlast(2,name) -> filename
    elseif not(object) and not(filename) then
        'a.out' -> filename
    endif;

    if filename and readable(filename) ->> dev then
        sysclose(dev);
        if fcomp_chatty then
            vedputmessage('RENAMING \'' sys_>< filename sys_>< '\' appending "-"');
            syssleep(200);
        endif;
        filename sys_>< '-' -> bakfile;
        sysunlink(bakfile) ->;
        syslink(filename,bakfile) ->;
        sysunlink(filename)->
    endif;

    ;;; create a multi-command argument for csh
    cons_with consstring
    {%
         explode('f77 '),
         if fcomp_pre then explode(fcomp_pre), `\s` endif,
         applist(args, procedure; explode(),`\s` endprocedure),
         if filename then explode('-o '), explode(filename) endif,
         if fcomp_post then `\s`, explode(fcomp_post) endif
         %} -> command;

    ;;; create a string of csh commands
    cons_with consstring
    {% `(`,
         ;;; compile the file
         explode(command), `;`,
         unless interact or object or not(filename) then
             explode('echo ""; echo "Running the program produced:-";');
             ;;; run the file
             explode(filename),                      ;;; run
         endunless;
     `)` %}     -> arglist(3);

    vedpipein('/bin/csh',
        arglist,
        if filename then
            filename sys_>< '.out', ;;; name of output file
        else
            systmpfile(false,'f77','out')
        endif,
        vedveddefaults <> procedure; 1 -> vedchanged endprocedure,
        true,                       ;;; display the file
        command);                   ;;; header for file

    ;;; handle interaction
    if interact and filename and (readable(filename) ->> dev) then
        sysclose(dev);
        vedscreenreset();
        true -> vedprintingdone;
        sysobey(filename);
    endif;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 22 1995
        Replaced use of mat*ches with = and new matchvars
--- Aaron Sloman, Jan 14 1989 removed vednullstring
--- Aaron Sloman, May  8 1988
        removed -f68881
*/
