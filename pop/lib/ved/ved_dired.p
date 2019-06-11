/* --- Copyright University of Sussex 2008.  All rights reserved. ---------
 > File:           C.unix/lib/ved/ved_dired.p
 > Purpose:        EDIT a directory listing and obey commands
 > Author:         Aaron Sloman, Oct 1 1988 (see revisions)
 > Documentation:   HELP *DIRED, *DIRED.SHORT
 > Related Files:   LIB *DIRED_CSH *DIRED_HELP *DIRED_MV *DIRED_PEEK
 >                  LIB * DIRED_PURGE *DIRED_RM *DIRED_VED *DIREDGETANSWER
 >                  LIB  *DIREDPIPEIN *VED_QDIRED
 */

#_TERMIN_IF DEF POPC_COMPILING

;;; An extendable directory browser/editor
;;; Very  loosely modelled on EMACS dired
;;; Extended by autoloadable files with name starting dired_
;;;     or by adding to the table dired_actions. See HELP * DIRED

;;; See $usepop/pop/lib/ved/*dired*.p
;;; Files starting dired_ are potential commands for dired.

section $-dired =>
            dired_protect_files, dired_current_file,
            procedure(ved_dired, dired_action);

global vars dired_current_file;

unless isboolean(dired_current_file) then
    true -> dired_current_file;
endunless;

;;; next variable can be used to over-ride dired_current_file
vars use_dired_current_file = true;


global vars
    dired_protect_files,    ;;; if true, default is files read only
    procedure(ved_dired, dired_action);

true -> popdefineprocedure;

lconstant macro(
    default_command = 'dired -',
    );

constant
    dired_header = 'DIRED:- ',          ;;; inserted in temporary files
    dired_extra_header = 'DIRED:- dired ', ;;; check for dired file
    NOFILE = 'FILE NOT FOUND:- ',       ;;; error message
    ;

global vars dired_protect_files = false;    ;;; switched by dired -r, dired -w

;;; DIRED ACTIONS
;;; A property mapping flags e.g. '-a' '-b' onto procedures to be run.
;;; See HELP *DIRED for inforation about the arguments passed to the procedure

global vars
    procedure dired_action =
        newanyproperty([],19,false,false,syshash,nonop =,true,false,false);

;;; Could hide the property inside a checking procedure

;;; Autoloadable procedures invoked via the dired_action table:

"dired_rm" ->> dired_action('-rm') ->> dired_action('-rm-r')
    -> dired_action('-rmdir');

"dired_csh" -> dired_action('-$');

"dired_help" ->> dired_action('-?') -> dired_action('-??');

"dired_ved"
    ->> dired_action('-rv') ->> dired_action('-vr')
    ->> dired_action('-wv') ->> dired_action('-vw')
    ->> dired_action('-w/') ->> dired_action('-r/')
    ->> dired_action('-')   -> dired_action('-/');

"dired_mv" ->> dired_action('-mv') -> dired_action('-cp');

lvars dired_no_arg_needed = ['-r' '-w'];


;;; ------------------------UTILITIES----------------------------
;;; A user-definable procedure to generate temporary file names

define dired_tempfile (filespec);
    ;;; trim trailing "/*" from filename
    lvars filespec;
    if isendstring('/*',filespec) then
        allbutlast(2,filespec) -> filespec
    endif;
    systmpfile('/tmp',sysfilename(filespec),nullstring);
enddefine;


define dired_setup();
    ;;; Set up default_command for next time
    unless vedcommand = default_command then
        vedputcommand(default_command);
    endunless;
enddefine;

define dired_obey();
    ;;; Spawn sub-process to run command in the string
    vedputmessage('\s');    ;;; clear command line for error messages
    0 -> vedscreencharmode; ;;; make error messages readable
    sysobey();
enddefine;

vars show_output_on_status; ;;; Used by vedpipein.

define lconstant dired_show_files(filespec, dirs_only, do_ls, buffer_arg);
    ;;; print out files in directory or files matching pattern corresponding
    ;;; to filespec
    lvars filespec, dirs_only, isdir, do_ls, buffer_arg, c,
         dired_repeater, file, newbuffer, oldfile = ved_current_file,
         oldbuffsize = vvedbuffersize, oldinterrupt=interrupt;

    dlocal vedautowrite = false;    ;;; suppress autowriting

    ;;; check if new temporary file is to be created
    not(dired_current_file and use_dired_current_file) -> newbuffer;
    unless newbuffer then vedpositionpush() endunless;

    if do_ls then
        ;;; NB diredpipein is sensitive to dired_current_file
        valof("diredpipein")
            ('ls ' sys_>< do_ls sys_><
                if dirs_only then ' % | grep "^d"' else 'd %' endif,
                vedargument);
    else

        define dlocal interrupt();
            dlocal interrupt = setpop;  ;;; prevent infinite error loops
            oldfile -> ved_current_file;
            true -> vedediting;
            vedputmessage('INTERRUPTED');
            oldinterrupt();
        enddefine;

        valof("sys_file_match")(filespec,nullstring,false,false) -> dired_repeater;

        if newbuffer then
            vedopen(dired_tempfile(filespec), vedhelpdefaults) ->> newbuffer
                                        -> ved_current_file;
            false -> vedediting;
        else
            vedlinebelow()
        endif;
        vedputmessage('Please wait');
        false -> vedbreak;
        ;;; record command before inserting directory entries
        vedinsertstring(dired_header);
        vedinsertstring(vedcommand);
        if buffer_arg then vedcharright(); vedinsertstring(filespec) endif;
        false -> vedediting;    ;;; only now if not newbuff
        0 -> c;
        repeat
            dired_repeater() -> file;
        quitif(file == termin);
            c fi_+ 1 -> c;
            ;;; Print information about file in VED buffer
            sysisdirectory(file) -> isdir;
            unless dirs_only and not(isdir) then
                vedlinebelow();
                if isdir then vedinsertstring('DIR ') else 5 -> vedcolumn endif;
                vedinsertstring(file)
            endunless
        endrepeat;
        if newbuffer then
            vedjumpto(1,1);
            oldfile -> ved_current_file;
        endif;
        vedsetcursor();
        true -> vedediting;
        if c == 0 then
            if newbuffer then
                vedputmessage(NOFILE sys_>< filespec)
            else
                vedtextright(); vedinsertstring(' EMPTY');
            endif
        elseif newbuffer then
            vedsetonscreen(newbuffer,false)
        endif;
    endif;
    unless newbuffer then
        vedpositionpop();
        ;;; mark end of inserts if necessary
        if vvedbuffersize > oldbuffsize + 1 then
            vedpositionpush();
            vedjumpto(vedline + vvedbuffersize - oldbuffsize, 1);
            vedlinebelow();
            vedinsertstring('ENDDIRED');
            vedpositionpop();
            vedrefreshrange(vedline, vvedbuffersize, undef);
        endif
    endunless;
    unless ved_current_file == oldfile then
        dired_setup();
        vedputmessage('Select line and press REDO to examine option')
    endunless
enddefine;


;;; MAIN PROCEDURE

define global dired_control(quit_first);
    ;;; The main controlling procedure.
    lvars c, file, quit_first, proc, name, val, first_time=true,
         buffer_arg=false,
         flag=false, dirs_only=false, do_ls=false, isdir=false;
    dlocal vedediting;

    define lconstant dired_thisfile() -> file;
        ;;; Get file name, from right hand end of current line
        lvars file, col, char, spacefound = false;
        if vvedlinesize == 0 then vederror('NO FILE NAME') endif;
        vedtrimline(); vedthisline() -> file;
        ;;; Find file name demarcated by spaces or tabs
        fast_for col from vvedlinesize by -1 to 1 do
            subscrs(col, file) -> char;
            if char == `\s` or char == `\t` then
                true -> spacefound;
                quitloop()
            endif;
        endfast_for;
        if spacefound then allbutfirst(col, file) -> file endif;
    enddefine;

    define lconstant setarg;
        ;;; get argument from current line in file
        returnif(vvedlinesize == 0 and member(flag, dired_no_arg_needed));
        true -> buffer_arg;
        dired_thisfile() -> vedargument;
    enddefine;

    vedrefreshstatus();
    if quit_first then vedputmessage('QUITTING') endif;
    ;;; Treat '-' as a null flag
    if vedargument = '-' then nullstring -> vedargument endif;
    repeat
        if vedargument = nullstring then
            ;;; no argument - get it from the VED buffer
            setarg();
            quitloop
        elseif strmember(`-`,vedargument) /== 1 then
            ;;; no (more) flags
            quitloop
        endif;
        ;;; vedargument starts with `-`. Break off flag(s)
        if strmember(`\s`, vedargument) ->> c then
            substring(1, c-1, vedargument) -> flag;
            allbutfirst(c, vedargument) -> vedargument;
        else
            ;;; the whole of vedargument is a flag
            vedargument -> flag;
            ;;; get real argument from the VED buffer
            setarg();
        endif;
        buffer_arg and isstartstring(dired_extra_header, vedbuffer(1))
        and not(quit_first) -> use_dired_current_file;

        ;;; check (once only) if the flag is in the table dired_action
        if first_time then
            if dired_action(flag) ->> proc then
                recursive_valof(proc) -> val;
                unless isprocedure(val) then
                    vederror('PROCEDURE REQUIRED ' sys_>< proc)
                endunless;
            elseif flag then
                allbutfirst(1, flag) -> name ;
                consword('dired_' sys_>< name) -> name;
                if testdef sys_autoload then weakref sys_autoload(name) -> endif;
                if isdefined(name) and isprocedure(valof(name)) then
                    name -> proc; valof(proc) -> val
                endif
            endif;
            if proc then
                ;;; set up arguments for val then chain to it
                if vedargument = nullstring then
                    setarg()
                endif;
                chain(flag,
                    vedargument,
                    if buffer_arg then false, else dired_thisfile() endif,
                    quit_first,
                    val,
                    if quit_first then vedqget endif);
            endif
        else false -> first_time
        endif;
        if flag = '-r' then
            vedputmessage('DEFAULT READ-ONLY');
            true -> dired_protect_files; dired_setup(); return
        elseif flag = '-w' then
            vedputmessage('DEFAULT WRITEABLE');
            false -> dired_protect_files; dired_setup(); return
        elseif flag = '-d' then true -> dirs_only;
        elseif isstartstring('-l', flag) then
            flag -> do_ls;
            strmember(`d`, flag) -> dirs_only;
        else vederror('UNKNOWN OPTION')
        endif;
    endrepeat;
    buffer_arg  and  isstartstring(dired_header, vedbuffer(1))
        and not(quit_first) -> use_dired_current_file;

    sysfileok(vedargument) -> vedargument;
    ;;; Is the string either a directory name or a file name pattern
    ;;; Either could specify more than one file
    strmember(`*`,vedargument)
    or strmember(`?`,vedargument)
    or sysisdirectory(vedargument)
        -> isdir;
    unless isdir or do_ls then
        ('-', vedargument, false, quit_first, valof("dired_ved")),
        if quit_first then vedqget() else apply() endif;
    else
        if sysisdirectory(vedargument) then
            ;;; if it's a directory, list all the files in it
            vedargument dir_>< '/*' -> vedargument
        endif;

        (vedargument, dirs_only, do_ls, buffer_arg, dired_show_files),
        if quit_first then vedqget() else chain() endif;
    endunless;
enddefine;

define global ved_dired =
    ;;; the main top level procedure
    dired_control(%false%)
enddefine;

/*
;;; ------------------------DEFAULT KEY SETTINGS  ???--------------------
vedsetkey('\^X\^D',vedenter<>vedinsertstring(%default_command%));

vedsetkey('\^X\^Z', procedure;
    dlocal vedcommand = nullstring;
    ved_dired();
endprocedure);
vedsetkey('\^X\^X', procedure;
    dlocal vedcommand = nullstring;
    ved_qdired();
endprocedure);

*/

endsection;


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jun 20 2008
        Commented out the suggested default key bindings as they cause
        autoloading errors, and can interfere with other uses of ^X
        in Ved
--- John Williams, Dec 11 1997
        dired -r and dired -w no longer give an error if executed when
        cursor is on an empty line (BR joew.15).
--- Robert John Duncan, Jun  8 1995
        Replaced vars statements inside procedures with dlocal
--- John Gibson, Apr 19 1993
        Converted to use ved_current_file instead of old vedsave/setglobals
--- Aaron Sloman, Jun  9 1991
    Delay autoloading of sys_file_match
--- John Williams, Mar 19 1991
    Changed second arg to -sys_file_match- to -nullstring- instead of
    -false- (which John Gibson considers "deprecated").
--- Aaron Sloman, Oct  7 1990
    Supress autowriting while inserting directory entries in file
--- Aaron Sloman, May 14 1990
    Added dired_extra_header for checking dired files
    Made dired_current_file default to true
    Improved interface when dired_current_file is true
    Allowed <ENTER> dired -ld  to restrict to directories.
--- Aaron Sloman, Aug  1 1989
    added dired_current_file and use_dired_current_file, to allow output
        to be put in current file when it is a dired file.
--- Aaron Sloman, May  1 1989
    removed use of pdr_valof
--- Aaron Sloman, Dec 18 1988
    dired_thisfile fixed to cope with single leading space!
--- Aaron Sloman, Nov 19 1988
    dired_thisfile fixed to cope with any mixture of leading tabs and spaces
--- Aaron Sloman, Oct 30 1988
    Made procedure mapped onto ^X^Z ignore vedcommand.
--- Aaron Sloman, Oct 22 1988
    Made dired -r and dired -w print out current default
 */
