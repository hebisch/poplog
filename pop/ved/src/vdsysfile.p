/* --- Copyright University of Sussex 1997.  All rights reserved. ---------
 > File:           C.all/ved/src/vdsysfile.p
 > Purpose:        Commands for examining system files in VED
 > Author:         Aaron Sloman, 1982 + others (see revisions)
 > Documentation:  HELP * VEDSYSFILE
 */

#_INCLUDE 'vddeclare.ph'
#_INCLUDE '../../src/dirs.ph'
#_INCLUDE '../../lib/include/doc_index.ph'

constant
        procedure (vedsetup, ved_try_search,
        Sys$-Ved$-Find_in_string, Sys$-Ved$-Safe_search
        )
    ;

vars
        procedure (vedrestorewindows, ved_backsearch, wved_raise_window),
        vvedgotoplace, vedlastcommand, vednamestring
    ;


vars

    veddocsubsystem     = "EXTERNAL",

    ;;; documentation directories
    vedhelpdirectory    = VEDHELPDIR,
    vedteachdirectory   = VEDTEACHDIR,
    vedrefdirectory     = VEDREFDIR,
    veddocdirectory     = VEDDOCDIR,

    ;;; default names for documentation files
    vedhelpname         = 'teach',
    vedteachname        = 'teach',
    vedrefname          = 'reffiles',
    veddocname          = 'docfiles',
    vedlibname          = 'libfiles',
    vedsrcname          = 'srcfiles',

    ;;; documentation lists
    vedhelplist     = [ [^VEDLOCHELPDIR help]
                        [^vedhelpdirectory help]
                        [^VEDLOCTEACHDIR teach]
                        [^vedteachdirectory teach]
                        [^veddocdirectory doc]
                        [^vedrefdirectory ref]
                      ],
    vedteachlist    = [ [^VEDLOCTEACHDIR teach]
                        [^vedteachdirectory teach]
                        [^VEDLOCHELPDIR help]
                        [^vedhelpdirectory help]
                        [^veddocdirectory doc]
                        [^vedrefdirectory ref]
                      ],
    vedreflist      = [ [^VEDLOCREFDIR ref]
                        [^vedrefdirectory ref]
                        [^veddocdirectory doc]
                        [^vedhelpdirectory help]
                        [^vedteachdirectory teach]
                      ],
    veddoclist      = [ [^VEDLOCDOCDIR doc]
                        [^veddocdirectory doc]
                        [^vedrefdirectory ref]
                        [^vedhelpdirectory help]
                      ],
    ;


    ;;; Identifying the system doc types. Could be extended by users
vars
    doctypelist =
        [   vedhelpname     'HELP'
            vedteachname    'TEACH'
            vedrefname      'REF'
            veddocname      'DOC'
            vedlibname      'LIB'
            vedsrcname      'SRC'
        ];


define vars vedgetlibfilename(list, defaultname, name) -> file;
    lvars fextn = false, srchlist, ss_extn;

    if defaultname == "vedlibname"
    and (sys_fname_extn(name) ->> fextn) /= nullstring then
        subsystem_searchlist(defaultname, fextn, true)
    else
        subsystem_searchlist(defaultname, veddocsubsystem, true)
    endif -> (srchlist, ss_extn);

    ;;; If libfile with empty extn, use the subsystem extn if there is one
    if fextn = nullstring then
        name sys_>< (ss_extn or pop_default_type)
    else
        name
    endif -> file;
    syssearchpath(srchlist, file, true) -> file;

    ;;; Search general list (unless already found)
    unless file then
        if fextn = nullstring then
            name sys_>< if pop_default_type = '.ph' then '.ph' else '.p' endif
        else
            name
        endif -> file;
        syssearchpath(list, file, true) -> file
    endunless;

    if file then
        name -> valof(defaultname)
    endif
enddefine;


define vars vedsysfileerror(doc_type, doc_name);
    lvars default;

    consstring(
        #|  if doc_type = 'LIB' or doc_type = 'SRC' then
                explode('HELP ')
            else
                explode(doc_type), `\s`
            endif;
            explode(doc_type);
            explode('FILES')
        |#) -> default;

    if vedinvedprocess then
        vederror('\{b}not found - try ' <> default)
    else
        consstring(
            #|  explode(doc_type), `\s`,
                appdata(doc_name, lowertoupper),
                explode(' not found - try')
            |#) -> doc_type;
        sys_pr_message(0, {^default ^doc_type 16:01}, nullstring, `W`);
        cucharerr(`\n`)
    endif
enddefine;


define consvedcrossref(doctype, name, gotoplace);

    define lconstant Ui_active(); lowertoupper() fi_|| `\{2iA}` enddefine;

    define lconstant Active(); () fi_|| `\{2A}` enddefine;

    consvedstring
        (#| appdata(doctype, Ui_active),
            appdata('\Ss*\Ss', Active);
            if Sys$-Ved$-Find_in_string(name, isuppercode)
            and Sys$-Ved$-Find_in_string(name, islowercode) then
                ;;; mixed case - leave alone
                appdata(name, procedure(); fi_|| `\{2iA}` endprocedure)
            else
                ;;; convert to upper case
                appdata(name, Ui_active)
            endif;
            if gotoplace then
                appdata('/' sys_>< gotoplace, Active)
            endif
        |#)
enddefine;


define lconstant Display_multiple_results(request, vec, defaultproc);
    lvars doctype, fname;

    if ispair(vec) then
        fast_front(vec), fast_front(fast_back(vec))
    else
        vec, "help"
    endif -> (vec, doctype);

    define lconstant Create_vedbuffer(setting_on_screen, doctype, vec);
        lvars i, entry;
        {% fast_for i from 1 to datalength(vec) do
            fast_subscrv(i, vec) -> entry;
            consvedcrossref(doctype, sys_fname_name(Info_file(entry)),
                            Info_hdr_start(entry))
        endfast_for %}
    enddefine;

    doctype sys_>< '\s' sys_>< request -> fname;
    vededit(
        consref({% [^fname ^doctype],
                   Create_vedbuffer(% doctype, vec %),
                   'More than one entry: Use ESC-n and ESC-h to select file'
                %}),
        defaultproc)
enddefine;


/*
    vedsysfile used for commands which edit files in system libraries.
    Assumes vedargument provided by veddocommand.  NAME is the name of the
    variable holding the default file name. LIST is a list of directories
    to try. DEFAULTPROC is a procedure specifying default values for some
    global variables, if false then it uses Vedsysdefaults which uses the
    tail of the entry found in the list. Currently that makes the second
    element the vedfileprops and third the vedcompilername (if present).
*/

define vars vedsysfile(name, list, defaultproc);
    lvars request, cllr, doc_type, file, n, s, c, string, gotoplace = false;

    vedargument -> request;     ;;; save the original requested name
    vedsetup();                 ;;; ensure vedinit.p compiled

    pdprops(caller(1)) -> cllr; ;;; e.g. "ved_help" or "ved_teach" etc.

    ;;; Get doc_type - defaults to 'HELP'
    Sys$-list_assoc_val(name, doctypelist) or 'HELP' -> doc_type;

    ;;; Check default file name
    unless isstring(valof(name)) and valof(name) /= nullstring then
        ;;; create the default name
        uppertolower(doc_type) sys_>< 'files' -> valof(name)
    endunless;

    if request = nullstring then
        ;;; No argument given, so use the default name
        ;;; Handle SHOWLIB and SRC specially - treat them as HELP
        vedgetlibfilename(
            if name == "vedlibname" or name == "vedsrcname" then
                vedhelplist, "vedhelpname" ->> request, valof(name)
            else
                list, name ->> request, valof(name)
            endif) -> file
    else
        ;;; argument was given so use it
        if strmember(`\s`, request) ->> n then
            ;;; second argument given
            request -> string;
            substring(1, n fi_- 1, request) -> request;
            if skipchar(`\s`, n, string) ->> n then
                allbutfirst(n fi_- 1, string) -> string;
                allbutfirst(1, string) -> s;
                fast_subscrs(1, string) -> c;
                if c == `/` then
                    s -> gotoplace
                elseif c == `@` and (strnumber(s) ->> n) then
                    n -> gotoplace
                else
                    vederror('\{b}invalid 2nd argument \'' <> string <> '\{b}\'')
                endif
            endif
        endif;

#_IF DEF UNIX
        ;;; don't convert if any characters already in lower case
        if Sys$-Ved$-Find_in_string(request, islowercode) then
            ;;; some lowercase in name
            vedgetlibfilename(list, name, request) -> file;
        else
            ;;; all uppercase, look for lowercase version
            vedgetlibfilename(list, name, uppertolower(request)) -> file;
            unless file then
                ;;; could't find that, look for uppercase
                vedgetlibfilename(list, name, request) -> file;
            endunless
        endif;
#_ELSE
        ;;; VMS - upper and lower case are equivalent
        vedgetlibfilename(list, name, request) -> file;
#_ENDIF
    endif;

    ;;; check for multiple doc_index entries
    if isvector(file)
    or (ispair(file) and isvector(fast_front(file))) then
        fast_chain(request, file, defaultproc or vedhelpdefaults,
                   Display_multiple_results)
    endif;

    ;;; see if logging is required
    if isprocedure(valof("vedsysfilelog"))
    and  pdnargs(valof("vedsysfilelog")) == 3
    then
        valof("vedsysfilelog")
        (cllr, request, if islist(file) then hd(file) else file endif);
    endif;

    ;;; display file
    if file then
        if defaultproc then
            if islist(file) then hd(file) -> file endif
        else
            vedhelpdefaults -> defaultproc
        endif;
        if gotoplace then gotoplace -> vvedgotoplace endif;
        chain(file, defaultproc, vededit)
    else
        vedsysfileerror(doc_type, request)
    endif
enddefine;


/*  Called when setting a file on screen (arg true), and when quitting
    a file (arg false).
    Returns true if vedvedname should NOT be set.
*/
define Sys$-Ved$-Set_sysfile_defaults(settingonscreen);
    lvars l, settingonscreen, type, defname;
    returnunless(vedfileprops) (false);
    returnunless(isword(vedfileprops)) (true);

    lowertoupper(fast_word_string(vedfileprops)) -> type;
    false -> defname;
    for l on doctypelist do
        if hd(tl(l)) = type then
            hd(l) -> defname;
            quitloop
        endif;
        tl(l) -> l
    endfor;
    if defname then
        if settingonscreen then
            sys_fname_name(vedcurrent)
        else
            ;;; quitting the file
            if vedfileprops == "teach" then 'teach' else nullstring endif
        endif -> valof(defname)
    endif;
    true
enddefine;


;;; --- COMMANDS FOR CALLING THE EDITOR FROM INSIDE THE EDITOR ------------


define vars ved_ref();
    vedsetup(); ;;; ensure vedinit.p compiled
    vedsysfile("vedrefname", vedreflist, false)
enddefine;

define vars ved_doc();
    vedsetup(); ;;; ensure vedinit.p compiled
    vedsysfile("veddocname", veddoclist, false)
enddefine;

define vars ved_help();
    vedsetup(); ;;; ensure vedinit.p compiled
    vedsysfile("vedhelpname", vedhelplist, false)
enddefine;

define vars ved_teach();
    vedsetup(); ;;; ensure vedinit.p compiled
    vedsysfile("vedteachname",vedteachlist, false)
enddefine;


;;; --- PROCEDURES FOR GETTING HELP FILES --------------------------------

define lconstant Helpchartype(char);
;;; temporary replacement for vedchartype in case redefined;
    lvars char;
    ;;; given a character return an integer code representing its type
    if isalphacode(char) or isnumbercode(char) then `a`
    elseif locchar(char, 1, '+-><*?=^!#$&~|\@:/') then `+`
    else    char
    endif
enddefine;

lconstant nhi_mess = '\{b}no help items in this file';

define vednexthelp();
    ;;; find next occurrence of "*" and move after it.
    ;;; Help file name should follow
    ;;; This procedure is associated with <ESC> n
    dlocal vedchartype = Helpchartype;
    unless Sys$-Ved$-Safe_search('*', [noembed], ved_try_search) then
        vederror(nhi_mess)
    endunless
enddefine;


define vedprevioushelp();
    ;;; search back for "*". Help file name should follow
    ;;; associated with <ESC> u
    dlocal vedchartype = Helpchartype;
    unless Sys$-Ved$-Safe_search('*', [back], ved_try_search) then
        vederror(nhi_mess)
    endunless
enddefine;




/* --- Revision History ---------------------------------------------------
--- John Williams, Apr  8 1997
        vedsysfile now handles gotoplace correctly.
--- John Gibson, Dec 12 1996
        Changed vedsysfile to allow 2nd arg following a space, which can be
        /searchstring or @line.
--- John Gibson, Mar  6 1996
        Fixed id-string arg to sys_pr_message (was false instead of
        nullstring)
--- John Williams, Feb 27 1996
        consvedcrossref now uses coloured spaces.
--- John Gibson, Feb  6 1996
        Uses sys_pr_message instead of sys*prmessage
--- John Williams, Jan 12 1996
        Fixed consvedcrossref for mixed-case file names.
--- John Williams, Jan 12 1996
        vedsysfile no longer calls sysfileok on vedargument (BR isl-fr.4521).
--- John Williams, Jan 11 1996
        vedgetlibfilename written more clearly.
--- John Williams, Jan  9 1996
        Multiple doc_index entries now displayed as active cross references.
--- John Williams, Jan  8 1996
        vedgetlibfilename now copes with duplicate doc_index entries.
--- John Gibson, Nov  4 1995
        Moved vedgetsys*file and vedgetsysfile*pdr out to library
--- John Gibson, Jun 29 1994
        Added UNIX as a synonym for MAN in vedgetsys*filepdr
--- John Gibson, Apr 29 1994
        Made vedgetsys*file check for EOF
--- John Gibson, Mar  7 1994
        Uses vededit
--- Jonathan Meyer, Sep 25 1993
        Converted to ved_try_search. Rewrote vedprevioushelp to use
        the [back] option to ved_try_search.
--- John Williams, Sep 17 1993
        vedgetsys*file now checks for \Ss after the *.
--- John Gibson, Jul 30 1993
        Made vedsrcname be initialised in this file rather than lib ved_src
--- John Gibson, May 17 1993
        Changed vedgetlibfilename so that for a lib file with an empty
        extension it uses the external/current subsystem extension (now
        returned by subsystem_searchlist with optional true arg)
--- John Gibson, Feb 16 1993
        Changed vedgetsys*file to use vedfileprops as default doctype when
        appropriate
--- John Gibson, Jan 11 1993
        Replaced vedgetlibfilename with subsystem version, etc.
--- John Gibson, Dec 21 1992
        o Moved vedhelpdefaults to vdprocess.p
        o Added Set_sysfile_defaults
--- John Williams, Feb 27 1992
        -vedgetsys*file- now ignores hyphens around an identifier name
        (fixes BR andrewl.23)
--- John Williams, Mar 19 1991
        -vednexthelp- and -vedprevioushelp- now use -Safe_search-
        (fixes BR johnw.52)
--- John Williams, Jan  4 1991
        Added INCLUDE, MAN, and SRC to -vedgetsys*filepdr-
--- John Williams, Nov  9 1990
        -vedsysfilerror- made more user-friendly.
--- John Gibson, Nov  1 1990
        Moved "help, "teach" and "ref" syntax words to library.
--- John Gibson, Oct 31 1990
        Made "help, "teach" and "ref" syntax words, and made them assign
        directly to vedargument, etc; test for vedediting being false then
        removed from vedsysfile.
--- Aaron Sloman, Jun 25 1990
        Fixed bug in -vedgetsys*file- that that prevented following search
        string being found in some cases.
--- John Williams, Mar  7 1990
        Fixed ER 50, by making -vedgetsys*file- treat ":" as a terminator
--- John Williams, Mar  5 1990
        -vedgetsys*file- tidied up, fixing FR 4300
--- John Williams, Jan  3 1990
        -vedsysfile- no longer calls -ved_??- if request fails
--- John Williams, Sep  6 1989
        Moved -Haslowercode- to 'vdutil.p'
--- Aaron Sloman, Jan  8 1989
        Made sure vedsetup called early enough.
--- Aaron Sloman, Dec  6 1988
        Last change to vedgetsys*file could screw up autoloading, with
        item_chartype changed. So localised the change to a procedure.
--- John Williams, Dec  6 1988
        Moved -syssearchpath- into 'syssearchpath.p'
--- Aaron Sloman, Oct 18 1988
        Fixed vedgetsys*file so that it no longer gives error if apostrophe
        found.
*/
