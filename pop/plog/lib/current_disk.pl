/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/plog/lib/current_disk.pl
 >  Purpose:        Predicates to give information about disk files
 >  Author:         Jonathan Laventhol, Mar 27 1984 (see revisions)
 >  Documentation:  HELP * CURRENT_DISK
 */

;;; current_disk(Name)
;;;     Name is the name of a file in the current directory

;;; current_disk(Spec, File, Stat)
;;;     Spec is a filename specification according to operating system
;;;         rules; it may contain wildcards.
;;;     File is a list of atoms, being the components of a file name
;;;         according to operating system rules:
;;;         [Fullname, Node, Device, Directory, Name, Type, Version]
;;;     Stat is a list of information about the file:
;;;         [Size, Date, Grp, Usr, Prot]
;;;         Size is in bytes
;;;         Date is an atom.  Format dependant on OS
;;;         Grp, Usr are owner's group and user numbers
;;;         Prot is number representing protection.  Dependant on OS.

:- prolog_language("pop11").

#_IF hd(sys_os_type) == "vms"

    ;;; Pattern to match all files in the current directory
    lconstant match_all = '*.*.0';

    ;;; Convert a string resulting from a -stat- or -sysfileparse- call to
    ;;; an atomic object
    define lconstant convert(c);
        lvars c, n;
        if not(c.isstring) then
            c;
        elseif strnumber(c) ->> n then
            ;;; version number
            n;
        else
            consword(c);
        endif;
    enddefine;

#_ELSE  ;;; UNIX

    lconstant match_all = '*';

    define lconstant convert(c);
        lvars c;
        if c.isstring then consword(c) else c endif;
    enddefine;

#_ENDIF


;;; current_disk/1:

procedure(Name, contn);
    lvars Name, pattern, name, rep, contn;
    if (prolog_deref(Name) ->> pattern).isword then
        pattern sys_>< vednullstring
    else
        match_all
    endif -> pattern;
    if sys_file_match(pattern, vednullstring, false, true) ->> rep then
        until (rep() ->> name) == termin do
            if name then
                prolog_unifyc(Name, consword(name), contn);
            else
                ;;; first call only: erase the current directory name
                -> ;
            endif;
        enduntil;
    endif;
endprocedure -> prolog_valof("current_disk", 1);


;;; current_disk/3:

procedure(Spec, Name, Stats, contn);
    lvars Spec, Name, Stats, pattern, name, stats, statvec, rep, contn;
    if (prolog_deref(Spec) ->> pattern).isword then
        pattern sys_>< vednullstring -> pattern;
    else
        mishap(Spec, 1, 'File specification must be an atom');
    endif;
    initv(5) -> statvec;
    if sys_file_match(pattern, vednullstring, statvec, false) ->> rep then
        until (rep() ->> name) == termin do
            -> stats;
            [%
                consword(name),
                appdata(sysfileparse(name), convert)
            %] -> name;
            if stats then
                [% appdata(statvec, convert) %]
            else
                [ ? ? ? ? ? ]
            endif -> stats;
            prolog_unifyc(Name, name,
                prolog_unifyc(% Stats, stats, contn %));
        enduntil;
    endif;
endprocedure -> prolog_valof("current_disk", 3);

/* --- Revision History ---------------------------------------------------
--- Rob Duncan, Feb 15 1988
    Fixed the UNIX -convert- procedure to stop it falling over on long
    (biginteger) dates;
    merged the UNIX and VMS versions by adding the #_IF ...;
    tidied up considerably.
--- Kathryn Seifert, Sep  2 1986
    Changed first argument of sys_file_match in the definition of
    current_disk/1 to work with UNIX directory specification format
*/
