/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_dir.p
 > Purpose:         VED interface to DCL "DIRECTORY" command
 > Author:          Aaron Sloman, April 1982 (see revisions)
 > Documentation:   HELP * DIR
 > Related Files:   LIB * VED_LS (Unix equivalent), LIB * VED_PURGE, LIB * VED_DEL
 */
compile_mode :pop11 +strict;

section;

#_IF hd(sys_os_type) == "vms"

define vars veddirdefaults();
    vedveddefaults();
    false ->> vedcompileable ->> vedwriteable ->> vedbreak -> vednotabs;
    20 -> vedindentstep
enddefine;

/* Create temporary mailbox to hold ouptut from sys_spawn. Use permanent
    identifier to point to mail box (otherwise GC will close the mailbox
    and VMS will destroy it, leaving a "log" file with the contents of the
    mailbox in the current directory.
*/

constant
    veddirmailboxname   =   systmpfile('', 'ved_dir', ''),
    veddirmailbox       =   sysmailbox(veddirmailboxname, 2, "line"),
    ;

define vars ved_dir();
    sys_spawn(vedcommand, false, veddirmailboxname, false, false) ->;
    if vedpresent(veddirmailboxname) then
        veddirmailboxname -> vedargument;
        ved_ved();
        ved_clear();
        ved_r()
    else
        vededit(veddirmailboxname, veddirdefaults)
    endif
enddefine;

#_ELSE /* Unix */

define vars ved_dir();
    vederror('Please use <ENTER> LS instead')
enddefine;

#_ENDIF

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 21 1994
        Changed to use new sys_spawn
--- John Williams, Nov 17 1992
        Completely rewritten, using sysmailbox, in order to fix BR tomk.22
 */
