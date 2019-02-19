/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/systmpfile.p
 > Purpose:
 > Author:          Various (see revisions)
 > Documentation:   REF *SYSUTIL
 */

;;; --------------- GENERATE A TEMPORARY FILENAME ---------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE '../lib/include/vedfile_struct.ph'

constant
        procedure sys_file_stat
    ;

weak constant
        procedure vedpresent
    ;

vars
        poppid
    ;

;;; ---------------------------------------------------------------------

lvars
    tmpfilenum = 0;

define systmpfile(dir, prefix, type);
    lvars prefix, dir, type, name, vfile, path;
#_IF DEF VMS
    unless dir then 'sys$scratch:' -> dir endunless;
#_ELSE
    unless dir then systranslate('TMPDIR') or '/tmp' -> dir endunless;
#_ENDIF
    'x' sys_>< abs(poppid) sys_>< type -> type;
    repeat
        tmpfilenum fi_+ 1 -> tmpfilenum;
        (prefix sys_>< tmpfilenum) <> type -> name;
        dir dir_>< name -> path;
        unless sys_file_stat(path, {})
        or testdef vedpresent
            and ( ((weakref vedpresent(name) ->> vfile)
                                and fast_subscrv(VF_DIRECTORY, vfile) = dir)
                    or weakref vedpresent(path)
                )
        then
            return(sysfileok(path))
        endunless
    endrepeat;
enddefine;



/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, May 25 1995
        Removed unsafe use of Clawback: vedpresent calls current_directory
        which may assign to a global variable
--- John Gibson, Dec  2 1994
        Changed to use sys_file_stat rather than readable to test whether
        file exists; also uses Clawback to recover space.
--- John Gibson, Apr  8 1994
        Changed for Unix to use $TMPDIR if defined
--- John Gibson, Apr  2 1992
        Made it use vedfile_struct.ph directly
--- John Gibson, Feb 14 1988
        Moved -systmpfile- into this from sysutil.p
 */
