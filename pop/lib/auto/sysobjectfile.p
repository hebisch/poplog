/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/lib/auto/sysobjectfile.p
 > Purpose:         Generate machine and OS-release specific object file name
 > Author:          John Williams, Nov 10 1993 (see revisions)
 > Documentation:   REF * sysobjectfile
 > Related Files:
 */
compile_mode:pop11 +strict;

section;

include sysdefs

lvars ARCH, OBJECT_SUFFIX, ARCHIVE_SUFFIX, item;

consstring
    (#| for item in sys_machine_type do `_`, dest_characters(item) endfor |#)
    -> ARCH;

#_IF DEFV SUNOS >= 5.0
    ARCH <> 'r5' -> ARCH;
#_ENDIF

#_IF DEFV SYSTEM_V >= 4.0 or DEF OSF1 or DEF LINUX_ELF or DEF AIX
    '.so', dup()
#_ELSEIF DEF HPUX and DEF SHARED_LIBRARIES
    '.sl', dup()
#_ELSEIF DEF VMS
    '.obj', '.olb'
#_ELSEIF DEF WINDOWS
    '.dll', dup()
#_ELSE
    '.o', '.a'
#_ENDIF
    -> (OBJECT_SUFFIX, ARCHIVE_SUFFIX);


define sysobjectfile(fname);
    lvars want_archive;
    if isboolean(fname) then
        fname -> want_archive;
        -> fname
    else
        false -> want_archive
    endif;
    consstring
        (#| explode(sys_fname(fname, 1, 4)),
            explode(ARCH),
            explode(if want_archive then
                        ARCHIVE_SUFFIX
                    else
                        OBJECT_SUFFIX
                    endif)
        |#)
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Jun  1 1998
        Added AIX
--- Robert Duncan, Aug 12 1996
        Changed to support a wider range of systems
--- John Williams, Jun 27 1995
        Fixed for Irix 5.x (cf. BR isl-fr.4567).
--- John Williams, Dec  1 1993
        Now copes with VMS.
--- John Williams, Nov 16 1993
        Now takes optional argument want_archive.
 */
