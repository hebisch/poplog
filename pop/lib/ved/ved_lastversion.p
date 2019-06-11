/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/ved/ved_lastversion.p
 *  Purpose:        get version before last write command.
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:
 *  Related Files:
 */

section;

define global ved_lastversion();
lvars vedold = vedcurrent;
    ;;; get back to before the last WRITE command. Most recently written
    ;;; version of the file will be lost.
    vedputmessage('DELETING ' >< vedcurrent);
#_IF hd(sys_os_type) == "vms"
    sysdelete(vedcurrent >< ';0') ->;
#_ELSE
    sysdelete(vedcurrent) ->;
#_ENDIF
    vedrestorescreen();
    ;;; save current file in vedbufferlist, in case. (Needs improving)
    'tmp_file_' -> vedargument;
    ved_name();
    vedold -> vedargument;
    vedputmessage('RESTORING PREVIOUS VERSION OF '>< vedargument);
    ved_ved();
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Oct  4 1985 - sectionised and lvarsed.
 */
