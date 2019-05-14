/* --- Copyright University of Birmingham 1997. All rights reserved. ------
 > File:            $poplocal/local/auto/issymlink.p
 > Purpose:         Detect symbolic links
 > Author:          John Williams and John Gibson, Sep  8 1997
 > Documentation:   HELP * SYS_FILE_STAT, MAN * STAT
 > Related Files:
 */

/*
;;; test

issymlink('$local/teach/faces') =>
issymlink('$local/rclib/teach/faces') =>
issymlink('$local/rclib/teach/rubbish') =>

*/

section;
compile_mode :pop11 +strict;

lconstant
    STM_IFMT = 8:170000, ;;; (mask for file type)
    STM_IFLNK = 8:120000; ;;; (value for symbolic link).

define issymlink(f);
    lconstant v = writeable {0 0 0 0 0};
        sys_file_stat(f, v, false) and v(5) && STM_IFMT == STM_IFLNK
enddefine;


endsection;
