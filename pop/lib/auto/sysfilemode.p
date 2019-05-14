/*  --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:           C.unix/lib/auto/sysfilemode.p
 > Purpose:        Access/update the mode of a file
 > Author:         Mark Rubinstein, Jan  9 1986 (see revisions)
 > Documentation:  HELP * SYSFILEMODE
 > Related Files:
 */
compile_mode:pop11 +strict;

section;

exload sysfilemode
(language C)
    lconstant chmod(file,mode) :int;
endexload;

define sysfilemode(file);
    lvars file;
    lconstant stats = writeable initv(5);
    if sys_file_stat(file, stats) then
        subscrv(5, stats);
    else
        mishap(file, 1, '%CAN\'T GET FILE MODE (%M)');
    endif;
enddefine;

define updaterof sysfilemode(mode, file);
    lvars mode, file;
    unless isinteger(mode) then
        mishap(mode, 1, 'INTEGER NEEDED (for file mode)');
    endunless;
    sysfileok(file) -> file;
    unless exacc chmod(file, mode) == 0 then
        ;;; %M works in Unix since it uses errno
        mishap(mode, file, 2, '%CAN\'T SET FILE MODE (%M)')
    endunless;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 11 1996
        Tidied, removed use of sys*iomessage
--- John Williams, Apr 26 1993
        Made stats writeable.
--- John Williams, Apr 15 1993
        Reversed previous change, which is no longer necessary.
--- Robert John Duncan, Apr  9 1991
        Added null terminator for filename passed to -chmod-
--- Robert John Duncan, Nov 23 1990
        Changed to use -exload- syntax. Tidied up.
 */
