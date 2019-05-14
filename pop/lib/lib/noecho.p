/* --- Copyright University of Sussex 1992.  All rights reserved. ---------
 > File:           C.unix/lib/lib/noecho.p
 > Purpose:        macro to turn off echoing on popdevout
 > Author:         Roger Evans, Dec 10 1986 (see revisions)
 > Documentation:  HELP *NOECHO
 > Related Files:  LIB *PTYFORK
 */
compile_mode :pop11 +strict;

/* SEE DISCLAIMER IN HELP FILE */

include unix_ioctl.ph;

section;

lconstant tty_mode = writeable initshortvec(3);

define global vars macro noecho;
    lvars ioctl_ok;
    if sys_io_control(popdevout,TIOCGETP,tty_mode) ->> ioctl_ok then
        tty_mode(3) &&~~ ECHO &&~~ CBREAK &&~~ RAW &&~~ CRMOD -> tty_mode(3);
        sys_io_control(popdevout,TIOCSETP,tty_mode) -> ioctl_ok;
    endif;
    unless ioctl_ok then
        mishap(0, 'IOCTL FAILED');
    endunless;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 22 1992
        Uses unix_ioctl
--- Robert John Duncan, Jun 29 1992
        Added check that the ioctls work and moved to C.unix
--- Robert John Duncan, Jun  1 1992
        Removed last call of -sys_io_control-: no longer needed.
--- Aaron Sloman, Oct 24 1988
        Reduced to lower case (agreed with Roger) and installed in non
        autoloadable public library.
 */
