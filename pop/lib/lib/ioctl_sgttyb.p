/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:           C.unix/lib/lib/ioctl_sgttyb.p
 > Purpose:        Constants for accessing 'sgttyb' struct (basic modes)
 > Author:         Chris Slymon, July 1984 (see revisions)
 > Documentation:  HELP * IOCTL, REF * SYS_IO_CONTROL, MAN 4 TTY
 > Related Files:  LIB * IOCTL_IO
 */

#_TERMIN_IF DEF POPC_COMPILING

uses ioctl_IO;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 11 1992
        Now just uses ioctl_IO (which loadincludes unix_ioctl.ph)
--- Robert John Duncan, Jun 25 1992
        Moved to C.unix
--- John Williams, Nov 10 1987
        Fixed SFR 4137 - swapped FIONREAD and TIOCOUTQ; added FIONBIO
--- Mark Rubinstein, Jan  9 1986 - sectionised and tidied.
*/
