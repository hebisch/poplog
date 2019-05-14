/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.unix/lib/lib/ioctl_IO.p
 > Purpose:         Procedures used by sys_io_control/ioctl libraries.
 > Author:          Chris Slymon, July 1984 (see revisions)
 > Documentation:   HELP * IOCTL
 > Related Files:   LIB *ioctl_locals, *ioctl_ltchars, *ioctl_sgttyb, *ioctl_tchars
 */

#_TERMIN_IF DEF POPC_COMPILING

loadinclude unix_ioctl.ph;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 11 1992
        Now just loadincludes unix_ioctl.ph
--- Robert John Duncan, Jun 25 1992
        Moved to C.unix
--- John Gibson, Feb 18 1989
        Got rid of popdprecision = true in these procedures (relic of
        pre-biginteger days).
--- Mark Rubinstein, Jan  9 1986 - sectionised.
*/
