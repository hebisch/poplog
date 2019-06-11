/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:           C.unix/lib/ved/vediocontrol.p
 > Purpose:        alters poprawdevin to release characters used by UNIX
 > Author:         Mark Rubinstein, Jan 23 1986 (see revisions)
 > Documentation:  HELP * VEDIOCONTROL, REF * SYSIO/ SYS_IO_CONTROL
 > Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

/*
If loaded, this library file alters poprawdevin/out so that using raw mode
alters the terminal as if one had done:

    sysobey('stty susp undef flush undef lnext undef dsusp undef',`%`)

I.e. the relevant control characters then become available for use
as VED control characters. However, this is much faster than the above
call of sysobey.

The devices popdevin and popdevout, used for instance by charin and charout
will reset the terminal to whatever it was before. Thus going into and out
of ved (or anything else which uses poprawdevin/out) will automatically alter
the terminal characteristics as required.
*/

section $-library$-temp;

;;; This must be altered for UNIX 4.1

define _IOW(x,y,s);
    lvars x y s;
    16:80000000 + (s<<16) + (x<<8) + y
enddefine;

constant TIOCSLTC;  _IOW(`t`, 117, 6) -> TIOCSLTC;

;;; The following could be put into a procedure if it was to be used
;;; repeatedly
unless sys_io_control(
            poprawdevin, TIOCSLTC, '\(255)\(255)\(255)\(255)\(255)\(255)')
then
    mishap(0, 'ERROR IN LIB VEDIOCONTROL');
endunless;

section_cancel(current_section,true);
endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jun 29 1992
        Added check that the ioctl actually worked and moved to C.unix
--- John Gibson, Nov 11 1987
        Replaced -popdevraw- with -poprawdevin-
 */
