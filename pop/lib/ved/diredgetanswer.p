/* --- Copyright University of Sussex 1988.  All rights reserved. ---------
 > File:           C.unix/lib/ved/diredgetanswer.p
 > Purpose:        Used with VED_DIRED, to get user confirmation
 > Author:         Aaron Sloman, Oct 1 1988
 > Documentation:   HELP *DIRED, *DIRED.SHORT
 > Related Files:   LIB *VED_DIRED
 */

#_TERMIN_IF DEF POPC_COMPILING

uses ved_dired;

section $-dired => diredgetanswer;

define global diredgetanswer(message, file) -> message;
    ;;; print message about file and return false unless character typed
    ;;; is y or Y
    lvars message, file;
    vedputmessage(message sys_>< file sys_>< ' ? y/n');
    strmember(vedinascii(), 'yY') -> message;
    vedputmessage(nullstring);
enddefine;

endsection;
