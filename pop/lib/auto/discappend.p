/*  --- Copyright University of Sussex 1999.  All rights reserved. ---------
 >  File:           C.all/lib/auto/discappend.p
 >  Purpose:        produce a character consumer for appending to files
 >  Author:         Mark Rubinstein, Jun 18 1985 (see revisions)
 >  Documentation:  HELP * DISCAPPEND
 */
compile_mode:pop11 +strict;

section;

include sysdefs.ph;

define global discappend(file);
    lvars file, dev;
    unless isdevice(file ->> dev) then
        if isword(file) then file sys_>< pop_default_type -> file endif;
        sysopen(file, 2, false) -> dev;
    endunless;
    if dev then
#_IF DEF UNIX or DEF WIN32
        sysseek(dev, 0, 2);
#_ELSE
        lconstant LEN = 512;
        lvars buff = device_init_buffer(dev, LEN);
        while sysread(dev, buff, LEN) == LEN do
            /* nothing */
        endwhile;
#_ENDIF
    endif;
    discout(dev or file);
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Mar 15 1999
        Updated for Windows and 16-bit strings
--- Robert John Duncan, Jan 14 1994
        Allowed file to be a device already, for consistency with discout
        (on VMS, it would have to be opened for read/write, i.e. mode 2).
--- John Gibson, Dec  6 1993
        Added missing include for sysdefs
--- Simon Nichols, Mar 16 1992
        UNIX only: changed to use -sysseek- (see bugreport isl-er.168).
--- John Gibson, Jul 26 1987
        Made to add -pop_default_type- for a word arg
--- John Williams, Nov 21 1986
        added 'uses sysstring'
--- Aaron Sloman, Nov  1 1986
        altered to use sysstring
--- Mark Rubinstein, Aug 19 1985
        altered to use LCONSTANT due to bugs with
    #_< on VMS.
 */
