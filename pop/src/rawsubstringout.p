/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/rawsubstringout.p
 > Purpose:
 > Author:          Ian Rogers (see revisions)
 > Documentation:   REF *CHARIO
 > Related Files:   C.all/src/rawcharout.p
 */

;;; ---------------- STANDARD RAW SUB-STRING OUTPUT ------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'

constant
        procedure Sys$-Io$-Put_char;
    ;

vars
        Sys$-Io$- _default_output16_char
    ;

uses (poprawdevout);


;;; ------------------------------------------------------------------------

section $-Sys$-Io => rawsubstringout;

define vars rawsubstringout(string, _index, _blen);
    lvars string, dev = raw_dev_out, _index, _blen;

    Check_device(dev, true);
    if string!KEY!K_FLAGS _bitst _:M_K_STRING16
    and not(fast_idval(dev!D_ENCODING_ID)) then
        ;;; output each char separately
        ;;; set this for compatibility with XVed
        dlocal _default_output16_char = `?`;
        while _blen fi_> 0 do
            Put_char(fast_subscrs(_index, string), dev);
            _index fi_+ 1 -> _index;
            _blen fi_- 1 -> _blen
        endwhile
    else
        fast_apply(dev, _index, string, _blen, dev!D_WRITE)
    endif
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 29 1997
        Made it set _default_output16_char when string16 and no encoding
--- John Gibson, Apr  4 1997
        D_ENCODING replaced by D_ENCODING_ID
--- John Gibson, Feb 18 1997
        String16 changes
--- John Gibson, Oct 22 1990
        Changed format of device write procedure args
 */
