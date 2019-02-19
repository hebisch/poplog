/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/sysreadwrite.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSIO
 */

;;; ------------------- DEVICE READING/WRITING ----------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'

;;; ----------------------------------------------------------------------

section $-Sys => sysread, syswrite;

define lconstant Io_trans(dev, iostruct, _nchars, _doing_write);
    lvars iostruct, dev, _startchar = 1, _nchars, _doing_write, _len;
    if isinteger(dev) then
        ;;; startchar supplied
        (), dev -> (dev, _startchar);
        Check_integer(_startchar, 1)
    endif;
    Check_device(dev, true);
    if fast_idval(dev!D_ENCODING_ID) then
        unless isstring(iostruct) then
            if _doing_write and isword(iostruct) then
                ;;; assume string of word to be used in a write
                iostruct!W_STRING -> iostruct
            else
                mishap(dev, iostruct, 2, 'STRING NEEDED FOR ENCODED CHARACTER I/O')
            endif
        elseunless _doing_write or iostruct!KEY!K_FLAGS _bitst _:M_K_STRING16
        then
            mishap(dev, 1, 'STRING16 NEEDED FOR ENCODED CHARACTER INPUT')
        endunless;
        iostruct!V_LENGTH -> _len
    else
        unless iscompound(iostruct)
        and iostruct!KEY!K_FLAGS _bitst _:M_K_BYTE_ACCESS then
            if _doing_write and isword(iostruct) then
                ;;; assume string of word to be used in a write
                iostruct!W_STRING -> iostruct
            else
                mishap(dev, iostruct, 2, 'INVALID STRUCTURE FOR I/O')
            endif
        endunless;
        ##(b){ @@POPBASE{fast_apply(iostruct, iostruct!KEY!K_GET_SIZE)} | w}
                        -> _len
    endif;
    Check_integer(_nchars, 0);
    (dev, _startchar, iostruct, _nchars);
    if _int(_startchar fi_+ _nchars fi_- 1) _lteq _len then
        ((), dev)
    else
        mishap((), 4, 'I/O REQUEST EXCEEDS STRUCTURE SIZE')
    endif
enddefine;

define sysread() with_nargs 3;
    fast_apply(Io_trans(false)!D_READ)
enddefine;

define syswrite() with_nargs 3;
    fast_apply(Io_trans(true)!D_WRITE)
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  4 1997
        Now insists on a string when fast_idval(dev!D_ENCODING_ID) is true
        (and a string16 for input).
--- John Gibson, Oct 22 1990
        Changed for new-style dev read/write procedure args
--- John Gibson, Dec  2 1989
        Changes for new pop pointers.
--- John Gibson, Feb 19 1989
        Included io.ph
--- John Gibson, Mar 15 1988
        Moved out of sysio.p
 */
