/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/lisp/flib/fbuffer.p
 > Purpose:         Utilities for LIB * FORMAT_PRINT
 > Author:          John Williams, Nov  2 1994 (see revisions)
 > Documentation:
 > Related Files:   LIB * FORMAT_PRINT
 */


section $-lisp$-fpr;

/* Utilities for determining the printed length of an item, and
    saving the printed representation in a string.

    Cannot simply set cucharout to identfn and count the stack length
    as this would confuse Lisp.
*/


global vars
    fbuffer         =   inits(128),
    fbuffer_i       =   0,
    fbuffer_len     =   128,
    ;

define lconstant Fbuffer_charout(c);
    lvars b;
    if c fi_> 255 then
        ;;; assume 16-bit character
        unless isstring16(fbuffer) then
            inits16(fbuffer_len) -> b;
            lvars n = fi_min(fbuffer_i, fbuffer_len);
            substring(1, n, fbuffer) -> substring(1, n, b);
            b -> fbuffer;
        endunless;
    endif;
    if fbuffer_i fi_>= fbuffer_len then
        class_init(datakey(fbuffer))(fbuffer_i fi_+ 128) -> b;
        fbuffer -> substring(1, fbuffer_len, b);
        b -> fbuffer;
        datalength(fbuffer) -> fbuffer_len
    endif;
    c -> fast_subscrs(fbuffer_i fi_+ 1 ->> fbuffer_i, fbuffer)
enddefine;

define fbuffer_charout(c);
    if isinteger(c) then
        Fbuffer_charout(c);
    else
        chain(c, fbuffer_charout, appdata);
    endif;
enddefine;

define fprint_to_string(item);
    lvars i = fbuffer_i;
    dlocal fbuffer_i, cucharout = Fbuffer_charout;
    pr(item);
    substring(i fi_+ 1, fbuffer_i fi_- i, fbuffer);     /* printed form */
    fbuffer_i fi_- i                                    /* length */
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Apr 10 1997
        Changed to support 16-bit characters
--- John Williams, Aug 23 1995
        Removed redundant lvar declarations.
 */
