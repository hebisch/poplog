/*  --- Copyright University of Sussex 1996.  All rights reserved. ---------
 >  File:           C.x/x/ved/src/xvedkey.p
 >  Purpose:        Multi Window VED - keyboard input event handler.
 >  Author:         Jonathan Meyer, 20 July 1990 (see revisions)
 >  Documentation:  REF *XVED
 >  Related Files:
 */
compile_mode :pop11 +strict;

include xved_constants.ph;
include xpt_coretypes.ph;
include x_keysyms.ph;

section $-xved;


/**************************************************************************
 * Xved Keyboard Event Management
 **************************************************************************/


define lconstant send_seq(w, keystr);
    lvars n, m, c, w, keystr;
    if isstring(keystr) then
        if keystr /= nullstring then
            chain(w, keystr, xved_raise_ascii)
        endif
    else
        ;;; keystr is an exptr to a non-empty nt string
        ;;; stack the chars to avoid creating garbage
        exacc[fast] :byte[] keystr[1];
        1 -> n; 2 -> m;
        while (exacc[fast] :byte[] keystr[m] ->> c) /== 0 do
            c, m -> n;
            m fi_+ 1 -> m
        endwhile;
        chain(n, w, false, xved_raise_ascii)    ;;; n chars on stack
    endif;
enddefine;

define xved_key_callback(w, client, keysym);
    lvars w, client, keysym, keystr, synth, key, modifiers;

    ;;; get an exptr to XtN key or false in keystr
    XptVal[fast,nc] w(XtN key:exptr, XtN synthetic:XptBoolean)
                                -> (keystr, synth);
    if exacc[fast] :byte keystr == 0 then false -> keystr endif;

    ;;; Test for "string" action first (may result from any event, not
    ;;; necessarily keys at all).
    if synth then
        if keystr then chain(w, keystr, send_seq) endif;
        return
    endif;

    ;;; Is a key event
    returnif(keysym == 0);
    XptVal[fast] w(XtN modifiers:int) -> modifiers;

    ;;; NEXT - entries in keyseq tables will alter the
    ;;; behaviour of the callback.

    if ((w.xvedwin_keyseqvec ->> key) and (key(keysym) ->> key))
    or (xvedkeyseqtable(keysym) ->> key) then
        if key.isprocedure then fast_apply(keysym, modifiers, key) -> key endif;
        returnif(key /== "undef")(send_seq(w, key));
    endif;

    ;;; DEFAULT - process the key and see if we can make some sense from it.

    ;;; ignore modifier keys and KeyRelease events.
;;;        printf(keysym, 'keysym = %p\n') ;
    returnif((keysym fi_< 0) or (536870911 < keysym) or IsModifierKey(keysym));

    ;;; test for keypad state
    if xvedkeypadon then
        if IsKeypadKey(keysym) then false -> keystr endif
    else
        ;;; translate function keys into numbers
        if vedkeymapname = 'sunxved' and key fi_>= XK_R7 and key fi_<= XK_R15
        then
            substring(key fi_- XK_R6, 1, '789456123') -> keystr
        endif;
    endif;

    ;;; If the meta key is down, send some escape sequence for it.
    if modifiers && XVM_METAMASK /== 0 then
        send_seq(w, xvedstartmetaseq);
    endif;

    send_seq(w, if keystr then
                    ;;; A key with a defined translation
                    keystr
                else
                    ;;; Send a keysym escape sequence
                    xved_keysym_seq(keysym)
                endif)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 17 1996
        Made xved_key_callback return if keysym is zero.
--- John Gibson, Feb 23 1994
        3rd arg to xved_key_callback now the integer keysym directly, not
        an exptr
--- John Gibson, Jan 15 1994
        Rewritten so as not to create a string from XtN key -- stacks the
        chars instead (which xved_raise_ascii can now accept), and thus
        doesn't make garbage.
--- John Gibson, Sep  7 1992
        Changed to use XptVal
--- John Gibson, Aug  4 1992
        Stopped xved_key_callback getting XtNkey string unnecessarily for
        modifier keys and release events
--- Adrian Howard, Aug 12 1991 : xkeysyms.ph --> x_keysyms.ph
--- Jonathan Meyer, Aug  1 1991
        xpt_xkeysym.ph renamed xkeysyms.ph
--- John Gibson, Jul 12 1991
        Moved interrupt test from send_seq to xved_raise_ascii
--- Jonathan Meyer, Jul  8 1991
        Added test for veddointerrupt - removed xvedinterruptstring
--- John Gibson, Jun 18 1991
        Promoted "string" actions in xved_key_callback (again)
--- Jonathan Meyer, Jun 17 1991
        Rewrote logic behind all of key sending (final time, I hope).
        Now keyseq tables have priority over other tables. Also, moved
        check for interrupt key into send_seq - will cope with users
        rebinding other keys to ctrl c.
--- John Gibson, Jun 10 1991
        Changed -xved_key_callback- to deal properly with "string"
        translations
--- Jonathan Meyer, Jun  4 1991
        Changed to use XtN synthetic to test for whether a string should
        be passed as is. Fixed xvedkeypadon to work on suns correctly
--- Jonathan Meyer, Jun  3 1991
        Added xvedwin_keyvec check
--- Jonathan Meyer, Jun  2 1991
        Added conditional version of IsKeypadKey to force keypads send
        escape sequences if -xvedkeypadon- is true. If xvedkeypadon is false,
        keypads will send numeric sequences, or sequences defined in a
        translation specifications. Note that translation specifications of
        things other than numbers and +-*=/. will be sent as is
        regardless of -xvedkeypadon-.
--- John Gibson, Jun  1 1991
        Commented out call of -IsKeypadKey-
 */
