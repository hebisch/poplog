/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/ved/src/xvedkeysymseq.p
 > Purpose:         Converts X Keysyms <-> ved escape sequences
 > Author:          Jonathan Meyer, Apr  6 1991 (see revisions)
 > Documentation:
 > Related Files:
 */

#_INCLUDE 'xved_declare.ph'
include xved_constants.ph;

uses wved_vedset_key_chars;

section $-xved => xved_server_vendor_string;

/*
Code to convert between X Keysyms and ved escape sequences, and visa-versa
*/


exload 'xvedkeysymseq' (batching)
lvars
        XStringToKeysym(1):uint,
        XKeysymToString(1):exptr#exacc_ntstring,
        XServerVendor(1):exptr#exacc_ntstring,
    ;
endexload;

lconstant BASE_LEN = length(XVK_BASE_SEQ);

define lconstant hexstring(i);
    lvars i, j;
    lconstant str = writeable inits(4);
    if i > 16:FFFF then
        procedure;
            dlocal pop_pr_radix = 16, pr = syspr, pop_pr_quotes = false;
            i >< nullstring
        endprocedure();
    else
        fast_for j from 4 by -1 to 1 do
            if dup((i fi_&& 16:F) fi_+ 48) fi_> `9 then fi_+ 7 endif;
                -> fast_subscrs(j, str);
            i fi_>> 4 -> i;
        endfast_for;
        str;
    endif;
enddefine;

define xved_is_keysym_seq(str);
    lvars str;
    str.isstring and issubstring(XVK_BASE_SEQ, str) == 1
enddefine;

define xved_keysym_seq(num);
    lvars num;
    if num.isstring or num.isword then
        exacc XStringToKeysym(num sys_>< '\(0)') -> num;
    endif;
    returnif(num == 0)(false);
    XVK_BASE_SEQ sys_>< hexstring(num);
enddefine;

define global constant xved_keysym_num(key);
    lvars key;
    unless key.isstring or key.isword then
        mishap(key,1,'STRING NEEDED');
    endunless;
    if xved_is_keysym_seq(key) then
        strnumber('16:' sys_>< allbutfirst(BASE_LEN, key))
    else
        exacc XStringToKeysym(key sys_>< '\(0)') -> key;
        key /== 0 and key;
    endif;
enddefine;

define xved_keysym_name(key);
    lvars key;
    if (key.isstring or key.isword) and xved_is_keysym_seq(key) then
        strnumber('16:' sys_>< allbutfirst(BASE_LEN, key)) -> key;
    elseunless key.isinteger then
        mishap(0,'STRING or INTEGER NEEDED');
    endif;
    exacc XKeysymToString(key) -> key;
    key /== termin and key;
enddefine;

;;; find vendor string for display
lvars vendor = false;

define xved_server_vendor_string;
    returnunless(xveddisplay)(false);
    unless vendor then
        exacc XServerVendor(xveddisplay)->vendor;
    endunless;
    vendor;
enddefine;

;;; Called (at run-time) by vedset keys for <expr> = ( <code> )
define :XVED_FOR wved_vedset_key_chars(code);
    lvars code, tmp;
    unless vedusewindows == "x" then
        mishap('(' <> code <> ')', 1,
                'vedset keys: SEQUENCE (...) IS NOT DEFINED FOR THIS TERMINAL')
    endunless;

    if code = 'Meta' then
        return(explode(xvedstartmetaseq))
    elseif isstartstring('keysym:', code) then
        ;;; allow keysym number
        strnumber('16:' sys_>< lowertoupper(allbutfirst(7, code))) -> code;
    endif;
    unless xved_keysym_seq(code) ->> tmp then
        mishap(code, 1, 'vedset: UNKNOWN KEYSYM NAME')
    endunless;
    explode(tmp)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 11 1995
        Fixed bug in hexstring -- for number > 16:FFFF was using sys_><
        instead of >< (which just reset pop_pr_radix to 10)
--- John Gibson, Nov 15 1992
        Added uses wved_vedset_key_chars;
--- John Gibson, Feb 27 1992
        Added -wved_vedset_key_chars-
--- Integral Solutions Ltd, Oct 22 1991 (Julian Clinton)
    Changed >< to sys_><.
--- John Gibson, Aug 10 1991
        Made lconstant -str- writeable
--- Robert John Duncan, Jun  5 1991 Made xved_keysym_seq table (big) integers
 */
