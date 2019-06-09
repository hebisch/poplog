/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xpt_typetest.p
 > Purpose:         Recognizers for XptDescriptor types
 > Author:          Jonathan Meyer, Feb 15 1991 (see revisions)
 > Documentation:   REF *XPT_TYPECHECK
 > Related Files:   LIB *XPT_TYPECHECK
 */
compile_mode:pop11 +strict;

uses
    XptIsType,
    XptIsLiveType,
;

constant $-xpt_typetest = true;     ;;; for "uses"


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 13 1993
        Made both XptIsType and XptIsLiveType autoloadable
--- John Williams, Jun 11 1992
        Added declaration of -xpt_typetest- for "uses" (cf BR ianr.28)
--- Jonathan Meyer, Jul  6 1991
        Changed to use is_null_external_ptr
 */
