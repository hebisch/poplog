/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptColorConvertArgs.p
 > Purpose:         Pop11 version of colorConvertArgs in C
 > Author:          Adrian Howard, Sep 23 1991 (see revisions)
 > Documentation:   REF *XT_CONVERTER
 > Related Files:
 */
compile_mode:pop11 +strict;

uses xt_converter;

include xt_constants.ph;
include xpt_xtypes.ph;
include xpt_widgettypes.ph;

section;

lvars cache = false;

define active XptColorConvertArgs;
    cache or (
    consXptConvertArgList(#|
        consXptConvertArgPtr(
            XtWidgetBaseOffset,
            #_< FIELDOFFSET(:XptCoreWidget,core)+FIELDOFFSET(:XptCorePart,screen) >_#,
            SIZEOFTYPE(:XptScreenPtr)
        ),
        consXptConvertArgPtr(
            XtWidgetBaseOffset,
            #_< FIELDOFFSET(:XptCoreWidget,core)+FIELDOFFSET(:XptCorePart,colormap) >_#,
            SIZEOFTYPE(:XptColormap)
        )
    |#) ->> cache)
enddefine;
;;; is a constant, no updater

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 22 1993
        Made an active constant (because POPC can't support compile-time
        construction of shadowclass instances, unfortunately).
--- John Gibson, Nov  3 1991
        includes xpt_widgettypes etc rather than uses
--- Adrian Howard, Oct 31 1991 : Changed to use -XptConvertArgPtr-.
 */
