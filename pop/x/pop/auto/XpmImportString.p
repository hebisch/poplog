/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XpmImportString.p
 > Purpose:         Support for Motif strings
 > Author:          Jonathan Meyer, Feb  8 1991 (see revisions)
 > Documentation:   HELP * MOTIF
 */
compile_mode :pop11 +strict;

section;
exload_batch;

include xpt_coretypes.ph;
uses fast_xt_util;


;;; These are duplicated from XmConstants.ph -- NOTE that this file can't
;;; include that, since it uses this file.

l_typespec
    XmStringDirection: byte,
    XmStringCharSet :XptString;

lconstant macro XmFONTLIST_DEFAULT_TAG = 'FONTLIST_DEFAULT_TAG_STRING';


/************************************************************************
 *  Support for Motif Strings
 ************************************************************************/

;;; coerce external pointer into motif string
define XpmImportString =
    XptImportAny(%"XmString"%);
enddefine;

l_typespec XmString :exptr#XpmImportString;

XptLoadProcedures XpmImportString [^^XM_EXLIBS]
lvars
    ;;; string creation
    XmStringCreate(text, tag) : XmString,
    XmStringCreateLocalized(text) : XmString,
    XmStringCreateLtoR(text, tag) : XmString,
    XmStringCreateSimple(text) : XmString,
    XmStringSegmentCreate(text, tag, direction, separator) : XmString,
    ;;; string coercion
    XmStringInitContext(context, string) : XptBoolean,
    XmStringGetNextSegment(context, text, tag, direction, separator) : XptBoolean,
    XmStringFreeContext(context) : void,
    XmStringFree(string) : void,
;

define XpmCheckString =
    XptLiveTypeCheck(%"XmString"%)
enddefine;


;;; Pop versions of XmString procedures

define XmStringCreate(text, tag);
    if isstring(text) then sys_encode_string(text) -> text endif;
    exacc[fast] raw_XmStringCreate(text, tag);
enddefine;

define XmStringCreateLocalized(text);
    if isstring(text) then sys_encode_string(text) -> text endif;
    exacc[fast] raw_XmStringCreateLocalized(text);
enddefine;

define XmStringCreateLtoR(text, tag);
    if isstring(text) then sys_encode_string(text) -> text endif;
    exacc[fast] raw_XmStringCreateLtoR(text, tag);
enddefine;

define XmStringCreateSimple(text);
    if isstring(text) then sys_encode_string(text) -> text endif;
    exacc[fast] raw_XmStringCreateSimple(text);
enddefine;

define XmStringSegmentCreate(text, tag, direction, separator);
    if isstring(text) then sys_encode_string(text) -> text endif;
    exacc[fast] raw_XmStringSegmentCreate(text, tag, direction, separator);
enddefine;

define XmStringFree(string);
    exacc[fast] raw_XmStringFree(string);
    if XptDescriptor(string, "XmString") ->> string then
        ;;; the descriptor is no longer valid for this address so we
        ;;; must dispose of it; XpmImportString should have registered a
        ;;; destroy action to do that
        lvars destroy_p = sys_destroy_action(string);
        if destroy_p then
            false -> sys_destroy_action(string);
            destroy_p(string);
        endif;
    endif;
enddefine;

define XmStringInitContext(context, string);
    exacc[fast] raw_XmStringInitContext(context, string);
enddefine;

define XmStringFreeContext(context);
    exacc[fast] raw_XmStringFreeContext(context);
enddefine;

define XmStringGetNextSegment(context, text, tag, direction, separator);
    exacc[fast] raw_XmStringGetNextSegment(context, text, tag, direction,
        separator);
enddefine;


;;; coerce motif string -> popstring
;;; in order to be re-entrant, we don't use lconstant ptrs, but get fresh
;;; ones for each call. We cache those pointers in a dynamic list, so
;;; as to not generate too much garbage.
lvars fixed_ptrs = pdtolist(exptr_copy_fixed(%null_external_ptr%));

define XpmCoerceString(xmstring) -> string;

    returnif(xmstring.isstring)(xmstring -> string);
    ;;; Should check, but may be dangerous for existing code...
    ;;; XpmCheckString(xmstring) -> ;

    ;;; get three pointers from the free list
    lvars (context_ptr, text_ptr, separator_ptr)
            = (repeat 3 times dest(fixed_ptrs) -> fixed_ptrs endrepeat);

    ;;; we never get these parameters, so lconstant ones are ok.
    lconstant
        charset_ptr     = EXPTRINITSTR(:XmStringCharSet),
        direction_ptr   = EXPTRINITSTR(:XmStringDirection),
    ;

    if exacc[fast] raw_XmStringInitContext(context_ptr, xmstring) then
        lvars context = external_ptr_props(context_ptr);
        lvars text = external_ptr_props(text_ptr);
        lvars last_len = -1, total_len = (#|
            while exacc[fast] raw_XmStringGetNextSegment(context, text_ptr,
                    charset_ptr, direction_ptr, separator_ptr)
            do
                ;;; exacc_ntstring will apply any decoding required for
                ;;; the locale. This takes it for granted that the
                ;;; charset is the locale charset: we should check for
                ;;; that really...
                deststring(exacc_ntstring(text) ->> string) -> last_len;
                if exacc[fast] :XptBoolean separator_ptr then `\n` endif;
                fast_XtFree(text);
            endwhile;
        |#);
        if total_len == last_len then
            erasenum(total_len);
        else
            consstring(total_len) -> string;
        endif;
        exacc[fast] raw_XmStringFreeContext(context);
    else
        false -> string;
    endif;

    text_ptr, context_ptr, separator_ptr,
    repeat 3 times conspair(fixed_ptrs) -> fixed_ptrs endrepeat;
enddefine;


define lconstant Pop_to_motif_string(string) with_props XpmImportString;
    if string.isstring then
        ;;; automatic Xm string creation for normal Pop-11 strings
        sys_encode_string(string) -> string;
        exacc[fast] raw_XmStringCreateLtoR(string, XmFONTLIST_DEFAULT_TAG)
    else
        ;;; check for XmString
        string -> XptImportAny("XmString");
    endif;
enddefine;
;;;
Pop_to_motif_string -> updater(XpmCoerceString);
Pop_to_motif_string -> updater(XpmImportString);

;;; XpmCopiedString typespec - this typespec will (1) free the XmString it gets
;;; after coercing it into a pop string and (2) free the XmString it creates
;;; from a pop string when that pop string becomes garbage. The typespec is
;;; used for XmNlabelString resources, since these always copy the xmstring
;;; that the application passes in/gets out.

define XpmCoerceCopiedString(xmstring) -> string;
    if xmstring.isstring then
        xmstring -> string;
    else
        if XpmCoerceString(xmstring) ->> string then
            XmStringFree(xmstring);
        endif;
    endif;
enddefine;
;;;
define updaterof XpmCoerceCopiedString(string) -> xmstring;
    define destroy_action =
        clearproperty(dup(copy(sys_destroy_action)));
    enddefine;
    define free(_, xmstring) with_props false with_nargs 2;
        ;;; check it's not been freed already
        unless is_null_external_ptr(xmstring) then
            exacc[fast] raw_XmStringFree(xmstring);
        endunless;
    enddefine;
    if destroy_action(string) ->> xmstring then
        ;;; seen this one before: we can't have more than one destroy
        ;;; action, and it saves creating another XmString anyway
        frozval(1, xmstring) -> xmstring;
    else
        Pop_to_motif_string(string) -> xmstring;
        ;;; when the string becomes garbage, free the XmString
        free(%xmstring%) -> destroy_action(string);
    endif;
enddefine;

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Nov  7 1997
        Fixed XmStringFree to kill any descriptor associated with the
        string: once the string has been freed the descriptor is no longer
        valid because the address could be reused for something else
--- Robert Duncan, Mar 12 1997
        Changed to do encoding and decoding of strings
--- Robert John Duncan, Jun 22 1995
        Fix for updater of XpmCoerceCopiedString for the case where it's
        applied more than once to the same string
--- John Gibson, Apr 14 1993
        Code part of XmConstants to this file, macros and typespecs to
        XmConstants.ph
--- Robert John Duncan, Jul  6 1992
        Changed forward declaration of -XmStringInitContext- to "vars"
        to match the default declaration in "MotifWidgetSet.p"
--- Adrian Howard, Jun 23 1992
        JonM's fixes fixed so they would compile :-)
--- Adrian Howard, Jun 23 1992
        JonM's fixes for XpmCoerceString and XpmCoerceCopiedString installed
--- Jonathan Meyer, Jun  16 1992
    Added XpmCoerceCopiedString. Fixed bug in XpmCoerceString so that it
    frees the C ascii string after use.
--- Adrian Howard, Nov  1 1991 :
        o typespec:XmAnyCallbackStruct renamed typespec:XpmAnyCallbackStruct,
          fields in typespec renamed as follows:
            reason --> XpmACSReason
            event  --> XpmACSEvent
        o Added padding field to typespec:XpmAnyCallbackStruct
        o XpmAnyCallbackStruct shadowclass renamed XpmAnyCallbackStructPtr
        o Removed XpmACSPad field from XpmAnyCallbackStructPtr
--- Jonathan Meyer, Jul  3 1991
        Moved XpmCoerceString into this file. Added XptLoadClassProc call.
        Defined XmConstants.
--- Jonathan Meyer, May 25 1991
        Added XpmImportString to typespec for XmStrings
--- Robert John Duncan, Mar 21 1991
        Added missing value field to FileSelectionBoxCallbackStruct
--- Jonathan Meyer, Feb 15 1991 Fixed shadowclass structures to follow
        Xpt naming conventions
 */
