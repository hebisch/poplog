/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/pop/include/xpt_coretypes.ph
 > Purpose:         typespec definitions for Xpt core types
 > Author:          John Gibson, Nov  1 1991 (see revisions)
 > Documentation:   REF *XTOOLKIT
 */

#_TERMIN_IF DEF XPT_CORETYPES_INCLUDED

section;


/* -- XptPopObj & XptPointer ----------------------------------- */

/*  These are just "exval" fields with the exval_to_popobj converter
    on top. Accessing the type will return an external ptr; updating it
    will convert as for an external procedure arg, except that fixed-address
    copies are made for anything that needs it (and cached against the
    original).
*/

i_typespec
    XptPopObj           :exval#exval_to_popobj,
    XptPointer          :XptPopObj,
;

/* -- XptString ------------------------------------- */

/*  Typespecs for strings as fields in structs. exval_to_string
    produces a string or false on accessing, and takes the same for
    updating (caching fixed-address copies of strings if necessary).
    exval_to_bytestring does the same with no character encoding/decoding.
*/

i_typespec
    XptString           :exval#exval_to_string,
    XptByteString       :exval#exval_to_bytestring,
    XptPopString        :XptString,
;


/* -- XptProcedure ----------------------------------- */

/*  a typespec for c-callable procedure. The updater of XptImportProcedure
    allows both external pointers and pop exfunc_closures (hence the base
    field is "exval" rather than "exptr").
*/

i_typespec
    XptProcedure        :exval#XptImportProcedure;


/* -- some more straightforward typespecs ------------ */

i_typespec
    XptAppContext       :exptr#XptImportApplicationContext,
    XptBoolean          :byte#XptCoerceBoolean,
    XptDisplayPtr       :exptr#XptImportDisplayPtr,
    XptInputId          :exptr#XptImportInputId,
    XptIntervalId       :exptr#XptImportIntervalId,
    XptWidget           :exptr#XptImportWidget,
    XptObject           :exptr,
    XptWidgetClass      :exptr#XptImportWidgetClass,


    XptEnum             :uint,      ;;; Warning: not the same as XtEnum

    XptCardinal         :uint,
    XptDimension        :ushort,
    XptPosition         :short,
    XptInt              :int,
    XptShort            :short,
    XptFloat            :sfloat,
    XptLongBoolean      :uint#XptCoerceBoolean,
    XptUnsignedChar     :byte,
    XptChar             :sbyte,

    XptFile             :exptr,
;

iconstant XPT_CORETYPES_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 13 1997
        Added XptByteString.
--- John Gibson, Mar 31 1995
        Corrected XptCardinal to :uint
--- John Gibson, Sep  4 1992
        Changed to use new "exval" field type. All types are now
        base field type + conversion procedures
 */
