/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/include/xpt_generaltypes.ph
 > Purpose:         Type specifications for general Poplog Xt types
 > Author:          John Gibson, Nov  3 1991 (see revisions)
 > Documentation:   REF *XTOOLKIT
 */

#_TERMIN_IF DEF XPT_GENERALTYPES_INCLUDED

section;

include xpt_coretypes.ph;

i_typespec

    /* arg list */
    XptArg  {
        XptAName    :XptString,
        XptAValue   :XptPointer,
    },

    XptActionsRec {
        XptARString :XptString,
        XptARProc   :XptProcedure
    },

    XptCallbackRec {
        XptCRCallback   :XptProcedure,
        XptCRClosure    :XptPointer
    },

    XptConvertArgRec {
        XptCVRAddressMode   :XptEnum,
        XptCVRAddressId     :XptPointer,
        XptCVRSize          :XptCardinal,
    },

    /* resource */
    XptResource {
        XptRName    :XptString,
        XptRClass   :XptString,
        XptRType    :XptString,
        XptRsize    :XptCardinal,
        XptRoffset  :XptCardinal,
        XptRDefType :XptString,
        XptRDefAddr :XptPointer,
    },

    /* option desc list */
    XptOptionDescRec {
        XptODROption        :XptString,
        XptODRSpecifier     :XptString,
        XptODROptionKind    :XptEnum,
        XptODRValue         :XptPointer,
    },

    XptXrmValue {
        XptXVSize   :uint,
        XptXVAddr   :XptPointer,
    },

    XptSubstitutionRec {
        XptSRChar   :byte,
        XptSRSubst  :XptString,
    },

    XptCacheRef     :exptr#XptImportCacheRef,

    XptTranslations :exptr,
    XptAccelerators :exptr,

    XptModifiers    :uint

;

iconstant XPT_GENERALTYPES_INCLUDED = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jan 13 1992 : Added XptModifiers
 */
