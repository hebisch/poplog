/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/include/XolScroll.ph
 > Purpose:         Xol defs for scrollbar/window etc
 > Author:          John Gibson, Apr  8 1993
 > Documentation:
 */

#_TERMIN_IF DEF XOLSCROLL_INCLUDED

include xpt_coretypes.ph;
include XolConstants.ph;

section;

/*
 *
 * Scrollbar Widget
 *
 */

i_typespec
    OlScrollbarVerify {
        new_location: int,
        new_page: int,
        ok: ShortBool,
        slidermin: int,
        slidermax: int,
        delta: int,
    };

/*
 * ScrolledWindowWidget
 *
 */

i_typespec
    OlSWGeometries {
        sw: exptr.XptImportWidget,
        vsb: exptr.XptImportWidget,
        hsb: exptr.XptImportWidget,
        bb_border_width: ushort,
        vsb_width: ushort,
        vsb_min_height: ushort,
        hsb_height: ushort,
        hsb_min_width: ushort,
        sw_view_width: ushort,
        sw_view_height: ushort,
        bbc_width: ushort,
        bbc_height: ushort,
        bbc_real_width: ushort,
        bbc_real_height: ushort,
        force_hsb: ShortBool,
        force_vsb: ShortBool,
};


/*
 *
 * Arrow Widget
 *
 */

;;;typedef struct {
;;;  XEvent *event;     /* the event causing the ArrowAction */
;;;  String *params;        /* the TranslationTable params */
;;;  Cardinal num_params;       /* count of params */
;;;} ArrowCallDataRec, *ArrowCallData;

i_typespec
    ntstring_arr : ntstring_ptr[],
    ntstring_arr_ptr: exptr.:ntstring_arr,

    ArrowCallDataRec {
        event: exptr,
        params: ntstring_arr_ptr,
        num_params: int,
    }, ArrowCallData: exptr.:ArrowCallDataRec,
;

/*
 *
 * Scrolling List Widget
 *
 */

iconstant macro (
        OL_B_LIST_ATTR_APPL     = 16:0000FFFF,
        OL_B_LIST_ATTR_CURRENT  = 16:00020000,
        OL_B_LIST_ATTR_SELECTED = 16:00040000,

        OL_LIST_ATTR_APPL       = OL_B_LIST_ATTR_APPL,
        OL_LIST_ATTR_CURRENT    = OL_B_LIST_ATTR_CURRENT,
        OL_LIST_ATTR_SELECTED   = OL_B_LIST_ATTR_SELECTED,
);

i_typespec
    OlListToken: exptr#XptImportOlListToken,
    OlListItem {
        label_type: short,
        label:      XptString,
        glyph:      exptr,
        attr:       ulong,
;;; NAME OF "tag" FIELD CHANGES AND EXTRA FIELD FOR MOUSELESS
;;; SELECTION OF ITEMS AT OLIT 2.5.
#_IF XOL_VERSION < 2005
        tag:        exptr,
#_ELSE
        user_data:  exptr,
        mnemonic:   byte,
#_ENDIF
    },
    OlListTokenArr: exptr[],

    OlListDelete {
        tokens: exptr.:OlListTokenArr,
        num_tokens: ulong,
    },
    applAddItem(4): OlListToken,
    applDeleteItem(2),
    applEditClose(1),
    applEditOpen(3),
    applTouchItem(2),
    applUpdateView(2),
    applViewItem(2),
;

iconstant XOLSCROLL_INCLUDED = true;

endsection;
