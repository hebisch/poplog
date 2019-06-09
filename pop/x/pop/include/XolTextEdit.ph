/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/include/XolTextEdit.ph
 > Purpose:         TextEditWidget definitions
 > Author:          Jonathan Meyer, Sep  2 1990 (see revisions)
 > Documentation:   HELP *OpenLook
 */

#_TERMIN_IF DEF XOLTEXTEDIT_INCLUDED

include XolDynamic.ph;

section;

i_typespec
    ShortArr: short[],
    TabTable: exptr.:ShortArr,

    OlTextMotionCallData {
        ok: ShortBool,
        current_cursor: int,
        new_cursor: int,
        select_start: int,
        select_end: int,
    },
    OlTextMotionCallDataPointer: exptr.:OlTextMotionCallData,


    OlTextModifyCallData {
        ok: ShortBool,
        current_cursor: int,
        select_start: int,
        select_end: int,
        new_cursor: int,
        new_select_start: int,
        new_select_end: int,
        text: ntstring_ptr,
        text_length: int,
    },
    OlTextModifyCallDataPointer: exptr.:OlTextModifyCallData,

    OlTextPostModifyCallData {
        requestor: ShortBool,
        new_cursor: int,
        new_select_start: int,
        new_select_end: int,
        inserter: ntstring_ptr,
        deleted: ntstring_ptr,
        delete_start: int,
        delete_end: int,
        insert_start: int,
        insert_end: int,
        cursor_position: int,
    },
    OlTextPostModifyCallDataPointer: exptr.:OlTextPostModifyCallData,

    OlTextMarginCallData {
        hint: int,
        rect: exptr,
    },
    OlTextMarginCallDataPointer: exptr.:OlTextMarginCallData,
;

iconstant macro (
    OL_MARGIN_EXPOSED =0, OL_MARGIN_CALCULATED =1,  ;;; OlTextMarginHint
    OL_TEXT_EDIT = 66, OL_TEXT_READ = 67,           ;;; OlEditMode
    OL_WRAP_OFF=74, OL_WRAP_ANY=75,
    OL_WRAP_WHITE_SPACE=76,                         ;;; OlWrapMode
    OL_DISK_SOURCE=15, OL_STRING_SOURCE=64,
    OL_TEXT_BUFFER_SOURCE=99,                       ;;; OlSourceType
);

iconstant XOLTEXTEDIT_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  7 1993
        Made an include file
--- Andreas Schoter, Jul 15 1991
    Added global constant XolTextEdit for compatibility with uses
 */
