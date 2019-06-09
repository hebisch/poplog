/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/include/XolTextField.ph
 > Purpose:         TextFieldWidget definitions
 > Author:          Jonathan Meyer, Sep  2 1990 (see revisions)
 > Documentation:   HELP *OpenLook
 */

#_TERMIN_IF DEF XOLTEXTFIELD_INCLUDED

include XolConstants.ph;

section;

iconstant macro (
    OlTextFieldReturn   = 0,
    OlTextFieldPrevious = 1,
    OlTextFieldNext     = 2,
);

i_typespec
    OlTextFieldVerify {
        string: ntstring_ptr,
        ok: ShortBool,
        reason: int,
    },
    OlTextFieldVerifyPointer: exptr.:OlTextFieldVerify,
;

iconstant XOLTEXTFIELD_INCLUDED = true;

endsection;
