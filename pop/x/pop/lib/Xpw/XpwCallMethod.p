/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xpw/XpwCallMethod.p
 > Purpose:         Call Xpw method
 > Author:          John Gibson, Apr  2 1993
 > Documentation:   REF * XpwCallMethod
 */
compile_mode :pop11 +strict;

section;

include xpt_xtypes.ph;

XptLoadProcedures XpwCallMethod [^^XPW_EXLIBS]
lvars
    XpwCallMethod
;

define XpwCallMethod(res_type);
    lvars res_type, res;
    if res_type then
        if res_type == "XptPixel" then
            ;;; - integer result (-1 = error)
            exacc [fast] (...):int raw_XpwCallMethod() -> res;
            res /== -1 and res;
        elseif res_type == "XptXID" then
            ;;; - integer or -false- if res is 0
            exacc [fast] (...):XptXID raw_XpwCallMethod() -> res;
            res /== 0 and res;
        elseif res_type == "exptr" then
            ;;; exptr or -false- if exptr is NULL
            exacc [fast] (...):exptr raw_XpwCallMethod() -> res;
            not(is_null_external_ptr(res)) and res;
        elseif res_type == "XptInt" then
            ;;; - integer
            exacc [fast] (...):XptInt raw_XpwCallMethod()
        else
            ;;; untouched exptr (for compatiblity)
            exacc [fast] (...):exptr raw_XpwCallMethod();
        endif
    else
        ;;; no result
        exacc [fast] (...):void raw_XpwCallMethod();
    endif;
enddefine;

endsection;
