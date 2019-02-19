/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/src/xt_support.p
 > Purpose:         Xpop routines that need to be in safepop11
 > Author:          John Gibson, Apr 13 1993
 > Documentation:   REF * XTOOLKIT
 > Related Files:   x/src/xt_*.p
 */

    /*  This file contains those parts of the xpop stuff that need to be
     *  in the safepop11 image used by POPC. Nothing in here must actually
     *  refer to real X routines.
     */

#_INCLUDE 'declare.ph'

constant procedure external_ptr_props;


;;; --------------------------------------------------------------------


section $-Sys =>    fast_XptDataType, XptDataType,
                    fast_XptDataProps, XptDataProps;

;;; Routines supporting the weak data typing used by Poplog X interface

define fast_XptDataType(desc);
    lvars desc;
    desc!XP_PROPS -> desc;
    if ispair(desc) then desc!P_FRONT else desc endif;
enddefine;

;;; getting the type from the PROPS field - checks it has one!
define XptDataType(desc);
    lvars desc;
    external_ptr_props(desc) -> ; ;;; check for props field
    fast_XptDataType(desc);
enddefine;

define fast_XptDataProps(desc);
    lvars desc;
    desc!XP_PROPS -> desc;
    if ispair(desc) then desc!P_BACK else false endif;
enddefine;

define updaterof fast_XptDataProps(v,desc);
    lvars v desc props;
    desc!XP_PROPS -> props;
    if ispair(props) then
        if v then
            v -> props!P_BACK;
        else
            props!P_FRONT -> desc!XP_PROPS;
        endif;
    else
        if v then
            conspair(props,v) -> desc!XP_PROPS;
        endif;
    endif;
enddefine;

define XptDataProps(desc);
    lvars desc;
    external_ptr_props(desc) -> ; ;;; check for props field
    fast_XptDataProps(desc);
enddefine;

define updaterof XptDataProps(desc);
    lvars desc;
    external_ptr_props(desc) -> ; ;;; check for props field
    -> fast_XptDataProps(desc);
enddefine;

endsection;
