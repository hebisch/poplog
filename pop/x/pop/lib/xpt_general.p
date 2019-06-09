/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xpt_general.p
 > Purpose:         General Xpt shadowclasses
 > Author:          John Gibson, Nov  3 1991 (see revisions)
 > Documentation:   REF *XTOOKIT
 */
compile_mode:pop11 +strict;

section;

include xpt_constants.ph;
include xpt_generaltypes.ph;

shadowclass XptStringList #_< [props ^XDT_STRINGLIST] >_#
    :XptString[];

shadowclass XptArgPtr {:XptArg};
shadowclass XptArgList #_< [props ^XDT_ARGLIST] >_#     [nc, prefix nc_]
    :XptArg[];

shadowclass XptOptionDescPtr {:XptOptionDescRec};
shadowclass XptOptionDescList #_< [props ^XDT_OPTIONDESCLIST] >_#
    :XptOptionDescRec[];


shadowclass XptWidgetList #_< [props ^XDT_WIDGETLIST] >_#
    :XptWidget[];

shadowclass XptCardinalPtr #_< [props ^XDT_CARDINALPTR] >_# {
    XptCPValue  :uint,
};

shadowclass XptTimePtr #_< [props ^XDT_TIMEPTR] >_# {
    XptTPValue  :uint,
};

shadowclass XptIntPtr #_< [props ^XDT_INTPTR] >_# {XptIPValue :int};

shadowclass XptXrmValuePtr #_< [props ^XDT_VALUEPTR] >_# {:XptXrmValue};
shadowclass XptXrmValueList #_< [props ^XDT_VALUELIST] >_# :XptXrmValue[];

;;; So uses works OK
constant xpt_general = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jul  8 1993
        Added XptXrmValuePtr|List shadowclasses from LIB * XT_CONVERTER
--- Adrian Howard, Dec  2 1991 : Added XptIntPtr shadowclass
--- Adrian Howard, Nov  4 1991 : Library renamed xpt_general.p
 */
