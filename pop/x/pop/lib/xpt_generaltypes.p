/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xpt_generaltypes.p
 > Purpose:         Type specifications for general Poplog X types
 > Author:          Roger Evans, Nov 16 1990 (see revisions)
 > Documentation:   REF *xtoolkit
 > Related Files:
 */
compile_mode:pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

loadinclude xpt_coretypes.ph;
loadinclude xpt_generaltypes.ph;
uses xpt_general.p;

constant xpt_generaltypes = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Nov  4 1991 : xt_general --> xpt_general
--- John Gibson, Nov  3 1991
        typespecs moved to xpt_generaltypes.ph; shadowclasses moved to
        xt_general.p
--- Adrian Howard, Oct 31 1991 :
    o -XptArg- & -XptOptionDescRec- changed to typespec
    o -XptArg- & -XptOptionDescRec- shadowclass renamed to -XptArgPtr- &
      -XptOptionDescPtr-
--- Roger Evans, Feb 10 1991 changed XptPopString to XptString
        and XptPopObj to XptPointer
--- Roger Evans, Jan 26 1991 added noncontrtsuctive arglist attributes
--- Roger Evans, Jan 26 1991 changed to new shadowclass props format
--- Roger Evans, Nov 19 1990 added XptTimePtr
--- Roger Evans, Nov 18 1990 added Arglist, OptionDescList, CardinalPtr,
        WidgetList
 */
