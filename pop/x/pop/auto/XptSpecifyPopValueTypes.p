/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptSpecifyPopValueTypes.p
 > Purpose:         Specify XptPopValue typespec information
 > Author:          Jonathan Meyer, Mar  9 1991 (see revisions)
 > Documentation:   REF *XPT_RESOURCE
 > Related Files:   LIB *XptPopValue, LIB *XptResourceInfo
 */
compile_mode :pop11 +strict;

/********************************************************************
 *      SPECIFYING XptPopValue TYPESPECS AND TYPED KEYS
 *
 *          (N.B. This file should contain only the procedure
 *          XptSpecifyPopValueTypes and nothing else -- so it will load
 *          into safepop11)
 *
 ********************************************************************/

section $-Xpt => XptSpecifyPopValueTypes;

uses
    XtN,
    typespec_utils,
    $-Xpt$-ConsAccess,
;

uses-by_name from XptPopValue ($-Xpt$-PopValueTypes);

lconstant
    macro VALTYPE = [sys_current_val("ident $-Xpt$-PopValueTypes")],
;

define XptSpecifyPopValueTypes(list);
    lvars list, item, name;
    for item in list do
        dest(item) -> (name, item);
        unless hd(item) then
            ;;; (4) clear type
            false
        elseif name.iskey then
            ;;; (3) set converter type
            hd(item) -> item;
            unless VALTYPE(item(1)) then
                mishap(name,item,item(1),3,'UNKNOWN REPRESENTATION TYPE');
            endunless;
            {%  item(1),if item(2) then item(2)>>3 else false endif,
                XtNLookup(item(1),'R')
            %}
        elseif name.isword or name.isinteger then
            ;;; (1) and (2): integer type or representation type
            ;;; turn item into typespec (set to proglist and read it)
            procedure(proglist);
                dlocal proglist, pop_autoload = false;
                $-typespec_utils$-read_typespec(false, false)
            endprocedure(item) -> (item, , );
            ;;; build a new type access procedure -- the updater of this
            ;;; just returns the converted field value
            ConsAccess(item)
        else
            mishap(name,1, 'INVALID KEY FIELD ENTRY FOR TABLE');
        endunless -> VALTYPE(name);
    endfor;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug  9 1995
        Moved dlocal setting of pop_autoload false into anon procedure around
        call of read_typespec (was preventing $-Xpt$-PopValueTypes getting
        autoloaded).
--- John Gibson, May 12 1995
        Changed to use $-Xpt$-ConsAccess to produce access procedure.
        Got rid of cache mechanism (didn't work anyway).
        Now in section $-Xpt.
--- John Gibson, Mar 25 1993
        Moved the initialisation of PopValueTypes to XptPopValue.p, so
        that this file contains only code that can be loaded at compile-time
        into safepop11 (thus its use by other files at execute-level will
        work with POPC).
--- Adrian Howard, Nov 18 1991 : includes xpt_coretypes.ph
--- John Gibson, Nov  3 1991
        includes xpt_xtypes.ph
--- Jonathan Meyer, Mar 12 1991 fixed spelling mistake in Accelerator (!)
--- Jonathan Meyer, Mar 11 1991 Changed to use Xpt types instead of "int"
    and "byte".
 */
