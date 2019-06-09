/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptImportAny.p
 > Purpose:         Imports arbitrary external ptr as descriptor
 > Author:          Jonathan Meyer, Dec  5 1990, v13.91 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

/*  This library is used to turn an external pointer into a descriptor or
    preferred representation of the given type. If a descriptor of that type
    already exists its preferred repn is returned, otherwise a new descriptor
    is created. The updater just does a type check. The convention that
    <NULL> imports as <false> is also observed.

    The library is intended for use by "Import" procedures for datatypes
    not defined in core Poplog eg. XptImportWindow.
*/

define XptImportAny(exptr, type);
    lvars exptr type desc;
    if isexternal_ptr_class(exptr) and not(is_null_external_ptr(exptr)) then
        if XptDescriptor(exptr,type) ->> desc then
            XptRegister(desc);
        else
            consXptDescriptor(exptr, type);
        endif;
    else
        false
    endif;
enddefine;

define updaterof XptImportAny(item, type);
    lvars item, type;
    if item then
        if type and XptDataType(item) /== type then
            mishap(item,1,type sys_>< ' NEEDED');
        endif;
        item;
    else
        null_external_ptr;
    endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jul  6 1991
        Changed to use null_external_ptr
        Changed to use is_null_external_ptr
--- Jon Meyer, Jul  4 1991
    Changed from using is_valid_external_ptr to an exacc ^int so that
    window ids get accepted correctly
--- Roger Evans, Feb  3 1991 renamed (formerly XptGetDescriptor) and revised
 */
