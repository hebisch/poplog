/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/auto/SIZEOFTYPE.p
 > Purpose:         Macro to produce field size of <typespec>
 > Author:          John Gibson, Jul 29 1990 (see revisions)
 > Documentation:   REF *DEFSTRUCT
 > Related Files:   LIB *TYPESPEC_UTILS
 */
compile_mode:pop11 +strict;

uses typespec_utils;


section $-typespec_utils => SIZEOFTYPE;

sysunprotect("SIZEOFTYPE");

define global macro SIZEOFTYPE -> n;
    lvars   n, u, spec, unit_spec;
    dlocal  pop_autoload = false;

    need_nextitem([(]) -> ;
    read_typespec(false, false) -> -> -> spec;
    if pop11_try_nextitem(",") then
        ;;; typespec for unit size
        read_typespec(false, false) -> ->
    else
        ;;; default to byte units
        "byte"
    endif -> unit_spec;
    need_nextitem([)]) -> ;
    field_spec_info(unit_spec) -> u -> ;
    unless u then
        mishap(unit_spec, 1, 'UNIT TYPESPEC IS UNSIZED')
    endunless;
    field_spec_info(spec) -> n -> ;
    unless n then
        mishap(spec, 1, 'TYPESPEC IS UNSIZED')
    endunless;
    if (n // u -> n) /== 0 then
        ;;; round up
        n + 1 -> n
    endif
enddefine;

sysprotect("SIZEOFTYPE");

endsection;     /* $-typespec_utils */

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jun  3 1991
    Made global
 */
