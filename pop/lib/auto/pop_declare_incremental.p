/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/pop_declare_incremental.p
 > Purpose:         Declare incremental identifier (redefined by POPC)
 > Author:          John Gibson, Nov 24 1992
 > Documentation:
 > Related Files:   LIB *DECLARE_INCREMENTAL
 */
compile_mode :pop11 +strict;

section;

    /* See declare_incremental for full list of flags */
lconstant macro (
    INCR_TYPE       = 2:11,     ;;; type
    ;;; types in INCR_TYPE
    INCRT_PROPERTY  = 0,
    INCRT_LIST      = 1,
    INCRT_PROCEDURE = 2,
);

sysunprotect("pop_declare_incremental");

define vars pop_declare_incremental(idname, flags, propexpr);
    lvars idname, flags, propexpr, arg = idname, val, pair;
    if not(isdefined(idname)) and isdeclared(idname) then
        ;;; weakly declared
        conspair(idname,"weakref") -> arg
    endif;
    returnunless(isundef(sys_current_val(arg)));

    ;;; initialise to suitable value
    flags && INCR_TYPE -> flags;
    if flags == INCRT_PROPERTY then
        ;;; property
        if isproperty(pop11_compile(propexpr) ->> val) then
            if ispair(word_dict_status(idname) ->> pair) then
                front(pair)
            else
                idname
            endif -> pdprops(val);
            val
        else
            mishap(idname, val, 2, 'PROPERTY NEEDED')
        endif
    elseif flags == INCRT_LIST then
        ;;; list
        []
    elseif flags == INCRT_PROCEDURE then
        identfn
    else
        mishap(flags, 1, 'INVALID INCREMENTAL IDENTIFIER TYPE')
    endif -> sys_current_val(arg)
enddefine;

sysprotect("pop_declare_incremental");

endsection;
