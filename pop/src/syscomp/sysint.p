/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/syscomp/sysint.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

/* -------------------------------------------------------------------------

                GENERAL LIBRARY ROUTINES FOR SYSTEM COMPILATION

--------------------------------------------------------------------------*/

#_INCLUDE 'common.ph'

section $-Popas;


;;; --- REPRESENTATION OF SYSTEM INTEGERS, LABELS, ETC -------------------

#_IF not(DEF _intval)

global constant procedure (
        _intval     = newproperty([], 32, false, false),
        islabel     = newproperty([], 256, false, false)
    );

    ;;; flags for islabel property
global constant macro (
    LAB_GLOBAL      = 2:1e0,        ;;; label has global value
    LAB_LITERAL     = 2:1e1,        ;;; label is literal (i.e. not an address)
    LAB_LINK_ASSIGN = 2:1e2,        ;;; label of struct poplink handles assignments for
    LAB_OF_STRUCT   = 2:1e3,        ;;; label is attached to generatable struct
    LAB_GENERATED   = 2:1e4,        ;;; structure has passed thru genstructure
    LAB_PROC_INNER  = 2:1e5,        ;;; label inside a procedure
    LAB_BACKWARD    = 2:1e6,        ;;; backward label inside a procedure
    LAB_EXTERN      = 2:1e7,        ;;; external global label
    LAB_EXPR        = 2:1e8,        ;;; is an expression, not a simple label
    LAB_SFLOAT      = 2:1e9,        ;;; is a single float value
    LAB_GEN_FULL_ID = 2:1e10,       ;;; generate proper record for lex id
    );

define lconstant lab_arith(x, y, op) -> lab;
    lvars lx, ly, x, y, lab, op;
    if y == 0 then
        ;;; must create new label
        if isstring(x) then copy(x) else x sys_>< nullstring endif
    else
#_IF DEF asm_label_diff
        ;;; return integer diff between two labs in same seg, if possible
        returnif(op == "-" and asm_label_diff(x, y) ->> lab);
#_ENDIF
        if isintegral(y) and y < 0 then
            -y -> y;
            if op == "+" then "-" else "+" endif -> op
        endif;
        x sys_>< op sys_>< y
    endif -> lab;
    unless islabel(x) ->> lx then 0 -> lx endunless;
    unless islabel(y) ->> ly then 0 -> ly endunless;
    ((lx fi_|| ly) fi_&& LAB_GLOBAL) fi_|| LAB_EXPR -> islabel(lab)
enddefine;

define global 5 label_+ = lab_arith(%"+"%) enddefine;
define global 5 label_- = lab_arith(%"-"%) enddefine;


;;; --- REPRESENTATION OF OTHER STRUCTURES THRU data_key -------------------

global constant
    ;;; Some dummy 'keys' (undefs)
    closure_key         = closure_key,
    gen_procedure_key   = gen_procedure_key,
    gen_key_key         = gen_key_key,
    dfloat_key          = dfloat_key,
    struct_init_key     = struct_init_key,
    popc_pointer_key    = popc_pointer_key,
    popc_pointer_into_key = popc_pointer_into_key,
    free_struct_key     = free_struct_key,
    pdprops_label_key   = pdprops_label_key,
    testdef_label_key   = testdef_label_key,

    ;;; these two are real, but inaccessible ...
    procedure_label_key = procedure_label_key,
    exfunc_closure_key  = exfunc_closure_key,

    ;;; this one real and accessible
    property_key        = datakey(frozval(1, islabel)),
    ;


global constant procedure
    data_key = newactproperty([], 16, false, false,
                procedure();
                    lvars (item, ) = (), dw;
                    if isclosure(item) then
                        if isundef(item) then undef_key else closure_key endif
                    elseif (dataword(item) ->> dw) == "procedure_label" then
                        ;;; currently difficult to get at the real one
                        procedure_label_key
                    elseif dw == "exfunc_closure" then
                        exfunc_closure_key
                    else
                        datakey(item)
                    endif
                endprocedure);

#_ENDIF

;;; ----------------------------------------------------------------------

define global is_power2(n) -> pow;
    lvars n, pow;
    lconstant pow2 = newassoc([]);
    unless pow2(n) ->> pow then
        if n > 0 and n &&~~ (n-1) == n then
            ;;; is a single bit, i.e. power of 2
            integer_leastbit(n) ->> pow -> pow2(n)
        else
            false -> pow
        endif
    endunless
enddefine;


;;; --- EXTERNAL NAME TRANSLATION ----------------------------------------

define get_extern_label(wname) -> lab;
    lvars wname, name, lab, n, lang, type;
    define lconstant cache = newproperty([], 8, false, "perm") enddefine;

    unless isword(wname) then consword(wname) -> wname endunless;
    returnif(cache(wname) ->> lab);

    nullstring ->> lang -> type;
    fast_word_string(wname) -> name;
    if locchar(`/`, 1, name) ->> n then
        ;;; part preceding the / is a language name
        substring(1, n-1, name) -> lang;
        allbutfirst(n, name) -> name
    endif;
    if locchar(`:`, 1, name) ->> n then
        ;;; part following the : is type ('data' etc)
        allbutfirst(n, name) -> type;
        substring(1, n-1, name) -> name
    endif;
    ;;; copy this in case it always returns the same string, otherwise
    ;;; that string will get marked as a label
    copy(extern_name_translate(lang, name, type)) ->> lab -> cache(wname);
    #_< LAB_GLOBAL || LAB_EXTERN >_# -> islabel(lab)
enddefine;


endsection;     /* $-Popas */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 15 1996
        Added get_extern_label.
--- John Gibson, Feb  9 1995
        Added asm_label_diff code in lab_arith
--- John Gibson, Jul 21 1992
        Version 14.21 changes
--- John Gibson, Feb 20 1990
        Fixed -lab_arith- so a single correct "+" or "-" is produced when
        second arg is a negative integer.
--- John Gibson, Jan  7 1990
        Version 13.7 for new pointers.
--- John Gibson, May 17 1989
        Version 13.6403 changes
--- John Gibson, Apr 26 1989
        Version 13.64 changes
--- John Gibson, Jan 29 1989
        Split off from lib.p
 */
