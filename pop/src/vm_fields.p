/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/vm_fields.p
 > Purpose:         planting code for record/vector/external field access
 > Author:          John Williams (see revisions)
 > Documentation:   REF *VMCODE
 */

;;; --------------- VM FIELD ACCESS/UPDATE INSTRUCTIONS ----------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'vmdefs.ph'
#_INCLUDE 'fields.ph'

section $-Sys$-Vm;

constant
        procedure (Get_operand, I_MAKE_EXPTR)
    ;

endsection;


;;; ------------------------------------------------------------------------

section $-Sys$-Vm => sysFIELD, sysUFIELD;

define lconstant Plant_field_acc(n, spec, checking, exptr, upd);
    lvars   spec, n, checking, exptr, upd, instr, fdesc, vsubs = false,
            array, l, opnd_instr, subs_instr;

    ispair(spec) and not(islist(spec)) -> array;

    fast_front(vm_code_end) -> instr;
    if isvector(instr) and instr!INST_OP == I_MAKE_EXPTR
    and not(pop_syntax_only) then
        ;;; stop it consing a new exptr in any case
        false -> instr!INST_ARGS[_0];
        if isinteger(exptr) and exptr &&/=_0 MODE_LEVEL
        and not(isvector(spec) or isref(spec) or (checking and array) or upd)
        then
            ;;; accessing struct field or simple type or fast array
            ;;; --- don't bother wrapping up the raw pointer
            Unplant(false);             ;;; remove the I_MAKE_EXPTR
            exptr fi_- 1 -> exptr;      ;;; reduce by 1 level
            exptr fi_|| MODE_FORCE_EXACC -> exptr;  ;;; hack to ensure external access
            false -> checking           ;;; no check for exptr
        endif
    endif;

    if isvector(spec) then
        ;;; external function call (exptr must specify 1 level of exptr)
        FLD Convert_func_spec(spec, exptr)

    elseif array then
        ;;; spec is an array spec (i.e. a pair with false or fixed length
        ;;; in the back) -- generate subscriptor into array
        FLD Convert_vec_spec(spec, exptr, n) -> vsubs

    elseif islist(spec) then
        ;;; spec is a list of structure fields -- generate access code
        ;;; for n-th field
        Check_integer(n, 1);
        FLD Convert_rec_speclist(spec, exptr, n)

    else
        ;;; single type access -- treat as 1 field external structure
        FLD Convert_simp_spec(spec, exptr)

    endif -> fdesc;

    returnif(pop_syntax_only);

    unless fsv(FD_EXTERN,fdesc) or iskey(checking) then
        false -> checking
    endunless;

    if vm_pas_mode then
        Plant("pas_FIELD", n, spec, checking, exptr, upd, 6);
        _S_NONE -> _simstack;
        return
    endif;

    false -> subs_instr;
    if Get_operand() ->> opnd_instr then
        Unplant(true);
        if vsubs and (Get_operand() ->> subs_instr) then
            Unplant(true)
        endif
    endif;

    [%
#_IF DEF OLD_FIELD_INSTRUCTIONS
        if subs_instr then subs_instr endif;
        if opnd_instr then opnd_instr endif;
        false ->> opnd_instr -> subs_instr;
        if checking then FLD Arg_check_instrs(checking, fdesc, vsubs) endif;
#_ELSE
        if checking then
            if subs_instr then subs_instr endif;
            if opnd_instr then opnd_instr endif;
            if upd and FLD Field_update_convert(fdesc)
            and opnd_instr and (not(vsubs) or subs_instr) then
                ;;; use opnd(s) twice to avoid using I_SWAP (do I_ERASE(s)
                ;;; after call)
                if vsubs then copy(subs_instr) -> subs_instr endif;
                copy(opnd_instr) -> opnd_instr
            else
                false ->> opnd_instr -> subs_instr
            endif;
            FLD Arg_check_instrs(checking, fdesc, vsubs);
            if opnd_instr then
                INST1(I_ERASE, true), if vsubs then INST1(I_ERASE, true) endif
            endif
        endif;
#_ENDIF
        FLD Field_instrs(fdesc, upd, opnd_instr, subs_instr)
    %] -> l;
    fast_for instr in l do
        Plant(destvector(instr));
        Garbage_inst(instr)
    endfor;
    sys_grbg_list(l);
    _S_NONE -> _simstack
enddefine;

protected
define vars sysFIELD  = Plant_field_acc(% false %) enddefine;

protected
define vars sysUFIELD = Plant_field_acc(% true %) enddefine;

define updaterof sysFIELD() with_nargs 4;
    chain(sysUFIELD)
enddefine;

endsection;     /* $-Sys$-Vm */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  4 1996
        Now brings in structure and subscript operands when new-style field
        instructions are available.
--- John Gibson, Aug 16 1993
        Replaced pop_p*as_mode with internal var vm_pas_mode
--- John Gibson, Jul 16 1992
        Changed -vm_pas_mode- interface to just plant pas_FIELD instruction
--- John Gibson, Nov 28 1990
        Added optimisation when applied to exptr result
--- John Gibson, Oct 15 1990
        -Convert_simp_spec- used for simple external access
--- John Gibson, Sep 25 1990
        Check on -exptr- removed (now done by fields.p procedures)
--- John Gibson, Sep 14 1990
        Field descriptors now contain exptr arg
--- John Gibson, Jun 27 1990
        Added sysFIELD updater
--- John Gibson, Jun  4 1990
        Added code for generating external function call
--- John Gibson, May 29 1990
        Included fields.ph
--- John Gibson, May  9 1990
        All the old field procedures replaced with the -sys(U)FIELD-
--- John Gibson, Mar 21 1990
        -Field_instrs- now takes -exptr- arg (when true) as integer
        number of times to deref.
--- John Gibson, Mar 15 1990
        Changes in args to -Field_instrs-, -Convert_rec_spec-, etc
--- John Gibson, Dec  8 1989
        Changes for new pop pointers -- code for computing record field
        offsets commoned with code in conskey.p and moved to fields.p.
--- John Gibson, Apr 30 1989
        Put into section $-Sys$-Vm.
--- John Gibson, Mar 12 1989
        Changes for POPC to deal with these instructions; tidied up,
        fixed various bugs.
--- Roger Evans, Oct 10 1988
        added external variants and fixed DDEC alignment
--- John Gibson, Feb 29 1988
        Moved procedures to here from vmplant.p
 */
