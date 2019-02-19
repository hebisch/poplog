/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/syscomp/pas_optimise.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

/* ------------------------------------------------------------------------

                Optimiser for POPLOG Virtual Machine Code

            Given a VM code list as passed to pas_assemble by
            sysENDPROCEDURE, pas_optimise produced an optimised
            list, which is the original with alterations (NOT a copy).
            pas_optimise is used as a front end by the system compiler
            POPC.

 ------------------------------------------------------------------------ */

#_INCLUDE 'common.ph'

section $-Popas$-M_trans;

    ;;; optimised types of jump instructions
lconstant
    IF      = 1,
    BOOL    = 2,
    GOTO    = 3,
    ;

    ;;; Optimise jumps (ie IFs, BOOLs and GOTOs)
define lconstant opt_jumps(codelist);
    lvars source, target, clist, codelist, target_clist, s_type, t_type,
        sametest;

    ;;; insert a label after an instruction
    define lconstant labafter(clist) -> label;
        lvars clist, label;
        f_tl(clist) -> label;
        if isvector(f_hd(label)) then
            ;;; not already a label, so new one needed
            0 :: label ->> label -> f_tl(clist)
        endif
    enddefine;

    f_hd(codelist) -> source;           ;;; this instruction
    ;;; if jump then change to optimised type, else return
    f_subv(1, source) -> s_type;        ;;; type of source instr
    if      s_type == "pas_IF"   then IF
    elseif  s_type == "pas_BOOL" then BOOL
    elseif  s_type == "pas_GOTO" then GOTO
    else
        return
    endif ->> s_type -> f_subv(1, source);

    ;;; the label pair where the jump goes -- deref it
    f_subv(2, source) -> clist;
    while ispair(f_front(clist)) do f_front(clist) -> clist endwhile;
    clist -> f_subv(2, source);

    ;;; find next instruction after the target label
    f_tl(clist) -> clist;           ;;; skip the label
    until isvector(f_hd(clist)) do
        ;;; 2 or more consecutive labels -- use the last of the sequence
        ;;; (thus making the others redundant)
        clist -> f_subv(2, source);     ;;; change label in instruction
        f_tl(clist) -> clist
    enduntil;
    f_hd(clist) -> target;          ;;; target of jump
    clist -> target_clist;

    ;;; optimise ahead from target, then return if not an (optimised) jump
    opt_jumps(target_clist);
    f_subv(1, target) -> t_type;        ;;; type of target

    ;;; now do this one
    if t_type == GOTO then
        ;;; just make source go to where target goes
        f_subv(2, target) -> f_subv(2, source)
    elseif s_type == BOOL and isinteger(t_type) then
        f_subv(2, target) -> clist;     ;;; label where target (now) goes
        f_subv(3, source) == f_subv(3, target) -> sametest; ;;; if same test
        if t_type == BOOL then
            if sametest then            ;;; stay BOOL - go to where it goes
                clist
            else
                IF ->> s_type ->  f_subv(1, source);    ;;; else become IF
                labafter(target_clist)  ;;; go to immediately after target
            endif
        else
            ;;; t_type is IF
            IF ->> s_type -> f_subv(1, source);     ;;; becomes IF
            if sametest then            ;;; go to where it goes
                clist
            else                        ;;; go to after it
                labafter(target_clist)
            endif
        endif -> f_subv(2, source);     ;;; insert the new label
    ;;; else nothing to do if s_type is IF or GOTO
    endif;

    define lconstant remove_stranded_code(clist);
        lvars l, i, clist;
        ;;; remove any code upto the next label
        f_tl(clist) -> l;
        while isvector(f_hd(l) ->> i) and f_subv(1, i) /== "pas_END" do
            f_tl(l) -> l
        endwhile;
        l -> f_tl(clist)
    enddefine;

    if s_type == GOTO then
        remove_stranded_code(codelist);
        return
    elseunless s_type == IF then
        return
    endif;

    f_tl(codelist) -> clist;
    f_hd(clist) -> target;
    if isvector(target) and f_subv(1, target) == "pas_GOTO" then
        remove_stranded_code(clist);
        f_subv(2, source) -> target_clist;
        repeat
            f_tl(clist) -> clist;
            if clist == target_clist then
                f_tl(codelist) -> clist;
                opt_jumps(clist);
                f_subv(2, target) -> f_subv(2, source);
                not(f_subv(3, source)) -> f_subv(3, source);
                f_tl(clist) -> f_tl(codelist);
                quitloop
            endif;
            quitif(isvector(f_hd(clist)))
        endrepeat
    endif
enddefine;

define genjumplab(backward) -> lab;
    lvars lab = genlab(), backward;
    if backward then
        ;;; label occurs before use (therefore any use will be backward)
        LAB_BACKWARD -> islabel(lab)
    endif
enddefine;


define lconstant mark(labpair) -> labpair;
    lvars labpair, val = f_front(labpair);
    ;;; deref the label if necessary
    while ispair(val) do val -> labpair, f_front(labpair) -> val endwhile;
    if isinteger(val) then
        ;;; Not yet marked -- allocate it a new label. If current value
        ;;; is -1, this is definitely a backward jump
        genjumplab(val == -1) -> f_hd(labpair)
    ;;; else already marked
    endif
enddefine;

    ;;; Mark referenced label pairs, and allocate them symbolic labels.
    ;;; Also change optimised jump types back to normal.
define lconstant marklab(clist);
    lvars op, clist, pair, instr;

    if isvector(f_hd(clist) ->> instr) then
        f_subv(1, instr) -> op;
        if isinteger(op) then
            ;;; optimised jump
            if op == GOTO then
                "pas_GOTO"
            elseif op == IF then
                "pas_IF"
            else
                "pas_BOOL"
            endif -> f_subv(1, instr);
            ;;; mark the label (already dereferenced)
            mark(f_subv(2, instr)) ->
        elseif op == "pas_GO_ON" then
            fast_for pair on f_subv(2, instr) do
                mark(f_hd(pair)) -> f_hd(pair)
            endfast_for;
            if f_subv(3, instr) ->> pair then
                mark(pair) -> f_subv(3, instr)
            endif
        elseif op == "pas_PLOG_IFNOT_ATOM" then
            mark(f_subv(2, instr)) -> f_subv(2, instr)
        elseif op == "pas_PLOG_TERM_SWITCH" then
            mark(f_subv(2, instr)) -> f_subv(2, instr);
            mark(f_subv(3, instr)) -> f_subv(3, instr)
        endif
    else
        ;;; labelpair in codelist -- set front to -1 if not already allocated a
        ;;; label, so that mark can know references to it are backward
        if isinteger(instr) then -1 -> f_hd(clist) endif
    endif
enddefine;

    ;;; Remove redundant labels. Also optimise -not- called before an IF
define lconstant removelab(clist) -> clist;
    lvars l, instr, clist, last = false, id,
        id_not  = identof_path([not]),
        id_lisp_true = identof_path([lisp_true]),
        id_nil  = identof_path([nil]),
        id_/==  = identof_path([/==]),
        id_==   = identof_path([==]),
        ;

    fast_for l on clist do
        f_hd(l) -> instr;
        if isinteger(instr) then
            ;;; unreferenced label - remove it
            f_tl(l) -> if last then f_tl(last) else clist endif
        elseunless isvector(instr) and f_subv(1,instr) == "pas_CALL"
        and ((f_subv(2,instr) ->> id) == id_not or id == id_lisp_true) then
            l -> last
        else
            ;;; optimise "not" or "lisp_true"
            removelab(f_tl(l)) ->> f_tl(l) -> l;
            f_hd(l) -> instr;
            returnunless(isvector(instr));
            if id == id_lisp_true then
                ;;; lisp_true
                if f_subv(1,instr) == "pas_PUSH"
                and f_subv(2,instr) == id_nil
                and isvector(f_hd(f_tl(l)) ->> instr)
                and f_subv(1,instr) == "pas_CALL"
                and ((f_subv(2,instr) ->> instr) == id_/== or instr == id_==) then
                    f_tl(l) -> l;
                    if instr == id_/== then
                        ;;; lisp_true() /== [] -> no code
                        f_tl(l) -> if last then f_tl(last) else clist endif
                    else
                        ;;; lisp_true() == [] -> not()
                        id_not -> f_subv(2, f_hd(l));
                        l -> if last then f_tl(last) else clist endif;
                        f_tl(l) -> l;
                        if isvector(f_hd(l) ->> instr) then
                            id_not -> id
                        endif
                    endif
                endif
            endif;
            if id == id_not and f_subv(1,instr) == "pas_IF" then
                ;;; reverse test and remove call of "not"
                not(f_subv(3,instr)) -> f_subv(3,instr);
                l -> if last then f_tl(last) else clist endif
            endif;
            return
        endif
    endfast_for
enddefine;

define pas_optimise(codelist, pdr_nl_labs) -> codelist;
    lvars plab, clist, codelist, pdr_nl_labs;
    ;;; 1. Optimise IFs, BOOLs and GOTOs
    fast_for clist on codelist do
        if isvector(f_hd(clist)) then   ;;; i.e. not a label
            opt_jumps(clist)
        endif
    endfast_for;

    ;;; 2. Mark referenced label pairs and allocate symbolic labels
    fast_for clist on codelist do marklab(clist) endfast_for;

    ;;; 3. Ensure procedure non-local labels marked
    fast_for plab in pdr_nl_labs do
        dest_procedure_label(plab) -> plab -> /*ident*/ -> /*owner*/;
        mark(plab) ->
    endfast_for;

    ;;; 4. Remove redundant label pairs, optimise "not", etc
    removelab(codelist) -> codelist
enddefine;

endsection;     /* $-Popas$-M_trans */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 11 1995
        Replaced some calls of f_hd with f_front (label in an instruction
        may be a pair, not a list).
--- John Gibson, Apr 26 1989
        Version 13.64 changes
--- John Gibson, Jan 29 1989
        New version of popc
--- John Gibson, Oct 31 1987
        Corrected bug in -removelab-
 */
