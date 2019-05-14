/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/auto/compile_mode.p
 > Purpose:         Syntax for controlling POP11 and VM compiler operation
 >                  through flag words
 >                  (e.g. -pop_pop11_flags- and -pop_vm_flags-).
 > Author:          John Gibson, Jun  1 1989 (see revisions)
 > Related Files:   HELP *COMPILE_MODE
                    $usepop/pop/lib/include/pop11_flags.ph
                    $usepop/pop/lib/include/vm_flags.ph
 */

;;; USAGE, e.g.
;;;
;;;     compile_mode:pop11 +defcon +defpdr;
;;;     compile_mode:vm    -bjmpch +goonch -pentch +typech -prmprt;
;;;
;;; or mixed, as in
;;;
;;;     compile_mode:pop11 -lprops  :vm +prmfix +typech;
;;;
;;; etc. Note that these take effect at COMPILE-TIME, not at run-time.
;;; Since both -pop_pop11_flags- and -pop_vm_flags- are localised to each
;;; compilation stream, procedure or lexical block, they can be used to set
;;; different modes for individual files, procedures or lblocks, e.g.
;;;
;;;         define foo();
;;;             ...
;;;         compile_mode:vm -pentch;
;;;         enddefine;
;;;
;;; turns offs procedure entry checking code for -foo- only. For "pentch",
;;; the mode is taken from the value of -pop_vm_flags- at the END of the
;;; procedure; for other options, the change should appear BEFORE the
;;; construct it is to affect, e.g.
;;;
;;;         define baz();
;;;         compile_mode:vm -bjmpch;
;;;             for ... endfor;
;;;             ...
;;;         enddefine;
;;;
;;; will turn off backward-jump checking code for the -for- construct (and
;;; any other loops following) upto the end of -baz-.


section;

;;; --- OPTIONS FOR :pop11 ----------------------------------------------

#_INCLUDE '$usepop/pop/lib/include/pop11_flags.ph'

;;; See pop11_flags.ph for a description of the flag bits appearing in the
;;; option masks.

lconstant pop11_assoc = [

    /*  +option             set mask                   clear mask  */
    /*  -option            clear mask                   set mask  */

        defcon  {%   POP11_DEFINE_CONSTANT,                 0           %}
        defpdr  {%   POP11_DEFINE_PROCEDURE,                0           %}
        lprops  {%              0,                POP11_NO_LEX_PDPROPS  %}
        varsch  {%      POP11_VARS_CHECK,                   0           %}
        constr  {%   POP11_CONSTRUCTOR_CONSTS,              0           %}
        global  {%      POP11_PERM_GLOBAL,                  0           %}
        oldvar  {%       POP11_OLD_VARS,                    0           %}

        ;;; only valid with +
        normal {%               0,               -1 &&~~ POP11_OLD_VARS %}

        ;;; 'macro' mode (- negates all options)
        strict  [ +varsch +defpdr +defcon -lprops +constr +global
                  :vm +prmfix
                  :popc -wrdflt -wrclos]

    ];


;;; --- OPTIONS FOR :vm --------------------------------------------------

#_INCLUDE '$usepop/pop/lib/include/vm_flags.ph'

;;; See vm_flags.ph for a description of the flag bits appearing in the
;;; option masks.

lconstant vm_assoc = [

    /*  +option             set mask                   clear mask  */
    /*  -option            clear mask                   set mask  */

        mixlex  {% VM_MIX_NONLOCAL_AND_LOCAL_LEX,           0           %}
        dislpp  {%  VM_DISCOUNT_LEX_PROC_PUSHES,            0           %}
        lexprt  {%              0,                  VM_NOPROT_LVARS     %}
        prmprt  {%              0,                  VM_NOPROT_PVARS     %}
        prmfix  {%    VM_PERM_FIXED_DECLARE,                0           %}
        bjmpch  {%              0,              VM_NO_BACK_JUMP_CHECKS  %}
        goonch  {%              0,              VM_NO_CHECK_GO_ON_INT   %}
        pentch  {%              0,              VM_NO_PDR_ENTRY_CHECKS  %}
        typech  {%              0,              VM_NO_TYPED_VAR_CHECKS  %}
        nofast  {%          VM_NO_FAST,                     0           %}

        ;;; only valid with +
        normal {%               0,                         -1           %}

    ];


;;; --- OPTIONS FOR :popc ----------------------------------------------

#_INCLUDE '$usepop/pop/lib/include/popc_flags.ph'

;;; See popc_flags.ph for a description of the flag bits appearing in the
;;; option masks.

lconstant popc_assoc = [

    /*  +option             set mask                   clear mask  */
    /*  -option            clear mask                   set mask  */

        syspop  {%      POPC_SYSPOP_MODE,                   0               %}
        wrdflt  {%              0,              POPC_NONWRITEABLE_DEFAULT   %}
        wrclos  {%   POPC_WRITEABLE_CLOSURES,               0               %}
    ];

    ;;; redefined by Popc
define global vars active pop_popc_flags; 0 enddefine;
define updaterof active pop_popc_flags;
    if () &&/=_0 POPC_SYSPOP_MODE then
        mishap(0, 'compile_mode: sysPOP ONLY AVAILABLE IN POPC')
    endif
enddefine;

;;; ---------------------------------------------------------------------

lconstant flag_groups = [%
        {% "pop11", pop11_assoc,    ident pop_pop11_flags %},
        {% "vm",    vm_assoc,       ident pop_vm_flags %},
        {% "popc",  popc_assoc,     ident pop_popc_flags %},
    %];

define global constant syntax compile_mode;
    lvars group, groups, globl, id, new;
    lconstant SET = 1, CLR = 2, NAME = 3, OPTIONS = 4, ID = 5;

    ;;; zero set/clr masks
    [%  fast_for group in flag_groups do
            consvector(0, 0, explode(group), 5)
        endfor
    %] -> groups;
    false -> group;

    define lconstant read_options(Set, Clr);
        lvars item, masks, set, clr, Set, Clr;
        until (itemread() ->> item) == termin or item == ";" do
            if item == ":" then
                ;;; flag group name
                readitem() -> item;
                fast_for group in groups do
                    quitif(group(NAME) == item);
                    false -> group
                endfor;
                unless group then
                    mishap(item, 1, 'compile_mode: UNKNOWN FLAG GROUP NAME')
                endunless;
                nextloop
            elseunless group then
                mishap(0, 'compile_mode: NO FLAG GROUP NAME SUPPLIED')
            endif;

            if item == "+" then
                Set, Clr
            elseif item == "-" then
                Clr, Set
            else
                mishap(item, 1, 'compile_mode: EXPECTING + OR - BEFORE OPTION')
            endif -> (set, clr);
            readitem() -> item;
            unless fast_lmember(item, group(OPTIONS)) ->> masks then
                mishap(item, 1, 'compile_mode: UNKNOWN OPTION KEYWORD')
            elseunless isvector(fast_front(fast_back(masks)) ->> masks) then
                ;;; 'macro'
                procedure;
                    dlocal proglist = masks <> [;], group;
                    read_options(set, clr)
                endprocedure()
            elseif masks(set) == -1 then
                mishap(if set==SET then "+" else "-" endif <> item, 1,
                                            'compile_mode: INVALID OPTION')
            else
                group(SET) || masks(set) -> group(SET);
                group(CLR) || masks(clr) -> group(CLR)
            endunless
        enduntil
    enddefine;

    pop11_try_nextitem("global") -> globl;
    read_options(SET, CLR);
    ";" :: proglist -> proglist;

    fast_for group in groups do
        if group(SET) &&/=_0 group(CLR) then
            mishap(":"<>group(NAME), 1, 'compile_mode: INCOMPATIBLE OPTIONS SUPPLIED')
        endif;
        group(ID) -> id;
        idval(id) || group(SET) &&~~ group(CLR) -> new;
        if globl then
            set_global_valof(new, id)
        else
            new -> idval(id)
        endif
    endfor
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 16 1996
        Added nofast for :vm
--- John Gibson, May 23 1995
        Added "oldvar" to pop11 options and "global" flag to compile_mode
        for setting in all contexts (i.e. useable in init.p)
--- John Gibson, Oct 26 1992
        Added "global" to pop11 flags and also to +strict
--- Jason Handby, Jul 17 1991
    Added reference to HELP *COMPILE_MODE
 */
