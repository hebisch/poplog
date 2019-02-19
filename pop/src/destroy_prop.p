/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/destroy_prop.p
 > Purpose:         support for destroy properties
 > Author:          Roger Evans, Aug 10 1988 (see revisions)
 > Documentation:   REF PROPS
 > Related Files:   property.p gcmain.p gccopy.p gcncopy.p sr_incr.p
 */

#_INCLUDE 'declare.ph'
#_INCLUDE 'destroy.ph'
#_INCLUDE 'gcdefs.ph'

constant
        procedure (fast_idval, isident, fast_get_prop_entry, isproperty, copy),
        Sys$-Gc$-dead_prop_entry,
    ;

section $-Sys =>    sys_destroy_action, sys_destroy_dependent,
                    sys_process_destroy_action, sys_destroy_isdependent;


    ;;; key for destroy_prop_entry records - struct is defined in destroy.ph
constant
    destroy_prop_entry_key = struct KEY_PROP_ENTRY =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_PROP_ENTRY _biset _:M_K_COPY _biset _:M_K_WRITEABLE,
                                ;;; K_FLAGS
        _:GCTYPE_PROPENT_DESTROY, ;;; K_GC_TYPE
        procedure; ->, @@(struct DESTROY_PROP_ENTRY)++ endprocedure,
                                ;;; K_GET_SIZE

        "prop_entry",           ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        false,                  ;;; K_RECOGNISER
        WREF Exec_nonpd,        ;;; K_APPLY
        nonop ==,               ;;; K_SYS_EQUALS
        WREF nonop ==,          ;;; K_EQUALS
        Minimal_print,          ;;; K_SYS_PRINT
        WREF Minimal_print,     ;;; K_PRINT
        WREF Fullrec1_hash,     ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        "destroy"               ;;; K_PROP_TYPE_NAME
        %},
    ;

    ;;; GC key for destroy_mark
constant destroy_mark_key = struct KEY_GC =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _0,                     ;;; K_FLAGS
        _:GCTYPE_DESTROY_MARK,  ;;; K_GC_TYPE
        erase,                  ;;; K_GET_SIZE (not used)
        %},
    ;


;;; -- USER INTERFACE TO DESTROY ACTIONS ---------------------------------

    ;;; somewhat kludgey - we use newproperty to create the closure
    ;;; procedure and then jam in our own special property record.
    ;;; If newproperty gets changed to create destroy properties this
    ;;; can be cleaned up.


;;; CONSTRUCTS A DESTROY PROPERTY NAMED -name-
define lconstant make_destroy_prop(name) /* -> property */;
    lvars name;
    lvars property = newproperty([],1,false,"tmparg");

    ;;; property record containing destroy_prop_entry_key
    writeable struct PROP =>>
        {%  writeable {% repeat DASIZE times 0 endrepeat %};    ;;; PT_TABLE
            property_key;       ;;; KEY
            false;              ;;; PT_ACTIVE
            false;              ;;; PT_COUNT
            false;              ;;; PT_DEFAULT
            destroy_prop_entry_key; ;;; PT_ENTRY_KEY
            false;              ;;; PT_EQ_PDR
            false;              ;;; PT_EXPAND
            false;              ;;; PT_HASH_PDR
            true;               ;;; PT_REHASH  - SET FOR INITIAL HASH
        %};
    ->> fast_frozval(1, property) -> fast_frozval(1, updater(property));

    name -> pdprops(property);
    /**RETURN**/ property;

enddefine;


;;; MAKE NORMAL DESTROY PROPERTY
global constant sys_destroy_action = make_destroy_prop('sys_destroy_action');

;;; MAKE DESTROY PROPERTY WHICH WILL BE CLEARED ON FORK
global constant sys_process_destroy_action
    = make_destroy_prop('sys_process_destroy_action');

    ;;; default setting for dependency flag
global vars sys_destroy_dependent = false;



    ;;; access to dependency flag

define lconstant Check_destroy_prop(prop);
    lvars prop;
    unless isproperty(prop) and
        (frozval(1,prop))!PT_ENTRY_KEY == destroy_prop_entry_key then
        mishap(prop,1,'DESTROY PROPERTY NEEDED');
    endunless;
enddefine;

define sys_destroy_isdependent(arg,prop);
    lvars arg prop entry;
    Check_destroy_prop(prop);
    fast_get_prop_entry(arg,prop) -> entry;
    if entry then entry!DPTE_FLAGS _bitst _DPTE_DEPEND else false endif;
enddefine;

define updaterof sys_destroy_isdependent(flag,arg,prop);
    lvars arg prop entry flag;
    Check_destroy_prop(prop);
    fast_get_prop_entry(arg,prop) -> entry;
    if entry then
        if flag then _DPTE_DEPEND else _0 endif -> entry!DPTE_FLAGS;
    else
        mishap(arg,prop,2,'NO DESTROY ENTRY DEFINED');
    endif;
enddefine;

/*  system interface to sys_destroy_action with extra arg for dependent
    flag (system can never assume a value for it)
*/
define Sys_destroy_action(/* arg */ sys_destroy_dependent) with_nargs 2;
    dlocal sys_destroy_dependent;
    sys_destroy_action(/* arg */);
enddefine;

define updaterof Sys_destroy_action(/* val, arg */ sys_destroy_dependent) with_nargs 3;
    dlocal sys_destroy_dependent;
    /* val */ -> sys_destroy_action(/* arg */);
enddefine;

/*  system interface to sys_process_destroy_action with extra arg for dependent
    flag (system can never assume a value for it)
*/
define Sys_process_destroy_action(/* arg */ sys_destroy_dependent) with_nargs 2;
    dlocal sys_destroy_dependent;
    sys_process_destroy_action(/* arg */);
enddefine;

define updaterof Sys_process_destroy_action(/* val, arg */ sys_destroy_dependent) with_nargs 3;
    dlocal sys_destroy_dependent;
    /* val */ -> sys_process_destroy_action(/* arg */);
enddefine;


;;; --- RUNNING DESTROY ACTIONS ----------------------------------------

lvars
    ;;; Chain of actions scheduled to be run
    scheduled_chain = 0,

    ;;; Set true while executing an action
    doing_destroy   = false,
    ;


    /*  Procedure called (by Sysgarbage) after gc to execute destroy actions
    */
define Do_destroy_actions();
    lvars entry, p, arg;
    dlocal doing_destroy;
    returnif(doing_destroy);    ;;; already in this procedure
    true -> doing_destroy;

    ;;; Otherwise, start running the actions
    while iscompound(scheduled_chain ->> entry) do
        entry!DPTE_DLINK -> scheduled_chain;
        0 -> entry!DPTE_DLINK;

        entry!PTE_ARG -> arg;
        entry!PTE_VALUE -> p;
        ;;; kill the property entry before running it
        Gc$-dead_prop_entry ->> entry!PTE_ARG -> entry!PTE_VALUE;

        if isident(p) then fast_idval(p) -> p endif;
        unless isprocedure(p) then
            mishap(arg, p, 2, 'DESTROY ACTION NOT A PROCEDURE')
        endunless;

#_IF DEF TRACE_DESTROY
        printf('destroy: %p(%p)\n', [^p ^arg]);
#_ENDIF
        fast_apply(arg, p)
    endwhile
enddefine;

endsection;     /* $-Sys */


;;; --- SCHEDULING ACTIONS FROM THE GARBAGE COLLECTOR -------------------

section $-Sys$-Gc;

lvars
    _scheduled_last;

    /*  Find the last entry in the scheduled chain. Called before
        starting a GC so we can use the old address of this in
        Add_destroy_actions.
    */
define Find_destroy_last();
    lvars _entry = scheduled_chain;
    while iscompound(_entry) do
        _entry -> _scheduled_last;
        _entry!DPTE_DLINK -> _entry
    endwhile
enddefine;

    /*  Add new destroy property entries in _entry (from _destroy_prop_chain)
        to the end of -scheduled_chain-. Called after finishing the
        GC scan phase.
    */
define Add_destroy_actions(_entry);
    lvars _entry, _last;

    if issimple(scheduled_chain) then
        (ident scheduled_chain)@~DPTE_DLINK -> _last
    else
        _scheduled_last -> _last;
        if _copy_gc and _last >=@(w) _lowest_garbageable then
            GC_SET_SEG _last;
            _last!FIRST@(w){_curr_gcseg_reloc} -> _last
        endif
    endif;

    while iscompound(_entry) do
        if _entry!DPTE_FLAGS _bitst _DPTE_KEYSAV then
            ;;; noncopy gc dependent -- arg key needs restoring
            _entry!DPTE_OLD -> _entry!PTE_ARG!KEY
        endif;
        ;;; mark new entry scheduled -- after this it behaves as a perm
        ;;; prop entry, and the DLINK field is scanned by GC.
        _entry!DPTE_FLAGS _biset _DPTE_SCHEDULED -> _entry!DPTE_FLAGS;

        ;;; now add to scheduled chain (DLINK in entry is a pointer
        ;;; to where _entry will be after GC)
        _entry!DPTE_DLINK -> _last!DPTE_DLINK;
        _entry -> _last;
        _entry!PTE_LINK -> _entry
    endwhile;
    0 -> _last!DPTE_DLINK
enddefine;

endsection;     /* $-Sys$-Gc */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  7 1995
        Revised key layout
--- Adrian Howard, Jun 12 1992
        Added -Sys_process_destroy_action-
--- Adrian Howard, Mar 17 1992
        Added -sys_process_destroy_action-, destroy property which gets cleared on
        forks.
--- John Gibson, Aug 16 1990
        Changes to permit destroy actions to be run in a 'normal'
        environment, i.e. where interrupts, garbage collection etc
        are not disabled.
--- Roger Evans, Jul 26 1990
        Added Sys_destroy_action
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Dec  1 1989
        Removed include for gcdefs.ph (doesn't use it).
--- Roger Evans, Aug 23 1989
        Added destroy trace printing if TRACE_DESTROY macro is defined
--- John Gibson, May 24 1989
        Made -destroy_prop_entry_key- a KEY_PROP_ENTRY struct
--- John Gibson, Jan 25 1989
        Assigning -prop_rec- to frozval of property -sys_destroy_action-
        changed to use -fast_frozval-.
--- Roger Evans, Aug 31 1988
        added sys_destroy_dependent and sys_destroy_isdependent
--- Roger Evans, Aug 30 1988
        changes for new destroy mark and independent destroy actions
 */
