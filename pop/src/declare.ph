/* --- Copyright University of Sussex 2003. All rights reserved. ----------
 > File:            C.all/src/declare.ph
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;; ----------------- PERM IDENTIFIER DECLARATIONS ------------------------

#_INCLUDE 'syspop.ph'

weak constant

        ;;; special structures
        true, false, nil, termin, popstackmark, nullstring,
        newline, space, tab, undef, pop_undef,
        _special_var_block,
        procedure (pop_undef_p),
    ;

        ;;; system variables
weak vars
        _disable, _trap, _system_end, _userlim, _userhi,
        _call_stack_hi, _call_stack_seg_hi, _plog_save_contn_sp,
                _user_base
    ;


global constant
    active
                ;;; added A.S.29 Jun 2003
                DO_ERRNO_VAL
    ;

        ;;; exported keys
weak constant
        key_key, procedure_key, word_key, ref_key,
        string_key, string16_key, dstring_key, dstring16_key,
        integer_key, biginteger_key, ratio_key, complex_key,
        decimal_key, ddecimal_key, boolean_key, vector_key, pair_key,
        ident_key, process_key, device_key,
        undef_key, prologvar_key, prologterm_key,
        intvec_key, shortvec_key, stackmark_key,
        external_ptr_key, exptr_mem_key,
                bytevec_key, sbytevec_key, ushortvec_key,
                uintvec_key, ulongvec_key, longvec_key,

    ;

        ;;; exported constant procedures
weak constant
        1   (writeable, nonwriteable),
        2   ( div, rem, fi_div, fi_rem ),
        4   ( ~~  ||  &&  &&~~  ||/&  <<  >> fi_~~ fi_|| fi_&& fi_&&~~ fi_||/& fi_<< fi_>> )
        4   ( ::  *  /  //  fi_*  fi_//  dir_><),
        5   ( +  -  <>  ><  sys_><  fi_+  fi_- ),
        -5  nc_<>,
        6   ( <  <=  >  >=  fi_<  fi_<=  fi_>  fi_>=  &&=_0  &&/=_0),
        7   ( ==  /==  /= ) 7 =,

        active (current_section, current_directory,
                popdevin, popdevout, popdeverr, poprawdevin, poprawdevout,
                pop_charin_device, pop_charout_device, pop_charerr_device,
                pop_pr_level, pop_asts_enabled, pop_pas_mode,
                pop_sys_encoding, pop_default_device_encoding
                ),

        active:7 (proglist_state),

        procedure (
            abs
            allbutfirst
            appdata
            applist
            apply
            atom
            back
            caller
            chain, chainfrom, chainto,
            charin, charout, charerr,
            clearstack
            consclosure
            conspair
            consref
            consstring
            consvector
            consword
            cont
            copy, copylist,
            datakey, datalength, dataword,
            dest
            destpair
            deststring
            destvector,
            device_encoding,
            discin
            discout
            dl
            dup
            erase
            erasenum
            exitfrom, exitto,
            explode
            fast_apply
            fast_back
            fast_chain
            fast_cont
            fast_destpair
            fast_front
            fast_frozval
            fast_idval
            fast_lmember
            fast_subscrs, fast_subscrv,
            fast_sysexit,
            fast_sysread, fast_syswrite,
            fast_word_string,
            fi_negate,
            fi_max, fi_min,
            fsub_b,
            fill
            front
            frozval
            hd
            identfn
            identprops
            idval
            incharitem
            inits, inits16, initv,
            isarray,
            isboolean
            iscaller
            isclosure
            iscompound
            isdefined,
            isdevice
            isident
            isinheap
            isinteger, isintegral,
            iskey
            islist
            isnumber
            ispair
            isprocedure,
            isref,
            issimple,
            isstartstring, issubstring, isendstring,
            isstring, isvector,
            isundef,
            isword,
            itemread,
            length, listlength,
            lmember, lmember_=,
            locchar, locchar_back,
            lowertoupper,
            max, min,
            mishap,
            negate,
            nextitem, nextreaditem,
            not,
            null,
            partapply,
            pdnargs, pdpart, pdprops,
            pdtolist,
            pop11_compile,
            printf,
            proglist_new_state,
            readable
            readitem
            recursive_front
            rev
            setpop
            skipchar, skipchar_back,
            spr
            stacklength
            stringin
            strmember
            subscrs, subscrv,
            substring,
            syshash,
            sys_clear_input,
            sys_exception_handler,
            sys_grbg_destpair,
            sys_grbg_list,
            sys_pr_message,
            sys_raise_exception,
            sys_syspr,
            sys_timer,
            sysanyvecons,
            sysclose,
            sysconslist, sysconslist_onto,
            syscreate,
            sysexit
            sysfileok
            sysflush
            sysgarbage
            sysopen
            syspr
            sysprarrow
            sysread
            systrace
            systranslate
            systrmdev
            sysvecons
            syswrite
            testbit
            tl
            trycompile
            updater
            uppertolower,
            valof,

            vedprocess,
            )
        ;

        ;;; declaration for anonymous var (i.e. weak constant active _, but
        ;;; this won't work as a normal declaration since _ is ignored)
sysSYNTAX("_", conspair(0,1), 2:11); sysGLOBAL("_", true);

        ;;; non-constant procedures
weak vars procedure (
        cucharout, cucharerr,
        interrupt, pr, pop_exception_handler, pop_exception_final,
        )
    ;


        ;;; (most) vm code planting procedures
weak vars procedure (
        sysAND,
        sysCALL,
        sysCALLQ,
        sysCALLS,
        sysCOMPILE,
        sysENDPROCEDURE,
        sysEXECUTE,
        sysGO_ON,
        sysGOTO,
        sysIFNOT,
        sysIFSO,
        sysLABEL,
        sysNEW_LABEL,
        sysNEW_LVAR,
        sysOR,
        sysPROCEDURE,
        sysPUSH,
        sysPUSHQ,
        sysPUSHS,
        sysPOP,
        sysSYNTAX,
    );

section $-Sys;

weak constant procedure (
        +_1, -_1,
        Adjust_pdr_exec,
        Alloc_user_space,
        Get_record, Get_store, Get_store_dalign,
        Initv, Init_uservec, Init_usernfvec, Cons_uservec, Cons_usernfvec,
        Dest_uservec, Dest_usernfvec, Vector_apply,
        Check_integer, Check_integer_range,
        Check_string, Check_vsubscr, Checkr_byte, Checkr_dchar, Checkr_dchar8,
        Check_bytevec, Check_device, Check_word, Check_procedure,
        Checkr_record, Checkr_vec_subscr,
        Checkr_exptrclass, Checkr_exptrclass_subscr0, Checkr_exptrclass_subscr,
        Checkr_exptrclass_nargs, Checkr_exptrclass_ptr, Cons_extern_ptr,
        Call_extern, Call_extern_nargs,
        Call_sys_nointr, Call_sys_nointr_se,
        Inline_checkr_integer, Inline_checkr_procedure,
        Clawback,
        Clear_ast_queue,
        Consclos_protect,
        Consstring_bptr,
        Get_nt_string,
        Dlocal_frame_offset,
        Exec_nonpd, Exec_only_updater,
        Exec_closure,
        Ensure_writeable,
        Get_perm_ident,
        Bitvec_getsize, Bytevec_getsize, Shortvec_getsize, Intvec_getsize,
        Doublevec_getsize, Wordvec_getsize, Rawstruct_getsize,
        Rec1_getsize, Rec2_getsize, Record_getsize,
        Fullrec1_hash, Record_hash, Vector_hash,
        Eq__Fullrec, Eq__Record Eq__Fullvec, Eq__Nfullvec,
        Pint_->_uint, Uint_->_pint, Uint_->_bigint,
        Sint_->_pint, Pint_->_sint, Sint_->_bigint,
        Simpint_->_sint, Simpint_->_uint,
        Print_str,
        Prglst$-Chop,
        Prop$-Get, Prop$-Get_hash, Prop$-Get_hash_gc
        Prop$-Put_exp, Prop$-Put_exp_hash, Prop$-Put_exp_hash_gc,
        Incharitem,
        Sysgarbage,
        Make_filler_struct, Init_rawstruct,
        Cons_word,
        Cons_rt_idents, Cons_rt_idents_untyped,
        Non_local_goto, Non_local_goto_id, Gen_stack_frame_num,
        Cons_lex_closure,
        Go_on_outrange, Dlexpr_stackerr, Array$-Sub_error, Record_needed,
        Str_allbutfirst,
        Syserr_mishap,
        On_line_term,
        list_assoc, list_assoc_val, cons_assoc, dest_assoc,
        Default_print, Minimal_print, Data_print,
        Mem_break_changed,
        Set_curr_heap_seg,
        Last_chance_exception_final,
    );

weak constant
        dictionary,

        _file_tab, _file_tab_limit,

        ;;; other built-in keys
        procedure_label_key,
        property_key,
        perm_prop_entry_key, tmparg_prop_entry_key,
        tmpval_prop_entry_key, tmpboth_prop_entry_key,
        tmpclr_prop_entry_key,
        rawstruct_key, nonwriteable_rawstruct_key, objmod_pad_key,
        exfunc_closure_key,
    ;

weak vars
        dev_in, dev_out, dev_err, raw_dev_in, raw_dev_out,
        sys_encoding, vm_pas_mode,
        _free_pairs,
        _nextfree_save,

        _file_tab_next_free, _file_tab_close_ptr,
        _open_seg_base_ptr, _open_seg_free_ptr,
        _curr_heap_seg, _curr_seg_free_ptr, _saved_curr_heap_seg,

        _external_flags, _in_external_control, _in_user_extern
    ;

#_IF DEF VMS
weak constant sysstring, _sysstring_desc;
weak vars _sysstring_len;
weak constant procedure Copy_sysstring;
#_ELSEIF DEF AIX
weak vars _cache_flush_needed;
#_ENDIF

endsection;     /* $-Sys */

section $-Sys$-Extern;

weak constant
        procedure (Callback),
        result_struct
    ;

weak vars
        _saved_sp, _saved_usp,
    ;

endsection;

section $-Sys$-Fld;

weak constant procedure (
        Float_val_s, Float_val_d, Float_val_s_C,
        Exval_val, Double_val_s, Double_val_u,
        Full_val_extern
    );

endsection;


    ;;; pop variables
weak vars
        pop_charout_col, pop_charerr_col,
        pop_pr_radix, pop_pr_quotes,
        pop_pr_exponent, pop_pr_places,
        pop_system_version,
        popclosebracket,
        poplastchar,
        poplinemax,
        poplinenum,
        poplineprefix,
        poplinewidth,
        popnewline,
        popprompt,
        proglist,
        popunderx,
        popgctrace,
        pop_vm_flags,
    ;

    ;;; other exported variables
weak vars
        subsystem,
        pop_debugging,
        pop_translate_envvars,
    ;

    ;;; incremental identifiers
weak constant procedure (
        vedstring_data_prop
    );

weak vars
        pop_runtime_actions,
        popautolist,
        sys_subsystem_table,
        procedure popexit,
    ;

declare_incremental
        list[sublists] pop_runtime_actions,
        list popautolist,
        list sys_subsystem_table,
        procedure popexit,
        property vedstring_data_prop =
            newanyproperty([], 4, 1, 16, false, false, "tmparg", false, false),
    ;

    ;;; subroutine/inline call operations
weak constant
        7 _eq           ;;; ==
        7 _neq          ;;; /==

        6 _gr           ;;; >   (unsigned machine integers)
        6 _greq         ;;; >=
        6 _lt           ;;; <
        6 _lteq         ;;; <=

        6 _sgr          ;;; >   (signed machine integers)
        6 _sgreq        ;;; >=
        6 _slt          ;;; <
        6 _slteq        ;;; <=

        6 _psgr         ;;; fi_>
        6 _psgreq       ;;; fi_>=
        6 _pslt         ;;; fi_<
        6 _pslteq       ;;; fi_<=

        5 _add          ;;; +   (machine integers)
        5 _sub          ;;; -   (machine integers)
        5 _padd         ;;; +   (pop integers)
        5 _psub         ;;; -   (pop integers)

        4 _mult         ;;; *   (machine integers)
        4 _pmult        ;;; *   (pop integers)
        4 _div          ;;; //  (machine integers)
        4 _pdiv         ;;; //  (pop integers)

        4 _biclear      ;;; bit clear (&&~~)
        4 _biset        ;;; bit set (||)
        4 _bimask       ;;; bit mask  (&&)
        4 _bixor        ;;; bit excl or (||/&)
        4 _bitst        ;;; bit test

        2 _divq         ;;; div quotient only (machine integers)
    ;


    ;;; subroutine/inline procedures
weak constant
        _negate,        ;;; negate
        _rshift,        ;;; right shift, optional
        _shift,         ;;; shift integer
        _logcom,        ;;; logical complement i.e. 1's complement
        _int,           ;;; pop integer to integer
        _pint,          ;;; integer to pop integer
        _pint_testovf,  ;;; integer to pop integer with overflow test
        _stklength,     ;;; stacklength in bytes (integer)
        _sp,            ;;; pointer to current call stack frame
        _sp_flush,      ;;; same with callstack flush
        _caller_sp,     ;;; point to caller's stack frame
        _caller_sp_flush, ;;; same with callstack flush
        _user_sp,       ;;; pointer to top of user stack
        _subsv,         ;;; fast_subscrv
        _subsv0,        ;;; fast_subscrv with first element at subscript 0
        _subss,         ;;; fast_subscrs
        _u_subss,       ;;; updater of fast_subscrs
        _subsfroz,      ;;; fast_frozval
        _iscompound,    ;;; true if pointer
        _issimple,      ;;; true if not pointer
        _isinteger,     ;;; true if pop integer
        _isaddress,     ;;; true if a valid address of a pop object
        _zero,          ;;; true if integer 0
        _nonzero,       ;;; true if not integer 0
        _neg,           ;;; true if negative
        _nonneg,        ;;; true if not negative
        _not,           ;;; not
        _dupstk,        ;;; same as -dup-

        _swap_in_continue, _swap_out_continue,
        _prolog_save_check, _prolog_restore,
        _prolog_newvar, _prolog_deref,
        _popenter, _popuenter, _popuncenter, _unwind_frame,
        _syschain, _sysncchain, _syschain_caller, _sysncchain_caller,
        _srchain, _mksimple, _mkcompound,
        _call_sys, _call_sys_se,
        _checkall, _checkplogall, _checkinterrupt,
        _bcmp, _icmp, _cmp,
        _datakey,
        _bfill, _ifill, _fill,
        _haskey,
        _locc,
        _bmove, _smove, _imove, _dmove, _move, _moveq,
        _nextframe,
        _conspair,
        Sys$- _exfunc_clos_action,

        ;;; Sun-4 only
        Sys$- _flush_regfile,
        Sys$- _cache_flush,
    ;

    ;;; The following constants are simple-valued, and must be vars in AIX
    ;;; because the stupid assembler/object file format doesn't support
    ;;; global symbols with absolute values.
#_IF DEF AIX

vars pop_internal_version;
section $-Sys$-Vm;
    vars arg_reg_0, arg_reg_1, arg_reg_2, chain_reg;
endsection;

#_ELSE

constant pop_internal_version;
section $-Sys$-Vm;
    constant arg_reg_0, arg_reg_1, arg_reg_2, chain_reg;
endsection;

#_ENDIF



/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, 1 Nov 2003
        Added pop_translate_envvars
        syfileok will translate environment variables if it is true (default)
--- John Gibson, Dec 11 1992
        Moved compile modes and macros out to new file src/syspop.ph and
        included it at top
--- John Gibson, Dec 31 1991
        Added _BYTEVEC_LIM_OFFS
--- John Gibson, Sep 14 1991
        Added _BYTEVEC_SIZE
--- John Gibson, Apr 16 1991
        Added proglist_state
--- John Gibson, Jan  2 1991
        Added SIZEOF and _INIT_NONPOP_STRUCT macros
--- Jonathan Meyer, Oct 19 1990
        Added -sys*prmessage-
--- John Gibson, Sep 10 1990
        Added _DISABLE_STKCHECKS
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, May 15 1990
        Changed _CHECKINTERRUPT test to _nonzero(_trap)
--- John Gibson, Mar 20 1990
        Added some Checkr_ procedures
--- John Gibson, Mar  5 1990
        Removed declaration for s*hort_ident_key (no longer used).
--- John Gibson, Nov  9 1989
        Added declaration for _call_stack_seg_hi
--- John Gibson, Jul 20 1989
        Added _DISABLE_INTERRUPTS
--- John Gibson, Jun  4 1989
        Added -compile_mode- declarations at top of file.
--- Rob Duncan, May  2 1989
        Added ||/& and fi_||/&
--- John Gibson, Mar 23 1989
        All keys for which there were autoloadable libraries taken out
        of section Sys.
--- John Gibson, Feb  3 1989
        Made all perm declarations 'weak ...'.
        Made macros lconstants
--- John Williams, Dec  5 1988
        Replaced -Isendstring- with -isendstring-
--- John Gibson, Feb 24 1988
        process_key into section Sys
--- John Gibson, Feb 22 1988
        Made WREF a syntax word, now copes with section pathnames.
        Various procedures into Sys$-
--- John Gibson, Feb 19 1988
        Added declarations for various subroutines
--- John Gibson, Feb 14 1988
        Added -nullstring-
--- John Gibson, Feb  9 1988
        Changes for splitting up files, putting in weakrefs, etc.
--- John Gibson, Feb  7 1988
        Made all declarations global.
--- John Gibson, Jan  7 1988
        Removed -popmemlim- and -popgcratio-
--- John Gibson, Dec 13 1987
        Added declarations for -intvec_key-, -Sys$-Incharitem- and
        -Get_prooperty-.
--- John Gibson, Nov 10 1987
        Replaced -popdevraw- with -poprawdevin- and -poprawdevout-,
        and -sys_purge_terminal- with -sys_clear_input-.
--- John Gibson, Oct 31 1987
        Added declaration for _call_stack_hi
--- John Gibson, Sep 26 1987
        Removed _procsp variables (replaced with -userstack_stack-)
--- John Gibson, Sep  4 1987
        Changes for new key format
--- John Gibson, Aug 14 1987
        Changed for segmented system
--- Aled Morris, Jun 25 1987
        Added -pop_pr_exponent- and -pop_pr_places-, and put all the tabs back!
 */
