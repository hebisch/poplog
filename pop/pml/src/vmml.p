/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/pml/src/vmml.p
 > Purpose:         PML: VM support
 > Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 */


section $-ml;

;;; Load definitions for vm control flags.
#_INCLUDE '$usepop/pop/lib/include/vm_flags.ph'


constant macro
    GO_ON_THRESHOLD = 5,
        ;;; the number of cases at which it's worth planting a GO_ON
        ;;; (this is pure guesswork, and will vary from machine to machine)
;

/*
 *  ML Run-Time Identifiers
 */

defclass mlident [writeable] {
    mlid_access     : full,     ;;; access to the value
    mlid_tracing    : full,     ;;; name/ident of trace flag
    mlid_arity      : ushort,   ;;; number of arguments
    mlid_kind       : byte,     ;;; kind of access
    mlid_tupled     : bool,     ;;; => tupled function
    mlid_primitive  : bool,     ;;; => can be evaluated at compile-time
};
;;;
lconstant mlident_spec = [full full ushort byte bool bool];
;;;
procedure(id);
    lvars id;
    appdata('<mlident ', cucharout); syspr(mlid_access(id)); cucharout(`>`);
endprocedure -> class_print(mlident_key);

;;; Identifier kinds
constant
    ID_NONE = 1,    ;;; not yet determined
    ID_VAL  = 2,    ;;; literal (constant)
    ID_LEX  = 3,    ;;; lexical
    ID_PERM = 4,    ;;; permanent
    ID_REF  = 5,    ;;; ident/ref (unused as yet)
    ID_ERR  = 6,    ;;; anything else
;

define newmlident();
    consmlident("undef", false, 0, ID_NONE, false, false);
enddefine;

/*
 *  Generating lvar names
 */

lvars
    new_var_cnt,
    new_fun_cnt,
    new_exn_cnt,
    new_lab_cnt,
;

define lconstant new_word(/* root, */ cnt) with_nargs 2;
    lvars cnt, n = fast_idval(cnt);
    n fi_+ 1 -> fast_idval(cnt);
    consword(/* root */ sys_>< n);
enddefine;

lconstant procedure (
    new_var = new_word(% 'var:', ident new_var_cnt %),
    new_fun = new_word(% 'fun:', ident new_fun_cnt %),
    new_exn = new_word(% 'exn:', ident new_exn_cnt %),
    new_lab = new_word(% 'lab:', ident new_lab_cnt %),
);

vars
    new_var_list,
    new_fun_list,
    new_exn_list,
;

;;; init_lexids:
;;;     reinitialises the lexical identifier lists

define init_lexids();
    1 ->> new_var_cnt ->> new_fun_cnt ->> new_exn_cnt -> new_lab_cnt;
    pdtolist(new_var) -> new_var_list;
    pdtolist(new_fun) -> new_fun_list;
    pdtolist(new_exn) -> new_exn_list;
enddefine;

;;; freevar:
;;;     allows a variable name to be reused

define freevar(/* id */) with_nargs 1;
    conspair(/* id, */ new_var_list) -> new_var_list;
enddefine;


/*
 *  Deferring VM Instructions
 */

lvars
    code_list,
        ;;; list of deferred code
    code_list_end,
        ;;; the last pair in -code_list-
;

define lconstant plant_deferred(id, nargs);
    lvars id, nargs;
    unless nargs == 0 then consvector(id, nargs fi_+ 1) -> id endunless;
    conspair(id, []) ->> Back(code_list_end) -> code_list_end;
enddefine;

define lconstant defer(op);
    lvars op;
    plant_deferred(% identof(op), pdnargs(valof(op)) %);
enddefine;

define lconstant interpret_deferred(id);
    lvars id;
    if isvector(id) then explode(id) -> id endif;
    fast_idval(id)();
enddefine;

;;; Each -sysXXX- VM instruction is given a -doXXX- equivalent which can
;;; be deferred, i.e. added to -code_list- rather than being executed
;;; immediately

app([
    [doAND          sysAND          1]
    [doCALLQ        sysCALLQ        1]
    [doENDLBLOCK    sysENDLBLOCK    0]
    [doERASE        sysERASE        1]
    [doFIELD        sysFIELD        4]
    [doGOTO         sysGOTO         1]
    [doIDENT        sysIDENT        1]
    [doIFNOT        sysIFNOT        1]
    [doIFSO         sysIFSO         1]
    [doLABEL        sysLABEL        1]
    [doLBLOCK       sysLBLOCK       1]
    [doLCONSTANT    sysLCONSTANT    2]
    [doLOCAL        sysLOCAL        1]
    [doLVARS        sysLVARS        2]
    [doNEW_LABEL    sysNEW_LABEL    0]
    [doOR           sysOR           1]
    [doPOP          sysPOP          1]
    [doPROCEDURE    sysPROCEDURE    2]
    [doPUSH         sysPUSH         1]
    [doPUSHQ        sysPUSHQ        1]
    [doPUSHS        sysPUSHS        1]
    [doUCALL        sysUCALL        1]
    [doUFIELD       sysUFIELD       4]
],
    procedure(arg);
        lvars arg;
        sysSYNTAX(arg(1), "procedure", false);
        sysPROCEDURE(arg(1), arg(3));
            sysCALL(arg(2));
        sysPASSIGN(sysENDPROCEDURE(), arg(1));
    endprocedure
);

define vars doCALL(w);
    lvars w;
#_IF pop_debugging
    sysCALL(w);
#_ELSE
    if identtype(sys_current_ident(w)) == "procedure" then
        sysCALL(w);
    else
        sysPUSH(w), sysCALL("fast_apply");
    endif;
#_ENDIF
enddefine;

define vars doCALLS(dummy);
    lvars dummy;
#_IF pop_debugging
    sysCALLS(dummy);
#_ELSE
    sysCALL("fast_apply");
#_ENDIF
enddefine;

define vars doCHAIN();
#_IF pop_debugging or DEF FAST_CHAIN_BUG
    sysCALL("chain");
#_ELSE
    sysCALL("fast_chain");
#_ENDIF
enddefine;

define vars doENDPROCEDURE(action);
    lvars action;
    sysENDPROCEDURE();
    if isword(action)
    or ismlident(action)
        and mlid_kind(action) == ID_LEX
        and isword(mlid_access(action) ->> action)
    then
        ;;; function binding: assign the procedure to the name
        sysPASSIGN(action);
    elseif action == true then
        ;;; call the procedure now
        sysCALLQ();
    elseif not(action) then
        ;;; push it as a value
        sysPUSHQ();
    else
        mishap(action, 1, 'ILLEGAL ARGUMENT TO "mlENDPROCEDURE"');
    endif;
enddefine;

define vars doGO_ON(/* labels, else_label */) with_nargs 2;
    dlocal  pop_vm_flags = pop_vm_flags fi_|| VM_NO_CHECK_GO_ON_INT;
    sysGO_ON(/* labels, else_label */);
enddefine;

define vars doSWAP();
    sysSWAP(1,2);
enddefine;

define vars doAPPLY();
    doSWAP(), doCALLS(0);
enddefine;

define vars doFUNCTOR(code_list);
    lvars code_list;
    sysLBLOCK(true);
    app(code_list, interpret_deferred);
    sysENDLBLOCK();
enddefine;

;;; defer_code:
;;;     defers all instructions planted by -plant-, returning the code
;;;     list at the end

define defer_code(plant);
    lvars   plant;
    dlocal  code_list, code_list_end;

    ;;; localise code-planting procedures to their deferred versions
    dlocal
        doAND           = #_< defer("doAND") >_#,
        doAPPLY         = #_< defer("doAPPLY") >_#,
        doCALL          = #_< defer("doCALL") >_#,
        doCALLQ         = #_< defer("doCALLQ") >_#,
        doCALLS         = #_< defer("doCALLS") >_#,
        doCHAIN         = #_< defer("doCHAIN") >_#,
        doENDLBLOCK     = #_< defer("doENDLBLOCK") >_#,
        doENDPROCEDURE  = #_< defer("doENDPROCEDURE") >_#,
        doERASE         = #_< defer("doERASE") >_#,
        doFIELD         = #_< defer("doFIELD") >_#,
        doFUNCTOR       = #_< defer("doFUNCTOR") >_#,
        doGOTO          = #_< defer("doGOTO") >_#,
        doGO_ON         = #_< defer("doGO_ON") >_#,
        doIDENT         = #_< defer("doIDENT") >_#,
        doIFNOT         = #_< defer("doIFNOT") >_#,
        doIFSO          = #_< defer("doIFSO") >_#,
        doLABEL         = #_< defer("doLABEL") >_#,
        doLBLOCK        = #_< defer("doLBLOCK") >_#,
        doLCONSTANT     = #_< defer("doLCONSTANT") >_#,
        doLOCAL         = #_< defer("doLOCAL") >_#,
        doLVARS         = #_< defer("doLVARS") >_#,
        doOR            = #_< defer("doOR") >_#,
        doPOP           = #_< defer("doPOP") >_#,
        doPROCEDURE     = #_< defer("doPROCEDURE") >_#,
        doPUSH          = #_< defer("doPUSH") >_#,
        doPUSHQ         = #_< defer("doPUSHQ") >_#,
        doPUSHS         = #_< defer("doPUSHS") >_#,
        doSWAP          = #_< defer("doSWAP") >_#,
        doUCALL         = #_< defer("doUCALL") >_#,
        doUFIELD        = #_< defer("doUFIELD") >_#,

        doNEW_LABEL     = new_lab,
    ;
    ;;; initialise deferred code list
    conspair(false, []) ->> code_list -> code_list_end;
    ;;; generate code
    plant();
    ;;; return code list
    Back(code_list);
enddefine;


/*
 *  ML VM Instructions
 */

;;; Most of these are just synonyms for the -doXXX- versions:

app([
    [mlAND          doAND           ]
    [mlAPPLY        doAPPLY         ]
    [mlCALLQ        doCALLQ         ]
    [mlCALLS        doCALLS         ]
    [mlENDLBLOCK    doENDLBLOCK     ]
    [mlENDPROCEDURE doENDPROCEDURE  ]
    [mlERASE        doERASE         ]
    [mlFIELD        doFIELD         ]
    [mlFUNCTOR      doFUNCTOR       ]
    [mlGOTO         doGOTO          ]
    [mlGO_ON        doGO_ON         ]
    [mlIDENT        doIDENT         ]
    [mlIFNOT        doIFNOT         ]
    [mlIFSO         doIFSO          ]
    [mlLABEL        doLABEL         ]
    [mlLBLOCK       doLBLOCK        ]
    [mlLCONSTANT    doLCONSTANT     ]
    [mlLOCAL        doLOCAL         ]
    [mlLVARS        doLVARS         ]
    [mlNEW_LABEL    doNEW_LABEL     ]
    [mlOR           doOR            ]
    [mlPROCEDURE    doPROCEDURE     ]
    [mlPUSHQ        doPUSHQ         ]
    [mlPUSHS        doPUSHS         ]
    [mlSWAP         doSWAP          ]
    [mlUCALL        doUCALL         ]
    [mlUFIELD       doUFIELD        ]
],
    procedure(arg);
        lvars arg;
        identof(arg(2)) -> identof(arg(1));
    endprocedure
);

define mlPUSH(id);
    lvars id;
    if isword(id) then
        doPUSH(id);
    elseif ispair(id) then
        ;;; data selector arising from a pattern match
        lvars (id, fn) = Destpair(id);
        if isinteger(fn) then
            ;;; -fn- is a vector index
            doPUSHQ(fn), mlPUSH(id), doCALL("fast_subscrv");
        else
            ;;; -fn- is a selector function
            mlPUSH(id), doCALL(fn);
        endif;
    elseif isref(id) then
        ;;; temporary lvar; push it and reuse the var
        doPUSH(Cont(id) ->> id);
        freevar(id);
    else
        if isval(id) then val_ident(id) -> id endif;
        go_on mlid_kind(id) to
            ID_NONE ID_VAL ID_LEX ID_PERM ID_REF else ID_ERR;
        ID_NONE:
            ;;; assume literal, not yet initialised
            doPUSHQ(id), doFIELD(1, mlident_spec, false, false);
            return;
        ID_VAL:
            doPUSHQ(mlid_access(id));
            return;
        ID_LEX:
        ID_PERM:
            mlPUSH(mlid_access(id));
            return;
        ID_REF:
        ID_ERR:
            mishap(id, 1, 'ILLEGAL ACCESS KIND');
    endif;
enddefine;

define mlPOP(id);
    lvars id;
    if isword(id) then
        doPOP(id);
    else
        if isval(id) then val_ident(id) -> id endif;
        go_on mlid_kind(id) to
            ID_NONE ID_VAL ID_LEX ID_PERM ID_REF else ID_ERR;
        ID_NONE:
        ID_VAL:
            doPUSHQ(id), doUFIELD(1, mlident_spec, false, false);
            return;
        ID_LEX:
        ID_PERM:
            doPOP(mlid_access(id));
            return;
        ID_REF:
        ID_ERR:
            mishap(id, 1, 'ILLEGAL ACCESS KIND');
    endif;
enddefine;

define mlCALL(id);
    lvars id;
    if isword(id) then
        doCALL(id);
    elseif ispair(id) or isref(id) then
        mlPUSH(id), doCALLS(0);
    else
        if isval(id) then val_ident(id) -> id endif;
        go_on mlid_kind(id) to
            ID_NONE ID_VAL ID_LEX ID_PERM ID_REF else ID_ERR;
        ID_NONE:
            ;;; assume literal, not yet initialised
            doPUSHQ(id), doFIELD(1, mlident_spec, false, false), doCALLS(0);
            return;
        ID_VAL:
            doCALLQ(mlid_access(id));
            return;
        ID_LEX:
        ID_PERM:
            mlCALL(mlid_access(id));
            return;
        ID_REF:
        ID_ERR:
            mishap(id, 1, 'ILLEGAL ACCESS KIND');
    endif;
enddefine;

define mlCHAIN(/* id */) with_nargs 1;
    mlPUSH(/* id */), doCHAIN();
enddefine;

define mlRAISE(name);
    lvars name;
    mlPUSHQ(exception(name)), mlCALL(MLWID(raise));
enddefine;

;;; mlPUSH_TRACE, mlPOP_TRACE:
;;;     globalise the trace flag for a function

define mlPUSH_TRACE(val);
    lvars val, id = val_ident(val), flag;
    if isword(mlid_tracing(id) ->> flag) then
        doIDENT(flag);
    elseif flag then
        doPUSHQ(id), doFIELD(2, mlident_spec, false, false);
    endif;
enddefine;

define mlPOP_TRACE(val);
    lvars val, id = val_ident(val);
    if mlid_tracing(id) then
        doPUSHQ(id), doUFIELD(2, mlident_spec, false, false);
        ;;; pop won't be done until executed, so fill in with a dummy
        ;;; until it is
        true -> mlid_tracing(id);
    endif;
enddefine;


/*
 *  Declaration of lexical identifiers
 */

define mlNEW_VAR() -> id;
    lvars id;
    dest(new_var_list) -> (id, new_var_list);
    mlLVARS(id, 0);
enddefine;

define mlNEW_FUN() -> id;
    lvars id;
    dest(new_fun_list) -> (id, new_fun_list);
    mlLCONSTANT(id, "procedure");
enddefine;

define mlNEW_EXN() -> id;
    lvars id;
    dest(new_exn_list) -> (id, new_exn_list);
    mlLVARS(id, 0);
enddefine;


/*
 *  Execute planted code
 */

;;; mlEXECUTE:
;;;     runs sysEXECUTE within an evironment suitable for ML evaluation

define mlEXECUTE();

    define lconstant toplevel();

        ;;; Set up initial exception handler
        dlocal exn_pdr = toplevel, exn_stacklength = stacklength();

        ;;; Initialise tracing
        dlocal trace_names = ml_trace_names, trace_depth = 0;

        ;;; Redefine equality on refs
        dlocal %class_=(ref_key)% = nonop ==;

        ;;; Redefine interrupt to raise exception Interrupt
        define dlocal interrupt();
            raise(exception("Interrupt"));
        enddefine;

        ;;; Redefine pop_exception_handler to raise exceptions for
        ;;; recognised conditions
        define dlocal pop_exception_handler(n, mess, idstring, severity);
            if severity == `E` or severity == `R` then
                ;;; Look for a recognised error and convert to an
                ;;; exception
                if isendstring(':arith-fltovf', idstring) then
                    erasenum(n);
                    raise(exception("Overflow"));
                elseif isendstring(':arith-div0', idstring) then
                    erasenum(n);
                    raise(exception("Div"));
                endif;
            endif;
            ;;; Default to standard behaviour
            false;
        enddefine;

        ;;; Execute the code, and return <true> for successful return
        sysEXECUTE(), true;
    enddefine;

    unless toplevel() then
        ;;; exception raised: exception packet should be on top of stack
        handle(/* exception-packet */);
    endunless;
enddefine;

/*
 *  Access to identifiers through val records
 */

define lconstant curried_wrapper(pdr);
    lvars pdr, name = pdprops(pdr), nargs = pdnargs(pdr), args = initv(nargs);
    define lconstant gen_wrapper(argno);
        lvars argno;
        sysPROCEDURE(name, 1);
            sysPOP(sysNEW_LVAR() ->> Subscrv(argno, args));
            if argno == nargs then
                appdata(args, sysPUSH);
                sysPUSHQ(pdr), sysCALL("chain");
            else
                sysPUSHQ(gen_wrapper(argno fi_+ 1));
            endif;
        sysENDPROCEDURE();
    enddefine;
    gen_wrapper(1);
enddefine;

define lconstant tupled_wrapper(pdr);
    lvars pdr, name = pdprops(pdr), nargs = pdnargs(pdr);
    sysPROCEDURE(name, 1);
        if nargs == 2 then
            sysCALL("fast_destpair");
        else
            sysCALL("destvector");
            sysERASE(0);
        endif;
        sysPUSHQ(pdr), sysCALL("chain");
    sysENDPROCEDURE();
enddefine;

define val_value(val);
    lvars val, wrap = false;
    if isboolean(val) then ((), val) -> (val, wrap) endif;
    lvars id = val_ident(val);
    go_on mlid_kind(id) to
        ID_NONE ID_VAL ID_LEX ID_PERM ID_REF else ID_ERR;
    ID_NONE:
        return("undef");
    ID_VAL:
        lvars p = mlid_access(id);
        if val_tracing(val) then
            ;;; must be a procedure, expecting a type as last arg.
            p(% val_type(val) %) -> p;
        endif;
        if wrap and mlid_arity(id) fi_> 1 then
            if mlid_tupled(id) then
                tupled_wrapper(p);
            else
                curried_wrapper(p);
            endif -> p;
        endif;
        return(p);
    ID_PERM:
        return(valof(mlid_access(id)));
    ID_LEX:
    ID_REF:
    ID_ERR:
        mishap(id, 1, 'ILLEGAL ACCESS TO IDENTIFIER VALUE');
enddefine;
;;;
define updaterof val_value(v, val);
    lvars val, v;
    lvars id = val_ident(val);
    go_on mlid_kind(id) to
        ID_NONE ID_VAL ID_LEX ID_PERM ID_REF else ID_ERR;
    ID_NONE:
        ID_VAL -> mlid_kind(id);
    ID_VAL:
        v -> mlid_access(id);
        return;
    ID_PERM:
        v -> valof(mlid_access(id));
        return;
    ID_LEX:
    ID_REF:
    ID_ERR:
        mishap(id, 1, 'ILLEGAL ASSIGNMENT TO IDENTIFIER VALUE');
enddefine;

define val_access(val);
    lvars val, id = val_ident(val);
    (mlid_kind(id), mlid_access(id));
enddefine;
;;;
define updaterof val_access(kind, access, val);
    lvars kind, access, val, id = val_ident(val);
    (kind, access) -> (mlid_kind(id), mlid_access(id));
enddefine;

define val_idkind =
    val_ident <> mlid_kind;
enddefine;

define val_arity =
    val_ident <> mlid_arity;
enddefine;

define val_tupled =
    val_ident <> mlid_tupled;
enddefine;

define val_isprimitive =
    val_ident <> mlid_primitive;
enddefine;

define val_tracing =
    val_ident <> mlid_tracing;
enddefine;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Apr 26 1996
        Changed mlEXECUTE to use new Poplog exception handling
--- Robert John Duncan, Jul  4 1995
        Made mlident class writeable by default
--- Robert John Duncan, Dec 20 1994
        Added support for function call tracing
--- Robert John Duncan, Dec 20 1994
        mlident changed to include all relevant run-time information, moved
        from val definition in "env.p". Added a new access mode which
        determines how the value is to be pushed/popped/called, etc., and
        removed the wrapped value: wrappers have to be generated afresh as
        needed (e.g. by val_value here).
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Oct 24 1994
        Dynamic mishap handler moved here into body of mlEXECUTE.
--- Robert John Duncan, Sep 27 1991
        Temporarily disabled use of -fast_chain- because of possible VM bug
--- Robert John Duncan, Feb 11 1991
        Exceptions, constructors and variables now represented by a common
        "val" record.
--- Robert John Duncan, Feb  4 1991
        Changed to use -defclass-.
--- Robert John Duncan, Jan 21 1991
        Change to -mlEXECUTE- for new exception protocol.
--- Robert John Duncan, Aug  8 1990
        Changed to use new -sysFIELD-.
        Used -pop_debugging- as debug flag.
--- Rob Duncan, Jun 22 1990
        Made CALL and CALLS use "fast_apply" where appropriate.
        Added CHAIN.
--- Simon Nichols, Jun 21 1990
        Changes to support optimisation of tupled functions. The
        -uncurried- field if an mlident has been renamed -unwrapped- as it
        now contains the identifier of the optimised form of both curried and
        tupled functions. Similarly, the procedure -uncurried- had been
        renamed -unwrapped-.
--- Rob Duncan, Mar 14 1990
        Added LBLOCK instructions.
        Changed -mlEXECUTE- to set up an initial exception handler.
 */
