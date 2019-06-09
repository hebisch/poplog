/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/pml/src/syntax.p
 > Purpose:         PML: Abstract syntax
 > Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 */


section $-ml;

;;; node_types:
;;;     list of all the node types in the syntax tree.
;;;     Format is:
;;;         [name   arity]

lconstant node_types = [

    /*
     *  Type expressions
     */

    [VarTyexp       1]      ;;; VarTyexp(tyvar)
    [RecordTyexp    2]      ;;; RecordTyexp(label list, tyexp list)
    [TupleTyexp     1]      ;;; TupleTyexp(tyexp list)
    [FunTyexp       2]      ;;; FunTyexp(tyexp, tyexp)
    [ConsTyexp      2]      ;;; ConsTyexp(tycon, tyexp list)

    /*
     *  Patterns
     */

    [ConstPat       1]      ;;; ConstPat(constant)
    [WildCardPat    0]      ;;; WildCardPat
    [VarPat         1]      ;;; VarPat(var)
    [ConPat         1]      ;;; ConPat(con)
    [ExnPat         1]      ;;; ExnPat(exn)
    [RecordPat      2]      ;;; RecordPat(label list, pat list)
    [WRecordPat     3]      ;;; WRecordPat(label list, pat list, label list)
    [UnitPat        0]      ;;; UnitPat
    [TuplePat       1]      ;;; TuplePat(pat list)
    [ListPat        1]      ;;; ListPat(pat list)
    [ConAppPat      2]      ;;; ConAppPat(con, pat)
    [ExnAppPat      2]      ;;; ExnAppPat(exn, pat)
    [TypedPat       2]      ;;; TypedPat(pat, tyexp)
    [LayeredPat     2]      ;;; LayeredPat(var, pat)

    /*
     *  Expressions
     */

    [ConstExp       1]      ;;; ConstExp(constant)
    [VarExp         1]      ;;; VarExp(var)
    [ConExp         1]      ;;; ConExp(con)
    [ExnExp         1]      ;;; ExnExp(exn)
    [RecordExp      2]      ;;; RecordExp(label list, exp list)
    [UnitExp        0]      ;;; UnitExp
    [TupleExp       1]      ;;; TupleExp(exp list)
    [SeqExp         1]      ;;; SeqExp(exp list)
    [ListExp        1]      ;;; ListExp(exp list)
    [SelectorExp    2]      ;;; SelectorExp(label, label list)
    [LetExp         2]      ;;; LetExp(dec list, exp)
    [AppExp         2]      ;;; AppExp(exp, exp)
    [TypedExp       2]      ;;; TypedExp(exp, tyexp)
    [AndExp         2]      ;;; AndExp(exp, exp)
    [OrExp          2]      ;;; OrExp(exp, exp)
    [IfExp          3]      ;;; IfExp(exp, exp, exp)
    [WhileExp       2]      ;;; WhileExp(exp, exp)
    [CaseExp        2]      ;;; CaseExp(exp, match)
    [FnExp          1]      ;;; FnExp(match)
    [RaiseExp       1]      ;;; RaiseExp(exp)
    [HandleExp      2]      ;;; HandleExp(exn, match)

    /*
     *  Matches
     */

    [Rule           2]      ;;; Rule(pat, exp)
    [Clause         3]      ;;; Clause(pat list, exp, tyexp option)
    [Match          1]      ;;; Match(rule list)

    /*
     *  Bindings
     */

    [ValBind        2]      ;;; ValBind(pat, exp)
    [FunBind        3]      ;;; FunBind(var, int, clause list)
    [ConBind        2]      ;;; ConBind(con, tyexp option)
    [TypeBind       3]      ;;; TypeBind(tycon, tyvar list, tyexp)
    [DataBind       3]      ;;; DataBind(tycon, tyvar list, conbind list)
    [ExnBind        3]      ;;; ExnBind(exn, tyexp option, exn option)

    /*
     *  Declarations
     */

    [ValDec         4]      ;;; ValDec(var list, tyvar list, valbind list, valbind list)
    [FunDec         3]      ;;; FunDec(var list, tyvar list, funbind list)
    [TypeDec        2]      ;;; TypeDec(tycon list, typebind list)
    [DatatypeDec    4]      ;;; DatatypeDec(tycon list, con list, databind list, typebind list)
    [AbstypeDec     3]      ;;; AbstypeDec(tycon list, datatypedec, dec list)
    [ExceptionDec   2]      ;;; ExceptionDec(exn list, exnbind list)
    [LocalDec       2]      ;;; LocalDec(dec list, dec list)
    [OpenDec        1]      ;;; OpenDec(str list)
    [Directive      3]      ;;; Directive(assoc, prec, ids)

    /*
     *  Descriptions
     */

    [ValDesc        2]      ;;; ValDesc(var, tyexp)
    [TypeDesc       2]      ;;; TypeDesc(tycon, tyvar list)
    [DataDesc       3]      ;;; DataDesc(tycon, tyvar list, conbind list)
    [ExnDesc        2]      ;;; ExnDesc(exn, tyexp option)
    [StrDesc        2]      ;;; StrDesc(str, sig option)
    [Shareq         1]      ;;; Shareq(str list/tycon list)

    /*
     *  Specifications
     */

    [ValSpec        3]      ;;; ValSpec(var list, tyvar list, valdesc list)
    [TypeSpec       2]      ;;; TypeSpec(tycon list, typedesc list)
    [EqtypeSpec     2]      ;;; EqTypeSpec(tycon list, typedesc list)
    [DatatypeSpec   3]      ;;; DatatypeSpec(tycon list, con list, datadesc list)
    [ExceptionSpec  2]      ;;; ExceptionSpec(exn list, exndesc list)
    [StructureSpec  2]      ;;; StructureSpec(str list, strdesc list)
    [SharingSpec    1]      ;;; SharingSpec(shareq list)
    [OpenSpec       1]      ;;; OpenSpec(str list)
    [IncludeSpec    1]      ;;; IncludeSpec(sig list)
    [LocalSpec      2]      ;;; LocalSpec(spec list, spec list)

    /*
     *  Signature Expressions
     */

    [IdSigexp       1]      ;;; IdSigexp(sig)
    [GenSigexp      2]      ;;; GenSigexp(strenv, spec list)

    /*
     *  Structure Expressions
     */

    [IdStrexp       1]      ;;; IdStrexp(str)
    [AppStrexp      2]      ;;; AppStrexp(fnc, strexp)
    [GenStrexp      2]      ;;; GenStrexp(strenv, strdec list)
    [LetStrexp      2]      ;;; LetStrexp(strdec list, strexp)

    /*
     *  Module bindings
     */

    [SigBind        2]      ;;; SigBind(sig, sigexp)
    [StrBind        3]      ;;; StrBind(str, sigexp option, strexp)
    [FncBind        5]      ;;; FncBind(fnc, str option, sigexp, sigexp option, strexp)

    /*
     *  Module Declarations
     */

    [LocalStrDec    2]      ;;; LocalStrDec(strdec list, strdec list)
    [StructureDec   2]      ;;; StructureDec(str list, strbind list)
    [SignatureDec   2]      ;;; SignatureDec(sig list, sigbind list)
    [FunctorDec     2]      ;;; FunctorDec(fnc list, fncbind list)
    [PervasiveDec   1]      ;;; PervasiveDec(str list)
    [ExternalDec    2]      ;;; ExternalDec(keyword, item)

    /*
     *  Top-Level Declarations
     */

    [StrTopDec      1]      ;;; StrTopDec(strdec list)
    [SigTopDec      1]      ;;; SigTopDec(signaturedec list)
    [FncTopDec      1]      ;;; FncTopDec(functordec list)
    [PerTopDec      1]      ;;; PerTopDec(pervasivedec list)
    [ExtTopDec      1]      ;;; ExtTopDec(externaldec list)

];

;;; mknode(linenum, field-1, ..., field-n, tag, n):
;;;     builds a syntax tree node. A node is a vector with the format:
;;;         {^tag ^info ^field-1 ... ^field-n}
;;;     where -tag- is a word identifying the node type and -info- is the
;;;     line number, possibly paired with other miscellaneous information.

define lconstant mknode(/* linenum, field, ... */ tag, size) -> node;
    lvars size, node, tag;
    initv(size) -> node;
    until size == 3 do
        /* field */ -> Subscrv(size, node);
        size fi_- 1 -> size;
    enduntil;
    false -> Subscrv(3, node);
    /* linenum */ -> Subscrv(2, node);
    tag -> Subscrv(1, node);
enddefine;

;;; bad_syntax_tree:
;;;     error procedure for wrong or unrecognisable trees

define bad_syntax_tree(node);
    lvars node, tag, info;
    if isvector(node) and datalength(node) >= 2 then
        node(1) -> tag;
        node(2) -> info;
        if ispair(info) then back(info) -> info endif;
        if isword(tag) and (isinteger(info) or info == false) then
            ;;; looks like a syntax tree node, but with the wrong tag
            mishap(node, 1, 'BAD SYNTAX TREE TAG');
        endif;
    endif;
    ;;; not a syntax tree node at all
    mishap(node, 1, 'SYNTAX TREE NEEDED');
enddefine;

;;; hastag:
;;;     tests the type of a node

define lconstant hastag(node, tag);
    lvars node, tag;
    Subscrv(1, node) == tag;
enddefine;

;;; checktag:
;;;     checks the type of a node and raises an error if it's wrong

define lconstant checktag(node, tag);
    lvars node, tag;
    returnif(Subscrv(1, node) == tag);
    bad_syntax_tree(node);
enddefine;

;;; Node constructors and recognisers:
;;;     for each node of type T there are procedures
;;;         mkT     constructs a T node from a linenumber and arguments
;;;         isT     tests whether a node is a T node
;;;     These are constructed from the -node_types- list.

applist(node_types, procedure with_nargs 1;
    lvars tag, arity, name;
    lvars (tag, arity) = dl();

    lvars name = "mk" <> tag;
    lvars pdr = mknode(% tag, arity + 3 %);
    sysSYNTAX(name, "procedure", not(pop_debugging));
    sysPASSIGN(pdr, name);
    name -> pdprops(pdr);
    arity + 1 -> pdnargs(pdr);

    lvars name = "is" <> tag;
    lvars pdr = hastag(% tag %);
    sysSYNTAX(name, "procedure", not(pop_debugging));
    sysPASSIGN(pdr, name);
    name -> pdprops(pdr);
endprocedure);

;;; Node access procedures:

applist([
    [nodetag    1]
    [nodeinfo   2]
    [nodetype   3]
    [first      4]
    [second     5]
    [third      6]
    [fourth     7]
    [fifth      8]
], procedure with_nargs 1;
    lconstant vector_spec = conspair("full", false);
    lvars name, index;
    dl() -> index -> name;
    sysSYNTAX(name, "procedure", not(pop_debugging));
    sysPROCEDURE(name, 1);
        sysFIELD(index, vector_spec, false, false);
    sysPASSIGN(sysENDPROCEDURE(), name);
    sysPROCEDURE(name, 2);
        sysUFIELD(index, vector_spec, false, false);
    sysUPASSIGN(sysENDPROCEDURE(), name);
endprocedure);

;;; nodeline:
;;;     extracts the line number from the -nodeinfo- field.

define nodeline(n);
    lvars n;
    Subscrv(2, n) -> n;
    if ispair(n) then Front(n) else n endif;
enddefine;

define updaterof nodeline(n) with_nargs 2;
    lvars n;
    if ispair(Subscrv(2, n)) then
        -> Front(Subscrv(2, n));
    else
        -> Subscrv(2, n);
    endif;
enddefine;

;;; nodeprops:
;;;     extracts other information from the -nodeinfo- field.

define nodeprops(n);
    lvars n;
    Subscrv(2, n) -> n;
    if ispair(n) then Back(n) else false endif;
enddefine;

define updaterof nodeprops(x, n);
    lvars n, x;
    if ispair(Subscrv(2, n)) then
        x -> Back(Subscrv(2, n));
    else
        conspair(Subscrv(2, n), x) -> Subscrv(2, n);
    endif;
enddefine;

;;; SWITCH:
;;;     syntax for pattern matching on a node type

constant syntax (ENDSWITCH, CASE);

define syntax SWITCH;
    lconstant CLOSERS = [ENDSWITCH CASE else];
    lvars   closer, node_v, tag_v, this_lab, next_lab, end_lab, id;
    dlocal  pop_new_lvar_list;

    define lconstant readargs();
        [%  if pop11_try_nextreaditem("(") then
                unless pop11_try_nextreaditem(")") then
                    repeat
                        readitem();
                    quitif(pop11_need_nextreaditem([),]) == ")");
                    endrepeat;
                endunless;
            endif;
        %];
    enddefine;

    define lconstant gen_case(node_v, args) -> closer;
        lvars arg, node_v, args, closer, i = 4;
        sysLBLOCK(false);
            until args == [] do
                Destpair(args) -> (arg, args);
                unless arg == "_" then
                    sysPUSHQ(i), sysPUSH(node_v), sysCALLQ(Subscrv);
                    if arg == node_v and args /== [] then
                        sysPUSH(node_v), sysPOP(sysNEW_LVAR() ->> node_v);
                    endif;
                    sysLVARS(arg, 0), sysPOP(arg);
                endunless;
                i fi_+ 1 -> i;
            enduntil;
            pop11_comp_stmnt_seq_to(CLOSERS) -> closer;
        sysENDLBLOCK();
    enddefine;

    unless isword(readitem() ->> node_v)
    and (sys_current_ident(node_v) ->> id)
    and identprops(id) == 0
    and (pop11_try_nextreaditem(CLOSERS) ->> closer)
    then
        node_v :: proglist -> proglist;
        pop11_comp_expr_to(CLOSERS) -> closer;
        sysPOP(sysNEW_LVAR() ->> node_v);
    endunless;
    sysPUSHQ(1), sysPUSH(node_v), sysCALLQ(Subscrv),
        sysPOP(sysNEW_LVAR() ->> tag_v);
    sysNEW_LABEL() -> end_lab;
    while closer == "CASE" do
        lvars n, tag, tags = [], args = false;
        repeat
            readitem() -> tag;
            unless isword(tag) and isdefined("mk" <> tag) then
                mishap(tag, 1, 'NODE TAG NEEDED');
            endunless;
            if args then
                unless readargs() = args then
                    mishap(tag, 1, 'DIFFERENT ARGUMENTS TO NODE TAG');
                endunless;
            else
                pdnargs(valof("mk" <> tag)) - 1 -> n;
                readargs() -> args;
                unless length(args) == n then
                    mishap(tag, 1, 'WRONG NUMBER OF ARGUMENTS TO NODE TAG');
                endunless;
            endif;
            tag :: tags -> tags;
        quitunless(pop11_try_nextreaditem("or"));
        endrepeat;
        pop11_need_nextreaditem("then") -> ;
        sysNEW_LABEL() -> this_lab;
        repeat
            fast_destpair(tags) -> (tag, tags);
            sysPUSH(tag_v), sysPUSHQ(tag), sysCALL("==");
        quitif(tags == []);
            sysIFSO(this_lab);
        endrepeat;
        sysIFNOT(sysNEW_LABEL() ->> next_lab);
        sysLABEL(this_lab);
        gen_case(node_v, args) -> closer;
        sysGOTO(end_lab);
        sysLABEL(next_lab);
    endwhile;
    if closer == "else" then
        sysLBLOCK(false);
            pop11_comp_stmnt_seq_to("ENDSWITCH") -> closer;
        sysENDLBLOCK();
    elseif closer == "ENDSWITCH" then
        define lconstant unmatched with_nargs 1;
            mishap(1, 'UNMATCHED PARSE-TREE NODE TAG');
        enddefine;
        sysPUSH(tag_v), sysCALLQ(unmatched);
    else
        closer :: proglist -> proglist;
        pop11_need_nextreaditem("ENDSWITCH") -> ;
    endif;
    sysLABEL(end_lab);
enddefine;

;;; nonexpansive:
;;;     tests whether an expression is non-expansive (i.e. guaranteed
;;;     not to raise exceptions or side-effects). This is a very simple
;;;     definition, taken direct from the standard.

define nonexpansive(e);
    lvars e;
    while isTypedExp(e) do first(e) -> e endwhile;
    isConstExp(e) or isVarExp(e) or isConExp(e) or isFnExp(e) or isUnitExp(e);
enddefine;

;;; Special nodes:

constant
    wildcard_node   = mkWildCardPat(false),
    nil_node        = mkConPat(false, "nil"),
        ;;; needs updating with constructor for "nil" in "StdTypes.ml"
    ::_node         = mkConAppPat(false, "::", false),
        ;;; needs updating with constructor for "::" in "StdTypes.ml"
;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul  3 1995
        Changed node-matching syntax to SWITCH/CASE/ENDSWITCH to avoid
        conflict with switchon/case
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Oct 24 1994
        Added extra nodetype field to each node, initialised to <false>.
        Suppressed definition of chkX procedure for each node tag X.
        Added new SWITCH syntax for switching on node tag.
--- Robert John Duncan, Feb  4 1991
        Removed redundant constructors field from some type nodes.
--- Robert John Duncan, Jan 21 1991
        Added -nodeprops- procedure to allow arbitrary information to be
        added to a node.
        Removed node-expansion functions -mk*consnode- and -mk*recordnode-:
        now done better by -pregen-.
        Removed references to -pop*defineconstant-.
--- Robert John Duncan, Oct  2 1990
        Added LocalStrDec.
--- Robert John Duncan, Aug  8 1990
        Changed to use new -sysFIELD- instruction.
--- Rob Duncan, Sep 12 1989
        Added TopDec syntax, to allow for sequences of declarations at
        top level.
 */
