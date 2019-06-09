/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/pml/src/external.p
 > Purpose:         PML: POP-11 interface
 > Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 */


section $-ml =>
    ml_valof
    ml_curried_valof
    ml_raise
    ml_handle
    ml_val
    ml_type
    ml_eqtype
    ml_exception
    ml_structure
    ml_endstructure
;

/*
 *  Check a POP-11 value against an ML type
 */

define lconstant conforms(val, ty);
    lvars val, ty, tys, i, n;
    type_deref(ty) -> ty;
    while is_alias_type(ty) do type_expand(ty) -> ty endwhile;
    if isvartype(ty) then
        true;
    elseif isrecordtype(ty) then
        recordtype_deref(ty) -> ty;
        type_fields(ty) -> tys;
        llength(tys) -> n;
        unless ispair(val) and n == 2
        or isvector(val) and datalength(val) == n
        or val == ml_unit and n == 0
        then
            return(false);
        endunless;
        For i to n do
            returnunless(conforms(ml_subscrtuple(i, val), Destpair(tys) -> tys))(false);
        endfor;
        true;
    elseif isfuntype(ty) then
        returnunless(isprocedure(val))(false);
        returnif(pdnargs(val) fi_<= 1)(true);
        if isfuntype(type_deref(type_range(ty))) then
            Repeat pdnargs(val) fi_- 1 times
                type_expand(type_deref(type_range(ty))) -> ty;
                returnunless(isfuntype(ty))(false);
            endrepeat;
            true;
        elseif isrecordtype(type_domain(ty))
        and is_tuple_recordtype(type_domain(ty))
        then
            type_labels(type_domain(ty)) == pdnargs(val);
        else
            false;
        endif;
    elseif (typename_contents(type_function(ty)) ->> n) == "int" then
        isintegral(val);
    elseif n == "real" then
        isddecimal(val);
    elseif n == "string" then
        isstring(val);
    elseif n == "exn" then
        ispacket(val);
    elseif n == "bool" then
        isboolean(val);
    elseif n == "ref" then
        Front(type_arguments(ty)) -> ty;
        isref(val) and conforms(Cont(val), ty)
        or isident(val) and conforms(idval(val), ty);
    elseif n == "list" then
        Front(type_arguments(ty)) -> ty;
        while ispair(val) do
            returnunless(conforms(Destpair(val) -> val, ty))(false);
        endwhile;
        val == [];
    elseif n == "vector" then
        returnunless(isvector(val))(false);
        Front(type_arguments(ty)) -> ty;
        For i to datalength(val) do
            returnunless(conforms(Subscrv(i, val), ty))(false);
        endfor;
        true;
    elseif n == "array" then
        returnunless(ismlarray(val))(false);
        Front(type_arguments(ty)) -> ty;
        For i to datalength(val) do
            returnunless(conforms(subscrmlarray(i, val), ty))(false);
        endfor;
        true;
    elseif n == "instream" then
        isinstream(val);
    elseif n == "outstream" then
        isoutstream(val);
    else
        true;
    endif;
enddefine;


/*
 *  External Declarations
 */

lvars
    popstrname = false,
        ;;; identifier of the current structure created by -ml_structure-
;

;;; new_pop_itemiser:
;;;     simulates ML itemisation using the Pop itemiser.
;;;     This is far from complete, but is sufficient for the limited
;;;     requirements of external declarations.

define new_pop_itemiser();
    lvars buff;

    define lconstant itemise() -> item;
        lvars item, i;
        dlocal
            % item_chartype(`'`, readitem) % = 1,
            % item_chartype(`_`, readitem) % = 1,
            % item_chartype(`%`, readitem) % = 3,
            % item_chartype(`/`, readitem) % = 3,
            % item_chartype(```, readitem) % = 3,
            % item_chartype(`*`, readitem) % = 3,
            % item_chartype(`.`, readitem) % = 4,
            % item_chartype(`"`, readitem) % = 7,
        ;
        readitem() ->> item -> buff;
        if isword(item) then
            tryreserved(item) -> item;
            if isword(item) then
                if isstartstring('_', item) then
                    allbutfirst(1, item) :: proglist -> proglist;
                    tryreserved("_" ->> buff) -> item;
                elseif isstartstring('\'', item) then
                    if locchar(`.`, 1, item) ->> i then
                        allbutfirst(i-1, item) :: proglist -> proglist;
                        consword(substring(1,i-1,item)) ->> item -> buff;
                    endif;
                    constvid(item,
                        isstartstring('\'\'', item),
                        isstartstring('\'_', item)
                            or isstartstring('\'\'_', item)) -> item;
                elseif locchar(`.`, 1, item) then
                    idpath(fast_word_string(item)) -> item;
                endif;
            endif;
        endif;
    enddefine;

    define updaterof lconstant itemise(item);
        lvars item;
        buff :: proglist -> proglist;
    enddefine;

    itemise;
enddefine;

define comp_external_structure();
    pop11_need_nextreaditem("struct") -> ;
    pop11_exec_stmnt_seq_to([ml_endstructure end]) -> ;
enddefine;

define external_structure() -> str;
    lvars   str, sig, struct;
    dlocal  popstrname, bound_names = false;
    new_strname(false, popstrname) -> popstrname;
    parse_external_structure() -> struct -> sig -> str;
    popstrname -> str_strname(str);
    unless structenv_strname(struct) then
        popstrname -> structenv_strname(struct);
    endunless;
    str_name(str) -> strname_name(popstrname);
    strname_parent(popstrname) -> popstrname;
    typecheck_external_structure(str, sig, struct);
enddefine;

define external_type(equality) -> tycon;
    lvars tycon, tyvars, equality;
    parse_external_type() -> tyvars -> tycon;
    new_type_name(llength(tyvars), equality) -> tycon_function(tycon);
    if current_section == ml_section then
        tycon_name(tycon) -> typename_contents(tycon_function(tycon));
    endif;
    popstrname -> tycon_parent(tycon);
enddefine;

define external_exception() -> exn;
    lvars exn, tyexp, id, p;
    parse_external_exception() -> tyexp -> exn;
    typecheck_external_exception(exn, tyexp);
    if current_section == ml_section then
        val_name(exn) -> id;
    else
        new_exn_value() -> id;
    endif;
    if val_arity(exn) == 0 then
        conspacket(ml_unit, id, val_name(exn), false) -> p;
    else
        conspacket(% id, val_name(exn), val_printer(exn) %) -> p;
    endif;
    (ID_VAL, p) -> val_access(exn);
    if current_section == ml_section then
        p -> exception(val_name(exn));
    endif;
    popstrname -> val_parent(exn);
enddefine;

define lconstant assign_external_val(val, var);
    lvars val, var, id, ty = val_type(var), domain, range;
    unless conforms(val, ty) then
        mishap(val, ty, 2, 'VALUE DOESN\'T CONFORM TO TYPE');
    endunless;
    (ID_VAL, val) -> val_access(var);
    if isprocedure(val) and pdnargs(val) > 0 then
        pdnargs(val) -> val_arity(var);
        if val_arity(var) > 1 then
            type_expand(type_deref(ty)) -> ty;
            type_expand(type_deref(type_domain(ty))) -> domain;
            type_expand(type_deref(type_range(ty))) -> range;
            if  isfuntype(ty) and isrecordtype(domain)
            and is_tuple_recordtype(domain) and not(isfuntype(range))
            then
                true -> val_tupled(var);
            endif;
        endif;
    endif;
enddefine;

define external_val() -> var;
    lvars var, tyvars, tyexp;
    parse_external_val() -> tyexp -> tyvars -> var;
    typecheck_external_val(var, tyvars, tyexp);
    if nextreaditem() == ";" then
        sysPUSH(val_name(var));
    else
        pop11_need_nextreaditem("=") -> ;
        pop11_comp_prec_expr(256, false) -> ;
    endif;
    sysPUSHQ(var);
    sysCALLQ(assign_external_val);
    sysEXECUTE();
    popstrname -> val_parent(var);
enddefine;


/*
 *  POP-11 Syntax for ML Interface
 */

;;; save_env:
;;;     guard the ML environment when called from POP-11 and not inside
;;;     a structure

define lconstant save_env();
    if popstrname then
        false;
    else
        (local_env, init_local_env() -> local_env)
    endif;
enddefine;

define updaterof lconstant save_env(saved_env);
    lvars saved_env;
    if saved_env then saved_env -> local_env endif;
enddefine;


constant syntax ml_endstructure;
#_IF not(DEF $-end)
;;; This must be an alternative to ml_endstructure to properly support
;;; external structure ... = struct ... end;
global constant syntax $-end;
#_ENDIF

define syntax ml_structure;
    lvars   str;
    dlocal  % save_env() %;
    external_structure() -> str;
    unless popstrname then
        sysPUSHQ(str);
        sysCALL(MLWID(globalise));
    endunless;
enddefine;

define syntax ml_type;
    lvars   tycon;
    dlocal  % save_env() %;
    external_type(false) -> tycon;
    unless popstrname then
        sysPUSHQ(tycon);
        sysCALL(MLWID(globalise));
    endunless;
enddefine;

define syntax ml_eqtype;
    lvars   tycon;
    dlocal  % save_env() %;
    external_type(true) -> tycon;
    unless popstrname then
        sysPUSHQ(tycon);
        sysCALL(MLWID(globalise));
    endunless;
enddefine;

define syntax ml_exception;
    lvars   exn;
    dlocal  % save_env() %;
    external_exception() -> exn;
    unless popstrname then
        sysPUSHQ(exn);
        sysCALL(MLWID(globalise));
    endunless;
enddefine;

define syntax ml_val;
    lvars   var;
    dlocal  % save_env() %;
    external_val() -> var;
    unless popstrname then
        sysPUSHQ(var);
        sysCALL(MLWID(globalise));
    endunless;
enddefine;


/*
 *  Accessing ML values and exceptions from POP-11
 */

;;; ml_valof:
;;;     gets the value of a variable from the ML environment. The argument
;;;     -wrap- indicates whether functions of more than 1 argument or which
;;;     take tuples as arguments should be returned as wrapped or unwrapped
;;;     values. Returns "undef" if the variable is not defined.

define ml_valof(id);
    lvars id, wrap = false, val;
    if isboolean(id) then ((), id) -> (id, wrap) endif;
    idpath(id) -> id;
    returnunless(lookup_val(id, false) ->> val)(undef);
    val_value(val, wrap);
enddefine;

;;; ml_wrapped_valof:
;;;     get the wrapped value of a variable.

define ml_wrapped_valof =
    ml_valof(% true %);
enddefine;

;;; ml_curried_valof:
;;;     same as -ml_wrapped_valof-, for backward compatibility.

define ml_curried_valof =
    ml_valof(% true %);
enddefine;


/*
 *  Raising and handling ML exceptions
 */

;;; ml_raise:
;;;     raise an exception packet.

define ml_raise(/* exn */) with_nargs 1;
    raise(/* exn */);
enddefine;

;;; ml_handle:
;;;     handle an exception packet.
;;;     -expr- is a procedure of no arguments returning one result: this
;;;     evaluates the expression in which the exception may arise.
;;;     -exn- is an exception value obtained by -ml_valof-.
;;;     -handler- is a procedure of one argument returning one result: it
;;;     is called on the value of the exception packet if -exn- is raised
;;;     by -expr-.

define ml_handle(/* expr, */ exn, handler) with_nargs 3;
    lvars exn, handler, p;

    define lconstant do_expr(expr);
        lvars   expr;
        dlocal  exn_stacklength = stacklength(), exn_pdr = do_expr;
        ;;; evaluate -expr- and return <true> to indicate success
        expr(), true;
    enddefine;

    unless do_expr(/* expr */) then
        ;;; exception raised: packet should be on the stack
        -> p;
        if isclosure(exn) and pdpart(exn) == conspacket then
            frozval(1, exn) -> exn;
        else
            packet_id(exn) -> exn;
        endif;
        if packet_id(p) == exn then
            if pdnargs(handler) fi_> 0 then packet_value(p) endif,
            chain(handler);
        else
            raise(p);
        endif;
    endunless;
enddefine;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Apr 26 1996
        Added conditional definition of end as a syntax word, as alternative
        to ml_endstructure
--- Robert John Duncan, Dec 20 1994
        New treatment of identifiers and wrapped values: wrappers are now
        generated on demand by val_value.
--- Robert John Duncan, Nov 24 1994
        Sectionised. Changed conforms to check for vectors and arrays.
--- Robert John Duncan, Mar  1 1991
        Changed -external_X- procedures to read all items from -proglist-:
        the ML parsing routines use -new_pop_itemiser- to simulate ML
        itemisation (cf. changes in "parse.p").
        Procedure -idpath- now defined in "lexitems.p".
--- Robert John Duncan, Feb 11 1991
        Exceptions, constructors and variables now represented by a common
        "val" record.
--- Robert John Duncan, Feb  4 1991
        Changes for new environment interface.
--- Robert John Duncan, Jan 21 1991
        Changed -ml_handle- to comply with new exception protocol.
--- Simon Nichols, Jun 21 1990
        Changes to support optimisation of tupled functions. The changes
        affect -conforms- and -assign_external_val-. The latter makes use
        of a new procedure, -tupled_fun-. The flag to -ml_valof- is now
        interpreted as meaning wrapper/unwrapped.
--- Rob Duncan, Apr 19 1990
        Simplified -ml_valof- to take an optional curry/uncurry flag.
        Fixed -ml_handle- not to do -erasenum- (now done by -raise- itself)
--- Rob Duncan, Jan 31 1990
        Moved out tuple construction and access functions to "data.p" and
        added an "ml_" prefix.
        Added "instream" and "outstream" types to -conforms-.
--- Rob Duncan, Nov  8 1989
        Made definition of -ml_raise- explicit so that it's properly exported
 */
