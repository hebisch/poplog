/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/auto/external_load.p
 > Purpose:         Old external load and external procedure datatype
 > Author:          John Gibson, Mar 23 1991 (see revisions)
 > Documentation:   REF *OBSOLETE
 */
compile_mode :pop11 +strict;

#_TERMIN_IF isdefined("isexternal_procedure") or DEF POPC_COMPILING

section;

lconstant expdr_key =
    conskey("external_procedure", [full exptr], {external_ptr nonwriteable});

define global isexternal_procedure =
    class_recognise(expdr_key)
enddefine;

define global external_apply(/*_nargs, */ result, expdr) with_nargs 3;
    lvars expdr, result, size;

    lconstant
        fast_dflt = cons_access([^true],[>-> dfloat],false,false)(1);

    unless result then
        exacc {(N)} expdr()
    elseif result == true then
        exacc {(N):int} expdr()
    elseif result == "decimal" then
        ;;; N.B. C-style result (for upward compatibility)
        exacc {(N):float} expdr()
    elseif result == "ddecimal" then
        exacc {(N):dfloat} expdr()
    elseif result == "external_ptr" then
        exacc {(N):exptr} expdr()
    elseif isvectorclass(result) or isrecordclass(result) then
        datasize(result) -> size;
        if size fi_>= 4 then
            exacc {(N):dfloat} expdr() -> fast_dflt(result)
        elseif size fi_>= 3 then
            exacc {(N):int} expdr() -> fast_subscrintvec(1,result)
        endif
    else
        mishap(result, 1, 'external_apply: INVALID STRUCTURE FOR RESULT')
    endunless
enddefine;

external_apply -> class_apply(expdr_key);

procedure(item, expdr);
    lvars item, expdr;
    isexternal_procedure(item) and fast_back(item) == fast_back(expdr)
endprocedure -> class_=(expdr_key);

procedure(expdr);
    lvars expdr, props;
    pr('<external_procedure');
    if external_ptr_props(expdr) ->> props then
        cucharout(`\s`), pr(props)
    endif;
    cucharout(`>`)
endprocedure -> class_print(expdr_key);

lconstant procedure cons_expdr = class_cons(expdr_key);

define global external_ptr_to_procedure(exptr);
    lvars exptr;
    if isexternal_procedure(exptr) then
        exptr
    else
        cons_expdr(external_ptr_props(exptr), exptr)
    endif
enddefine;
;;;
define updaterof external_ptr_to_procedure(expdr) -> expdr;
    lvars expdr;
    unless isexternal_procedure(expdr) then
        mishap(expdr, 1, 'EXTERNAL PROCEDURE NEEDED')
    endunless
enddefine;


define global external_load(mark, objfiles, symbol_list);
    lvars   w, item, spec, symbol_list, objfiles, type, symtrans, symbol,
            idname, mark, entry
        ;

    ;;; discard old optional memory alloc arg
    if isinteger(symbol_list) then
        objfiles -> symbol_list;
        mark -> objfiles;
        -> mark
    endif;

    ;;; process symbol specs
    [% "procedure" -> type;
        identfn -> symtrans;
        for spec in symbol_list do
            if isvector(spec) and datalength(spec) == 2
            and (explode(spec) -> item -> w;
                    w == "type"
                    and (isprocedure(item)
                         or lmember(item, [procedure pointer absolute]))
                 or w == "symtrans"
                    and (isword(item) or isprocedure(item))
                )
            then
                ;;; type or symbol translation specifier
                spec(2) -> if spec(1) == "type" then type else symtrans endif;
                nextloop
            elseif islist(spec) and length(spec) == 2 then
                ;;; symbol and identifier to assign to
                dl(spec) -> idname -> symbol;
                if isword(symbol) then symbol sys_>< '' -> symbol endif;
            elseif isstring(spec) then
                spec -> symbol; consword(spec) -> idname
            elseif isword(spec) then
                spec -> idname; spec sys_>< '' -> symbol
            else
                mishap(spec, 1, 'INVALID ITEM IN SYMBOL SPEC LIST')
            endif;
            unless isstring(symbol) then
                mishap(symbol, 1, 'INVALID EXTERNAL SYMBOL NAME');
            endunless;
            unless isassignable(idname) then
                mishap(idname, 1, 'INVALID RECIPIENT OF EXTERNAL VALUE');
            endunless;
            if isword(symtrans) then valof(symtrans) -> symtrans endif;
            {% symtrans(symbol), false, false, false, {%type, idname%} %}
        endfor
    %] -> symbol_list;

    external_do_load(mark, objfiles, symbol_list);

    ;;; get values of requested symbols and assign to identifiers
    fast_for spec in symbol_list do
        explode(explode(spec)) -> (symbol, , item, entry, (type, idname));
        if type == "absolute" then
            ;;; absolute
            exacc {^uint} item -> item
        else
            ;;; external procedure/pointer
            symbol -> external_ptr_props(item);
            if type == "procedure" then
                ;;; Replace exptr key with external procedure key.
                ;;; NOTE that this is NOT just an efficiency hack -- it's
                ;;; essential that the exptr records returned by
                ;;; -external_do_load- are maintained (because only they
                ;;; undergo external symbol relocation, etc).
                expdr_key -> fast_subscrv(0, item)
            elseif isprocedure(type) then
                ;;; run procedure with external_ptr as arg
                type(item) -> item
            endif
        endif;
        item -> fast_prop_entry_value(entry);   ;;; for history list
        if identprops(idname) == "undef" then
            sysSYNTAX(idname, 0, false);
            ;;; if -idname- is a POP-11 word, make it global
            ;;; (but it could be something else -- like a Lisp symbol)
            if isword(idname) then sysGLOBAL(idname) endif;
        endif;
        item -> valof(idname)
    endfor
enddefine;


endsection;



/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jun 10 1991
        Made sysGLOBAL conditional on -idname- being a word
--- John Gibson, May 31 1991
        Added SYSGLOBAL declaration for identifiers
 */
