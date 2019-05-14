/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/define_prolog.p
 > Purpose:         Define form for Prolog continuation-passing procedures
 > Author:          Robert Duncan, Apr 27 1993
 > Documentation:   HELP * DEFINE_PROLOG
 > Related Files:   LIB * PROLOG
 */

compile_mode:pop11 +strict;

uses-by_name ( $-prolog$-converter_from_contn );

/***********************************************************************

 Synopsis:

    define:prolog <declaration> <name>/<arity>(<args>);
        <procedure-body>
    enddefine;

 ***********************************************************************/

section;

define:define_form prolog;
    lvars item = readitem();
    ;;; read declaration -- allow [global|nonglobal] [vars|constant]
    lvars (globl_p, decl_p) = (false, false);
    if item == "global" then
        (sysGLOBAL, readitem()) -> (globl_p, item);
    elseif item == "nonglobal" then
        (sysGLOBAL(%false%), readitem()) -> (globl_p, item);
    endif;
    if item == "vars" then
        (sysVARS, readitem()) -> (decl_p, item);
    elseif item == "constant" then
        (sysCONSTANT, readitem()) -> (decl_p, item);
    elseif lmember(item, #_< [lvars dlvars lconstant] >_#) then
        mishap(item, 1, 'ILLEGAL DECLARATION FOR PROLOG PREDICATE');
    endif;
    if item == "procedure" then
        ;;; always a procedure, so ignore it
        readitem() -> item;
    endif;
    ;;; read predicate name -- fn/arity
    lvars (fn, arity) = (item, 0);
    unless isword(fn) then
        ;;; allow a string for symbolic names
        consword(fn) -> fn;
    endunless;
    pop11_need_nextreaditem("/") -> ;
    readitem() -> arity;
    unless isinteger(arity) and arity fi_>= 0 then
        mishap(arity, 1, 'INTEGER >= 0 NEEDED');
    endunless;
    ;;; declare the predicate
    lvars name = consword(#| explode(fn), `/`, dest_characters(arity) |#);
    pop11_define_declare(name, globl_p, decl_p, "procedure");
    ;;; compile the base procedure
    lvars proc = pop11_comp_procedure(#_< [enddefine {end}] >_#, name, name);
    ;;; emit code to create the Prolog closure
    define lconstant make_prolog_closure(proc, arity) -> clos;
        lvars proc, arity, clos;
        writeable sys_current_val("ident $-prolog$-converter_from_contn")
                                                    (% proc, arity %) -> clos;
        pdprops(proc) -> pdprops(clos);
        arity -> pdnargs(clos);
    enddefine;
    sysPUSHQ(proc), sysPUSHQ(arity), sysCALLQ(make_prolog_closure),
        sysPOP(name);
enddefine;

endsection;     /* $- */
