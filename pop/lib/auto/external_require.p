/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/auto/external_require.p
 > Purpose:
 > Author:          Ian Rogers, Oct  5 1989 (see revisions)
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

/*****

    ***** Obsolete. Use exload_batch (see REF * EXLOAD_BATCH) ******

 *****/

section $-external => external_require_mark
                      pr_external_require_load_warn
                      sys_pr_external_require_load_warn
                      external_require_load
                      external_require
    ;

lvars
    allmarks = [],
    allobjs = [],
    allsymbols = [],
    ;

define active external_require_mark;
    allmarks;
enddefine;

define global constant procedure sys_pr_external_require_load_warn with_nargs 1;
    lconstant printfargs = [0];
    -> fast_front(printfargs);
    printf(';;; EXTERNAL-LOADING %p\n', printfargs)
enddefine;

define pr_external_require_load_warn = erase(%%); enddefine;

define external_require_load;
    pr_external_require_load_warn(allmarks);
    external_load(allmarks, allobjs, allsymbols);
    [] ->> allmarks ->> allobjs -> allsymbols;
enddefine;

define load_then_do_me(name);
lvars name
    ;
    external_require_load();
    chain(idval(name));
enddefine;

define external_require(mark, objfiles, symbol_list);
lvars   mark objfiles symbol_list spec item w
        pdrtype = true,
    ;
    allmarks nc_<> [^mark] -> allmarks;
    allobjs nc_<>
        [%  for spec in objfiles do
                unless member(spec, allobjs) do spec endunless;
            endfor;
         %]-> allobjs;
    allsymbols nc_<> [{symtrans identfn} {type procedure}] <> symbol_list
        -> allsymbols;
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
            if w == "type" then
                item == "procedure" -> pdrtype
            endif;
            nextloop
        elseif islist(spec) and length(spec) == 2 then
            ;;; symbol and identifier to assign to
            front(back(spec)) -> spec
        elseif isstring(spec) then
            consword(spec) -> spec
        elseif isword(spec) then
            /* nothing */
        else
            mishap(spec, 1, 'INVALID ITEM IN SYMBOL SPEC LIST')
        endif;
        if identprops(spec) == "undef" then
            sysSYNTAX(spec, 0, false);
            sysGLOBAL(spec);
        endif;
        if pdrtype then
            load_then_do_me(%identof(spec)%) -> valof(spec);
        else
            consexternal_ptr() -> valof(spec);
        endif
    endfor;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, May 31 1991
        Added call of sysGLOBAL
--- Ian Rogers, Jan 29 1991
    Added "if pdrtype..." test, and the else clause, around the
    -load_then_do_me- part-apply
 */
