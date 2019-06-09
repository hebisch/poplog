/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptPopLoadProcedures.p
 > Purpose:         Loading external X toolkit functions as pop procedures
 > Author:          John Gibson, Mar 29 1993 (see revisions)
 > Documentation:   REF * XT_LIBS
 */
compile_mode:pop11 +strict;

section;

uses exload;
uses-by_name XptPopImportProcedure;

define macro XptPopLoadProcedures;
    lvars procs, item, mark, objfiles, declarator, attributes, p;

    lconstant call_p_prop = newanyproperty([], 4, false, false, syshash,
                                            nonop =, "tmpval", false, false);
    dlocal pop_autoload = false;

    define lconstant get_call_p(name) -> (call_p, nargs);
        lvars name, call_spec, call_p, nargs;
        $-typespec_utils$-read_typespec(false, false) -> (call_spec, , );
        unless isvector(call_spec) then
            mishap(name, 1, 'XptPopLoadProcedures: NOT A FUNCTION TYPESPEC')
        endunless;
        unless call_p_prop(call_spec) ->> call_p then
            cons_access(true, call_spec, false, 1) ->> call_p
                        -> call_p_prop(copy(call_spec))
        endunless;
        front(subscrv(1,call_spec)) -> nargs;
    enddefine;

    $-typespec_utils$-get_exload_initial("XLINK_EXLIBS", true)
                                -> (mark, objfiles, declarator, attributes);

    [%  repeat
            itemread() -> item;
            quitif(item == ";");
            if item == termin then
                mishap(0, 'UNEXPECTED END OF INPUT')
            elseif item /== "," then
                ;;; Use the 3-element closure of XptPopImportProcedure itself
                ;;; as the final procedure
                true -> pop_autoload;
                sys_current_val("XptPopImportProcedure") -> p;
                false -> pop_autoload;
                writeable p(%0, get_call_p(item)%) -> p;
                p -> frozval(1,p);
                item -> pdprops(p);
                false -> updater(p);    ;;; so POPC doesn't gen the updater
                ;;; giving this pair to exload means assign p to the
                ;;; identifier at compile-time, and make it a runtime action
                ;;; producing no result
                conspair(p, p);
                conspair((), if pop11_try_nextitem("<-") then
                                readitem()  ;;; external name
                             else
                                false
                             endif)
            endif;
        endrepeat
    %] -> procs;

    [   exload ^mark ^objfiles ^^attributes
        %   if declarator then declarator endif,
            for p in procs do
                destpair(p) -> (p, item);
                ;;; uses undocumented feature of exload allowing a
                ;;; pair to follow the idname -- see code
                "procedure", pdprops(front(p)), "#_<", p, ">_#",
                if item then "<-", item endif, ","
            endfor
        %
        endexload;
    ] nc_<> proglist -> proglist;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, May 10 1993
        Changed to use XLINK_EXLIBS instead of XTB*ASELIBS
 */
