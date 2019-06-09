/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptLoadProcedures.p
 > Purpose:         Loading external X toolkit functions
 > Author:          Roger Evans, Oct 19th 1990 (see revisions)
 > Documentation:
 > Related Files:   C.x/x/pop/lib/fast_xt*
 */
compile_mode:pop11 +strict;

section;

uses exload;

define global macro XptLoadProcedures;
    lvars   procs, item, mark, objfiles, declarator, attributes, pair,
            some_typespecs = false, prefix = 'raw_';
    dlocal  pop_autoload = false;

    define lconstant try_typespecread() -> specitems;
        lvars specitems;
        proglist_read_by(false, true, $-typespec_utils$-read_typespec)
                                        -> specitems -> (,,);
        if specitems /== [] then true -> some_typespecs endif
    enddefine;

    $-typespec_utils$-get_exload_initial("XLINK_EXLIBS", true)
                                -> (mark, objfiles, declarator, attributes);
    if lmember("prefix", attributes) ->> item then
        if (hd(tl(item)) ->> prefix) == "," or prefix == ")" then
            nullstring -> prefix    ;;; assume it's no prefix
        endif
    else
        if attributes == [] then
            [(prefix ^prefix)]
        else
            [(prefix ^prefix,] nc_<> tl(attributes)
        endif -> attributes
    endif;

    [%  repeat
            itemread() -> item;
            quitif(item == ";");
            if item == termin then mishap(0,'UNEXPECTED END OF INPUT'); endif;
            if item /== "," then
                conspair(item, try_typespecread())
            endif;
        endrepeat
    %] -> procs;

    [   exload ^mark ^objfiles ^^attributes
        %   if declarator then declarator endif,
            for pair in procs do
                front(pair); "="; "#"; "XptImportProcedure"; ",";
            endfor
        %
        endexload;
        %   if some_typespecs then
                if isstring(prefix) then consword(prefix) -> prefix endif;
                if not(declarator)
                or declarator == "constant" or declarator == "vars" then
                    "p_typespec"
                else
                    "l_typespec"
                endif;
                for pair in procs do
                    if back(pair) /== [] then
                        prefix <> front(pair), dl(back(pair)), ","
                    endif
                endfor;
                ";"
            endif
        %
    ] nc_<> proglist -> proglist;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, May 10 1993
        Changed to use XLINK_EXLIBS instead of XTB*ASELIBS
--- John Gibson, Mar 27 1993
        Uses valof on XTB*ASELIBS instead of getting the value when this
        file is compiled. Also uses get_exload_initial.
--- John Gibson, Aug 26 1992
        Changed to allow <typespec> following any name (like exload)
 */
