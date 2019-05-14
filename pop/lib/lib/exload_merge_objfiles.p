/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/lib/exload_merge_objfiles.p
 > Purpose:         Merging object-file lists from exloads
 >                  -- used by exload and poplink
 > Author:          John Gibson, May  7 1993 (see revisions)
 */
compile_mode :pop11 +strict;

section;

include sysdefs.ph;

lconstant libdir_chars = 'LR';

define exload_merge_objfiles(existing, new, xtype_p);
    lvars existing, new, xtype_p, xtype, xversion, libs;

    define lconstant structure_new(objfiles);
        lvars l, obj, objfiles;
        for l on objfiles do
            hd(l) -> obj;
            if isident(obj) then
                ;;; dereference identifier
                if islist(idval(obj) ->> obj) then
                    structure_new(obj);
                    nextloop
                endif
            endif;
            unless isstring(obj) or isword(obj) then
                mishap(obj, 1, 'INVALID ITEM IN EXTERNAL OBJECT FILE LIST');
            elseif pop_pas_mode == "popc" then
                ;;; don't interpret anything else now -- done at poplink stage
                obj
            elseif isstartstring('==', obj) then
                ;;; external env var
                valof("sys_translate_exlibs")(allbutfirst(2,obj))
                                            -> (xtype, xversion, libs);
                if xtype_p then xtype_p(xtype, xversion, libs) -> libs endif;
                structure_new(libs)
            elseif datalength(obj) >= 2
            and obj(1) == `-` and strmember(obj(2), libdir_chars)
            then
#_IF DEF HPUX
                if datalength(obj) == 2 then
                    obj <> hd(tl(l)->>l) -> obj
                endif;
#_ENDIF
                consvector(obj, 1)
            else
                obj
            endunless
        endfor
    enddefine;

    define lconstant merge_objfiles(A, B);
        lvars a, b, Atl, Btl, a_<_b, b_<_a, A, B, ap, bp;

        define lconstant prec(obj);
            lvars obj;
            if isvector(obj) then
                2       ;;; Unix library dir
#_IF DEF UNIX
            elseif isstartstring('-l', obj) then
#_ELSE
            elseif issubstring('/lib', obj) and not(issubstring('/inc', obj))
            then
#_ENDIF
                3       ;;; library
            else
                1       ;;; assume ordinary obj (but could be Unix lib)
            endif
        enddefine;

        if pop_pas_mode == "popc" and A /== [] and B /== [] then
            ;;; merging is left till poplink stage -- just concatenate
            ;;; lists, but separating them with marker string '-|'
            return(dl(A), '-|', dl(B))
        endif;

        repeat
            returnif(A == []) (dl(B));
            returnif(B == []) (dl(A));

            dest(A) -> (a, Atl);
            dest(B) -> (b, Btl);
            if a = b then
                a, Atl -> A, Btl -> B
            else
                member(a, Btl) -> b_<_a;
                member(b, Atl) -> a_<_b;
                if a_<_b then
                    if b_<_a then
                        ;;; list order contradictory -- use prec
                        prec(a) -> ap, prec(b) -> bp;
                        if ap fi_< bp then
                            a, fast_ncdelete(b, Atl) -> A
                        elseif bp fi_< ap then
                            b, fast_ncdelete(a, Btl) -> B
                        else
                            Atl -> A, Btl -> B  ;;; leave them till later
                        endif
                    else
                        a, Atl -> A
                    endif
                elseif b_<_a or prec(b) fi_< prec(a) then
                    b, Btl -> B
                else
                    a, Atl -> A
                endif
            endif
        endrepeat
    enddefine;

    [% structure_new(new) %] -> new;
    [% merge_objfiles(existing, new) %];    ;;; the result
    sys_grbg_list(new)
enddefine;

define exload_flatten_objfiles(objfiles);
    lvars obj, objfiles;
    [%  fast_for obj in objfiles do
            if isvector(obj) then
                fast_subscrv(1,obj) -> obj;
#_IF DEF HPUX
                allbutlast(datalength(obj)-2, obj);
                allbutfirst(2, obj) -> obj;
#_ENDIF
            endif;
            obj
        endfor
    %]
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul 19 1995
        Changed to recognise any item beginning with `-` followed by
        a char in libdir_chars as a lib directory argument
--- John Gibson, Nov 25 1993
        Made structure_new do an explicit check for non-ident items being
        strings or words
--- John Gibson, Aug 16 1993
        Test for Popc now pop_pas_mode == "popc"
 */
