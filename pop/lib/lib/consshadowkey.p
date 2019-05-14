/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/lib/consshadowkey.p
 > Purpose:         Construct a shadowclass key structure
 > Author:          Roger Evans, Nov 15 1990 (see revisions)
 > Documentation:   REF *SHADOWCLASS
 > Related Files:   LIB * SHADOWKEY_KEY, * SHADOWKEY_DATA, *SHADOWCLASS
 */
compile_mode:pop11 +strict;

section $-shadowkey => consshadowkey, shadow_construct;

lconstant names = [consshadowkey shadow_construct];
applist(names, sysunprotect);

include shadowkey.ph;

uses shadowkey_key;

/*
 * Number of fields in a record spec
 */
define lconstant Num_fields(spec);
    lvars spec;
    if spec == [] or spec == ">->" then
        0;
    elseif isword(spec) or isinteger(spec) then
        ;;; NORMAL FIELDS
        1;
    elseif islist(spec) then
        ;;; LIST OF SPECS
        Num_fields(spec.fast_front) + Num_fields(spec.fast_back);
    elseif isclosure(spec) then
        ;;; IMPLICIT-ACCESS/CONVERSION PROCS
        Num_fields(fast_frozval(1, spec));
    elseif ispair(spec) then
        ;;; ARRAYS
        1;
    elseif isref(spec) then
        Num_fields(spec.fast_cont);
    endif;
enddefine;

;;; property mapping shadowkey names onto dest procedures whose updaters are
;;; cons procedures (typically instances of 'Dest' defined below)
define shadow_construct =
    newproperty([],16,false,false)
enddefine;

;;; create the class procs for a key and install in the class_procs vector
define lconstant Create_class_procs(key, isrec);
    lvars key, isrec, vec = shk_procs(key),
        nargs = isrec and Num_fields(key.shk_spec) or false;

    /*  The pdparts of these closures must be perm constants for POPC.
        Also, for POPC to be able to handle nc_ versions of shadowclasses
        defined (usually lexically) in files other than the one defining the
        class proper, the closures mustn't have any sub-parts of the key in
        their frozvals.
    */
    shkP_Cons(%true,  key, isrec%)          -> fast_subscrv(SHK_CONS,vec);
    shkP_Cons(%false, key, isrec%)          -> fast_subscrv(SHK_NC_CONS,vec);
    if isrec then nargs else 2 endif ->> fast_subscrv(SHK_CONS,vec).pdnargs
        -> fast_subscrv(SHK_NC_CONS,vec).pdnargs;

    shkP_Dest(%true,  key, isrec%)          -> fast_subscrv(SHK_DEST,vec);
    shkP_Dest(%false, key, isrec%)          -> fast_subscrv(SHK_NC_DEST,vec);

    shkP_Subscr(%true,  key, isrec%)        -> fast_subscrv(SHK_SUBSCR,vec);
    shkP_Subscr(%false, key, isrec%)        -> fast_subscrv(SHK_NC_SUBSCR,vec);
    shkP_Refresh_subscr(%true, key, isrec%) -> fast_subscrv(SHK_RF_SUBSCR,vec);
    shkP_Refresh_subscr(%false, key, isrec%)-> fast_subscrv(SHK_NC_RF_SUBSCR,vec);

    shkP_Fill(%true,  key, isrec%)          -> fast_subscrv(SHK_FILL,vec);
    shkP_Fill(%false, key, isrec%)          -> fast_subscrv(SHK_NC_FILL,vec);
    if isrec then nargs+1 else 1 endif ->> fast_subscrv(SHK_FILL,vec).pdnargs
        -> fast_subscrv(SHK_NC_FILL,vec).pdnargs;

    shkP_Recog(%key%)                       -> fast_subscrv(SHK_RECOG,vec);

    shkP_Cons(%"init", key, isrec%)         -> fast_subscrv(SHK_INIT,vec);
    if isrec then 0 else 1 endif -> fast_subscrv(SHK_INIT,vec).pdnargs;

    shkP_Refresh(%key, isrec%)              -> fast_subscrv(SHK_REFRESH,vec);
    shkP_Subscr_check(%key%)                -> fast_subscrv(SHK_CHECK,vec);

    shkP_Import(%key, isrec%)               -> fast_subscrv(SHK_IMPORT,vec);
    unless isrec then 2 -> fast_subscrv(SHK_IMPORT,vec).pdnargs endunless;
enddefine;

define lconstant Byte_size(spec);
    lvars spec size;
    field_spec_info(spec) -> size ->;
    if size then
        if (size // 8 -> size) /== 0 then size fi_+ 1 -> size; endif;
        size;
    else
        0;
    endif;
enddefine;

define lconstant Compile_exaccs(spec) -> (exaccs, extsize) ;
    lvars spec, exaccs, extsize = false;

    define lconstant Exaccs(spec, specexacc);
        lvars spec subspec i l specexacc tspec subspecexaccs;

        define lconstant Exacc_rec(subspecs);
            lvars subspecs;
            unless specexacc then
                subspecs;
            else
                {%  subspecs;
                    specexacc;
                    spec and shadow_construct(spec);
                %};
            endunless;
        enddefine;

        if extsize then mishap(0,'EMBEDDED VARIABLE-SIZED FIELD'); endif;
        if ispair(spec) then
            ;;; bit 10 in cons_access mode arg means fixed exptrs for
            ;;; sub-structs only (i.e. where access procedure will be used
            ;;; internally)
            lconstant macro CA_MODE = 2:1e10 || 1;

            if islist(spec) then
                if hd(spec) == ">->" then tl(spec) else spec endif -> tspec;
                listlength(tspec) -> l;
                cons_access(
                    [%repeat l times; true; endrepeat%],
                    spec, false, CA_MODE) -> subspecexaccs;
                Exacc_rec(
                    consvector(#|
                        for i from 1 to l do
                            Exaccs(tspec(i),subspecexaccs(i));
                        endfor;
                    |#));
            else
                Exacc_rec(
                    conspair(
                        Exaccs(front(spec), cons_access(true,spec,false,CA_MODE)),
                        back(spec)));
                unless back(spec) then
                    conspair(front(spec),Byte_size(front(spec))) -> extsize;
                endunless;
            endif;
        elseunless specexacc then
            mishap(spec,1,'INVALID ARRAY SPEC - USE \'[]\' FORMAT');
        else
            ;;; code initialisation requirements in pdprops of spec proc
            if spec == "full" then undef
            elseif spec == "exptr" then #_< consexternal_ptr() >_#
            else false
            endif -> pdprops(specexacc);
            specexacc;
        endif;
    enddefine;

    Exaccs(spec,false) -> exaccs;
    if extsize then
        if islist(spec) then
            Byte_size([%dl(spec)->; front(extsize)%]) - back(extsize)
        else
            0
        endif -> front(extsize);
    else
        Byte_size(spec) -> extsize;
    endif;
enddefine;

define consshadowkey(name,spec,props) -> key;
    lvars name, spec, props, key, extsize, exaccs;

    check_word(name);

    if islist(props) and (lmember("props",props) ->> props)
    and back(props) /== [] then
        front(back(props)) -> props;
    endif;

    ;;; build exacc structure
    Compile_exaccs(spec) -> (exaccs, extsize);

    class_cons(shadowkey_key)(name, spec, props, exaccs, extsize,
                                initv(SHK_NUM_CLASS_PROCS),false) -> key;
    Create_class_procs(key,islist(spec));
    ;;; assign constructor for this spec
    fast_subscrv(SHK_NC_DEST,shk_procs(key)) -> shadow_construct(spec);
enddefine;

applist(names, sysprotect);

endsection;     /* $-shadowkey */



/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun 18 1993
        pdnargs of class procs now correct
--- John Gibson, Jun 11 1993
        Changed Compile_exaccs to use new cons_access mode bit (see
        comment in code)
--- John Gibson, May  5 1993
        Split off from lib shadowkey;
 */
