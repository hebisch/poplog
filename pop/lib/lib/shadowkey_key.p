/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/lib/shadowkey_key.p
 > Purpose:         Run-time parts of a shadowkey
 > Author:          Roger Evans, Nov 15 1990 (see revisions)
 > Documentation:   REF * SHADOWCLASS
 > Related Files:   LIB * CONSSHADOWKEY, * SHADOWKEY_DATA, * SHADOWCLASS
 */
compile_mode:pop11 +strict;

section $-shadowkey => shadowkey_key, shadow_length, shadow_=, isshadow;

lconstant names = [shadowkey_key shadow_length shadow_= isshadow];
applist(names, sysunprotect);

include shadowkey.ph;


;;; --- shadow key structures -----------------------------------

    /*  Can't use defclass for this since it would define consshadowkey
        and we want this for the user interface procedure.
    */

lconstant
    Ref0 = consref(0),

    ;;;         name     spec   props  exaccs  extsize  procs  fldmode
    Key_spec = [full >-> full   full   full    full     full   full],
    Want_fast= [^false   ^false ^false ^Ref0   ^false   ^false ^false],
;

constant $-shadowkey_key = conskey("shadowkey", Key_spec);

constant procedure (
   (shk_name,       ;;; class name
    shk_spec,       ;;; spec of shadowed data
    shk_props,      ;;; props for instances
    shk_exaccs,     ;;; access info
    shk_extsize,    ;;; size (number of words in ext data)
    shk_procs,      ;;; vector of class procedures
    shk_fldmode)    ;;; slot for fldmode (shadowclass only)
                = explode(shadowkey_key),

    isshadowkey = class_recognise(shadowkey_key),
);

lconstant procedure (
    /* make a fast shk_exaccs for internal use */
    (,,,Fast_exaccs,,,)
            = explode(cons_access(Want_fast, Key_spec, false, false)),
);

;;; ------------------------------------------------------------

;;; shadowkey instance records
defclass lconstant shadowrec [external_ptr] {
        shr_props      :full,       ;;; external_ptr_props field
    >-> shr_exptr      :exptr,      ;;; external ptr to extvec field
        shr_uservec    :full,       ;;; contents in pop form
        shr_extvec     :full,       ;;; contents in external form
        shr_key        :full,       ;;; shadowkey for this instance
        shr_vsize      :full,       ;;; length of variable field if any
};

;;; class_print for shadow rec prints classname and uservec data
define lconstant shadowrec_print(rec);
    lvars rec;
    cucharout(`<`);
    appdata(shk_name(shr_key(rec)), cucharout);
    unless pop_pr_level == 0 do
        appdata(shr_uservec(rec),
                procedure(i); lvars i; cucharout(`\s`); pr(i); endprocedure);
    endunless;
    cucharout(`>`);
enddefine;

shadowrec_print -> class_print(shadowrec_key);

;;; subscriptor - run associated shadowkey subscriptor
define lconstant shadowrec_subscr(i,rec);
    lvars i rec vec;
    shk_procs(shr_key(rec)) -> vec;
    fast_subscrv(SHK_SUBSCR,vec)(fast_subscrv(SHK_CHECK,vec)(i,rec)),
enddefine;

define updaterof shadowrec_subscr(i,rec);
    lvars i rec vec;
    shk_procs(shr_key(rec)) -> vec;
    -> fast_subscrv(SHK_SUBSCR,vec)(fast_subscrv(SHK_CHECK,vec)(i,rec)),
enddefine;

shadowrec_subscr -> class_apply(shadowrec_key);

define shadow_length(rec);
    lvars rec;
    unless isshadowrec(rec) then
        mishap(rec,1,'SHADOWCLASS RECORD NEEDED');
    endunless;
    datalength(shr_uservec(rec));
enddefine;

define shadow_=(rec1, rec2);
    lvars rec1, rec2;

    returnunless(isshadowrec(rec1))(false);
    returnunless(isshadowrec(rec2))(false);

    shr_props(rec1) = shr_props(rec2) and
    shr_uservec(rec1) = shr_uservec(rec2) and
    shr_extvec(rec1) = shr_extvec(rec2) and
    shr_key(rec1) = shr_key(rec2)

enddefine;

define isshadow = isshadowrec(%%); enddefine;

shadow_= -> class_=(shadowrec_key);


;;; --- class procedures -----------------------------------------

define lconstant Recursive_explode(vec, shrec);
    lvars vec, shrec;
    define lconstant Recursive(vec);
        lvars vec;
        if isvector(vec) then
            appdata(vec,Recursive);
        else
            vec
        endif;
    enddefine;
    Recursive(vec);
    if shrec and (shr_vsize(shrec)->>shrec) then
        ;;; stack length of final variable-length field
        shrec;
    endif;
enddefine;

define lconstant Exacc(n, shrec, update, exaccs, isrec);
    lvars n, shrec, update, exaccs, isrec;

    define lconstant Do_exacc(i,item,uservec,rec,shrec,update);
        lvars i item uservec newvec size rec shrec update initval;
        if isprocedure(item) then
            if update then
                if update == "init" then
                    pdprops(item) -> initval;
                    if initval then
                        dup(initval);
                        updater(item);
                    else
                        ;;; external repn already initialised to 0, push
                        ;;; base exacc routine to pull in pop value
                        item
                    endif;
                else
                    dup();  ;;; copy value - it will be put in two places
                    updater(item);
                endif;
            else
                item
            endif(unless rec then i endunless, shrec);
                -> fast_subscrv(i,uservec);
        else
            ;;; run intermediate accessor
            fast_subscrv(2,item)(unless rec then i endunless,shrec) -> shrec;
            fast_subscrv(1,item) -> item;
            fast_subscrv(i,uservec) -> newvec;
            datalength(newvec) -> size;
            if isvector(item) then
                for i from size by -1 to 1 do
                    Do_exacc(i,fast_subscrv(i,item),newvec,true,shrec,update);
                endfor;
            else
                if update == true and not(back(item)) then
                    ;;; variable sized field will have count supplied:
                    ;;; erase it (assume size is correct - we could
                    ;;; compare and mishap...?)
                    ->;
                endif;
                front(item) -> item;
                for i from size by -1 to 1 do
                    Do_exacc(i,item,newvec,false,shrec,update);
                endfor;
            endif;
        endif;
    enddefine;

    Do_exacc(n,
             if isrec then fast_subscrv(n,exaccs)
             else fast_front(exaccs)
             endif,
             shr_uservec(shrec),
             isrec,
             shrec,
             update);
enddefine;

    /* Following shkP_ procedures must be perm constants for POPC */

define shkP_Recog(shrec, key);
    lvars shrec, key;
    isshadowrec(shrec) and shr_key(shrec) == key;
enddefine;

define shkP_Subscr(n, shrec, construct, key, isrec);
    lvars   item, n, proc, shrec, construct, key, isrec, sl, uservec,
            refresh, exaccs;

    fast_subscrv(n,shr_uservec(shrec)->>uservec) -> item;
    if isvector(item) then
        Fast_exaccs(key) -> exaccs;
        ;;; item may be a real vector or an embedded struct - find out which
        if isrec then fast_subscrv(n,exaccs)
        else fast_front(exaccs)
        endif -> proc;
        unless isvector(proc) then
            ;;; item is a real vector - just return it
            return(item);
        endunless;
        stacklength() -> sl;        ;;; save for consvector call below
        Recursive_explode(item, isrec and (n==datalength(uservec)) and shrec);
        if construct then
            ;;; fetch constructor proc from key's exacc data
            fast_subscrv(3,proc) -> proc;
            if proc then -> proc();                 ;;; run updater if found
            else consvector(stacklength() fi_- sl); ;;; return flat vector
            endif;
        endif;
    else
        item;
    endif;
enddefine;
;;;
define updaterof shkP_Subscr(n, shrec, construct, key, isrec);
    lvars   item, n, shrec, construct, key, isrec, proc,
            exaccs = Fast_exaccs(key);

    if construct then
        if isrec then fast_subscrv(n,exaccs) else fast_front(exaccs) endif
            -> item;
        unless isprocedure(item) then
            fast_subscrv(3,item) -> proc;
            if proc then proc();
            else destvector() ->;
            endif;
        endunless;
    endif;

    Exacc(n,shrec,true,exaccs,isrec);
enddefine;

define shkP_Refresh_subscr(n, shrec, construct, key, isrec);
    lvars n, shrec, construct, key, isrec;

    Exacc(n,shrec,false,Fast_exaccs(key),isrec);
    shkP_Subscr(n,shrec,construct,key,isrec);
enddefine;
;;;
updater(shkP_Subscr) -> updater(shkP_Refresh_subscr);

define shkP_Refresh(shrec, key, isrec) -> shrec;
    lvars i, size, shrec, key, isrec, exaccs = Fast_exaccs(key);
    datalength(shr_uservec(shrec)) -> size;
    for i from size by -1 to 1 do
        Exacc(i,shrec,false,exaccs,isrec);
    endfor;
enddefine;

define shkP_Fill(shrec, construct, key, isrec) -> shrec;
    lvars i, size, shrec, construct, key, isrec;
    datalength(shr_uservec(shrec)) -> size;
    for i from size by -1 to 1 do
        -> shkP_Subscr(i,shrec,construct,key,isrec);
    endfor;
enddefine;

define shkP_Cons(construct, key, isrec) -> shrec;
    lvars   i, extvec, uservec, shrec, item, size, proc, sl, construct,
            key, isrec, vsize = false, extsize = shk_extsize(key),
            exaccs = Fast_exaccs(key);

    lvars exptr = #_< writeable consexternal_ptr() >_#;

    define lconstant Do_init(i,item,uservec);
        lvars i item uservec newvec size;
        unless isprocedure(item) then
            fast_subscrv(1,item) -> item;
            if isvector(item) then datalength(item) -> size;
            elseunless back(item)->>size then vsize -> size;
            endif;
            writeable initv(size) ->> newvec -> fast_subscrv(i,uservec);
            if isvector(item) then
                for i from size by -1 to 1 do
                    Do_init(i,fast_subscrv(i,item),newvec);
                endfor;
            else
                front(item) -> item;
                for i from size by -1 to 1 do
                    Do_init(i,item,newvec);
                endfor;
            endif;
        endunless;
    enddefine;

    if isexternal_ptr_class(construct) then
        ;;; importing - ext data already present - may need size arg though
        construct ->> extvec -> exptr;
        if ispair(extsize) then -> vsize; endif;
    else
        ;;; next -writeable- not actually necessary for fixed ...
        writeable init_fixed(
            if ispair(extsize) then
                -> vsize;
                if isrec and construct /== "init" then
                    vsize;
                    if construct then  ;;; get size of final elt
                        fast_subscrv(datalength(exaccs),exaccs) -> item;
                        fast_subscrv(3,item) -> proc;
                        stacklength() -> sl;
                        if proc then proc(vsize)
                        else destvector(vsize) -> ;
                        endif -> vsize;
                        erasenum(stacklength() - sl);
                    endif;
                endif;
                vsize * back(extsize) + front(extsize)
            else
                extsize
            endif, string_key) -> extvec;
            fill_external_ptr(extvec,exptr) -> ;
    endif;

    #_< writeable initv(1) >_# -> uservec;
    #_< writeable initv(1) >_# -> item; exaccs -> fast_subscrv(1,item);
    Do_init(1,item,uservec);
    uservec(1) -> uservec;

    consshadowrec(shk_props(key), exptr, uservec, extvec, key, vsize)
                                            -> shrec;

    if construct == extvec then
        ;;; importing - just refresh from external data
        shkP_Refresh(shrec,key,isrec) ->;
    else
        datalength(uservec) -> size;
        if construct == "init" then
            ;;; initialize to default values
            for i from size by -1 to 1 do
                Exacc(i,shrec,"init",exaccs,isrec);
            endfor;
        else
            ;;; initialize from stack data
            for i from size by -1 to 1 do
                -> shkP_Subscr(i,shrec,construct,key,isrec);
            endfor;
        endif;
    endif;
enddefine;

define shkP_Dest(shrec, construct, key, isrec);
    lvars   i, shrec, construct, fsize, uservec, key, isrec, exaccs;

    shr_uservec(shrec) -> uservec;
    if construct then
        datalength(uservec) -> fsize;
        for i from 1 to fsize do
            shkP_Subscr(i,shrec,true,key,isrec);
        endfor;
        Fast_exaccs(key) -> exaccs;
        if ispair(exaccs) and not(back(exaccs)) then fsize endif;
    else
        Recursive_explode(uservec,shrec);
    endif;
enddefine;
;;;
shkP_Cons -> updater(shkP_Dest);  ;;; to make shkP_Dest ok as a shadow_construct proc

define shkP_Subscr_check(n, shrec, key) -> (n, shrec);
    lvars n, shrec, key;
    unless shr_key(shrec) == key then
        mishap(shrec,1,shk_name(key) sys_>< ' NEEDED');
    endunless;
    unless  n > 0 and n fi_<= datalength(shr_uservec(shrec)) then
        mishap(n,shrec,2, 'SUBSCRIPT OUT OF RANGE');
    endunless;
enddefine;

define shkP_Import(exptr, key, isrec);
    lvars exval, exptr, key, isrec, shrec, size = false;
    lconstant exptr_map =
        newanyproperty( [], 64, false, false,
                        syshash, sys_=, "tmpval",
                        false, false
        );
    lconstant index = writeable {%false, false, false%};

    if isinteger(exptr) then
        ;;; size arg - swap with real exptr
        exptr -> size; -> exptr;
    endif;

    exacc ^uint exptr -> exval;

    (size, key, exval) -> explode(index);

    if exptr_map(index) ->> shrec then
        ;;; refresh shadow (and return it)
        shkP_Refresh(shrec,key,isrec);
    else
        if size then size endif;  ;;; If size, stack for cons routine
        shkP_Cons(exptr,key,isrec) ->> exptr_map(copy(index));
    endif;
enddefine;
;;;
;;; updater does nothing - shrec is valid external representation
define updaterof shkP_Import(shrec, key, isrec) -> shrec;
    lvars shrec, key, isrec;
enddefine;

;;; used by shadowclass_field
define shkP_Field_swap(shrec,p,n);      ;;; must be perm const for POPC
    lvars shrec, p, n;
    fast_chain(n, shrec, p)
enddefine;

applist(names, sysprotect);

endsection;     /* $-shadowkey */



/* --- Revision History ---------------------------------------------------
--- John Gibson, May  5 1993
        Split off from lib shadowkey
 */
