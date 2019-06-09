/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/src/xt_descriptor.p
 > Purpose:         Support for X toolkit descriptors
 > Author:          Roger Evans, May 24 1990 (see revisions)
 > Documentation:   REF xtoolkit
 > Related Files:   xt_*.p
 */

#_INCLUDE 'xt_declare.ph'
#_INCLUDE '../../src/gctypes.ph'


section $-Sys$-Xt =>
                        isXptDescriptor
                        XptDescriptor
                        XptRegister
                        consXptDescriptor
                        freeXptDescriptor
                        XptDescriptor_key,
                        XptDescriptor_apply,
;

;;; -- Locating descriptors ------------------------------------------

;;; an (expandable) property mapping from ptr value to XptDescriptor
;;; temporary on value, so won't keep descriptors alive
;;; NOTE: This is used in C.x/x/src/xt_display.p
constant
    DescriptorProp = newanyproperty([], 64,1,50, false, false,
                                    "tmpval", false, false);

;;; return descriptor associated with ptr, and do a type check if required
define Descriptor(type, _ptr) -> _ptr;
    lvars type _ptr;
    DescriptorProp(_pint(_ptr)) -> _ptr;
    if type and _ptr then
        unless fast_XptDataType(_ptr) == type then
            false -> _ptr;
        endunless;
    endif;
enddefine;

;;; NB: updater doesn't have type argument!
define updaterof Descriptor(desc,_eptr);
    lvars desc, _eptr;
    desc -> DescriptorProp(_pint(_eptr));
enddefine;

;;; return descriptor given exptclass record - with fast check for
;;; descriptor itself
define Get_Descriptor(type,desc);
    lvars type desc;
    if  iscompound(desc) and desc!KEY == XptDescriptor_key then
        if type and fast_XptDataType(desc) /== type then
            false;
        else
            desc;
        endif;
    else
        Descriptor(type,Checkr_exptrclass_ptr(desc));
    endif;
enddefine;

;;; public routine - args in sensible order for closures!
define XptDescriptor(desc,type);
    lvars desc type;
    Get_Descriptor(type,desc);
enddefine;


;;; -- Preferred representations ----------------------------------

;;; a property mapping from preferred reps back to descriptors,
;;; so users don't have to keep hold of their descriptors
lconstant PreferredProp = newproperty([], 64, false, "tmparg");

;;; return preferred representation of a descriptor if any
define Register(desc);
    lvars desc;
    desc!XD_PREFERRED or desc;
enddefine;

;;; user procedure for accessing/setting preferred representation
define XptRegister(ptr);
    lvars ptr, pref;
    Get_Descriptor(false, ptr) -> ptr;
    if ptr then
        Register(ptr)
    else
        false
    endif;
enddefine;

define updaterof XptRegister(desc,rec);
    lvars ptr, rec, desc;
    if desc and Checkr_exptrclass_ptr(desc) /== Checkr_exptrclass_ptr(rec) then
        mishap(desc,rec,2,'INCOMPATIBLE STRUCTURES FOR REGISTRATION');
    endif;
    Get_Descriptor(false,rec) -> ptr;
    if ptr then
        ;;; clear association with old preferred value
        false -> PreferredProp(ptr!XD_PREFERRED);
        ;;; add association for new
        if ptr == desc then
            false   ;;; putting in descriptor itself is same as clearing
        else
            ;;; hang on to system descriptor as long as user desc alive
            ptr -> PreferredProp(desc);
            desc;
        endif -> ptr!XD_PREFERRED;
    else
        mishap(desc,rec,2,'NO XptDescriptor FOR REGISTRATION');
    endif
enddefine;

;;; -- Creating and destroying descriptors ------------------------------

/*  we maintain a list of free descriptors, plus a table of lists
    of vectors used for dependency fields */
lvars free_descriptors = undef;
lvars free_depvectors = writeable initv(XD_FREELIST_TABLE_SIZE);

define XptClearFreelists;
    lvars i;
    undef -> free_descriptors;
    for i from 1 to XD_FREELIST_TABLE_SIZE do
        undef -> fast_subscrv(i,free_depvectors);
    endfor;
enddefine;

;;; creator (for system use)
;;; sets XD_DEPENDENTS to a vector of length deplen, or false
;;; tries to use freelist of descriptors with assvectors
define Cons_XptDescriptor(_eptr, type, props, deplen) /* -> _desc */;
    lvars free, _desc, props, _eptr, type, deplen;

    ;;; obtain a vector for dependencies if required
    if deplen then
        fast_subscrv(deplen,free_depvectors) -> free;
        if free == undef then
            initv(deplen)
        else
            free;
            fast_subscrv(1,free) -> fast_subscrv(deplen,free_depvectors);
        endif -> deplen;
    endif;

    ;;; build the props field from type and props (if any)
    type; if props then conspair((), props) endif -> props;

    ;;; fetch descriptor last so that there's no risk of a GC before
    ;;; we manage to fill in its fields.
    free_descriptors -> free;
    if free == undef then
        ;;; got to make one
        Get_store(@@(struct XDESCRIPTOR)++) -> _desc;
        XptDescriptor_key -> _desc!KEY;
    else
        free -> _desc;
        ;;; freelists are chained on XD_DEPENDENTS field
        free!XD_DEPENDENTS -> free_descriptors;
    endif;

    ;;; fill in fields
    props -> _desc!XP_PROPS;
    _eptr -> _desc!XP_PTR;
    deplen -> _desc!XD_DEPENDENTS;
    false -> _desc!XD_PREFERRED;

    ;;; register it in reverse mapping and return descriptor on stack
    ;;; (N.B. MUST return _desc on stack BEFORE calling Descriptor,
    ;;; because that can cause a GC which would invalidate _desc)
    _desc;

    ;;; Don't assign to Descriptor for a widget being destroyed -- it
    ;;; leaves junk entries in the property
    unless type == XDT_WIDGET and _nonzero(_eptr!XRW_being_destroyed) then
        _desc -> Descriptor(_eptr);
    endunless
enddefine;

;;; kill (ie make invalid) a descriptor (for system use)
define Kill_XptDescriptor(desc);
    lvars desc;
    false -> Descriptor(desc!XP_PTR);
    _NULL -> desc!XP_PTR;
    if desc!XD_PREFERRED ->> desc then
        ;;; kill registered representation (if any) too
        _NULL -> desc!XP_PTR;
    endif;
enddefine;

;;; public routines
define consXptDescriptor(exptr,type) -> rec;
    lvars exptr type rec;
    Cons_XptDescriptor(Checkr_exptrclass_ptr(exptr),type,false,false) -> rec;
    Kill_XptDescriptor -> Sys_destroy_action(rec,false);
enddefine;

;;; a value for marking free descriptors (for printer purposes)
lconstant free_mark = '(FREE) ';

define freeXptDescriptor(desc);
    lvars i desc dep len;
    unless isXptDescriptor(desc) then
        mishap(desc,1,'XptDescriptor NEEDED');
    endunless;
    false -> Descriptor(desc!XP_PTR);
    _NULL -> desc!XP_PTR;
    if (desc!XD_DEPENDENTS ->> dep) then
        if isvector(dep)
        and (_pint(dep!V_LENGTH) ->> len) fi_<= XD_FREELIST_TABLE_SIZE then
            ;;; clear vector
            for i from 2 to len do
                undef -> fast_subscrv(i,dep);
            endfor;
            ;;; add to appropriate freelist;
            fast_subscrv(len,free_depvectors) -> fast_subscrv(1,dep);
            dep -> fast_subscrv(len,free_depvectors);
        endif;
    endif;
    false -> desc!XP_PROPS;
    free_mark -> desc!XD_PREFERRED;
    free_descriptors -> desc!XD_DEPENDENTS;
    desc -> free_descriptors;
enddefine;

;;; --- XptDescriptor KEY ----------------------------------------------


;;; testing for descriptors
define isXptDescriptor(_desc);
    lvars _desc;
    iscompound(_desc) and _desc!KEY == XptDescriptor_key;
enddefine;

;;; equality (single =) check
define lconstant Eq__XptDescriptor(_item, _exptr);
    lvars _item, _exptr;
    iscompound(_item) and _item!KEY == XptDescriptor_key
    and _item!XP_PTR == _exptr!XP_PTR
    and _item!XP_PROPS = _exptr!XP_PROPS
    and _item!XD_DEPENDENTS = _exptr!XD_DEPENDENTS
    and _item!XD_PREFERRED = _exptr!XD_PREFERRED
enddefine;

;;; printer - print type and other props data if any
define lconstant XptDescriptor_print(desc);
    lvars desc d;

    cucharout(`<`);
    if desc!XD_PREFERRED == free_mark then appdata(free_mark,cucharout)
    elseif desc!XP_PTR == _NULL then appdata('(NULL) ',cucharout);
    endif;
    desc!XP_PROPS -> d;
    if d then
        if ispair(d) then
            pr(d!P_FRONT);
            if d!P_BACK and pop_pr_level /== 0 then
                cucharout(` `);
                pr(d!P_BACK);
            endif;
        else
            pr(d);
        endif;
    else
        appdata('XptDescriptor',cucharout);
    endif;
    cucharout(`>`);
enddefine;

lvars apply_cache = [];
define lconstant Descriptor_apply_proc(desc) -> proc;
    lvars type, name, proc desc;
    XptDataType(desc) -> type;
    if (fast_lmember(type,apply_cache)->>proc) then
        valof(proc!P_BACK!P_FRONT) -> proc;
    else
        "Xpt" <> type <> "Apply" -> name; ;;; eg XptWidgetApply
        if testdef sys_autoload then weakref sys_autoload(name) -> endif;
        if isdefined(name) and isprocedure(valof(name) ->> proc) then
            conspair(type,conspair(name,apply_cache)) -> apply_cache;
        else
            mishap(desc, 1,'INVALID DATATYPE FOR XptDescriptor CLASS APPLY');
        endif;
    endif;
enddefine;

define XptDescriptor_apply(desc);
    lvars desc;
    fast_chain(desc,Descriptor_apply_proc(desc));
enddefine;

define updaterof XptDescriptor_apply(desc);
    lvars desc;
    -> (Descriptor_apply_proc(desc))(desc);
enddefine;


global constant
    XptDescriptor_key = struct KEY_R_NAFULL =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL_RECORD
            _biset _:M_K_NONWRITEABLE
            _biset _:M_K_COPY
            _biset _:M_K_EXTERN_PTR
            _biset _:M_K_EXTERN_PTR_PROPS,
                                ;;; K_FLAGS
        _:GCTYPE_NFULLREC,      ;;; K_GC_TYPE
        Record_getsize,         ;;; K_GET_SIZE

        "XptDescriptor",        ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        isXptDescriptor,        ;;; K_RECOGNISER
        WREF XptDescriptor_apply,;;; K_APPLY
        Eq__XptDescriptor,      ;;; K_SYS_EQUALS
        WREF Eq__XptDescriptor, ;;; K_EQUALS
        XptDescriptor_print,    ;;; K_SYS_PRINT
        WREF XptDescriptor_print,  ;;; K_PRINT
        WREF Extern_ptr_hash,   ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_DEREF,    ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        @@(struct XDESCRIPTOR)++,
                                ;;; K_RECSIZE_R
        false,                  ;;; K_CONS_R
        false,                  ;;; K_DEST_R
        false,                  ;;; K_ACCESS_R

        @@(int)[_3],            ;;; K_FULL_OFFS_SIZE
        =>> {% @@XP_PROPS,      ;;; K_FULL_OFFS_TAB[_3]
               @@XD_DEPENDENTS,
               @@XD_PREFERRED
            %},
        %};

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 23 1995
        Removed EXPTR*_NEEDS_DALIGN stuff
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Mar 18 1995
        Added EXPTR*_NEEDS_DALIGN #_IFs
--- John Gibson, Jun  9 1993
        Changed Cons_XptDescriptor to not assign to Descriptor for a widget
        being destroyed
--- Robert Duncan, May 12 1993
        Changed Descriptor to encode external pointers with _pint rather
        than Uint_->_pint to avoid the creation of bigintegers (which
        aren't unique): this assumes that any information lost from the
        high bits of the pointer isn't significant for the table lookup.
--- John Gibson, May  3 1993
        weakref'ed sys_autoload in XptDescriptor_apply
--- John Gibson, Dec 11 1992
        Removed declarations for things from src
--- Adrian Howard, Nov 26 1992
        DestroyProp no longer lexical so it can be used in xt_display.p
--- John Gibson, Mar  5 1992
        Fixed PreferredProp to be "tmparg" instead of "tmpval" (a fairly
        serious bug ...)
--- John Gibson, Nov 28 1991
        Fixed major bug in -Cons_XptDescriptor- where nonpop var _desc was
        being returned (as a formal result) AFTER calling the updater of
        -Descriptor- (which can cause a GC and invalidate the pointer in
        _desc).
        Fixed similar bug in updater of XptRegister where nonpop _ptr was being
        used across a call to the updater of PreferredProp.
--- Jonathan Meyer, Sep  2 1991
        Made descriptor property expandable to cope better with times
        when hundreds of widgets are being created/destroyed.
--- Roger Evans, Aug  1 1991 fixed cache bug in Descriptor_apply_proc
--- Roger Evans, Jul  2 1991 added PreferredProp
--- Roger Evans, Jul  1 1991 altered chaining and printing of free descriptors
--- Roger Evans, Jun 28 1991 removed RegisterDesc, added freelists
--- Roger Evans, Jun 27 1991 added XptDescriptor_apply
--- Roger Evans, Jan 30 1991 added RegisterDesc
--- Roger Evans, Dec  5 1990 exported XptDescriptor_key
--- Roger Evans, Nov 19 1990 added consXptDescriptor
--- Roger Evans, Nov  4 1990 added XptDescriptor
--- Roger Evans, Oct 23 1990 changed 'dead' to 'NULL' in print form
--- Roger Evans, Oct 11 1990 Much revised
--- Roger Evans, Jun 22 1990  added XptDescriptorMap etc.
 */
