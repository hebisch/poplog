/*  --- Copyright University of Sussex 1996.  All rights reserved. ---------
 > File:            C.all/lib/auto/newc_dec.p
 > Purpose:         Improved C interface for LIB EXTERNAL
 > Author:          Ian Rogers (see revisions)
 > Documentation:   HELP * EXTERNAL * NEWEXTERNAL, * NEWC_DEC
 >                  REF * EXTERNAL
 > Related Files:   LIB * EXTERNAL, * NEWEXTERNAL, * C_DEC
 >                  LIB * FORTRAN_DEC, * NEWEXTERNAL, REF * KEYS
 */

#_TERMIN_IF DEF POPC_COMPILING

#_TERMIN_IF DEF newc_dec

uses newexternal;
uses p_typespec; ;;; For -def_typespec-

include sysdefs.ph;

compile_mode:pop11 /* +defcon */ +defpdr +varsch;


section $-external SIZEOFTYPE =>    newc_dec #: #->
                        isstruct isstructkey
                        consstruct deststruct initstruct
                        struct_field_access struct_field_update
                        define_c_type
                        external_import_procedure
    ;


global vars
    show_new_types = false,
    error_warn_only = false,
    external_import_procedure = identfn,
    ;

define ExtImportProcedure =
    newanyproperty([], 10, 1, 7, false, false, "perm", false,
               procedure;
                    erasenum(2);
                    external_import_procedure
               endprocedure
    );
enddefine;

lvars procedure_header = false; ;;; used non-locally

/* The argument to a struct init pdr to make an instance without
    an external block */
global constant EMPTY_STRUCT = consexternal_ptr();
2 ** 32 - 1 -> exacc ^uint EMPTY_STRUCT;

/* structure_info fields. */
constant macro (SI_SIZE=1, SI_INITPROC = 2, SI_CONSPROC = 3,
                SI_DESTPROC = 4, SI_FIELDS = 5, SI_SPECLIST = 6,
                SI_NAME = 7, SI_ISUNION = 8, SI_INFOSIZE = 8);

/* struct key fields. */
constant  macro (SK_EBLOCK = 1, SK_EPTR = 2, SK_FIRSTSUBSTRUCT = 3);

recordclass Array [external_ptr]
    A_bounds A_eptr:exptr A_coercepdr A_exsubpdr;

define pop11_try_nextitem_noautoload;
    dlocal pop_autoload = false;
    pop11_try_nextitem();
enddefine;

define lconstant Structure_info
    = newproperty([],100, false, "tmparg");
enddefine;

define structure_info(item) -> i;
    lvars item i;
    if isvector(item) then
        if (fast_subscrv(1, item) ->> i) == "struct" or i == "union" then
            unless Structure_info(fast_subscrv(2, item)) ->> i then
                mishap(item, 1, 'Barf: STRUCT/UNION IMPOSTOR');
            endunless;
        else
            false -> i;
        endif;
    else
        Structure_info(item) -> i;
    endif;
enddefine;

define updaterof structure_info;
    -> Structure_info();
enddefine;

define coerce_table
    = newproperty([], 256, false, "perm");
enddefine;

/*
 * Start the property off with the following:-
 *
 *      typedef char Char, *String;
 *
 */
define constant Typedef_alias
    = newproperty([ [String [char]] [Char char]
                  ],
                  64, false, "perm");
enddefine;

define typedef_alias(i) -> i;
lvars i f = true, o
    ;
    define lconstant reserved =
            lmember(%[unsigned short long char int float double void]%);
    enddefine;

    i -> o;
    while f do
        if Typedef_alias(i) ->> f then
            f -> i;
        elseif not(reserved(i)) and not(coerce_table(i))
                and isword(i) and identprops(i) == "undef" then
            sys_autoload(i) -> f;
        endif;
    endwhile;
    if o = i then false -> i endif;
enddefine;

define updaterof typedef_alias;
    -> Typedef_alias();
enddefine;

recordclass FieldRecord fr_name fr_accesslist fr_updatelist;

define FieldPrint(f);
    lvars f
        ;
    printf(
        '<FieldRecord %P - %P entries>',
        [% f.fr_name, length(f.fr_accesslist) %]
    );
enddefine;

FieldPrint -> class_print(FieldRecord_key);

define lconstant field_record_table
    = newproperty([], 256, false, "perm");
enddefine;

define EnsureFieldRecord(name) -> fieldrecord;
lvars name fieldrecord
    ;
    unless field_record_table(name) ->> fieldrecord then
        consFieldRecord(name, [], [])
            ->> fieldrecord
            -> field_record_table(name);
    endunless;
enddefine;

/* looks through the keymap for the access procedure, and applies it. */
define struct_field_access(struct, fieldrecord);
lvars
        keymap  = fr_accesslist(fieldrecord),
        key     = datakey(struct),
        struct fieldrecord
    ;
    struct;
    until keymap == [] do
        if fast_destpair((fast_destpair(keymap) -> keymap)) == key then
            /* procedure left on stack by the destpair */
            apply();
            return;
        else
            erase();
        endif;
    enduntil;
    mishap("'Type key -'", key, "'field name -'", fr_name(fieldrecord),
            4, 'INVALID DATA TYPE FOR STRUCT ACCESS');
enddefine;

/* looks through the keymap for the update procedure, and applies it. */
define updaterof struct_field_access(v, struct, fieldrecord);
lvars
        keymap  = fr_updatelist(fieldrecord),
        key     = datakey(struct),
        v struct fieldrecord
    ;
    v; struct;
    until keymap == [] do
        if fast_destpair((fast_destpair(keymap) -> keymap)) == key then
            apply();
            return;
        else
            erase();
        endif;
    enduntil;
    mishap("'Type key -'", key, "'field name -'", fr_name(fieldrecord),
            4, 'INVALID DATA TYPE FOR STRUCT UPDATE');
enddefine;

struct_field_access -> class_apply(FieldRecord_key);

define add_accessproc(newproc, newkey, name);
lvars fieldrecord newproc newkey name
    ;
    define lconstant addpk(list, pdr, k);
        lvars list k pdr b f;
        if list == [] then
            [% conspair(pdr, k) %]
        else
            ;;; drop the list pointer
            list,
            until (fast_back(list) ->> b) == [] do
                if fast_back(fast_front(list)) == k then
                    pdr -> fast_front(fast_front(list));
                    return;
                endif;
                b -> list;
            enduntil;
            [% conspair(pdr, k) %] -> back(list);
        endif;
    enddefine;

    EnsureFieldRecord(name) -> fieldrecord;

    addpk(fr_accesslist(fieldrecord), newproc, newkey)
        -> fr_accesslist(fieldrecord);
    addpk(fr_updatelist(fieldrecord), updater(newproc), newkey)
        -> fr_updatelist(fieldrecord);
enddefine;

/* checks next item is a valid structure field, and plants code to
 * call its accesspdr.
 */
define global syntax 1 #: ;
lvars name, fieldrecord
    ;
    readitem() -> name;

    unless field_record_table(name) ->> fieldrecord then
        mishap(name, 1, 'Unknown structure field name');
    endunless;

    pop_expr_inst(pop_expr_item);
    sysPUSHQ(fieldrecord);
    struct_field_access -> pop_expr_item;
    sysCALLQ -> pop_expr_inst;
enddefine;

global constant macro #-> = [( 0 ) #:];

define global isstruct
    = datakey <> structure_info;
enddefine;

define global isstructkey(k);
lvars k i;
    if isvector(k) then
        if (k(1) ->> i) == "struct" or i == "union" then
            k(2) -> k;
        else
            return(false);
        endif;
    endif;
    if isword(k) then
        if structure_info(k) ->> i then
            i -> k;
        else
            k <> "_key" -> k;
            if identprops(k) == "undef" then
                return(false);
            else
                valof(k) -> k;
            endif;
        endif;
    endif;
    structure_info(k);
enddefine;

define lconstant sysASSIGN(pdr, id);
    ;;; assignes a procedure to an identifier.
    lvars id pdr;
    sysSYNTAX(id, if popdefineprocedure then "procedure" else 0 endif,
                  popdefineconstant);
    sysGLOBAL(id);
    sysPASSIGN(pdr, id);
    if isinheap(pdr) then
        id -> pdprops(pdr)
    endif;
    if (updater(pdr) ->> pdr) and isinheap(pdr) then
        id -> pdprops(pdr)
    endif
enddefine;

/* constructs an instance of this type, after constructing each substructure
 * that this type of structure uses.
 */
define Newstruct(eblock, eptr, key, substruct_conspdrs);
lvars cpdr eptr eblock substruct_conspdrs key;
    class_cons(key)(
        eblock, eptr,
        for cpdr in substruct_conspdrs do
            cpdr(EMPTY_STRUCT)
        endfor,
        );
enddefine;

lvars Sprintlevel = 0;

define Printstruct(struct);
lvars   struct
        structspec  = structure_info(datakey(struct)),
        isunion     = structspec(SI_ISUNION),
        name        = structspec(SI_NAME),
        fields      = structspec(SI_FIELDS),
        fnames pdr f
        maxlen      = 0
    ;
dlocal  pop_pr_quotes = false,
        Sprintlevel = Sprintlevel + 1,
    ;
    {%  for f in_vector fields do
            fr_name(f) -> f;
            max(length(f), maxlen) -> maxlen;
            f,
        endfor;
     %} -> fnames;
    printf('%p %p%',
           [%   if name then
                    if isunion then 'union' else 'struct' endif, name
                else
                    'Anonymous', if isunion then 'union' else 'struct' endif
                endif %]);
    for pdr,name in_vector fields,fnames do
        nl(1);
        repeat Sprintlevel times pr('\t'); endrepeat;
        spr(name); spr('-'); sp(maxlen - length(name)); pr(pdr(struct));
        quitif(isunion); ;;; This way we only get one of the (many) alternatives
    endfor;
enddefine;

vars procedure MassageSpec;

define lconstant sizeof_spec(spec) -> spec;
lvars   spec
    ;
lconstant
        bsize = SIZEOFTYPE(:byte, :1),
    ;
    field_spec_info(spec) -> spec -> ;
    if spec then
        if (spec // bsize -> spec) /== 0 then
            ;;; round up
            spec + 1 -> spec;
        endif;
    endif;
enddefine;

define lconstant primitive_type = newassoc([
                [C_uint     uint]   [C_sint     int]
                [C_ushort   ushort] [C_sshort   short]
                [C_uchar    byte]   [C_schar    sbyte]
                [C_float    float]  [C_double   dfloat]
            ]);
enddefine;

define NormaliseSpec(spec) -> pdr -> bits;
lvars pdr bits spec;
    MassageSpec(spec) -> pdr -> bits;

    ;;; now massage the pdr to be something that just takes an address
    if pdr == true then
        if primitive_type(spec) then
            identfn -> pdr;
        elseunless coerce_table(spec) ->> pdr then
            ;;; This should never happen
            mishap(spec, 1, 'Oh dear!!');
        endif;
    elseif ispair(bits) or isprocedure(pdr) then
        ;;; complex but in-line (ie. struct, union or array)
    elseif pdr == false and bits == false then
        ;;; hopefully just a struct forward reference
    elseunless bits == "exptr" then
        mishap(spec, pdr, bits, 3, 'UN-NORMALISABLE SPEC');
    endif;
enddefine;

define MkPointer(spec) -> pdr -> size;
lvars spec pdr size accpdr
    ;
    ;;; see if spec is valid yet
    NormaliseSpec(spec) -> pdr -> size;

    if pdr == false and size == false then
        /** a forward reference, so construct something that works it out the
            first time it is accessed **/

        procedure(eptr, spec, accpdr); /* -> array; */
        lvars eptr accpdr spec pdr
            ;
            unless cont(accpdr) ->> pdr then
                NormaliseSpec({array ^spec ^false []}) -> pdr -> ;
                if isprocedure(pdr) then
                    pdr -> cont(accpdr);
                else
                    mishap(spec, 1, 'SLOT SPEC STILL UNDEFINED AT FIRST USAGE');
                endif;
            endunless;

            if is_valid_external_ptr(eptr) then
                pdr(eptr); /* -> array */
            else
                eptr
            endif;
        endprocedure(% spec, consref(false) %)
            -> pdr;
    else
        /*  A pointer is a boundless, 1 dimensional array, so there!
         */
        NormaliseSpec({array ^spec ^false []}) -> pdr -> ;
    endif;

    identfn -> updater(pdr);

    gensym("Pointer") -> pdprops(pdr);

    ;;; This ensures that the slot access procedure performs the pointer
    ;;; dereference and then applies the MkArray pdr (an "implicit access")
    pdr(%"exptr", false%) -> size;
enddefine;

define ArrayIndex(array);
lvars   array
    ;
    define lconstant Calc(bounds);
    lvars bounds o;
        if bounds == [] then
            (); ;;; return the top item on the stack;
        else
            -> o;
            Calc(back(bounds)) * front(bounds) + o;
        endif;
    enddefine;

    A_exsubpdr(array)(
        Calc(A_bounds(array)) + 1,
        A_eptr(array)
    )
enddefine;

define ArrayAccess(array);
lvars array;
    A_coercepdr(array)(ArrayIndex(array));
enddefine;

define updaterof ArrayAccess(array);
lvars array;
    -> (A_coercepdr(array))(ArrayIndex(array));
enddefine;

ArrayAccess -> class_apply(Array_key);

define ArrayPrint(a);
lvars a;
dlocal pop_pr_quotes = false;
    if A_bounds(a) == [] then
        ;;; Ok, so it's a bit of an assumtion to think that
        ;;; one dimensional array's are pointers, but it won't be
        ;;; far off.
        ;;;
        sys_syspr('<pointer');
        pdprops(A_coercepdr(a)) -> a;
        if a and isstartstring('init', word_string(a) ->> a) then
            sys_syspr(' to ');
            sys_syspr(allbutfirst(4, a));
        endif;
        sys_syspr('>');
    else
        sys_syspr(a);
    endif;
enddefine;

ArrayPrint -> class_print(Array_key);

define MkArray(spec) -> initpdr -> bits;
lvars spec bits size bounds initpdr pdr expdr
    ;

    spec(3) -> size;
    spec(4) -> bounds;
    NormaliseSpec(spec(2)) -> pdr -> bits;

    unless pdr or bits then
        mishap(spec, 1, 'SPEC STILL UNKNOWN AT ARRAY/POINTER CONSTRUCT TIME');
    endunless;

    conspair(bits, size) -> bits;

    cons_access(true, bits, true, 1) -> expdr;

    procedure(eptr, bounds, accpdr, exaccpdr, atab);
        lvars   i eptr bounds bytes accpdr exaccpdr atab addr
            ;

        unless isexternal_ptr(eptr) then
            mishap(eptr, 1, 'Illegal eptr for array init pdr');
        endunless;

        returnunless(is_valid_external_ptr(eptr))(eptr);

        if eptr == EMPTY_STRUCT then
            consArray(bounds, consexternal_ptr(), accpdr, exaccpdr)
        else
            ;;; hash the table on the base address of the array
            exacc ^uint eptr -> addr;
            if atab(addr) ->> i then
                i
            else
                consArray(bounds, eptr, accpdr, exaccpdr) ->> atab(addr)
            endif;
        endif;
    endprocedure(%  bounds, pdr, expdr, newproperty([], 100, false, "tmpval")
                 %)
        -> initpdr;
enddefine;

define MassageSpec(item) -> field_pdr -> field_spec;
lvars item i field_pdr field_spec
    ;

MASS:

    if isinteger(item) then
        true -> field_pdr;
        item -> field_spec;
    elseif isprocedure(item) then
        ;;; user defined type
        item -> field_pdr;
        "exptr" -> field_spec;
    elseif primitive_type(item) ->> i then
        true -> field_pdr;
        i    -> field_spec;
    elseif isstructkey(item) ->> i then
        /** complex slot type within structure **/
        i(SI_INITPROC) -> field_pdr;
        i(SI_SPECLIST) -> field_spec;
    elseif isvector(item) then
        item(1) -> i;
        if i == "array" then
            ;;; this makes it look just like a struct
            MkArray(item) -> field_pdr -> field_spec;
        elseif i == "function" then
            ExtImportProcedure(current_tag) -> field_pdr;
            "exptr" -> field_spec;
        elseif i == "pointer" then
            MkPointer(item(2)) -> field_pdr -> field_spec;
        else
            ;;; mishap(item, 1, 'SOMETHING WRONG');
            ;;; A struct forward reference (we hope)
            false ->> field_pdr -> field_spec;
        endif;
    elseif typedef_alias(item) ->> i then
        i -> item;
        goto MASS
    else
        mishap(item,1,'Not a valid coerce spec');
    endif;
enddefine;

define StructAlias(name, key);
lvars   name key info keyid fields
    ;
dlocal  popdefineprocedure = true,
    ;
    if isvector(key) then key(2) -> key endif;

    unless iskey(key) then
        mishap(key, 1, 'Struct Forward Reference');
    endunless;

    unless isstructkey(key) ->> info then
        mishap(key, 1, 'Huh??');
    endunless;

    name <> "_key" -> keyid;
    sysSYNTAX(keyid, 0, popdefineconstant);
    sysGLOBAL(keyid);
    key ->> key_of_dataword(name) -> valof(keyid);

    sysASSIGN(info(SI_INITPROC), "init" <> name);
    sysASSIGN(info(SI_CONSPROC), "cons" <> name);
    sysASSIGN(info(SI_DESTPROC), "dest" <> name);
    sysASSIGN(class_recognise(key), "is" <> name);
    datalist(info(SI_FIELDS)) -> fields;
    $-typespec_utils$-def_typespec( name,
                                    false,
                                    info(SI_SPECLIST),
                                    ncmaplist(fields, fr_name)
                                );
enddefine;

define global newstructure(name, fields, speclist, isunion) -> key;
lvars   key keyid initpdr accesspdr
        accessors item index name fields
        fn sub_struct isunion
        Pdr Spec
        key_spec = copylist([full exptr]), ;;; the start of the speclist
        cons_pdrs = [], want_list = [],
        speclist size
        structspec = initv(SI_INFOSIZE),
    ;
lconstant
        mshp_noupd = mishap(%0, 'CAN\'T UPDATE A SUB-STRUCT FIELD'%),
        mshp_noapp = mishap(%2, 'CAN\'T APPLY STRUCTURE'%),
        union_spec = [>-> 0],
    ;
dlocal
        popdefineprocedure
    ;

    ;;; this is all done by pointers so nothing is returned
    define lconstant add_to_back(item, list);
        lvars b list item;
        until (back(list) ->> b) == [] do b -> list enduntil;
        unless islist(item) do conspair(item, []) -> item endunless;
        item -> back(list);
    enddefine;

    if speclist == [] then
        mishap(0, 'empty structure class definition');
    endif;

    isunion -> structspec(SI_ISUNION);
    name    -> structspec(SI_NAME);

    ;;;  Here we sort out all the field access pdr's
    ;;;  First change the intermediate specs into Poplog Idiom form
    ;;;  then call -cons_access- to make the pdr's
    ;;;
    SK_FIRSTSUBSTRUCT -> sub_struct;
    [>->
     %  for fn,item in fields,speclist do
            MassageSpec(item) -> Pdr -> Spec;
            if ispair(Spec) then
                /** a sub struct/array**/
                add_to_back("full", key_spec);

                sysPROCEDURE(fn, 1);                    ;;; s apdr
                    sysLVARS("acc_pdr", "procedure");
                    sysPOP("acc_pdr");                  ;;; s
                    sysPUSHS(0);                        ;;; s s
                    sysCALL("acc_pdr");                 ;;; s eptr
                    sysSWAP(1,2);                       ;;; eptr s
                    sysFIELD_VAL(sub_struct, key_spec); ;;; eptr ss
                    sysPUSHS(0);                        ;;; eptr ss ss
                    sysSWAP(2,3);                       ;;; ss eptr ss
                    sysUFIELD(2, [full exptr], true, false); ;;; ss
                sysENDPROCEDURE() -> accesspdr;         ;;; substruct returned

                mshp_noupd -> updater(accesspdr);

                sub_struct + 1 -> sub_struct;

                Spec,
                accesspdr :: want_list -> want_list;
                Pdr :: cons_pdrs -> cons_pdrs;
            else
                Spec,
                true :: want_list -> want_list;
            endif;
        endfor;
     %] ->> speclist -> structspec(SI_SPECLIST);
    ncrev(want_list) -> want_list;

    if isunion then
        0 -> size;
        {%  fast_for item, Spec in want_list, back(speclist) do
                max(size, sizeof_spec(Spec)) -> size;
                Spec -> front(back(union_spec));
                cons_access([], union_spec, true, 1)(1) -> accesspdr;
                if isprocedure(item) then
                    item(%accesspdr%)
                else
                    accesspdr
                endif
            endfor
         %} -> accessors;
        sys_grbg_list(want_list);
    else
        sizeof_spec(speclist) -> size;
        cons_access(want_list, speclist, true, 1) -> accessors;
        for Pdr with_index index in_vector accessors do
            sys_grbg_destpair(want_list) -> want_list -> item;
            nextunless(isprocedure(item));
            item(% Pdr %) -> accessors(index);
        endfor;
    endif;
    size -> structspec(SI_SIZE);

    ncrev(cons_pdrs) -> cons_pdrs;

    /*** Now we've got all the fields sorted out we can make up the
         struct key and associated pdr's ***/

    conskey(name or gensym("'Struct_'"), key_spec, {external_ptr})
        -> key;

    {%  for accesspdr in_vector accessors do
            add_accessproc(accesspdr, key, dest(fields)->fields->>name);
            EnsureFieldRecord(name),
        endfor
     %} -> structspec(SI_FIELDS);

    procedure(eptr, key, cprocs, size, stab);
        lvars   i eptr key info stab size cprocs
                eblock = false, addr
            ;

        if eptr == false then
            init_fixed(size, string_key) -> eblock;
            consexternal_ptr() -> eptr;
            fill_external_ptr(eblock, eptr) -> eptr;
        endif;

        unless isexternal_ptr_class(eptr) or eptr == EMPTY_STRUCT then
            mishap(eptr, 1, 'Illegal eptr for struct init pdr');
        endunless;

        returnunless(is_valid_external_ptr(eptr))(eptr);

        if eptr == EMPTY_STRUCT then
            Newstruct(false, consexternal_ptr(), key, cprocs)
        else
            ;;; hash the table on the base address of the structure
            exacc ^uint eptr -> addr;
            if stab(addr) ->> i then
                i
            else
                Newstruct(eblock, eptr, key, cprocs) ->> stab(addr)
            endif;
        endif;
    endprocedure(%  key, cons_pdrs, structspec(SI_SIZE),
                    newproperty([], 100, false, "tmpval")
                 %)
        ->> initpdr -> structspec(SI_INITPROC);

    initpdr -> coerce_table({%"struct", key, isunion%});
    if name then
        initpdr -> coerce_table({%"struct", name, isunion%});
    endif;

    procedure;
        lvars field accessv newstruct
            ;
        initpdr(false) -> newstruct;    ;;; initialize one structure.
        structure_info(datakey(newstruct))(SI_FIELDS) -> accessv;
        for field from datalength(accessv) by -1 to 1 do
            -> fast_subscrv(field, accessv)(newstruct);
        endfor;
        newstruct;
    endprocedure -> structspec(SI_CONSPROC);

    procedure(struct);
        lvars procedure (pdr) struct
              accessv = structure_info(datakey(struct))(SI_FIELDS),
            ;
        for pdr in_vector accessv do
            pdr(struct);
        endfor;
    endprocedure -> structspec(SI_DESTPROC);

    mshp_noapp ->> class_apply(key) -> updater(class_apply(key));

    Printstruct -> class_print(key);

    structspec -> structure_info(key);
    if name then
        key -> structure_info(name)
    endif;
enddefine;

define parse_array() -> s -> b;
lvars
        x s = false, b = [],
    ;
    define lconstant getoneint;
        lvars len;
        stacklength() -> len;
        pop11_exec_stmnt_seq_to("]") -> ;
        unless stacklength() - len == 1 and isinteger(dup()) then
            mishap(0, 'Illegal expression inside array brackets');
        endunless;
    enddefine;

    unless pop11_try_nextitem("]") then ;;; first dimension (optional in C)
        getoneint() -> s;   ;;; and is not required in the bounds list
    endunless;

    while pop11_try_nextitem("[") do
        if pop11_try_nextitem("]") then
            mishap(0, 'C PARSE ERROR: Underspecified array dimensions');
        else
            getoneint() -> x;
            if s then
                s * x -> s;
            endif;
            x :: b -> b;
        endif;
    endwhile;
    ;;; Note that the bounds list is reversed, this is good.
enddefine;

define read_one_var(spec) -> spec -> var;
lvars spec var head,
    ;
    define lconstant add_tail(head, tail);
    lvars head tail
        ;
        if head then
            if isvector(head) then
                add_tail(head(2), tail) -> head(2);
            endif;
            head
        else
            tail
        endif;
    enddefine;

    define lconstant DeclaratorTail() -> spec;
        lvars spec item = pop11_try_nextreaditem([( %"["%]), bounds
            ;
        if item == "[" then
            parse_array() -> item -> bounds;
            {array %DeclaratorTail(), item, bounds%} -> spec
        elseif item == "(" then
            if procedure_header then
                item :: proglist -> proglist;
                false -> spec;
                return;
            else
                pop11_need_nextitem(")") -> ;
                {function %DeclaratorTail()%} -> spec;
            endif;
        else
            false -> spec;
        endif;
    enddefine;

    define lconstant Declarator() -> spec -> var;
        lvars item head tail spec = false, var = false
            ;
        dlocal  % item_chartype(`*`) % = 5, ;;; seperator
                % item_chartype(`_`) % = 1, ;;; alphabetic
            ;
        pop11_try_nextreaditem([* ( )]) -> item;
        if item == "*" then
            Declarator() -> head -> var;
            add_tail(head, {pointer ^false}) -> spec;
        elseif item == "(" then
            Declarator() -> head -> var;
            pop11_need_nextitem(")") -> ;
            DeclaratorTail() -> tail;
            add_tail(head, tail) -> spec;
        elseif item == ")" then
            ")" :: proglist -> proglist;
        else
            readitem() -> var;
            unless isword(var) then
                mishap(var, 1, 'Illegal item for variable name');
            endunless;
            if pop11_try_nextreaditem(":") then
                readitem() -> spec;
                unless isinteger(spec) then
                    mishap(spec, 1, 'INTEGER NEEDED FOR BIT-FIELD SPECIFIER');
                endunless;
            else
                DeclaratorTail() -> spec;
            endif;
        endif;
    enddefine;

    Declarator() -> head -> var;
    if isinteger(head) then
        head -> spec;
    else
        add_tail(head, spec) -> spec;
    endif;
enddefine;

vars procedure read_spec; ;;; forward reference

define read_vars() -> speclist -> namelist;
lvars   fspec aspec speclist = [], namelist = [],
    ;
lconstant
        term = ";"
    ;
dlocal  % item_chartype(`*`) % = 5,
        % item_chartype(`_`) % = 1,
    ;
    read_spec() -> fspec;
    returnif(pop11_try_nextreaditem(term));
    [%  repeat
            read_one_var(fspec) -> aspec ; /* -> name; */
            aspec :: speclist -> speclist;
            quitif(pop11_try_nextreaditem(term));
            pop11_need_nextitem(",") -> ;
        endrepeat;
     %] -> namelist;
    ncrev(speclist) -> speclist;
enddefine;

define read_enum -> spec;
lvars item tag spec count = 0, had = [],
    ;
lconstant enum_ident = newproperty([], 11, false, true),
    ;
dlocal  % item_chartype(`*`) % = 11,
        % item_chartype(`_`) % = 4,
    ;

    nextreaditem() -> tag;
    if tag == "{" then
        false -> tag;
    else
        readitem() -> tag;
    endif;

    if pop11_try_nextreaditem("{") then
        until pop11_try_nextreaditem("}") do
            readitem() -> item;
            if pop11_try_nextreaditem("=") then
                readitem() -> count;
                unless isinteger(count) then
                    mishap(count, 1, 'Illegal evaluation value in enum');
                endunless;
            endif;
            count :: had -> had;
            sysCONSTANT(item, "macro");
            sysGLOBAL(item);
            sysPUSHQ(count);
            sysPOP(item);

            quitif(pop11_try_nextreaditem("}"));

            pop11_need_nextitem(",") -> ;
            count + 1 -> count;
        enduntil;
        if tag then
            had -> enum_ident(tag);
        endif;
    elseunless (tag and enum_ident(tag))
               or (sys_autoload(tag) and enum_ident(tag))
    then
        mishap(tag, 1, 'Unknown enum definition');
    endif;

    "C_sint" -> spec;
enddefine;

define read_struct(type) -> speclist;
lvars   names specs
        fieldlist type
        speclist = [],
        tag = false,
        key
    ;
dlocal  % item_chartype(`*`) % = 11,
        % item_chartype(`_`) % = 4,
        popautolist = [],
    ;

    nextreaditem() -> tag;
    if tag == "{" then
        false -> tag;
    else
        readitem() -> tag;
    endif;

    if pop11_try_nextreaditem("{") then
        [%  until pop11_try_nextitem("}") do
                read_vars() -> specs -> names;
                speclist nc_<> specs -> speclist;
                dl(names),
            enduntil;
         %] -> fieldlist;
        newstructure(tag, fieldlist, speclist, type == "union") -> key;
        if tag then
            StructAlias(tag, key);
        endif;
    elseunless (tag and isstructkey(key_of_dataword(tag) ->> key))
               or (sys_autoload(tag) and isstructkey(key_of_dataword(tag) ->> key))
    then
        ;;; mishap(tag, 1, 'Unknown struct definition');
        ;;; perhaps a foward reference. The info will be filled in later.
        tag -> key;
    endif;
    {%"struct", key, type == "union"%} -> speclist;
enddefine;

define read_spec -> spec;
lvars   signed  = false,
        size    = false,
        type    = false,
        spec    = false,
        item stype tsign tsize ttype c = 2,
    ;
dlocal  % item_chartype(`*`) % = 11,
        % item_chartype(`_`) % = 4,
        popsyscall,
    ;
        /* The following list represents this table
         *
         *  want     signed     size    type
         *  ~~~~     ~~~~~~     ~~~~    ~~~~
         *  C_uint   unsigned   false   int
         *           unsigned   long    {false int}

            C_sint   false      false   int
                     false      long    {false int}

            C_uchar  unsigned   false   char

            C_schar  false      false   char

            C_ushort unsigned   short   {false int}

         *  C_sshort false      short   {false int}
         *
         *  C_float  false      false   float
         *
         *  C_double false      long    float
         *           false      false   double
         *
         *  void     false      false   void
         */
    lconstant spectab =
            [^false
                [^false
                    [^false [C_sint] char [C_schar] int [C_sint]
                     float [C_float] double [C_double] void [void]
                    ]
                 long
                    [^false [C_sint] char [^false] int [C_sint]
                     float [C_double] double [^false] void [^false]
                    ]
                 short
                    [^false [C_sshort] char [^false] int [C_sshort]
                     float [^false] double [^false] void [^false]
                    ]
                ]
             unsigned
                [^false
                    [^false [C_uint] char [C_uchar] int [C_uint]
                     float [^false] double [^false] void [^false]
                    ]
                 long
                    [^false [C_uint] char [^false] int [C_uint]
                     float [^false] double [^false] void [^false]
                    ]
                 short
                    [^false [C_ushort] char [C_uchar] int [C_uint]
                     float [^false] double [^false] void [^false]
                    ]
                ]
            ],
    ;

    define lconstant issimple_dec(item);
        lvars item;
            isword(item) and
            lmember(item, #_<[unsigned 1 short 2 long 2
                              char 3 int 3 float 3 double 3 void 3]>_#);
    enddefine;

    ;;; this one will expand any macros (eg. #_IF)
    pop11_try_nextitem_noautoload([struct union enum ;]) -> item;


    if item == "struct" or item == "union" then
        read_struct(item) -> spec;
    elseif item == "enum" then
        read_enum() -> spec;
    elseif item == ";" then
        mishap(0, 'Empty spec definition, unexpected ;');
    elseif typedef_alias(nextreaditem()) ->> spec then
        readitem() -> ;
    else
        while issimple_dec(nextreaditem()) ->> stype do
            5 -> item_chartype(`*`);
            1 -> item_chartype(`_`);
            readitem() -> item;
            stype(2) -> stype;
            if stype == 1 then
                item, size, type
            elseif stype == 2 then
                signed, item, type
            else
                signed, size, item
            endif -> ttype -> tsize -> tsign;

            if hd(lmember(ttype,
                          lmember(tsize,
                                  lmember(tsign, spectab)(2)
                                 )(2)
                         )(2)
                  ) ->> spec
            then
                item -> if stype == 1 then
                            signed
                        elseif stype == 2 then
                            size
                        else
                            type
                        endif;
            else
                mishap(item,
                        "'ALREADY FOUND:'",
                        if signed then c + 1 -> c; signed endif,
                        if size then c + 1 -> c; size endif,
                        if type then c + 1 -> c; type endif,
                        c, 'Illegal type specifier ');
            endif;
        endwhile;
    endif;
    unless spec then
        if error_warn_only then
            "C_sint" -> spec;
            false -> popsyscall;
            sysprmishap(readitem(), 1, '(WARNING) Unknown spec definition');
        else
            mishap(readitem(), 1, 'Unknown spec definition');
        endif;
    endunless;
enddefine;

define read_typedef;
lvars spec newtype deref i b
    ;

    for newtype,spec in read_vars() do
        spec -> typedef_alias(newtype);

        sysCONSTANT(newtype, 0);    ;;; Probably not useful,
        sysGLOBAL(newtype);         ;;; but it stops auto-
        sysPUSHQ(spec);                 ;;; loading
        sysPOP(newtype);

        if isstructkey(spec) then
            StructAlias(newtype, spec);
        endif;

        if show_new_types then
            printf(spec, newtype, '** Newtype %p has spec %p\n');
        endif;
    endfor;
enddefine;

define new_pop_ext_call(callpdr, coercepdr, extpdr_id, self);
lvars   callpdr, extpdr_id, coercepdr, self, extpdr = fast_idval(extpdr_id);
        ;
    unless extpdr.is_valid_external_ptr then
        mishap(extpdr, 'Applying unloaded External Procedure ' >< pdprops(self))
    endunless;

    callpdr(extpdr);
    if coercepdr then ;;; coerce the return val if needed
        coercepdr()
    endif;
enddefine;

define vars new_external_import(sysname, popname, argspec, retspec, errpdr, cbr);
lvars   sysname, popname, argspec, retspec, errpdr, cbr,
        extpdr_id, nargs, val, Ext_call
        ;
lvars   callpdr,
        coercepdr = false,
    ;

    if retspec == "void" then
        "uint" -> retspec;
        erase -> coercepdr;
    elseif primitive_type(retspec) then
        primitive_type(retspec) -> retspec;
    elseif coerce_table(retspec) ->> coercepdr then
        "exptr" -> retspec;
    else
        mishap(retspec, 1, 'INVALID RETURN TYPE');
    endif;

    /*  Now we're going to do something about varargs.
    */
    datalength(argspec) -> nargs;
    if nargs /== 0 and subscrv(nargs, argspec) == "varargs" then
        ;;; Variadic function
        false -> nargs;
    endif;

    cons_access(consref(true), {^nargs ^retspec}, true, 1)
        -> callpdr;

    sys_current_val("ident $-external$-new_pop_ext_call") -> Ext_call;

    false -> extpdr_id;
    if isdefined(popname) then
        if valof(popname).isclosure
        and pdpart(valof(popname)) == Ext_call then
            frozval(3, valof(popname)) -> extpdr_id
        endif;
    else
        sysSYNTAX(popname, "procedure", false);
        sysGLOBAL(popname);
    endif;
    unless extpdr_id then
        consident(0, false, "lex") -> extpdr_id;
        false -> idval(extpdr_id)
    endunless;

    Ext_call(% callpdr, coercepdr, extpdr_id, 0 %)
                ->> val ->> frozval(4,val) -> sys_current_val(popname);
    false -> updater(val);              ;;; for Popc
    pop11_define_props(word_identifier(popname, current_section, true),
                        popname, false) -> pdprops(val);

    add_import(popname, sysname, extpdr_id, ExtImportProcedure(current_tag));
enddefine;

lconstant C_ref = consref("C");

define read_procedure(pdr_spec, pdr_name);
lvars   n s variables pdr_spec pdr_name var_types
        names specs ret_spec ret_coerce
        speclist = [],
    ;
        [%  until pop11_try_nextreaditem(")") then
                readitem(),
                quitif(pop11_try_nextreaditem(")"));
                pop11_need_nextitem(",") -> ;
            enduntil;
            pop11_try_nextreaditem(";") -> ;
         %] -> variables;

        false -> procedure_header;

        until pop11_try_nextreaditem("{") do
            read_vars() -> specs -> names;
            for n,s in names,specs do
                if lmember(n, speclist) then
                    mishap(n, 1, 'Duplicated variable declaration');
                elseunless lmember(n, variables) then
                    mishap(n, 1, 'Declaring non argument variable');
                endif;
                n :: (consref(s) :: speclist) -> speclist;
            endfor;
        enduntil;
        pop11_need_nextitem("}") ->;

        {%  for n in variables do
                if lmember(n, speclist) ->> s then
                    cont(hd(tl(s)))
                else
                    mishap(pdr_name, n, 2, 'FORMAL PARAMETER NOT EXPLICITLY DECLARED');
                endif;
            endfor;
         %} -> var_types;
        sys_grbg_list(speclist);
        sys_grbg_list(variables);

        unless pdr_spec == "void"
               or primitive_type(pdr_spec)
               or coerce_table(pdr_spec) do
            NormaliseSpec(pdr_spec) -> ret_coerce -> ret_spec;

            unless isprocedure(ret_coerce) then
                mishap(pdr_name, pdr_spec, 2, 'CAN\'T MAKE COERCE PDR FOR EXT-PDR RESULT');
            endunless;

            ret_coerce -> coerce_table(pdr_spec);
        endunless;

        new_external_import(C_ref, pdr_name, var_types, pdr_spec, mishap,
                                                    false /* cbr */);
enddefine;

define read_global_ident();
lvars spec name ret_coerce
    ;
dlocal procedure_header = true,
    ;

    read_spec() -> spec;
    read_one_var(spec) -> spec -> name;

    if pop11_need_nextitem([( ;]) == "(" then
        /* it's a procedure */
        read_procedure(spec, name);
    else
        /* it's some wierd global constant */
        unless coerce_table(spec) ->> ret_coerce do
            NormaliseSpec(spec) -> ret_coerce -> ;

            unless isprocedure(ret_coerce) then
                mishap(name, spec, 2, 'CAN\'T MAKE COERCE PDR FOR THIS TYPE OF GLOBAL IDENTIFIER');
            endunless;
        endunless;

        unless isdefined(name) then
            sysSYNTAX(name, 0, false);
            sysGLOBAL(name);
        endunless;

        add_import(name, C_ref, identof(name), ret_coerce);
    endif;
enddefine;

define parse_tag_attributes;
lvars val;
    until pop11_try_nextreaditem(")") do
        pop11_need_nextreaditem("external_import_procedure") -> ;
        readitem() -> val;
        unless val.isword then
            mishap(val, 1, 'WORD NEEDED')
        endunless;
        valof(val) -> ExtImportProcedure(current_tag);
        pop11_try_nextreaditem(",") -> ;
    enduntil;
enddefine;

define global vars newc_dec;
    lvars item
        ;
    if pop11_try_nextreaditem("(") then
        parse_tag_attributes();
    endif;
    until pop11_try_nextitem_noautoload("endexternal") do
        pop11_try_nextitem_noautoload([struct union typedef]) -> item;
        if item == "typedef" then
            read_typedef();
        elseif item == "struct" or item == "union" then
            ;;; not interested in the spec, just want to define the tag
            read_struct(item) -> ;
        else
            read_global_ident();
        endif;
    enduntil;
enddefine;

/*
    A method of creating an arbitrary coercion type.
 */
define:define_form c_type;
lvars   cpdr
        cname
    ;
    readitem() -> cname;

    consword('c:type:' sys_>< cname) -> cpdr;
    cpdr :: proglist -> proglist;
    nonsyntax define();

    sysPUSH(cpdr);
    sysPUSHQ(cname);
    sysUCALLQ(typedef_alias);

    ;;; redundant but necessary
    sysPUSH(cpdr);
    sysPUSH(cpdr);
    sysUCALLQ(coerce_table);
enddefine;

endsection;     /* $-external */


/* --- Revision History ---------------------------------------------------
--- John Williams, Sep  9 1996
        Fixed BR davidy.101 (bug in updater of ArrayAccess).
--- John Gibson, Dec 21 1993
        Changed to work with new lib external.
--- Robert Duncan, May 11 1993
        Changed not to add "_" to external symbols on HP 9000/700
--- John Williams, May 10 1993
        Added "include sysdefs.ph" at the start of the file
--- John Williams, Jan 25 1993
        Now adds "_" to external symbols on HP machines too.
        (See BR isl-fr.4493).
--- Ian Rogers, Oct 22 1992
        Fixed my Nov 25 fix (see bugreport davidy.62).
--- Adrian Howard, Aug 18 1992
        Stopped -new_external_import- stacking the closures it made writeable
--- Robert John Duncan, Aug 11 1992
        Changed not to add leading underscore to external names under
        System V
--- Ian Rogers, Nov 25 1991
        Made identifiers created in -new_external_import- global (as per
    documentation). Made closures of -new_pop_ext_call- writeable (cf.
    BR rogere.53)
--- Andreas Schoter, Sep  9 1991
    Changed occurrances of -popliblist- to -popautolist-
--- John Gibson, May 31 1991
        Replaced typespec "external_ptr" with "exptr" (former is defunct)
--- John Gibson, Apr  3 1991
        Changed translation of C_float in -primitive_type- to "float"
        rather than "sfloat" ("float" is now what C returns, and "sfloat"
        is a definite single-float).
--- Ian Rogers, Feb  7 1991
    Added 2 calls to -sys_grbg_list- to -read_procedure-
--- Ian Rogers, Dec 13 1990
    Fixed to work with 13.91 cf. isl.15
--- Aaron Sloman, Sep 15 1990
    Added more cross references to header
--- Ian Rogers, Aug 31 1990
    Added -define_c_type- and -external_import_procedure-.
    General tidying.
--- Ian Rogers, Jun 28 1990
    Installed a half-decent varargs mechanism
--- Ian Rogers, Jun 20 1990
    Extensive re-write to make use of 13.8 cons_access etc.
 */
