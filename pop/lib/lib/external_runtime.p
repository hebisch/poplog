/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/lib/external_runtime.p
 > Purpose:         Runtime part of lib external
 > Author:          John Gibson, Nov 23 1993 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF external_runtime

section $-external =>   array_of_short, array_of_int, array_of_float,
                        array_of_integer, array_of_real,
                        array_of_double, array_of_pointer,
                        conspointer_to,
                        external_type_check,
                        external_runtime,
                        ;

;;; frozval subscripts of pop_ext_call
lconstant
    EXT_RETSPEC     = 2,
    EXT_EXPDR_ID    = 3,
;

vars external_type_check = true;     ;;; user assignable

/*****************************************************************************/
/*****                    raw external structures                        *****/
/*****************************************************************************/

/* the structures here assume the machines compiler uses 32bit integers
 * to pass all (vaguely-)integer style arguments, and doubles to pass
 * floats.
 */
defclass pointer {
    contents        :full,
>-> ptr_contents    :full
};

defclass pointer_to_short   :short;
defclass pointer_to_int     :int;
defclass pointer_to_float   :sfloat;
defclass pointer_to_double  :dfloat;
defclass pointer_to_pointer :full;      /* use only for arrays thereof */

/* procedures exported for the user to construct arrays */
vars procedure
    array_of_short  = newanyarray(% pointer_to_short_key %),
    array_of_int    = newanyarray(% pointer_to_int_key %),
    array_of_float  = newanyarray(% pointer_to_float_key %),
    array_of_double = newanyarray(% pointer_to_double_key %),

    ;;; FORTRAN names
    array_of_integer    = array_of_int,
    array_of_real       = array_of_float,
    ;

define lconstant list_assoc_val(item, assoc_list);
    lvars item, assoc_list;
    until assoc_list == [] do
        if fast_front(assoc_list) == item then
            return(fast_front(fast_back(assoc_list)))
        else
            fast_back(fast_back(assoc_list)) -> assoc_list
        endif
    enduntil;
    false
enddefine;


/*****************************************************************************/
/*****                external structure building                        *****/
/*****************************************************************************/

define array_of_pointer(points_to) -> array with_nargs 2;
    lvars   points_to,
            array   = newanyarray(/* size on stack, */ pointer_to_pointer_key),
            vec     = arrayvector(array),
        ;
    points_to -> pdprops(array);
enddefine;

/* A routine to construct a pointer to any given datum */
define conspointer_to(x);
    lvars   x, dw = dataword(x);

    if list_assoc_val(dw, #_< [ short       ^conspointer_to_short
                                integer     ^conspointer_to_int
                                decimal     ^conspointer_to_float
                                ddecimal    ^conspointer_to_double] >_# )
            ->> dw then
        dw(x, 1);
    else
        conspointer(x, x);
    endif;
enddefine;



/*****************************************************************************/
/*****                 generalised type-check procedure                  *****/
/*****************************************************************************/

constant procedure pop_ext_call;

lvars lang_name, call_by_ref, self_clsr;        ;;; args to pop_ext_call

lconstant pop_datawords = [
    short   integer
    int     integer
    float   decimal
    sfloat  decimal
    double  ddecimal
];

define lconstant error_pdr(obj, type);
    lvars obj, type;
    if isprocedure(lang_name) then chain(obj, type, lang_name) endif;
    unless lang_name then 'EXTERNAL' -> lang_name endunless;
    printf(pdprops(self_clsr), lang_name,
                        ';;; %S ERROR - Argument type mismatch (in %S)\n');
    printf(obj,         ';;; Found: %p\n');
    printf(type,        ';;; Expecting: %p\n');
    interrupt();
enddefine;


/* Verify that the -object- is of the -type- and within the -range-
 * Written for speed efficiency.
 */
define lconstant check_type_range(type, range, object);
lvars   type range object
        procedure checkpdr =
            list_assoc_val(type, #_< [  char    ^isinteger
                                        int     ^isintegral
                                        float   ^isdecimal
                                        sfloat  ^isdecimal
                                        double  ^isdecimal] >_#),
        ;
    if type == "char" then range or [0 255] -> range endif;
    unless checkpdr(object)
    and (range == []
        or (object >= fast_front(range)
            and object <= fast_front(fast_back(range))))
    then
        error_pdr(object,   if range == [] then type
                            else [% type, "range", range(1), "to", range(2) %]
                            endif);
    endunless;
enddefine;

/* Confirm that object -obj- conforms to the type specification -type-
 * The object is returned, and may be an altered form of the one given
 * in the case of arrays (where the arrayvector is returned) or when
 * simulating call-by-reference, when given a simple type, a pointer
 * may be returned.
 * This has been written for speed efficiency (not clarity!).
 */
define lconstant check_ext_arg(obj, type) -> obj;
lvars   obj,
        type,
        typeword,
        obj_type = dataword(obj),
        temp,
        tempvec,
        upper,
        lower,
        array_size,
        ;
    fast_destpair(type) -> type -> typeword;

    if typeword == "special" then
        return;
    elseif fast_lmember(typeword, #_< [char int float sfloat double] >_#) then
        check_type_range(typeword, type, obj);
    elseif typeword == "pointer" then
        if obj_type == "pointer" then
            check_ext_arg(contents(obj), type) -> temp;
            unless temp == contents(obj) then
                temp -> ptr_contents(obj)
            endunless;
        elseif call_by_ref  /* this is where pseudo-call-by-reference occurs */
        and list_assoc_val(front(type), pop_datawords) == obj_type then
            conspointer_to(obj) -> obj;
        elseif front(type) == "pointer"
        and front(back(type)) == "function" then
            if obj.isclosure and pdpart(obj) == pop_ext_call
            and frozval(EXT_RETSPEC, obj) = type then
                fast_idval(frozval(EXT_EXPDR_ID, obj)) -> obj;
            elseunless isexternal_ptr(obj) then
                error_pdr(obj, "function" :: type);
            endif;
        elseif front(type) == "function" then
            error_pdr(obj, #_< [pointer function] >_#);
        elseunless (front(type) == "char" and obj.isstring)
        or (issubstring("pointer_to_", 1, obj_type)
            and fast_front(type) == allbutfirst(11, obj_type))
        then
            error_pdr(obj, "pointer" :: type)
        endif;
    elseif typeword == "array" then
        fast_destpair(type) -> type -> array_size;
        if obj.isarray then
            pdprops(obj) -> typeword;
            arrayvector(obj) -> obj;
            dataword(obj) -> obj_type;

            unless datalength(obj_type) fi_>= 11 /* length('pointer_to_') */
            and subword(1, 11, obj_type) == "pointer_to_" then
                error_pdr(obj, "array" :: type);
            else
                unless typeword then        /* 11 = length('pointer_to_') */
                    allbutfirst(11, obj_type) :: [] -> typeword;
                endunless;

                /* check the contents of the array are of the right type */
                if typeword /= /*sic*/ type then
                    error_pdr(obj, type)
                endif;

                /* now check the size of the array, deals with upper and lower
                   bounds. As much as possible is calculated at compile time.
                   Array size has the form [size {l 10 v 4} ...] were size
                   is the precalculated size and each vector represents the
                   lower and upper bound of a dimension. A "v" proceeding the
                   number signifies that the bound is a variable, and the
                   number is its position on the stack. A "l" proceeding
                   signifies that the number represents the actual vaule of
                   the bound.*/

                if array_size.ispair then
                    fast_destpair(array_size) -> temp -> array_size;
                    until temp.null do
                        fast_destpair(temp) -> temp -> tempvec;
                        if fast_subscrv(1,tempvec) == "v" then
                            subscrpointer_to_int(1,subscr_stack(fast_subscrv(2,tempvec)))
                                -> lower
                        else
                            fast_subscrv(2,tempvec) -> lower
                        endif;
                        if fast_subscrv(3,tempvec) == "v" then
                            subscrpointer_to_int(1,subscr_stack(fast_subscrv(4,tempvec)))
                                -> upper
                        else
                            fast_subscrv(4,tempvec) -> upper
                        endif;
                        array_size fi_* (upper fi_- lower fi_+ 1) -> array_size
                    enduntil
                endif;

                if array_size and datalength(obj) fi_< array_size then
                    error_pdr(  'Array size ' sys_>< obj.datalength,
                                'Array size ' sys_>< array_size);
                endif;

                /* Deal with arrays of pointers by recursively checking
                 * all the members are pointers (and they point to objects
                 * of the right type...)
                 */
                if front(typeword) == "pointer" then

                    /* WARNING: lvars are re-used here, so their names have
                                no significance anymore */

                    fast_for type from 1 to datalength(obj) do
                        check_ext_arg((fast_subscrpointer_to_pointer(type, obj)
                                            ->> obj_type), typeword) -> temp;
                        if temp /== obj_type then
                            temp -> fast_subscrpointer_to_pointer(type, obj);
                        endif;
                    endfast_for;

                endif;
            endunless;
        elseunless front(type) == "char" and obj.isstring then
            error_pdr(obj, "array" :: type);
        endif;
    else
        error_pdr(obj, typeword);
    endif;
enddefine;

/* fast version which doesn't actually check the object */
define lconstant fast_check_ext_arg(obj, type) -> obj;
lvars   obj,
        type,
        typeword,
        obj_type = dataword(obj),
        temp,
        array_size,
        ;
    fast_destpair(type) -> type -> typeword;

    if typeword == "pointer" then
        if obj_type == "pointer" then
            fast_check_ext_arg(contents(obj), type) -> temp;
            unless temp == contents(obj) then
                temp -> ptr_contents(obj)
            endunless;
        elseif call_by_ref
        and list_assoc_val(front(type), pop_datawords) == obj_type then
            conspointer_to(obj) -> obj;
        elseif front(type) == "pointer"
        and front(back(type)) == "function"
        and obj_type == "procedure" then
            idval(frozval(EXT_EXPDR_ID, obj)) -> obj;
        endif;
    elseif typeword == "array" then
        fast_destpair(type) -> type ->;
        arrayvector(obj) -> obj;
        if front(typeword) == "pointer" then
            fast_for type from 1 to datalength(obj) do
                fast_check_ext_arg((fast_subscrpointer_to_pointer(type, obj)
                                            ->> obj_type), typeword) -> temp;
                if temp /== obj_type then
                    temp -> fast_subscrpointer_to_pointer(type, obj);
                endif;
            endfast_for;
        endif;
    endif;
enddefine;


/*****************************************************************************/
/*****                 the Pop11 procedure wrapping                      *****/
/*****************************************************************************/

/*****
 *  Mechanism for calling an external procedure, coercing and checking
 *  any arguments beforehand.
 *
 *  Takes as arguments:
 *      name:   the name of this external procedure (usually the Pop
 *              variable which holds it - i.e. it's Pop name)
 *      args:   a vector of type specifiers (see HELP * EXTERNAL)
 *              there should be one type specifier per input argument
 *              for the external procedure - the arguments are assumed
 *              to be on the stack.
 *      result: a single type specifier which indicates the return value
 *              of the external procedure
 *      expdr_id:
 *              an identifier containing an external pointer
 *      lang_name:
 *              name of language (a word), or false, or a procedure for
 *              printing fatal error messages
 *      call_by_ref:
 *              a flag indicating whether pointers should be constructed
 *              to simulate call-by-reference for languages like Fortran.
 *      self_clsr:
 *              the closure of pop_ext_call being executed
 *
 *  NOTE:   for reasons of efficiency it is assumed that all the arguments
 *          are well formed.
 *****/

define pop_ext_call(args, result, expdr_id, lang_name, call_by_ref, self_clsr);
lvars   i, oldval, val, type, expdr,
        name, args, result, expdr_id,
        rettype     = fast_front(result),
        nargs       = datalength(args),
        ident_list  = [],
        ;
dlocal  lang_name, call_by_ref, self_clsr
        ;

    unless fast_idval(expdr_id) ->> expdr then
        mishap(0, 'Applying unloaded External Procedure')
    endunless;

    if rettype == "void" then
        1 -> rettype    ;;; NONE
    else
        list_assoc_val(rettype, #_< [   char    2   ;;; INT
                                        short   2   ;;; INT
                                        int     2   ;;; INT
                                        float   3   ;;; FLOAT
                                        sfloat  4   ;;; SFLOAT
                                        double  5   ;;; DFLOAT
                                ] >_#) -> rettype;
        unless rettype then
            mishap(result, 1, 'COMPLEX RESULT NOT SUPPORTED');
        endunless;
    endif;

    /* the main argument checking loop.
     * Each actual parameter is examined in turn - any identifiers
     * encountered (where a pointer is expected) are converted to
     * pointers.  Each parameter is verified against its type specifier
     * by the routine -check_ext_arg- (below)
     */
    fast_for i from 1 to nargs do
        subscr_stack(i) -> oldval;
        fast_subscrv((nargs fi_- i) fi_+ 1, args) -> type;
        if oldval.isident and fast_front(type) == "pointer" then
            conspair(oldval, ident_list) -> ident_list;
            conspointer_to(oldval.idval) -> val;
            val ->> idval(oldval) ->> oldval -> subscr_stack(i);
        endif;

        if external_type_check then
            check_ext_arg(oldval, type)
        else
            fast_check_ext_arg(oldval, type)
        endif -> val;
        unless val == oldval then val -> subscr_stack(i) endunless;
    endfast_for;

    nargs;      ;;; push number of args

    go_on rettype to NONE INT FLOAT SFLOAT DFLOAT;
    ;;;               1    2    3      4      5

    NONE:
        exacc (...) expdr();
        goto CONTINUE;

    INT:
        exacc (...):int expdr();
        goto CONTINUE;

    FLOAT:
        exacc (...):float expdr();     ;;; C "float" result
        goto CONTINUE;

    SFLOAT:
        exacc (...):sfloat expdr();    ;;; definite single-float
        goto CONTINUE;

    DFLOAT:
        exacc (...):dfloat expdr();
        goto CONTINUE;

    CONTINUE:

    /* Any identifiers which were previously converted to pointers
     * are now restored.
     */
    fast_for i in ident_list do
        idval(i) -> oldval;
        if (datakey(oldval) ->> val) == pointer_key then
            contents(oldval) -> idval(i);
        elseif val == pointer_to_double_key then
            ;;; force return of a double precision value
            procedure; dlocal popdprecision=true;
                class_fast_subscr(val)(1, oldval) -> idval(i);
            endprocedure();
        else
            class_fast_subscr(val)(1, oldval) -> idval(i);
        endif;
    endfor;
    sys_grbg_list(ident_list)
enddefine;

constant external_runtime = true;

endsection;     /* $-external */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 25 1995
        Substituted lvar self with self_clsr (clashed with lib flavours)
 */
