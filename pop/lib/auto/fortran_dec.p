/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:            C.all/lib/auto/fortran_dec.p
 > Purpose:         Fortran interface for LIB EXTERNAL
 > Author:          Aled Morris and Robert James Duncan (see revisions)
 > Documentation:   HELP * EXTERNAL
 > Related Files:   LIB * EXTERNAL, LIB * C_DEC
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF fortran_dec

section $-external => fortran_dec;

uses external;


/*****************************************************************************/
/*****                         utility procedures                        *****/
/*****************************************************************************/

;;; compile time errors.
define lconstant fortran_mishap(n, mess);
    lvars n, mess;
    mishap(n, 'FORTRAN ERROR: ' >< mess);
enddefine;

;;; check that the given word is a valid Fortran symbol
define lconstant check_name(name) -> name;
    lvars name;
    unless isalphacode(subscrw(1, name)) then
        fortran_mishap(name, 1, 'Invalid Fortran symbol');
    endunless;
enddefine;

/*****************************************************************************/
/*****                          fortran front end                        *****/
/*****************************************************************************/

;;; check that it is a valid Fortran type specifier and if it is return
;;; a type specifier suitable for using with -pop_ext_call-.
define lconstant check_type(type, mode) -> type;
    lvars type mode;
    if type == "INTEGER" then        [pointer int]
    elseif type == "REAL" then       [pointer float]
    elseif type == "DOUBLE" then     [pointer double]
    elseif type == "EXTERNAL" then   [special]
    else
        fortran_mishap(type, 1, 'Invalid FORTRAN type');
    endif -> type;
    ;;; output and array variables are different
    if mode == "output" then
        back(type) -> type;
        ;;; REAL results must be type "sfloat"
        if front(type) == "float" then [sfloat] -> type endif;
    elseif mode == "array" then
        front(back(type)) -> type;
    endif;
enddefine;

;;; turn an undeclared variable into the correct default type specifier
define lconstant default_type(name, mode);
    lvars name c mode;
    if (lowertoupper(subscrw(1, name)) ->> c) >= `I` and c <= `N` then
        check_type("INTEGER", mode);
    else
        check_type("REAL", mode);
    endif;
enddefine;

;;; turn all undeclared variables into the default type and return
;;; a vector suitable for -pop_ext_call-
define lconstant do_default_typing(arglist, type_assoc);
    lvars arglist type_assoc;

    {% applist(arglist,
            procedure(i);
                lvars i j;
                ;;; completely undefined
                unless (type_assoc(i) ->> j) then
                    default_type(i, false) -> j;
                ;;; semi defined array
                elseif front(j) == "array" and not(j(3)) then
                    default_type(i, "array") -> j(3);
                endunless;
                j;
            endprocedure) %}
enddefine;

;;; read the 'bounds' and return the length of the underlying vector
;;; if this is known.
define lconstant parse_array_spec() -> size;
    lvars size = 1, bound;
    repeat
        lowertoupper(readitem()) -> bound;
        if size and isinteger(bound) then
            size * bound -> size;
        else
            check_name(bound) -> ;
            false -> size;
        endif;
    quitif(pop11_try_nextitem(")"));
        pop11_need_nextreaditem(",") -> ;
    endrepeat;
enddefine;

;;; occasionally (e.g. with an array) the "simple part" of a type spec is
;;; needed e.g. [pointer integer] should be "integer".
define lconstant deref(type);
    lvars type;
    if type.ispair then
        until back(type) == nil do back(type) -> type; enduntil;
        front(type);
    else type;
    endif;
enddefine;

;;; turn a type specification into the length of its vector and the type of
;;; its contents
define lconstant array_form(type) /* -> size -> type */ ;
    lvars type tmp;
    if front(type) == "array" then
        front(back(back(type))), front(back(type));
    else
        deref(type), false;
    endif
enddefine;

;;; take two specifications and unify them into a coherent array type specifier.
define lconstant unify_array_spec(spec1, spec2, name);
    lvars s1 s2 t1 t2 tmp spec1 spec2 name;

    array_form(spec1) -> s1 -> t1;
    array_form(spec2) -> s2 -> t2;

    [array %
        if s1 and s2 and (s1 /== s2) then
            fortran_mishap(name, 1, 'Conflict in declaration of array bounds');
        else s1 or s2;
        endif,

        if t1 and t2 and (t1 /== t2) then
            fortran_mishap(name, 1, 'Conflict in declaration of array type');
        else t1 or t2;
        endif %];
enddefine;

;;; associate a type specifier with a variable name.
define lconstant register_type_specifier(arglist, name, type, type_assoc);
    lvars arglist name type old_type type_assoc;

    unless lmember(name, arglist) then
        fortran_mishap(name, 1, 'Attempt to type non-variable');
    endunless;

    unless (type_assoc(name) ->> old_type) then
        type -> type_assoc(name);
    elseif type = old_type then
        return;
    elseif front(type) == "array" or front(old_type) == "array" then
        unify_array_spec(type, old_type, name) -> type_assoc(name);
    else
        fortran_mishap(name, 1, 'Attempt to redeclare variable');
    endunless;
enddefine;

;;; parse a Fortran type statement and collate and associate the information
;;; obtained with the variables thus declared
define lconstant type_definition(arglist, type_assoc);
    lvars type arglist type_assoc name arrtype;
    if (lowertoupper(itemread()) ->> type) == "DIMENSION" then
        repeat
            lowertoupper(readitem()) -> name;
            pop11_need_nextreaditem("(") -> ;
            [array % parse_array_spec(), default_type(name, "array") %] -> type;
            register_type_specifier(arglist, name, type, type_assoc);
        quitunless(pop11_try_nextitem(","));
        endrepeat;
    else
        if type == "DOUBLE" then
            pop11_try_nextitem([precision PRECISION]) ->;
        endif;
        ;;; check that it is a valid type
        check_type(type, false) -> type;
        repeat
            lowertoupper(itemread()) -> name;
            if pop11_try_nextitem("(") then
                [array % parse_array_spec(), deref(type) %] -> arrtype;
                register_type_specifier(arglist, name, arrtype, type_assoc);
            else
                register_type_specifier(arglist, name, type, type_assoc);
            endif;
        quitunless(pop11_try_nextitem(","));
        endrepeat;
    endif;
enddefine;

;;; parse a fortran procedure declaration with a given name and result type
define lconstant fortran_procedure(name, result);
    lvars name result arglist argvec type;
    pop11_need_nextreaditem("(") ->;

    ;;; construct a list of the variables
    [% unless pop11_try_nextreaditem(")") then
        repeat
            lowertoupper(check_name(itemread()));
            quitif(pop11_try_nextitem(")"));
            pop11_need_nextreaditem(",") ->;
        endrepeat;
    endunless %] -> arglist;

    ;;; to associate vars with their type specifiers
    newassoc([]) -> type;

    ;;; insert typing information
    until pop11_try_nextitem( [end END] ) do
        type_definition(arglist, type);
    enduntil;

    ;;; any variables that are still untyped default to integers or reals
    do_default_typing(arglist, type) -> argvec;

    ;;; construct Pop11 variable/procedure which safely applies
    ;;; the external procedure
    external_import(#_<consref("FORTRAN")>_#,
                    name,
                    argvec,
                    result,
                    false,
                    true);  ;;; simulate call by reference
enddefine;

;;; deal with SUBROUTINEs
define lconstant fortran_subroutine;
    fortran_procedure(itemread(),  [void] );
enddefine;

;;; deal with FUNCTIONS
define lconstant fortran_function(result);
    lvars result;
    if result == "DOUBLE" then
        pop11_try_nextitem( [precision PRECISION] ) ->;
    elseif result == "EXTERNAL" then
        fortran_mishap(0, 'Cannot have EXTERNAL functions')
    endif;
    pop11_need_nextreaditem( [function FUNCTION] ) ->;
    fortran_procedure(itemread(), check_type(result, "output"));
enddefine;

;;; the exported parser called by the syntax procedure -external-
define vars fortran_dec;
    lvars item;
    until (itemread() ->> item) == "endexternal" do
        lowertoupper(item) -> item;
        if item == "SUBROUTINE" then
            fortran_subroutine();
        else
            fortran_function(item);
        endif;
    enduntil;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 21 1993
        Changed to work with new lib external. Runtime parts moved to
        lib external_runtime.
--- John Williams, Apr 27 1993
        Added "include sys*defs.ph" at the start of the file
--- David S Young, Nov 27 1992
        Modified to allow macro expansion in pseudo-fortran input,
        as suggested in BR davidy.40
--- John Williams, Aug 25 1992
        Fixed BR jamesg.42 (fortran_dec.p can't parse fortran proc with no
        args)
--- Robert John Duncan, Sep 25 1991
        Modified the last change so that "sfloat" is used only for result
        (output) types, otherwise the runtime type checking breaks on
        compound arguments.
--- John Gibson, Apr  3 1991
        REAL now translates to "sfloat" not "float".
--- John Gibson, Aug 13 1989
        Replaced old sys- procedures with pop11_ ones
--- Aled Morris, Sep 15 1987 - added extra arg. to -fortran_type-mishap-
--- Rob Duncan 29th March, 1987 - removed annoying printing, and fixed bug
    which incorrectly declared output values.
 */
