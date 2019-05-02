/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.all/lisp/src/clos-types.p
 > Purpose:         Create classes for built-in Lisp datatypes
 > Author:          John Williams, Apr 26 1994 (see revisions)
 > Documentation:
 > Related Files:   C.all/lisp/src/clos.p, C.all/lisp/src/types.p
 */

lisp_compile_mode;

section $-lisp;


define lconstant System_class(name, meta, supers, key_or_p);
    if key_or_p
    and not(type_predicate(name)) then
        ;;; [Assigning type_predicate for ^name] =>
        if iskey(key_or_p) then
            class_recognise(key_or_p)
        else
            key_or_p
        endif -> type_predicate(name)
    endif;
    clos_define_class(name, meta, supers, [], [], [], false);
    if iskey(key_or_p) then
        get_class_by_name(name) -> get_class_by_key(key_or_p)
    endif
enddefine;


define built_in_class(name, supers, key);
    dlocal lisp_system_building = true;
    System_class(name, @BUILT-IN-CLASS, supers, key)
enddefine;


define structure_class(name, includes, key);
    System_class(
        name,
        @STRUCTURE-CLASS,
        [% includes or @STRUCTURE-OBJECT %],
        key)
enddefine;


/* Create classes for built-in Common Lisp types.
    (simple array types & generic function types not right yet)
*/

constant procedure (is_gfn_clos);

define generic_function_p() with_nargs 1;
    if is_gfn_clos() then true else false endif
enddefine;


built_in_class(@T, [], false);

built_in_class(@FIXNUM,         [% @INTEGER %],             integer_key);
built_in_class(@BIGNUM,         [% @INTEGER %],             biginteger_key);
built_in_class(@INTEGER,        [% @RATIONAL %],            isintegral);
built_in_class(@RATIO,          [% @RATIONAL %],            ratio_key);
built_in_class(@RATIONAL,       [% @REAL %],                isrational);
built_in_class(@SINGLE-FLOAT,   [% @FLOAT %],               decimal_key);
built_in_class(@DOUBLE-FLOAT,   [% @FLOAT %],               ddecimal_key);
built_in_class(@FLOAT,          [% @REAL %],                isdecimal);
built_in_class(@REAL,           [% @NUMBER %],              isreal);
built_in_class(@COMPLEX,        [% @NUMBER %],              complex_key);
built_in_class(@NUMBER,         [],                         isnumber);

built_in_class(@BIT-VECTOR,     [% @VECTOR %],              bitvectorp);
built_in_class(@BASE-STRING,    [% @STRING %],              stringp);
built_in_class(@STRING,         [% @VECTOR %],              stringp);
built_in_class(@VECTOR,         [% @ARRAY, @SEQUENCE %],    vectorp);
built_in_class(@ARRAY,          [],                         arrayp);

built_in_class(@SIMPLE-ARRAY,       [% @ARRAY %],           simple_array_p);
built_in_class(@SIMPLE-BIT-VECTOR,  [% @BIT-VECTOR, @SIMPLE-ARRAY %], bitvector_key);
built_in_class(@SIMPLE-STRING,      [% @STRING, @SIMPLE-ARRAY %], string_key);
built_in_class(@SIMPLE-VECTOR,      [% @VECTOR, @SIMPLE-ARRAY %], vector_key);
built_in_class(@SIMPLE-BASE-STRING, [% @BASE-STRING, @SIMPLE-STRING %], string_key);

built_in_class(@NULL,           [% @SYMBOL, @LIST %],       nil_key);
built_in_class(@CONS,           [% @LIST %],                pair_key);
built_in_class(@LIST,           [% @SEQUENCE %],            listp);
built_in_class(@SEQUENCE,       [],                         sequencep);

built_in_class(@KEYWORD,        [% @SYMBOL %],              keywordp);
built_in_class(@SYMBOL,         [],                         symbol_key);

built_in_class(@STANDARD-CHAR,  [% @BASE-CHAR %],           standard_char_p);
built_in_class(@BASE-CHAR,      [% @CHARACTER %],           character_key);
built_in_class(@EXTENDED-CHAR,  [% @CHARACTER %],           not);
built_in_class(@CHARACTER,      [],                         character_key);

built_in_class(@FUNCTION,       [],                         functionp);
built_in_class(@COMPILED-FUNCTION,  [% @FUNCTION %],        compiled_function_p);
built_in_class(@GENERIC-FUNCTION,   [% @COMPILED-FUNCTION %],       generic_function_p);
built_in_class(@STANDARD-GENERIC-FUNCTION, [% @GENERIC-FUNCTION %], generic_function_p);

built_in_class(@HASH-TABLE,     [],                         isproperty);
built_in_class(@PACKAGE,        [],                         package_key);
built_in_class(@PATHNAME,       [],                         pathname_key);
built_in_class(@RANDOM-STATE,   [],                         random_state_key);
built_in_class(@READTABLE,      [],                         readtable_key);
built_in_class(@STREAM,         [],                         stream_key);


/* CLASS-OF */

define lconstant Choose_procedure_class(item);
    if isarray(item) then
        if pdnargs(item) == 1 then
            datakey(arrayvector(item)) -> item;
            if item == string_key then
                CLASS @STRING
            elseif item == bitvector_key then
                CLASS @BIT-VECTOR
            else
                CLASS @VECTOR
            endif
        else
            CLASS @ARRAY
        endif
    elseif isproperty(item) then
        CLASS @HASH-TABLE
    elseif is_gfn_clos(item) then
        CLASS @STANDARD-GENERIC-FUNCTION
    else
        CLASS @COMPILED-FUNCTION
    endif
enddefine;


define sys_class_of(item);
    lvars key, class;
    if isinstance(item) then
        i_class(item)
    else
        datakey(item) -> key;
        if (get_class_by_key(key) ->> class) then
            class
        elseif key == procedure_key then
            Choose_procedure_class(item)
        elseif item == true then
            CLASS @SYMBOL
        else
            /* Pop-11 class */
            System_class(
                word_to_sym(class_dataword(key)),
                @BUILT-IN-CLASS, [], key);
            get_class_by_key(key)
        endif
    endif
enddefine;


define sys_type_of() with_nargs 1;
    class_name(sys_class_of())
enddefine;


define class_of() with_nargs 1;
    Return_ioc(sys_class_of())
enddefine;


isinstance -> type_predicate(@STANDARD-OBJECT);


endsection;


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Aug 27 1999
        replaced define_class with clos_define_class
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
 */
