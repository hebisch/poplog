/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/exportv.p
 > Purpose:         Export built-in variables to Lisp
 > Author:          John Williams, Oct 18 1987 (see revisions)
 > Documentation:
 > Related Files:   C.all/lisp/src/exports
 */

lisp_compile_mode;

section $-lisp => lispsynonym;


define global lispsynonym(sym, word);
    identof(word) -> identof(sym);
    proclaim([^@SPECIAL ^sym])
enddefine;


/* Variables */

lispsynonym(@***,                             "star3");
lispsynonym(@**,                              "star2");
lispsynonym(@*,                               "star1");
lispsynonym(@*DEBUG-IO*,                      "debug_io");
lispsynonym(@*DEFAULT-PATHNAME-DEFAULTS*,     "default_pathname");
lispsynonym(@*ERROR-OUTPUT*,                  "error_output");
lispsynonym(@*FEATURES*,                      "features");
lispsynonym(@*GENSYM-COUNTER*,                "gensym_count");
lispsynonym(@*LOAD-PRINT*,                    "load_print");
lispsynonym(@*LOAD-VERBOSE*,                  "load_verbose");
lispsynonym(@*MACROEXPAND-HOOK*,              "macroexpand_hook");
lispsynonym(@*MODULES*,                       "modules");
lispsynonym(@*PACKAGE*,                       "package");
lispsynonym(@*PRINT-ARRAY*,                   "print_array");
lispsynonym(@*PRINT-BASE*,                    "print_base");
lispsynonym(@*PRINT-CASE*,                    "print_case");
lispsynonym(@*PRINT-CIRCLE*,                  "print_circle");
lispsynonym(@*PRINT-ESCAPE*,                  "print_escape");
lispsynonym(@*PRINT-GENSYM*,                  "print_gensym");
lispsynonym(@*PRINT-LENGTH*,                  "print_length");
lispsynonym(@*PRINT-LEVEL*,                   "print_level");
lispsynonym(@*PRINT-LINES*,                   "print_lines");
lispsynonym(@*PRINT-MISER-WIDTH*,             "print_miser_width");
lispsynonym(@*PRINT-PRETTY*,                  "print_pretty");
lispsynonym(@*PRINT-RADIX*,                   "print_radix");
lispsynonym(@*PRINT-READABLY*,                "print_readably");
lispsynonym(@*PRINT-RIGHT-MARGIN*,            "print_right_margin");
lispsynonym(@*PRINT-PPRINT-DISPATCH*,         "print_pprint_dispatch");
lispsynonym(@*QUERY-IO*,                      "query_io");
lispsynonym(@*RANDOM-STATE*,                  "sys_random_state");
lispsynonym(@*READ-BASE*,                     "read_base");
lispsynonym(@*READ-DEFAULT-FLOAT-FORMAT*,     "read_default_float_format");
lispsynonym(@*READ-EVAL*,                     "read_eval");
lispsynonym(@*READ-SUPPRESS*,                 "read_suppress");
lispsynonym(@*READTABLE*,                     "readtable");
lispsynonym(@*STANDARD-INPUT*,                "standard_input");
lispsynonym(@*STANDARD-OUTPUT*,               "standard_output");
lispsynonym(@*TERMINAL-IO*,                   "terminal_io");
lispsynonym(@*TRACE-OUTPUT*,                  "trace_output");
lispsynonym(@+++,                             "plus3");
lispsynonym(@++,                              "plus2");
lispsynonym(@+,                               "plus1");
lispsynonym(@-,                               "minus1");
lispsynonym(@/,                               "slash1");
lispsynonym(@//,                              "slash2");
lispsynonym(@///,                             "slash3");


/* Poplog-specific variables (and the constant termin) */

lispsynonym(@POPLOG:*BREAK-ON-ERRORS*,        "break_on_errors");
lispsynonym(@POPLOG:*BREAK-ON-INTERRUPTS*,    "break_on_interrupts");
lispsynonym(@POPLOG:*BREAK-ON-WARNINGS*,      "break_on_warnings");
lispsynonym(@POPLOG:*CONSTANT-FUNCTIONS*,     "constant_functions");
lispsynonym(@POPLOG:*CURRENT-DIRECTORY*,      "current_directory");
lispsynonym(@POPLOG:*DEBUGGER-CONDITION*,     "db_condition");
lispsynonym(@POPLOG:*DEFAULT-PACKAGE-SIZE*,   "default_package_size");
lispsynonym(@POPLOG:*DEFAULT-PACKAGE-USE-LIST*,"default_package_use_list");
lispsynonym(@POPLOG:*ERROR-PRINT-LENGTH*,     "error_print_length");
lispsynonym(@POPLOG:*ERROR-PRINT-LEVEL*,      "error_print_level");
lispsynonym(@POPLOG:*INSPECT-PRINT-LENGTH*,   "inspect_print_length");
lispsynonym(@POPLOG:*INSPECT-PRINT-LEVEL*,    "inspect_print_level");
lispsynonym(@POPLOG:*LISP-CALLING-LIMIT*,     "lisp_calling_limit");
#_IF VED_LOADED
lispsynonym(@POPLOG:*LISPFILETYPES*,          "lispfiletypes");
#_ENDIF
lispsynonym(@POPLOG:*LOAD-LOCK*,              "load_lock");
lispsynonym(@POPLOG:*MAKING-SAVED-IMAGE*,     "making_saved_image");
lispsynonym(@POPLOG:*MODULE-DIRECTORY-LIST*,  "module_directory_list");
lispsynonym(@POPLOG:*RAW-TERMINAL-IO*,        "raw_io");
lispsynonym(@POPLOG:*READ-PROMPT*,            "popprompt");
lispsynonym(@POPLOG:*TIME-ZONE*,              "pop_time_zone");
lispsynonym(@POPLOG:*TRACE-ARGS*,             "trace_args");
lispsynonym(@POPLOG:*TRACE-PRINT-LENGTH*,     "trace_print_length");
lispsynonym(@POPLOG:*TRACE-PRINT-LEVEL*,      "trace_print_level");
lispsynonym(@POPLOG:*TRACE-RESULTS*,          "trace_results");
lispsynonym(@POPLOG:<TERMIN>,                 "termin");

lispsynonym(@SYS:*CATCH-TAG*,                 "Catch_tag");
lispsynonym(@SYS:*CATCH-OK*,                  "Catch_ok");
lispsynonym(@SYS:*THROW-TAG*,                 "Throw_tag");

{^@SYS:*CATCH-TAG* ^@SYS:*CATCH-OK*} -> f_specials(catch);

{^@SYS:*THROW-TAG* } -> f_specials(throw);


/* Constants  (NIL and T set up in base POPLOG system) */

lispsynonym(@ARRAY-DIMENSION-LIMIT,           "array_dimension_limit");
lispsynonym(@ARRAY-RANK-LIMIT,                "array_rank_limit");
lispsynonym(@ARRAY-TOTAL-SIZE-LIMIT,          "array_total_size_limit");
lispsynonym(@BOOLE-1,                         "boole_1");
lispsynonym(@BOOLE-2,                         "boole_2");
lispsynonym(@BOOLE-AND,                       "boole_and");
lispsynonym(@BOOLE-ANDC1,                     "boole_andc1");
lispsynonym(@BOOLE-ANDC2,                     "boole_andc2");
lispsynonym(@BOOLE-C1,                        "boole_c1");
lispsynonym(@BOOLE-C2,                        "boole_c2");
lispsynonym(@BOOLE-CLR,                       "boole_clr");
lispsynonym(@BOOLE-EQV,                       "boole_eqv");
lispsynonym(@BOOLE-IOR,                       "boole_ior");
lispsynonym(@BOOLE-NAND,                      "boole_nand");
lispsynonym(@BOOLE-NOR,                       "boole_nor");
lispsynonym(@BOOLE-ORC1,                      "boole_orc1");
lispsynonym(@BOOLE-ORC2,                      "boole_orc2");
lispsynonym(@BOOLE-SET,                       "boole_set");
lispsynonym(@BOOLE-XOR,                       "boole_xor");
lispsynonym(@CALL-ARGUMENTS-LIMIT,            "call_arguments_limit");
lispsynonym(@CHAR-CODE-LIMIT,                 "char_code_limit");
lispsynonym(@INTERNAL-TIME-UNITS-PER-SECOND,  "internal_time_units_per_sec");
lispsynonym(@LAMBDA-LIST-KEYWORDS,            "lambda_list_keywords");
lispsynonym(@LAMBDA-PARAMETERS-LIMIT,         "lambda_parameters_limit");
lispsynonym(@MOST-NEGATIVE-FIXNUM,            "pop_min_int");
lispsynonym(@MOST-POSITIVE-FIXNUM,            "pop_max_int");
lispsynonym(@MULTIPLE-VALUES-LIMIT,           "multiple_values_limit");
lispsynonym(@PI,                              "pi");

lispsynonym(@MOST-POSITIVE-SINGLE-FLOAT,      "pop_most_positive_decimal");
lispsynonym(@LEAST-POSITIVE-SINGLE-FLOAT,     "pop_least_positive_decimal");
lispsynonym(@LEAST-NEGATIVE-SINGLE-FLOAT,     "pop_least_negative_decimal");
lispsynonym(@MOST-NEGATIVE-SINGLE-FLOAT,      "pop_most_negative_decimal");
lispsynonym(@MOST-POSITIVE-DOUBLE-FLOAT,      "pop_most_positive_ddecimal");
lispsynonym(@LEAST-POSITIVE-DOUBLE-FLOAT,     "pop_least_positive_ddecimal");
lispsynonym(@LEAST-NEGATIVE-DOUBLE-FLOAT,     "pop_least_negative_ddecimal");
lispsynonym(@MOST-NEGATIVE-DOUBLE-FLOAT,      "pop_most_negative_ddecimal");
lispsynonym(@SINGLE-FLOAT-EPSILON,            "pop_plus_epsilon_decimal");
lispsynonym(@SINGLE-FLOAT-NEGATIVE-EPSILON,   "pop_minus_epsilon_decimal");
lispsynonym(@DOUBLE-FLOAT-EPSILON,            "pop_plus_epsilon_ddecimal");
lispsynonym(@DOUBLE-FLOAT-NEGATIVE-EPSILON,   "pop_minus_epsilon_ddecimal");

lispsynonym(@MOST-POSITIVE-SHORT-FLOAT,       "pop_most_positive_decimal");
lispsynonym(@LEAST-POSITIVE-SHORT-FLOAT,      "pop_least_positive_decimal");
lispsynonym(@LEAST-NEGATIVE-SHORT-FLOAT,      "pop_least_negative_decimal");
lispsynonym(@MOST-NEGATIVE-SHORT-FLOAT,       "pop_most_negative_decimal");
lispsynonym(@MOST-POSITIVE-LONG-FLOAT,        "pop_most_positive_ddecimal");
lispsynonym(@LEAST-POSITIVE-LONG-FLOAT,       "pop_least_positive_ddecimal");
lispsynonym(@LEAST-NEGATIVE-LONG-FLOAT,       "pop_least_negative_ddecimal");
lispsynonym(@MOST-NEGATIVE-LONG-FLOAT,        "pop_most_negative_ddecimal");
lispsynonym(@SHORT-FLOAT-EPSILON,             "pop_plus_epsilon_decimal");
lispsynonym(@SHORT-FLOAT-NEGATIVE-EPSILON,    "pop_minus_epsilon_decimal");
lispsynonym(@LONG-FLOAT-EPSILON,              "pop_plus_epsilon_ddecimal");
lispsynonym(@LONG-FLOAT-NEGATIVE-EPSILON,     "pop_minus_epsilon_ddecimal");


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Jun 29 1995
        Now exports making_saved_image.
--- John Williams, Jun  1 1995
        Ved identifiers now guarded with #_IF VED_LOADED
--- John Williams, May 16 1995
        Now exports print_right_margin, print_lines, print_miser_width &
        print_pprint_dispatch.
--- John Williams, Apr  3 1995
        Added *PRINT-READABLY* (print_readably).
--- John Williams, Feb 28 1995
        Added POPLOG:*DEBUGGER-CONDITION* (db_condition).
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Jul 28 1994
        Catch_tag, Catch_ok, and Throw_tag now exported.
--- John Williams, Jun  7 1994
        Changes for POPLOG package.
--- John Williams, Aug 27 1993
        Removed CHAR-BITS-LIMIT and friends.
--- John Williams, Aug 11 1993
        Added *read-eval* and *gensym-counter*.
--- John Williams, Jun 17 1993
        Now exports *error_print_length* and *error_print_level*.
 */
