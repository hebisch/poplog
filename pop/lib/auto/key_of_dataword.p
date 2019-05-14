/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/key_of_dataword.p
 > Purpose:         Create a property mapping datawords to keys
 > Author:          John Williams, Feb 12 1990 (see revisions)
 > Documentation:   HELP * KEY_OF_DATAWORD
 > Related Files:
 */
compile_mode:pop11 +strict;

section;

define key_of_dataword =
    newproperty([], 64, false, "tmpboth")
enddefine;


appdata({%
        biginteger_key,
        boolean_key,
        complex_key,
        ddecimal_key,
        decimal_key,
        device_key,
        external_ptr_key,
        ident_key,
        integer_key,
        intvec_key,
        key_key,
#_IF DEF matchvar_key or DEF POPC_COMPILING
        matchvar_key,
#_ENDIF
        nil_key,
        pair_key,
        procedure_key,
        process_key,
        prologterm_key,
        prologvar_key,
        ratio_key,
        ref_key,
        section_key,
        shortvec_key,
        stackmark_key,
        string_key,
        termin_key,
        undef_key,
        vector_key,
        word_key,
        $-lisp$-character_key,
        $-lisp$-function_token_key,
        $-lisp$-package_external_entry_key,
        $-lisp$-package_internal_entry_key,
        $-lisp$-package_key,
        $-lisp$-symbol_key,
#_IF DEF XptDescriptor_key
        XptDescriptor_key
#_ENDIF
        %},
        procedure(key);
            key -> key_of_dataword(class_dataword(key))
        endprocedure);

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 11 1995
        Added matchvar_key
--- Jonathan Meyer, Oct  3 1992
        Added external_ptr_key and XptDescriptor_key.
--- John Williams, Feb 12 1990
        Completely re-written using '_key' constants.
        Includes keys for Lisp datatypes too.
 */
