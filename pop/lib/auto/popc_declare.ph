/* --- Copyright University of Sussex 2005. All rights reserved. ----------
 > File:            C.all/lib/auto/popc_declare.ph
 > Purpose:         $popautolib identifier declarations for POPC
 > Author:          John Gibson, Oct 10 1992 (see revisions)
 */

library_declare_section '$popautolib/'

section;

weak global constant syntax (
        defclass,
        exacc,
        exload,
        in_property,
        load,
        p_typespec,
        recordclass,
        with_index,
    );

weak global constant macro (
        include,
    );

weak global constant active (
        $-typespec_utils$-exload_isbatching,
    );

weak global constant procedure (
        appdic,
        catch,
        check_string,
        check_vector,
        check_word,
        checkinteger,
        class_subscr_loop,
        cons,
        cons_network_device,
        database_add,
        database_allpresent,
        database_present,
        database_remove,
        delete,
        denominator,
        discappend,
        exload_do_batch_load,
        exload_runtime_assign,
        expandlist,
        exprread,
        exval_to_popobj,
        fast_ncrev,
        fi_check,
        float_digits,
        gensym,
        imagpart,
        is_subsystem_loaded,
        is_subsystem_sensitive,
        is_syntax_word,
        isdynamic,
        ismidstring,
        issection,
        key_of_dataword,
        last,
        listread,
        mapdata,
        mapdic,
        nc_listsort,
        ncdelete,
        ncrev,
        newanysparse,
        newassoc,
        newmapping,
        nl,
        npr,
        nprintf,
        numerator,
        oneof,
        popindex,
        proglist_read_by,
        readimp,
        readtill,
        realpart,
        set_global_valof,
        sort,
        sp,
        sprintf,
        subscr_loop,
        sys_compiler_subsystem,
        sys_file_exists,
        sys_file_match,
        sys_parse_url,
        sys_restore_matchvars,
        sys_runtime_apply,
        sys_search_unix_path,
        sys_split_url,
        sysgetusername,
        syslibwarning,
        sysobey_list,       ;;; VMS
        sysobeylist,
        sysparse_string,
        syssort,
        tabs,
        typespecread,
        uses_lib_idents,

        $-typespec_utils$-def_typespec,
        $-typespec_utils$-exacc_result_spec,
        $-typespec_utils$-exload_addbatch,
);

weak global constant
        csh_subsystem,
        dcl_subsystem,      ;;; VMS
        exptrvec_key,
        nullvector,
        sh_subsystem,
        sysstring,
        sysstringlen,
        typespec_utils,
;

weak global vars syntax (
        external,
    );

weak global vars macro (
        echoload,
    );

weak global vars procedure (
        gensym_property,
        loadlib,
        popready,
        princludewarn,
        rawoutflush,
        warning,
        $-external$-external_import,
);

weak global vars
        database_it,
        database_them,
        popincludelist,
        poppackagelist,
        popuseslist,
        popsyslist,
        $-typespec_utils$-exld_batch_count,
;

declare_incremental list (
        popincludelist,
        poppackagelist,
        popuseslist,
        popsyslist,
);


endsection;

end_library_declare_section;

#_INCLUDE '../lib/popc_declare.ph'

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jan 16 2005
        Added poppackageslist
 */
