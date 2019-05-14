/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/lib/popc_declare.ph
 > Purpose:         Identifier declarations for POPC in this directory
 > Author:          John Gibson, Oct 10 1992
 */

library_declare_section '$popliblib/'

section;

weak global constant procedure (
        consshadowkey,
        define_inline_macro_base,
        exload_flatten_objfiles,
        exload_merge_objfiles,
        shadowclass_cons,
        shadowclass_dest,
        shadowclass_field,
        shadowclass_fill,
        shadowclass_import,
        shadowclass_init,
        shadowclass_recognise,
        shadowclass_refresh,
        shadowclass_subscr,
        shell_compile,
        sys_socket,
        sys_socket_accept,
        sys_socket_name,
        sys_socket_peername,
        sys_socket_to_service,
        $-lib$-db_try_all_matches,
        $-lib$-shell_send,
        $-lib$-newanysparse_base,
    );

weak global constant
        actor,
        external_runtime,
        popxlib,
        shadowclass_data,
        shadowkey_key,
        unix_sockets,
        $-in_array$-runtime,
    ;

weak global vars syntax (
        time,
    );

weak global vars procedure (
        drawline,
        newpr,
        newqueue,
        showtree,
        showtree_mid,
        showtree_node_daughters,
        showtree_node_location,
    );

weak global vars
        prologliblist,
        showtree_name,
    ;

declare_incremental list (
        prologliblist,
    );

endsection;


section $-typespec_utils;

weak global constant procedure (
        checkr_name,
        compound_fldmode,
        deref_struct1,
        exacc_attributes,
        get_declarator,
        need_nextitem,
        prefix_field,
        read_typespec,
        split_chars,
        type_idname,
    );

weak global constant
        POINTER,
        spec_starters,
    ;

endsection;

section $-shadowkey;

weak global constant procedure (
        isshadowkey,
        shkP_Cons,
        shkP_Dest,
        shkP_Field_swap,
        shkP_Fill,
        shkP_Import,
        shkP_Recog,
        shkP_Refresh,
        shkP_Refresh_subscr,
        shkP_Subscr,
        shkP_Subscr_check,
        shk_fldmode,
        shk_procs,
        shk_spec,
    );

endsection;

end_library_declare_section;

#_INCLUDE '../ved/popc_declare.ph'
