/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.all/lib/lib/odbc.p
 > Purpose:         Pop interface to ODBC
 > Author:          John Gibson, May  1 1996 (see revisions)
 > Documentation:   REF * ODBC
 > Related Files:   INCLUDE * ODBC
 */
compile_mode :pop11 +strict;

section;
exload_batch;

include odbc.ph;
include sysdefs.ph;

l_typespec RETCODE :short;

exload odbc [^^ODBC_EXLIBS]

lconstant
    SQLAllocConnect(henv,phdbc) :RETCODE,
    SQLAllocEnv(phenv) :RETCODE,
    SQLAllocStmt(hdbc,phstmt) :RETCODE,
    SQLBindCol(hstmt,icol,fCType,rgbValue,cbValueMax,pcbValue) :RETCODE,
    SQLBindParameter(hstmt,ipar,fParamType,fCType,fSqlType,cbColDef,ibScale,
                rgbValue,cbValueMax,pcbValue) :RETCODE,
    SQLBrowseConnect(hdbc,szConnStrIn,cbConnStrIn,szConnStrOut,cbConnStrOutMax,
                pcbConnStrOut) :RETCODE,
    SQLCancel(hstmt) :RETCODE,
    SQLColAttributes(hstmt,icol,fDescType,rgbDesc,cbDescMax,pcbDesc,pfDesc)
                                                                :RETCODE,
    SQLColumnPrivileges(hstmt,szCatalogName,cbCatalogName,szSchemaName,
                cbSchemaName,szTableName,cbTableName,szColumnName,
                cbColumnName) :RETCODE,
    SQLColumns(hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName,
                szTableName,cbTableName,szColumnName,cbColumnName) :RETCODE,
    SQLDataSources(henv,fDirection,szDSN,cbDSNMax,pcbDSN,szDescription,
                cbDescriptionMax,pcbDescription) :RETCODE,
    SQLDescribeCol(hstmt,icol,szColName,cbColNameMax,pcbColName,pfSqlType,
                pcbColDef,pibScale,pfNullable) :RETCODE,
    SQLDescribeParam(hstmt,ipar,pfSqlType,pcbParamDef,pibScale,pfNullable)
                                                        :RETCODE,
    SQLDisconnect(hdbc) :RETCODE,
    SQLDriverConnect(hdbc,hwnd,szConnStrIn,cbConnStrIn,szConnStrOut,
                cbConnStrOutMax,pcbConnStrOut,fDriverCompletion) :RETCODE,
    SQLDrivers(henv,fDirection,szDriverDesc,cbDriverDescMax,pcbDriverDesc,
                szDriverAttributes,cbDrvrAttrMax,pcbDrvrAttr) :RETCODE,
    SQLError(henv,hdbc,hstmt,szSqlState,pfNativeError,szErrorMsg,
                cbErrorMsgMax,pcbErrorMsg) :RETCODE,
    SQLExecDirect(hstmt,szSqlStr,cbSqlStr) :RETCODE,
    SQLExecute(hstmt) :RETCODE,
    SQLExtendedFetch(hstmt,fFetchType,irow,pcrow,rgfRowStatus) :RETCODE,
    SQLFetch(hstmt) :RETCODE,
    SQLForeignKeys(hstmt,szPkCatalogName,cbPkCatalogName,szPkSchemaName,
                cbPkSchemaName,szPkTableName,cbPkTableName, szFkCatalogName,
                cbFkCatalogName,szFkSchemaName,cbFkSchemaName,szFkTableName,
                cbFkTableName) :RETCODE,
    SQLFreeConnect(hdbc) :RETCODE,
    SQLFreeEnv(henv) :RETCODE,
    SQLFreeStmt(hstmt,fOption) :RETCODE,
    SQLGetConnectOption(hdbc,fOption,pvParam) :RETCODE,
    SQLGetCursorName(hstmt,szCursor,cbCursorMax,pcbCursor) :RETCODE,
    SQLGetData(hstmt,icol,fCType,rgbValue,cbValueMax,pcbValue) :RETCODE,
    SQLGetFunctions(hdbc,fFunction,pfExists) :RETCODE,
    SQLGetInfo(hdbc,fInfoType,rgbInfoValue,cbInfoValueMax,pcbInfoValue) :RETCODE,
    SQLGetStmtOption(hstmt,fOption,pvParam) :RETCODE,
    SQLGetTypeInfo(hstmt,fSqlType) :RETCODE,
    SQLNativeSql(hdbc,szSqlStrIn,cbSqlStrIn,szSqlStr,cbSqlStrMax,pcbSqlStr)
                                                :RETCODE,
    SQLNumParams(hstmt,pcpar) :RETCODE,
    SQLNumResultCols(hstmt,pccol) :RETCODE,
    SQLParamData(hstmt,prgbValue) :RETCODE,
    SQLPrepare(hstmt,szSqlStr,cbSqlStr) :RETCODE,
    SQLPrimaryKeys(hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName,
                szTableName,cbTableName) :RETCODE,
    SQLPutData(hstmt,rgbValue,cbValue) :RETCODE,
    SQLRowCount(hstmt,pcrow) :RETCODE,
    SQLSetConnectOption(hdbc,fOption,vParam) :RETCODE,
    SQLSetCursorName(hstmt,szCursor,cbCursor) :RETCODE,
    SQLSetPos(hstmt,irow,fOption,fLock) :RETCODE,
    SQLSetStmtOption(hstmt,fOption,vParam) :RETCODE,
    SQLSpecialColumns(hstmt,fColType,szCatalogName,cbCatalogName,szSchemaName,
                cbSchemaName,szTableName,cbTableName,fScope,fNullable) :RETCODE,
    SQLStatistics(hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName,
                szTableName,cbTableName,fUnique,fAccuracy) :RETCODE,
    SQLTablePrivileges(hstmt,szCatalogName,cbCatalogName,szSchemaName,
                cbSchemaName,szTableName,cbTableName) :RETCODE,
    SQLTables(hstmt,szCatalogName,cbCatalogName,szSchemaName,cbSchemaName,
                szTableName,cbTableName,szTableType,cbTableType) :RETCODE,
    SQLTransact(henv,hdbc,fType) :RETCODE,

endexload;


lconstant macro (
    ;;; handle types (_d versions mean dead is OK)
    ENV_HANDLE      = 2:0001,   ENV_HANDLE_d    = -2:1e4 || ENV_HANDLE,
    CONN_HANDLE     = 2:0110,   CONN_HANDLE_d   = -2:1e4 || CONN_HANDLE,
    CCNN_HANDLE     = 2:0010,
    STMT_HANDLE     = 2:1000,   STMT_HANDLE_d   = -2:1e4 || STMT_HANDLE,

    ;;; handle_data vector for ENV_HANDLE
    EN_CONNECTIONS  = 1,    ;;; temp prop
    EN_VEC_LEN      = 1,

    ;;; handle_data vector for CONN_HANDLE
    CN_ENV_HANDLE   = 1,
    CN_STATEMENTS   = 2,    ;;; temp prop
    CN_SOURCE       = 3,
    CN_LONG_DATA_MAX= 4,
    CN_LONG_LEN_NA  = 5,
    CN_ASYNC_P      = 6,
    CN_FUNC_MASK    = 7,
    CN_VEC_LEN      = 7,

    ;;; handle_data vector for STMT_HANDLE
    ST_CONN_HANDLE  = 1,
    ST_PARAM_LIST   = 2,
    ST_COL_LIST     = 3,
    ST_COL_MAXROWS  = 4,
    ST_COLS_BOUND   = 5,
    ST_LONG_DATA_MAX= 6,
    ST_WORK_PTR     = 7,
    ST_ROWSET_SIZE  = 8,
    ST_EF_ROWSTAT   = 9,
    ST_EF_ROWSTAT_VEC= 10,
    ST_ERRMS_STR    = 11,
    ST_ERRMS_LEN    = 12,
    ST_SQLSTATE_STR = 13,
    ST_ASYNC_P      = 14,
    ST_BUSY_FUNC    = 15,
    ST_BUSY_ARGS    = 16,
    ST_BUSY_RES     = 17,
    ST_VEC_LEN      = 17,

    ;;; parameter/column vectors
    PC_NUM          = 1,
    PC_POP_TYPE     = 2,
    PC_BUF          = 3,
    PC_LEN_PTR      = 4,
    PC_ID           = 5,
    PC_MODE         = 6,
    PC_FIXED_VEC    = 7,
    PC_BUF2         = 8,
    P_IO_TYPE       = 9,
    C_CHAR_TYPE     = 9,
    PC_VEC_LEN      = 9,

    MAX_NAME_LEN    = 128,
    MAX_CONN_LEN    = 256,
    INIT_MAXROWS    = 1e6,
);


defclass lconstant sql_handle [external_ptr]
  { handle_type :full,      ;;; ENV_HANDLE, CONN_HANDLE or STMT_HANDLE
>-> handle_ptr  :exptr,
    handle_data :full       ;;; a vector
  };

procedure(h);
    lvars type = handle_type(h), t = type && 2:1111;
    dlocal pop_pr_quotes = false;
    printf('<sql-');
    printf( if t == STMT_HANDLE then
                'statement'
            elseif t == ENV_HANDLE then
                'environment'
            else
                'connection'
            endif);
    if type < 0 then
        printf('\s(DEAD)')
    elseif t == CCNN_HANDLE then
        printf('\s(CONNECTED:\s');
        pr(subscrv(CN_SOURCE,handle_data(h)));
        cucharout(`)`)
    endif;
    cucharout(`>`)
endprocedure -> class_print(sql_handle_key);


lconstant
    exptr_ptr   = EXPTRINITSTR(:exptr),
    int_ptr     = EXPTRINITSTR(:int),
    short_ptr   = EXPTRINITSTR(:short),
    signed_stypes = [^SQL(Sql_TINYINT) ^SQL(Sql_SMALLINT)
                     ^SQL(Sql_INTEGER) ^SQL(Sql_BIGINT)],
    long_stypes = [^SQL(Sql_LONGVARCHAR) ^SQL(Sql_LONGVARBINARY)
                     ^SQL(Sql_WLONGVARCHAR)],
    macro fexacc    = [exacc[nc,fast]],
    macro fexacc_@  = [exacc[@,nc,fast]],
;


define lconstant tmpclr_cache_prop =
    newproperty([], 2, false, "tmpclr")
enddefine;

define lconstant tmpval_cache_prop =
    newproperty([], 2, false, "tmpval")
enddefine;

define sql_cons_sql_type(stype, precision, scale, unsigned);
    if fast_lmember(stype, long_stypes) then
        0 -> precision
    endif;
    fi_check(precision, 0, 16:FF) -> ;
    fi_check(scale, 0, precision) -> ;
    if unsigned and fast_lmember(stype, signed_stypes) then
        precision fi_|| 16:80 -> precision
    endif;
    (fi_check(stype,false,false) fi_<< 16) fi_|| precision
                    fi_|| (scale fi_<< 8)
enddefine;

define sql_dest_sql_type(sql_type) /* -> (stype, precision, scale, unsigned) */;
    unless isinteger(sql_type) then
        checkinteger(sql_type, false, false)
    endunless;
    sql_type fi_>> 16,
    sql_type fi_&& 16:FF,
    (sql_type fi_>> 8 ->> sql_type) fi_&& 16:7F,
    sql_type &&/=_0 16:80
enddefine;

define lconstant dest_pop_type(pop_type) /* -> (ptype, nbytes, ctype) */;
    pop_type && 16:1F, (pop_type>>5) && 16:7FF, pop_type >> 16
enddefine;

define lconstant default_pop_type(sql_type);

    define macro SNT t; t >> 15 enddefine;      ;;; includes sign bit

    define prop = newproperty([
        [^SNT Sql_BIT           ^Sql_P_boolean  ]
        [^SNT Sql_TINYINT       ^Sql_P_sbyte    ]
        [^SNT Sql_UTINYINT      ^Sql_P_byte     ]
        [^SNT Sql_SMALLINT      ^Sql_P_short    ]
        [^SNT Sql_USMALLINT     ^Sql_P_ushort   ]
        [^SNT Sql_INTEGER       ^Sql_P_int      ]
        [^SNT Sql_UINTEGER      ^Sql_P_uint     ]
        [^SNT Sql_REAL          ^Sql_P_sfloat   ]
        [^SNT Sql_FLOAT         ^Sql_P_dfloat   ]
        [^SNT Sql_DOUBLE        ^Sql_P_dfloat   ]
        [^SNT Sql_DATE          ^Sql_P_date     ]
        [^SNT Sql_TIME          ^Sql_P_time     ]
        [^SNT Sql_TIMESTAMP     ^Sql_P_timestamp]
        [^SNT Sql_BOOKMARK      ^Sql_P_bookmark ]
        [^SNT Sql_CHAR(1)       ^Sql_P_string   ]
        [^SNT Sql_NUMERIC(1,0)  ^Sql_P_string   ]
        [^SNT Sql_DECIMAL(1,0)  ^Sql_P_string   ]
        [^SNT Sql_BIGINT        ^Sql_P_longlong ]
        [^SNT Sql_UBIGINT       ^Sql_P_ulonglong]
        [^SNT Sql_BINARY(1)     ^Sql_P_binary   ]
        [^SNT Sql_VARBINARY(1)  ^Sql_P_binary   ]
        [^SNT Sql_LONGVARBINARY ^Sql_P_binary   ]
        [^SNT Sql_LONGVARCHAR   ^Sql_P_string   ]
        [^SNT Sql_VARCHAR(1)    ^Sql_P_string   ]
        ;;; UNICODE TYPES
        [^SNT Sql_WCHAR(1)      ^Sql_P_wstring  ]
        [^SNT Sql_WVARCHAR(1)   ^Sql_P_wstring  ]
        [^SNT Sql_WLONGVARCHAR  ^Sql_P_wstring  ]
        ;;; END UNICODE TYPES
        ], 8, 0, "perm")
    enddefine;

    prop(sql_type >> 15)
enddefine;

define lconstant check_mode_arg(arg);
    returnunless(arg);
    unless isprocedure(arg)
    or isvector(arg) and datalength(arg) == 3 then
        mishap(arg, 1, 'ODBC: 3-VECTOR OR PROCEDURE NEEDED')
    endunless
enddefine;

define lconstant check_handle(h, type);
    lvars t;
    unless issql_handle(h) then
        nullstring
    elseif (handle_type(h) ->> t) fi_&&~~ type == 0 then
        return(if type fi_< 0 then t fi_> 0 endif)
    else
        t && 2:1111 -> t;
        if t &&~~ type == 0 then
            '(LIVE) '
        elseif type == CCNN_HANDLE and t == CONN_HANDLE then
            CONN_HANDLE -> type;
            '(CONNECTED) '
        else
            nullstring
        endif
    endunless;
    mishap((),  if type && 2:1111 /== STMT_HANDLE then 'CONNECTION'
                else 'STATEMENT'
                endif, h, 3, '%ODBC: %S%S HANDLE NEEDED')
enddefine;


lvars errmess_string = false, errmess_len, sqlstate_string;

define lconstant stmt_sqlstate(hstmt);
    lvars r, dvec, msbuf, sqlstate, state_only = true;
    lconstant
        int_ptr     = EXPTRINITSTR(:int),
        short_ptr   = EXPTRINITSTR(:short);

    unless hstmt then (), false -> (hstmt, state_only) endunless;
    handle_data(hstmt) -> dvec;
    if fast_subscrv(ST_ERRMS_STR,dvec) ->> msbuf then
        fast_subscrv(ST_SQLSTATE_STR,dvec) -> sqlstate
    else
        inits(SQL_MAX_MESSAGE_LENGTH) ->> msbuf
                            -> fast_subscrv(ST_ERRMS_STR,dvec);
        inits(SQL_SQLSTATE_SIZE) ->> sqlstate
                            -> fast_subscrv(ST_SQLSTATE_STR,dvec)
    endif;
    fexacc SQLError(false, false, hstmt, sqlstate, int_ptr, msbuf,
                            #_<SQL_MAX_MESSAGE_LENGTH-1>_#, short_ptr) -> r;
    if r == SQL_SUCCESS or r == SQL_SUCCESS_WITH_INFO then
        fexacc :short short_ptr
    else
        0
    endif -> r;

    if state_only then
        r -> fast_subscrv(ST_ERRMS_LEN,dvec);
        r /== 0 and sqlstate
    else
        msbuf, r, sqlstate
    endif
enddefine;

define lconstant other_sqlstate(h);
    lvars r, henv, hdbc, msbuf, sqlstate, state_only = true;
    lconstant
        int_ptr     = EXPTRINITSTR(:int),
        short_ptr   = EXPTRINITSTR(:short);

    unless h then (), false -> (h, state_only) endunless;
    if handle_type(h) == ENV_HANDLE then
        h -> henv, false -> hdbc
    else
        false -> henv, h -> hdbc
    endif;
    if errmess_string ->> msbuf then
        sqlstate_string -> sqlstate
    else
        inits(SQL_MAX_MESSAGE_LENGTH) ->> msbuf -> errmess_string;
        inits(SQL_SQLSTATE_SIZE) ->> sqlstate -> sqlstate_string
    endif;
    fexacc SQLError(henv, hdbc, false, sqlstate, int_ptr, msbuf,
                        #_<SQL_MAX_MESSAGE_LENGTH-1>_#, short_ptr) -> r;
    if r == SQL_SUCCESS or r == SQL_SUCCESS_WITH_INFO then
        fexacc :short short_ptr -> r
    else
        0 -> r
    endif;

    if state_only then
        r -> errmess_len;
        r /== 0 and sqlstate
    else
        msbuf, r, sqlstate
    endif
enddefine;

define lconstant check_result(count, h, res);
    lvars r, str, dvec, msbuf, sqlstate, got_error;
    count fi_+ 1 -> count;
    subscr_stack(count) -> got_error;
    if handle_type(h) == STMT_HANDLE then
        if got_error then
            ;;; already called stmt_sqlstate for this error
            handle_data(h) -> dvec;
            fast_subscrv(ST_ERRMS_STR,dvec),
            fast_subscrv(ST_ERRMS_LEN,dvec),
            fast_subscrv(ST_SQLSTATE_STR,dvec)
        elseif res == SQL_CANCELLED then
            'Operation cancelled', datalength(dup()), 'S1008'
        else
            stmt_sqlstate(h, false)
        endif
    else
        if got_error then
            ;;; already called other_sqlstate for this error
            errmess_string, errmess_len, sqlstate_string
        else
            other_sqlstate(h, false)
        endif
    endif -> (msbuf, r, sqlstate);

    if r /== 0 then
        inits(r) -> str;
        move_bytes(1, msbuf, 1, str, r);
        str
    else
        'Unknown error'
    endif -> subscr_stack(count);

    count fi_+ 1 -> count;
    external_ptr_props(subscr_stack(count)) -> str;     ;;; func name
    str -> subscr_stack(count);
    consstring(#|
        explode('odbc-'), explode(str),
        explode(':exsys-odbc-'), explode(sqlstate)
    |#) -> str;         ;;; idstring
    sys_raise_exception(count, if res == SQL_SUCCESS_WITH_INFO then
                                    {'%ODBC %S: %S' 16:02}, str, `W`
                               else
                                    '%ODBC %S: %S', str, `E`
                               endif)
enddefine;

define :inline lconstant CALL(func, args, res, ns_expr, check_args);
    unless (fexacc func args ->> res) == SQL_SUCCESS then
        ns_expr;
        check_result(func, check_args, res)
    endunless
enddefine;

define :inline lconstant CALL_ASYNC(func, args, res, ns_expr, check_args);
    unless (fexacc func args ->> res) == SQL_SUCCESS
    or res == SQL_STILL_EXECUTING
       and (wait_for_stmt(popstackmark, args, func) -> res)
    then
        ns_expr;
        check_result(func, check_args, res)
    endunless
enddefine;


;;; --- ATTRIBUTE & OPTION PROCESSING -------------------------------------

define lconstant get_desc_value(dtype, buf);
    go_on dtype to DType:
        DType SQL_dt_string:
            exacc_ntstring(buf);
            sys_grbg_fixed(buf);
            return();
        DType SQL_dt_YNstring:
            return(fexacc :byte buf == `Y`);
        DType SQL_dt_bool:
            return(fexacc :int buf /== 0);
        DType SQL_dt_short:
            return(fexacc :short buf);
        DType SQL_dt_ushort:
            return(fexacc :ushort buf);
        DType SQL_dt_int:
            return(fexacc :int buf);
        DType SQL_dt_uint:
            return(fexacc :uint buf);
        DType default:
            mishap(0, 'ODBC: INVALID VALUE TYPE IN OPTION/ATTRIBUTE DESCRIPTOR')
    endgo_on
enddefine;

define lconstant desc_prepare(desc, intbuf, maxstrlen)
                                            -> (desc, dtype, buf, maxstrlen);
    unless isintegral(desc) then
        mishap(desc, 1, '(BIG)INTEGER NEEDED', ':type-integral')
    endunless;
    desc && 16:F -> dtype;
    desc >> 16 -> desc;
    if dtype == SQL_dt_string then
        initexptr_mem(maxstrlen)
    else
        SIZEOFTYPE(:int) -> maxstrlen;
        intbuf
    endif -> buf
enddefine;

define lconstant option_value(h, option, htype);
    lvars isstmt, res, dtype, buf, func;
    check_handle(h, htype);
    htype == STMT_HANDLE -> isstmt;
    desc_prepare(option, int_ptr, SQL_MAX_OPTION_STRING_LENGTH)
                                        -> (option, dtype, buf, _);
    if option == SQL(Sql_ASYNC_ENABLE) then
        return(subscrv(if isstmt then ST_ASYNC_P else CN_ASYNC_P endif,
                        handle_data(h)))
    elseif option == SQL(Sql_LONG_DATA_MAX) then
        return(subscrv(if isstmt then ST_LONG_DATA_MAX else CN_LONG_DATA_MAX endif,
                        handle_data(h)))
    endif;

    l_typespec func :SQLGetConnectOption;
    if isstmt then SQLGetStmtOption else SQLGetConnectOption endif -> func;

    CALL(func, (h, option, buf), res,
         returnif(res == SQL_NO_DATA_FOUND) (pop_undef),
         (false, 0, h));
    get_desc_value(dtype, buf)
enddefine;
;;;
define updaterof option_value(val, h, option, htype);
    lvars isstmt, res, dtype, buf, default, func, org_val = val;
    check_handle(h, htype);
    htype == STMT_HANDLE -> isstmt;
    checkinteger(option, false, false);
    option fi_&& 16:F -> dtype;
    option &&/=_0 2:1e4 and (option fi_>> 5) fi_&& 16:7FF -> default;
    option fi_>> 16 -> option;

    if val == "default" then
        if default then
            default -> val
        else
            mishap(val, 1, 'ODBC: CONNECT/STATEMENT OPTION HAS NO DEFAULT VALUE')
        endif
    else
        go_on dtype to DType:
            DType SQL_dt_string:
                check_string(val);
                goto DType end;
            DType SQL_dt_bool:
                if val then 1 else 0 endif -> val;
                goto DType end;
            DType SQL_dt_int:
                val -> fexacc :int int_ptr;
                goto DType end;
            DType SQL_dt_uint:
                val -> fexacc :uint int_ptr;
                goto DType end;
            DType default:
                mishap(0, 'ODBC: INVALID VALUE TYPE IN OPTION/ATTRIBUTE DESCRIPTOR')
        endgo_on
    endif;

    if option == SQL(Sql_LONG_DATA_MAX) then
        ;;; special for pop
        val -> subscrv(if isstmt then ST_LONG_DATA_MAX else CN_LONG_DATA_MAX endif,
                        handle_data(h));
        return
    elseif option == SQL(Sql_BIND_TYPE) and val /== 0 then
        mishap(0, 'ODBC: ROW-WISE BINDING NOT SUPPORTED')
    endif;

    l_typespec func :SQLSetConnectOption;
    if isstmt then SQLSetStmtOption else SQLSetConnectOption endif -> func;

    CALL(func, (h, option, val), res, , (false, 0, h));

    if option == SQL(Sql_ASYNC_ENABLE) then
        val /== 0
        and (if isprocedure(org_val) then org_val
             else #_< syssleep(%1%) >_#
             endif)
            -> subscrv(if isstmt then ST_ASYNC_P else CN_ASYNC_P endif,
                                                            handle_data(h))
    elseif isstmt and option == SQL(Sql_ROWSET_SIZE) then
        val -> subscrv(ST_ROWSET_SIZE,handle_data(h))
    endif
enddefine;


;;; --- ENVIRONMENT -------------------------------------------------------

constant procedure sql_disconnect;

define active sql_environment -> henv;
    returnif((tmpval_cache_prop("env") ->> henv)
                and handle_type(henv) == ENV_HANDLE);

    ;;; SQLAllocEnv tries to open odbc.ini (and it doesn't appear to like
    ;;; being called again after it's failed), so make sure there's a
    ;;; free file descriptor.
    sysclose(readable('/dev/null'));
    unless fexacc SQLAllocEnv(exptr_ptr) == SQL_SUCCESS then
        mishap(0, 'ODBC SQLAllocEnv: FAILED TO ALLOCATE ENVIRONMENT HANDLE',
                        'odbc-SQLAllocEnv:exsys-odbc-S1000')
    endunless;
    conssql_handle(ENV_HANDLE, fexacc :exptr exptr_ptr,
                    consvector(newproperty([],1,false,"tmparg"), EN_VEC_LEN))
                            ->> henv -> tmpval_cache_prop("env");

    ;;; Since this is only called when henv is garbage, any connections
    ;;; must also be garbage (although destroy actions must have been posted
    ;;; for them)
    procedure(henv);
        lvars res;
        returnif(handle_type(henv) == ENV_HANDLE_d);
        appproperty(subscrv(EN_CONNECTIONS,handle_data(henv)),
                        procedure(hdbc, _); sql_disconnect(hdbc) endprocedure);
        CALL(SQLFreeEnv, (henv), res, , (false, 0, henv));
        ENV_HANDLE_d -> handle_type(henv);
        null_external_ptr -> handle_ptr(henv)
    endprocedure -> sys_process_destroy_action(henv)
enddefine;


;;; --- STATEMENTS -------------------------------------------------------

define sql_statement_option() with_nargs 2;
    option_value((), STMT_HANDLE)
enddefine;
;;;
define updaterof sql_statement_option() with_nargs 3;
    () -> option_value((), STMT_HANDLE)
enddefine;

define lconstant insert_pcvec(/*N, pop_type, buf, len_ptr, id, mode,
                        fxd_vec, buf2, io_type,*/ d_subs, hstmt) -> new_vec;
    lvars   pair, vec, n, lpair = false, N, data = handle_data(hstmt),
            list = subscrv(d_subs,data);

    if tmpclr_cache_prop("pcvec") ->> new_vec then
        fast_subscrv(1,new_vec) -> tmpclr_cache_prop("pcvec");
        fill(new_vec)
    else
        consvector((), PC_VEC_LEN)
    endif -> new_vec;
    fast_subscrv(PC_NUM,new_vec) -> N;

    fast_for pair on list do
        fast_front(pair) -> vec;
        if (fast_subscrv(PC_NUM,vec) ->> n) fi_< N then
            pair -> lpair
        elseif n == N then
            new_vec -> fast_front(pair);
            return
        else
            quitloop
        endif
    endfor;

    if lpair then
        new_vec :: fast_back(lpair) -> fast_back(lpair)
    else
        new_vec :: list -> subscrv(d_subs,data)
    endif
enddefine;

define lconstant garbage_pcvec_list(d_subs, hstmt);
    lvars   vec, free_vec, dvec = handle_data(hstmt),
            l = fast_subscrv(d_subs,dvec);
compile_mode :vm -pentch -bjmpch;
    [] -> fast_subscrv(d_subs,dvec);
    tmpclr_cache_prop("pcvec") -> free_vec;
    fast_for vec in l do
        sys_grbg_fixed(fast_subscrv(PC_BUF,vec),
                        fast_subscrv(PC_LEN_PTR,vec), 2);
        free_vec -> fast_subscrv(1,vec);
        vec -> free_vec
    endfor;
    free_vec -> tmpclr_cache_prop("pcvec");
    sys_grbg_list(l)
enddefine;

define sql_free_statement(hstmt);
    lvars res, hdbc, dvec;
    returnunless(check_handle(hstmt, STMT_HANDLE_d));
    CALL(SQLFreeStmt, (hstmt, SQL_DROP), res, , (false, 0, hstmt));
    STMT_HANDLE_d -> handle_type(hstmt);
    null_external_ptr -> handle_ptr(hstmt);
    subscrv(ST_CONN_HANDLE,handle_data(hstmt)) -> hdbc;
    false -> subscrv(CN_STATEMENTS,handle_data(hdbc))(hstmt);
    garbage_pcvec_list(ST_PARAM_LIST, hstmt);
    garbage_pcvec_list(ST_COL_LIST, hstmt)
enddefine;

define sql_alloc_statement(hdbc) -> hstmt;
    lvars res, hstmt, dvec, rs_size;
    check_handle(hdbc, CCNN_HANDLE);
    CALL(SQLAllocStmt, (hdbc, exptr_ptr), res, , (false, 0, hdbc));
    handle_data(hdbc) -> dvec;
    conssql_handle(STMT_HANDLE, fexacc :exptr exptr_ptr,
            consvector(hdbc, [], [], INIT_MAXROWS, false,
                        subscrv(CN_LONG_DATA_MAX,dvec), EXPTRINITSTR(:exptr),
                        false, false, false,
                        false, 0, false,
                        subscrv(CN_ASYNC_P,dvec), false, false, false,
                        ST_VEC_LEN)) -> hstmt;
    true -> subscrv(CN_STATEMENTS,dvec)(hstmt);
    sql_free_statement -> sys_process_destroy_action(hstmt)
enddefine;

define sql_statement_busy(hstmt);
    lvars res, dvec, args;
    check_handle(hstmt, STMT_HANDLE);
    handle_data(hstmt) -> dvec;
    returnunless(fast_subscrv(ST_BUSY_ARGS,dvec) ->> args) (false);
    fexacc (...):int (fast_subscrv(ST_BUSY_FUNC,dvec))(destlist(args)) -> res;
    returnif(res == SQL_STILL_EXECUTING) (true);
    sys_grbg_list(args);
    false -> fast_subscrv(ST_BUSY_ARGS,dvec);
    res -> fast_front(fast_subscrv(ST_BUSY_RES,dvec));
    false
enddefine;
;;;
define updaterof sql_statement_busy(busy, hstmt);
    lvars res, dvec, args;
    check_handle(hstmt, STMT_HANDLE);
    returnif(busy);         ;;; ignore anything but false
    handle_data(hstmt) -> dvec;
    if fast_subscrv(ST_BUSY_ARGS,dvec) ->> args then
        ;;; still running -- cancel
        CALL(SQLCancel, (hstmt), res, , (false, 0, hstmt));
        while sql_statement_busy(hstmt) do syssleep(1) endwhile
    endif;
    false -> fast_subscrv(ST_BUSY_RES,dvec)
enddefine;

define lconstant wait_for_stmt(func);
    lvars   args = sysconslist(), hstmt = fast_front(args),
            dvec = handle_data(hstmt), res;
    func -> fast_subscrv(ST_BUSY_FUNC,dvec);
    args -> fast_subscrv(ST_BUSY_ARGS,dvec);
    conspair(0, 0) ->> res -> fast_subscrv(ST_BUSY_RES,dvec);
    repeat
        fast_subscrv(ST_ASYNC_P,dvec)();    ;;; call user wait procedure
        quitunless(sql_statement_busy(hstmt))
    endrepeat;
    if fast_subscrv(ST_BUSY_RES,dvec) == res then
        false -> fast_subscrv(ST_BUSY_RES,dvec);
        sys_grbg_destpair(res) ->
    else
        ;;; cancelled
        SQL_CANCELLED
    endif -> res;
    res == SQL_SUCCESS, res
enddefine;


;;; --- CONNECTIONS -------------------------------------------------------

define sql_data_source_repeater() -> ds_rep;
    lconstant procedure ds_rep;
    lconstant MAX_DSN_LEN = SQL_MAX_DSN_LENGTH+1;
    lvars   res, henv = sql_environment, state = SQL_FETCH_FIRST,
            ds_buf = initexptr_mem(MAX_DSN_LEN),
            dr_buf = initexptr_mem(MAX_CONN_LEN);

    define lconstant ds_rep();
        returnunless(state) (termin);
        CALL(SQLDataSources, (henv, state, ds_buf, MAX_DSN_LEN, short_ptr,
                                            dr_buf, MAX_CONN_LEN, short_ptr),
                res,
                if res == SQL_NO_DATA_FOUND then
                    false -> state;
                    sys_grbg_fixed(ds_buf, dr_buf, 2);
                    return(termin)
                endif,
                (false, 0, henv));
        conspair(exacc_ntstring(ds_buf), exacc_ntstring(dr_buf));
        SQL_FETCH_NEXT -> state
    enddefine
enddefine;

define sql_driver_repeater() -> dr_rep;
    lconstant procedure dr_rep;
    lvars   res, henv = sql_environment, state = SQL_FETCH_FIRST,
            dr_buf = initexptr_mem(MAX_CONN_LEN),
            drat_buf = initexptr_mem(512);

    define lconstant dr_rep();
        lvars at_buf, attr, len;
        returnunless(state) (termin);
        0 -> exacc :byte[] drat_buf[1];
        CALL(SQLDrivers, (henv, state, dr_buf, MAX_CONN_LEN, short_ptr,
                                        drat_buf, 512, short_ptr),
                res,
                if res == SQL_NO_DATA_FOUND then
                    false -> state;
                    sys_grbg_fixed(dr_buf, drat_buf, 2);
                    return(termin)
                endif,
                (false, 0, henv));

        drat_buf -> at_buf;
        [%  exacc_ntstring(dr_buf),
            repeat
                exacc_ntstring(at_buf) -> attr;
                quitif((datalength(attr) ->> len) == 0);
                attr;
                fexacc_@ :byte[] at_buf[len fi_+ 2] -> at_buf
            endrepeat
        %];
        SQL_FETCH_NEXT -> state
    enddefine
enddefine;

define sql_connect_attribute(hdbc, attr);
    lvars res, dtype, buf, buflen;
    check_handle(hdbc, CONN_HANDLE);
    desc_prepare(attr, int_ptr, SQL_MAX_OPTION_STRING_LENGTH)
                                            -> (attr, dtype, buf, buflen);
    CALL(SQLGetInfo, (hdbc, attr, buf, buflen, short_ptr), res, ,
                                                    (false, 0, hdbc));
    get_desc_value(dtype, buf)
enddefine;

define sql_connect_option() with_nargs 2;
    option_value((), CONN_HANDLE)
enddefine;
;;;
define updaterof sql_connect_option() with_nargs 3;
    () -> option_value((), CONN_HANDLE)
enddefine;

define sql_disconnect(hdbc);
    lvars res, dvec, henv;
    returnunless(check_handle(hdbc, CONN_HANDLE_d));
    handle_data(hdbc) -> dvec;
    if handle_type(hdbc) == CCNN_HANDLE then
        appproperty(subscrv(CN_STATEMENTS,dvec),
                            #_< erase<>sql_free_statement >_#);
        CALL(SQLDisconnect, (hdbc), res, , (false, hdbc, 1, hdbc));
        CONN_HANDLE -> handle_type(hdbc)
    endif;
    CALL(SQLFreeConnect, (hdbc), res, , (false, 0, hdbc));
    CONN_HANDLE_d -> handle_type(hdbc);
    null_external_ptr -> handle_ptr(hdbc);
    subscrv(CN_ENV_HANDLE, dvec) -> henv;
    false -> subscrv(EN_CONNECTIONS, handle_data(henv))(hdbc)
enddefine;

define sql_connect(connstr) -> hdbc;
    lvars   hdbc, res, henv, dvec, fullconn, kw, sqlstate, tried_gc,
            changes_p = erase, retries = 5,
            hwin = false, completion_flag = SQL_DRIVER_NOPROMPT,
            browse_p = false;

    dlocal 0 % if dlocal_context == 1 then false -> hdbc endif,
               if dlocal_context == 2 and hdbc then sql_disconnect(hdbc) endif
             %;

    if ispair(connstr) and not(islist(connstr)) then
        ;;; optional pair with window arg and completion flag
        (), destpair(connstr) -> (connstr, hwin, completion_flag);
        checkinteger(completion_flag, false, false)
    elseif isref(connstr) then
        ;;; browse procedure
        unless isprocedure(fast_cont(connstr) ->> browse_p) then
            mishap(browse_p, 1, 'PROCEDURE NEEDED')
        endunless;
        () -> connstr
    endif;
    if isinteger(connstr) then
        ;;; optional retries count
        (), connstr -> (connstr, retries);
        checkinteger(retries, 1, false)
    endif;
    if isprocedure(connstr) then
        ;;; optional changes procedure
        (), connstr -> (connstr, changes_p)
    endif;

    if isstring(connstr) and not(strmember(`;`, connstr)) then
        connstr :: [] -> connstr
    endif;
    if islist(connstr) then
        if connstr /== [] then
            consstring(#|
                fast_for kw in ['DSN' 'UID' 'PWD'] do
                    explode(kw), `=`;
                    deststring(dest(connstr) -> connstr) -> , `;`;
                    quitif(connstr == [])
                endfor
            |#)
        else
            nullstring
        endif -> connstr
    else
        check_string(connstr)
    endif;

    sql_environment -> henv;
    CALL(SQLAllocConnect, (henv, exptr_ptr), res, , (false, 0, henv));

    consvector(henv, newproperty([],1,false,"tmparg"), false, 16:7FF,
                                    false, false, false, CN_VEC_LEN) -> dvec;
    conssql_handle(CONN_HANDLE, fexacc :exptr exptr_ptr, dvec) -> hdbc;
    true -> subscrv(EN_CONNECTIONS,handle_data(henv))(hdbc);
    sql_disconnect -> sys_process_destroy_action(hdbc);

    ;;; do any pre-connect option setting
    changes_p(hdbc);


    ;;; make the connection

    define lconstant error_retry(hdbc);
        lvars comm_failure;
        other_sqlstate(hdbc) -> sqlstate;
        sqlstate = '08004' or sqlstate = '08S01' -> comm_failure;
        if (tried_gc and not(comm_failure))
        or retries == 0
        or lmember_=(sqlstate, ['28000' 'IM001' 'IM002' 'IM007' 'IM010'
                                'IM011' 'IM012' 'S1110' 'S1T00'])
        then
            false
        else
            unless comm_failure then sysgarbage(), true -> tried_gc endunless;
            retries-1 -> retries;
            true
        endif
    enddefine;

    initexptr_mem(MAX_CONN_LEN) -> fullconn;
    false -> tried_gc;

    if browse_p then
        lvars save_retries = retries;
        repeat
            false -> sqlstate;
            0 -> exacc :byte[] fullconn[1];
            CALL(SQLBrowseConnect, (hdbc, connstr,SQL_NTS, fullconn,
                                            MAX_CONN_LEN, short_ptr),
                res,
                if res == SQL_NEED_DATA then
                    CCNN_HANDLE -> handle_type(hdbc);   ;;; can disconnect
                    if (exacc_ntstring(fullconn) ->> connstr) = nullstring then
                        ;;; error
                        other_sqlstate(hdbc, false) -> (connstr, _, sqlstate);
                        exacc_ntstring(connstr) -> connstr
                    endif;
                    browse_p(connstr, sqlstate) -> connstr;
                    save_retries -> retries;
                    nextloop
                elseif res == SQL_ERROR then
                    nextif(error_retry(hdbc))
                endif,
                (sqlstate, connstr, 1, hdbc)
            );
            quitloop
        endrepeat

    else
        repeat
            false -> sqlstate;
            CALL(SQLDriverConnect, (hdbc, hwin, connstr,SQL_NTS, fullconn,
                                    MAX_CONN_LEN, short_ptr, completion_flag),
                res,
                if res == SQL_NO_DATA_FOUND then
                    ;;; user cancelled dialog
                    sys_grbg_fixed(fullconn);
                    sql_disconnect(hdbc);
                    return(false -> hdbc)
                elseif res == SQL_ERROR then
                    nextif(error_retry(hdbc))
                endif,
                (sqlstate, connstr, 1, hdbc)
            );
            quitloop
        endrepeat
    endif;

    exacc_ntstring(fullconn) -> subscrv(CN_SOURCE,dvec);
    sys_grbg_fixed(fullconn);
    CCNN_HANDLE -> handle_type(hdbc);       ;;; says connected

    not(sql_connect_attribute(hdbc, Sql_NEED_LONG_DATA_LEN))
                                        -> subscrv(CN_LONG_LEN_NA,dvec)
enddefine;

define sql_connect_functions(hdbc) -> res;
    lvars i, res, function = SQL_API_ALL_FUNCTIONS, farray;
    if isinteger(hdbc) then
        ;;; optional function number
        (), hdbc -> (hdbc, function)
    endif;
    check_handle(hdbc, CCNN_HANDLE);
    fast_subscrv(CN_FUNC_MASK,handle_data(hdbc)) -> res;

    if function == SQL_API_ALL_FUNCTIONS then
        returnif(res);
        initexptr_mem(#_< SIZEOFTYPE(:ushort) * 100 >_#) -> farray;
        CALL(SQLGetFunctions, (hdbc, function, farray),
                    res, , (false, 0, hdbc));
        0 -> res;
        fast_for i to 99 do
            if exacc :ushort[] farray[i fi_+ 1] /== 0 then
                true -> testbit(res, i) -> res
            endif
        endfor;
        sys_grbg_fixed(farray);
        res -> fast_subscrv(CN_FUNC_MASK,handle_data(hdbc))
    else
        returnif(res) (testbit(res, function) -> res);
        CALL(SQLGetFunctions, (hdbc, function, short_ptr),
                    res, , (false, 0, hdbc));
        exacc :ushort short_ptr /== 0 -> res
    endif
enddefine;

define sql_transact(hdbc);
    lvars commit = true, henv = null_external_ptr, res;
    unless issql_handle(hdbc) then
        (), hdbc -> (hdbc, commit)
    endunless;
    if commit then SQL_COMMIT else SQL_ROLLBACK endif -> commit;
    if hdbc then
        check_handle(hdbc, CCNN_HANDLE)
    else
        sql_environment -> henv;
        null_external_ptr -> hdbc
    endif;
    CALL(SQLTransact, (henv, hdbc, commit), res, , (false, 0, hdbc))
enddefine;

define sql_native_sql(hdbc, stmt);
    lvars buf, res;
    check_handle(hdbc, CCNN_HANDLE);
    check_string(stmt);
    initexptr_mem(512) -> buf;
    CALL(SQLNativeSql, (hdbc, stmt, SQL_NTS, buf, 512, int_ptr),
                            res, , (false, stmt, 1, hdbc));
    exacc_ntstring(buf);
    sys_grbg_fixed(buf)
enddefine;


;;; --- DATA INPUT/OUTPUT -------------------------------------------------

define lconstant macro PT pop_type; pop_type && 16:1F enddefine;

define lconstant make_str_exptr(hstmt, bsubs, str) -> str;
    if isexternal_ptr_class(str) then
        returnif(bsubs == 1)
    else
        if fast_subscrv(ST_ASYNC_P,handle_data(hstmt)) and not(is_fixed(str))
        then
            mishap(0, 'ODBC: BUFFER MUST BE FIXED-ADDRESS FOR ASYNC OPERATION')
        endif;
        returnif(bsubs == 1);
        lconstant full_ptr = EXPTRINITSTR(:full);
        str -> fexacc :full full_ptr;
        exacc[nc,fast] :exptr full_ptr -> str
    endif;
    exacc[@,nc] :byte[] str[bsubs] -> str
enddefine;


#_IF not(DEF SQL_C_SLONGLONG)

define lconstant bigint_to_chars(i, sbsubs, buf) /* -> nb */;
    dlvars bsubs = sbsubs, buf;
    dlocal pop_pr_radix = 10;

    define dlocal cucharout(c);
        c -> exacc :byte[] buf[bsubs];
        bsubs fi_+ 1 -> bsubs
    enddefine;

    sys_syspr(i);
    bsubs fi_- sbsubs
enddefine;

define lconstant chars_to_bigint(buf, nb) /* -> i */;
    lvars n = 1, c = exacc :byte[] buf[1], i = 0, m = 1, I = 0, neg = false;

    if c == `+` then
        2 -> n
    elseif c == `-` then
        2 -> n;
        true -> neg
    endif;

    while n fi_<= nb do
        unless isnumbercode(exacc :byte[] buf[n] ->> c) then
            inits(nb) -> c;
            move_bytes(1, buf, 1, c, nb);
            mishap(c, 1, 'ODBC: INVALID VALUE FOR (u)longlong RESULT')
        endunless;

        n fi_+ 1 -> n;
        c - `0` -> c;
        if m == 1e8 then
            I * m + i -> I;
            c -> i;
            10 -> m
        else
            i fi_* 10 fi_+ c -> i;
            m fi_* 10 -> m
        endif
    endwhile;
    if neg then I * (-m) - i else I * m + i endif
enddefine;

#_ENDIF

define lconstant checkr_vec_vals(val, keyword, len);
    if isvector(val) and datalength(val) == len
    and fast_subscrv(1,val) == keyword then
        explode(val)
    else
        mishap(keyword, val, 2, '%%P VECTOR NEEDED')
    endif
enddefine;

define lconstant data_input(hstmt, N, pop_type, buf, len_ptr, id, mode,
                                                fxd_vec, buf2, row, lastrow);
    lvars   val, nbytes, bsubs, str, nb, ptype, ctype, bufmax,
            idvec = isvector(id);
    lconstant size_err
        = '%ODBC: BYTE RANGE FOR PARAMETER %P VALUE IS WRONG SIZE OR TOO LARGE';

    if isident(mode) then check_mode_arg(idval(mode) ->> mode) endif;
    dest_pop_type(pop_type) -> (ptype, nbytes, ctype);
    if isinteger(fxd_vec) then fxd_vec -> nbytes, false -> fxd_vec endif;
    fi_max(SIZEOFTYPE(:exptr), nbytes) -> bufmax;

    if ctype == SQL_C_CHAR then
        nbytes fi_- 1 -> nbytes;
#_IF not(DEF SQL_C_SLONGLONG)
        lvars ll_trans = false;
        if ptype == PT Sql_P_longlong or ptype == PT Sql_P_ulonglong then
            SIZEOFTYPE(:longlong) -> nbytes;
            true -> ll_trans
        endif
#_ELSE
        lconstant ll_trans = false;
#_ENDIF
    elseif ctype == SQL_C_WCHAR then
        nbytes fi_- 2 -> nbytes;
    endif;

    unless buf2 then buf -> buf2 endunless;
    if row == 1 then
        1 -> bsubs
    else
        (row fi_- 1) fi_* bufmax fi_+ 1 -> bsubs
    endif;

    while row fi_<= lastrow do
        if idvec then fast_subscrv(row,id) else idval(id) endif -> val;
        if isundef(val) then
            ;;; pass NULL_DATA
            SQL_NULL_DATA, goto ASSIGN_LEN
        elseif val == termin then
            ;;; pass IGNORE
            SQL_IGNORE, goto ASSIGN_LEN
        elseif val == "default" then
            ;;; pass DEFAULT_PARAM
            SQL_DEFAULT_PARAM, goto ASSIGN_LEN
        endif;

        if mode then
            unless (val ->> nb)
            or (fxd_vec and ptype fi_>= PT Sql_P_string and isprocedure(mode)
              and fast_subscrv(CN_LONG_LEN_NA,
                    handle_data(fast_subscrv(ST_CONN_HANDLE,handle_data(hstmt)))))
            then
                mishap(N, 1, '%ODBC: BYTE LENGTH REQUIRED FOR PARAMETER %P VALUE')
            endunless;
            if ptype fi_< PT Sql_P_string and nb /== nbytes
            or not(fxd_vec) and nb > nbytes then
                mishap(N, nb, 2, size_err)
            endif;
            if isvector(mode) then
                explode(mode) -> (/*bsubs, str,*/ _);
                if fxd_vec and not(ll_trans) then goto DATA_AT_EXEC endif;
                move_bytes((), bsubs, buf, nb)
            else
                if fxd_vec and not(ll_trans) then
                    if idvec then row else 0 endif, mode;
                    goto DATA_AT_EXEC
                endif;
                dlvars pos = bsubs, outbuf = buf;
                -> fast_apply(if idvec then row endif, nb,
                                procedure(bsubs, str, nb);
                                    move_bytes(bsubs, str, pos, outbuf, nb);
                                    pos fi_+ nb -> pos
                                endprocedure, mode)
            endif;
#_IF not(DEF SQL_C_SLONGLONG)
            if ll_trans then
                fexacc_@ :byte[] buf[bsubs] -> str;
                goto LL_TRANS
            endif;
#_ENDIF
            nb, goto ASSIGN_LEN
        endif;


        fexacc_@ :byte[] buf2[bsubs] -> str;
        nbytes -> nb;

        go_on ptype to Type:
            Type PT Sql_P_boolean:
                if val then 1 else 0 endif -> fexacc :byte str; goto Type end;
            Type PT Sql_P_bit:
                fi_check(val, 0, 1)        -> fexacc :byte str; goto Type end;
            Type PT Sql_P_sbyte:
                val -> fexacc :sbyte str;       goto Type end;
            Type PT Sql_P_byte:
                val -> fexacc :byte str;        goto Type end;
            Type PT Sql_P_short:
                val -> fexacc :short str;       goto Type end;
            Type PT Sql_P_ushort:
                val -> fexacc :ushort str;      goto Type end;
            Type PT Sql_P_int:
                val -> fexacc :int str;         goto Type end;
            Type PT Sql_P_uint:
                val -> fexacc :uint str;        goto Type end;
            Type PT Sql_P_longlong:
                val -> fexacc :longlong str;    goto LL_TRANS;
            Type PT Sql_P_ulonglong:
                val -> fexacc :ulonglong str;   goto LL_TRANS;
            Type PT Sql_P_sfloat:
                val -> fexacc :sfloat str;      goto Type end;
            Type PT Sql_P_dfloat:
                val -> fexacc :dfloat str;      goto Type end;

            Type PT Sql_P_date:
                lvars sql_date = str;
                checkr_vec_vals(val, "sql_date", 4) -> (_,
                                            fexacc sql_date.year,
                                            fexacc sql_date.month,
                                            fexacc sql_date.day);
                goto Type end;

            Type PT Sql_P_time:
                lvars sql_time = str;
                checkr_vec_vals(val, "sql_time", 4) -> (_,
                                            fexacc sql_time.hour,
                                            fexacc sql_time.minute,
                                            fexacc sql_time.second);
                goto Type end;

            Type PT Sql_P_timestamp:
                lvars sql_timestamp = str;
                checkr_vec_vals(val, "sql_timestamp", 8) -> (_,
                        fexacc sql_timestamp.year,
                        fexacc sql_timestamp.month,
                        fexacc sql_timestamp.day,
                        fexacc sql_timestamp.hour,
                        fexacc sql_timestamp.minute,
                        fexacc sql_timestamp.second,
                        fexacc sql_timestamp.fraction);
                goto Type end;

            Type PT Sql_P_string:
            Type PT Sql_P_binary:
                check_string(val);
                datalength(val) -> nb;
                if fxd_vec then
                    ;;; use data-at-execution
                    1, val;
                    goto DATA_AT_EXEC
                elseif nb fi_> nbytes then
                    mishap(N, val, 2, '%ODBC: INVALID STRING FOR PARAMETER %P VALUE (too large)')
                else
                    move_bytes(1, val, 1, str, nb)
                endif;
                goto Type end;

            ;;; UNICODE TYPES
            Type PT Sql_P_wstring:
                lvars wval = val;
                unless isstring16(wval) then
                    check_string(val);
                    datalength(val) -> nb;
                    val -> substring(1, nb, inits16(nb) ->> wval);
                endunless;
                datalength(wval) fi_* 2 -> nb;
                if fxd_vec then
                    ;;; use data-at-execution
                    1, wval;
                    goto DATA_AT_EXEC
                elseif nb fi_> nbytes then
                    mishap(N, val, 2, '%ODBC: INVALID STRING FOR PARAMETER %P VALUE (too large)')
                else
                    move_bytes(1, wval, 1, str, nb)
                endif;
            ;;; END UNICODE TYPES

        endgo_on;

TEST_DAE:
        unless fxd_vec then nb, goto ASSIGN_LEN endunless;
        bsubs, buf2;

DATA_AT_EXEC:
        nb -> fexacc :full[] fxd_vec[3];
        () -> fexacc :full[] fxd_vec[2];
        () -> fexacc :full[] fxd_vec[1];
        fxd_vec -> fexacc :exptr (fexacc_@ :byte[] buf[bsubs]);
        fexacc_@ :full[] fxd_vec[4] -> fxd_vec;
        SQL_LEN_DATA_AT_EXEC(nb or 0), goto ASSIGN_LEN;

LL_TRANS:
#_IF not(DEF SQL_C_SLONGLONG)
        if ptype == PT Sql_P_longlong then
            fexacc :longlong str
        else
            fexacc :ulonglong str
        endif;
        bigint_to_chars((), bsubs, buf2) -> nb;
#_ENDIF
        goto TEST_DAE;

ASSIGN_LEN:
        () -> fexacc :int[] len_ptr[row];
        row fi_+ 1 -> row;
        bsubs fi_+ bufmax -> bsubs
    endwhile
enddefine;

define lconstant put_data_at_exec(hstmt) -> res;
    dlvars  hstmt;
    lvars   fxd_vec, bsubs, str, nb,
            ex_ptr = fast_subscrv(ST_WORK_PTR,handle_data(hstmt));

    define put_data(bsubs, str, nb);
        lvars res;
        returnif(fi_check(nb, 0, false) == 0);
        make_str_exptr(hstmt, bsubs, str) -> str;
        CALL_ASYNC(SQLPutData, (hstmt, str, nb), res, , (false, 0, hstmt))
    enddefine;

    repeat
        if (fexacc SQLParamData(hstmt, ex_ptr) ->> res) == SQL_STILL_EXECUTING
        then
            wait_for_stmt(popstackmark, (hstmt, ex_ptr), SQLParamData)
                                                                -> (_, res)
        endif;
        quitunless(res == SQL_NEED_DATA);
        fexacc :exptr (fexacc :exptr ex_ptr) -> fxd_vec;
        fexacc :full[] fxd_vec[1] -> bsubs;
        fexacc :full[] fxd_vec[2] -> str;
        fexacc :full[] fxd_vec[3] -> nb;
        if isprocedure(str) then
            -> fast_apply(if bsubs /== 0 then bsubs endif, nb, put_data, str)
        else
            put_data(bsubs, str, nb)
        endif
    endrepeat
enddefine;


lconstant out_size_err
    = '%ODBC: BYTE BUFFER TOO SMALL FOR COLUMN/PARAM %P RESULT';

define lconstant GetData(hstmt, N, ctype, bsubs, buf, nbytes, len_ptr, row);
    lvars res, got_error;
    make_str_exptr(hstmt, bsubs, buf) -> buf;
    unless row == 1 then fexacc_@ :int[] len_ptr[row] -> len_ptr endunless;
    CALL_ASYNC(SQLGetData, (hstmt, N, ctype, buf, nbytes, len_ptr), res,
            if res == SQL_SUCCESS_WITH_INFO then
                returnif(stmt_sqlstate(hstmt) = '01004') (false); ;;; long data
                true -> got_error
            elseif res == SQL_NO_DATA_FOUND then
                0 -> fexacc :int len_ptr;
                return(true)
            else
                false -> got_error
            endif,
            (got_error, 0, hstmt));
    true
enddefine;

define lconstant data_output_long(hstmt, N, buf, nbytes, len_ptr, id, mode,
                row, chartype);
    lvars bsubs, str, nb, buflen, idvec = isvector(id);
    dlvars hstmt, N;

    if (fexacc :int[] len_ptr[row] ->> nb) == SQL_NO_TOTAL then
        false -> nb
    endif;

    if mode then
        if isvector(mode) then
            explode(mode) -> (bsubs, str, buflen);
            fi_check(buflen, 0, false) ->
        else
            nb -> if idvec then fast_subscrv(row,id) else idval(id) endif;
            dlvars pos = 1, inbuf = buf, c = nbytes;

            define get_data(bsubs, str, nb) -> nb;
                lvars take;
                fi_check(nb, 0, false) -> ;
                fi_min(nb, c) -> take;
                if take /== 0 then
                    move_bytes(pos, inbuf, bsubs, str, take);
                    pos fi_+ take -> pos;
                    c fi_- take -> c;
                    bsubs fi_+ take -> bsubs;
                    nb fi_- take -> nb
                endif;
                if nb /== 0 then
                    if GetData(hstmt, N, SQL_C_BINARY, bsubs, str, nb,
                                                int_ptr, 1) then
                        fexacc :int int_ptr -> nb
                    endif
                endif;
                take fi_+ nb -> nb
            enddefine;

            fast_apply(if idvec then row endif, id, get_data, mode);
            return
        endif

    else

        define init_buf(nbytes, chartype, fixed);
            if chartype == SQL_C_WCHAR then
                nbytes fi_div 2, string16_key
            else
                nbytes, string_key
            endif;
            if fixed then
                init_fixed()
            else
                fast_apply(class_init())
            endif;
        enddefine;

        if nb then
            init_buf(nb, chartype, fast_subscrv(ST_ASYNC_P,handle_data(hstmt)))
                ->> str
                -> if idvec then fast_subscrv(row,id) else idval(id) endif;
            1 -> bsubs; nb -> buflen;
        else
            ;;; total length unknown
            lvars grbg = false;
            repeat
                initexptr_mem(nbytes fi_+ nbytes) -> str;
                move_bytes(1, buf, 1, str, nbytes);
                if grbg then sys_grbg_fixed(buf) endif;
                quitif(GetData(hstmt, N, SQL_C_BINARY, nbytes fi_+ 1, str,
                                                    nbytes, len_ptr, row));
                str -> buf; true -> grbg;
                nbytes fi_+ nbytes -> nbytes
            endrepeat;
            nbytes fi_+ fexacc :int[] len_ptr[row] -> nb;
            init_buf(nb, chartype, false) ->> buf
                    -> if idvec then fast_subscrv(row,id) else idval(id) endif;
            move_bytes(1, str, 1, buf, nb);
            sys_grbg_fixed(str);
            return
        endif
    endif;

    if nbytes fi_>= buflen then mishap(N, buflen, 2, out_size_err) endif;
    move_bytes(1, buf, bsubs, str, nbytes);
    unless GetData(hstmt, N, SQL_C_BINARY, bsubs fi_+ nbytes, str,
                                        buflen fi_- nbytes, len_ptr, row)
    then
        mishap(N, buflen, 2, out_size_err)
    elseif mode then
        nbytes fi_+ fexacc :int[] len_ptr[row]
            -> if idvec then fast_subscrv(row,id) else idval(id) endif
    endunless
enddefine;

define lconstant data_output(hstmt, N, pop_type, buf, len_ptr, id, mode,
                fxd_vec, row, lastrow, rowstat, data_got, chartype);
    lvars   val, ptype, nbytes, bsubs, str, nb, ctype, bufmax,
            idvec = isvector(id);
    dlvars  pos, inbuf, c;

    if isident(mode) then check_mode_arg(idval(mode) ->> mode) endif;
    if isvector(mode) then explode(mode) -> (bsubs, str, c) endif;
    dest_pop_type(pop_type) -> (ptype, nbytes, ctype);
    if isinteger(fxd_vec) then fxd_vec -> nbytes endif;
    fi_max(SIZEOFTYPE(:exptr), nbytes) -> bufmax;
    if row /== 1 then
        fexacc_@ :byte[] buf[(row fi_- 1) fi_* bufmax fi_+ 1] -> buf
    endif;

    while row fi_<= lastrow do
        if rowstat then
            fexacc :ushort[] rowstat[row] -> nb;
            if nb == SQL_ROW_NOROW or nb == SQL_ROW_DELETED
            or nb == SQL_ROW_ERROR then
                ;;; skip this row
                termin, goto ASSIGN_VAL
            endif
        endif;

        unless data_got or GetData(hstmt, N, ctype, 1, buf, nbytes,
                                                    len_ptr, row) then
            ;;; long data
            chain(hstmt, N, buf, nbytes, len_ptr, id, mode, row, chartype,
                                                    data_output_long)
        endunless;

        if (fexacc :int[] len_ptr[row] ->> nb) == SQL_NULL_DATA then
            pop_undef, goto ASSIGN_VAL
        elseif nb == SQL_NO_TOTAL then
            if ctype == SQL_C_CHAR then
                nbytes fi_- 1
            elseif ctype == SQL_C_WCHAR then
                nbytes fi_- 2
            else
                nbytes
            endif -> nb
        endif;

#_IF not(DEF SQL_C_SLONGLONG)
        if ptype == PT Sql_P_longlong then
            chars_to_bigint(buf, nb) -> fexacc :longlong buf;
            SIZEOFTYPE(:longlong) -> nb
        elseif ptype == PT Sql_P_ulonglong then
            chars_to_bigint(buf, nb) -> fexacc :ulonglong buf;
            SIZEOFTYPE(:ulonglong) -> nb
        endif;
#_ENDIF

        if mode then
            nb -> if idvec then fast_subscrv(row,id) else idval(id) endif;
            if isvector(mode) then
                if nb > c then mishap(N, c, 2, out_size_err) endif;
                move_bytes(1, buf, bsubs, str, nb);
                bsubs fi_+ c -> bsubs
            else
                define get_data(bsubs, str, nb) -> nb;
                    if nb > c then c -> nb endif;
                    move_bytes(pos, inbuf, bsubs, str, nb);
                    pos fi_+ nb -> pos;
                    c fi_- nb -> c
                enddefine;
                1 -> pos; buf -> inbuf; nb -> c;
                fast_apply(if idvec then row endif, id, get_data, mode)
            endif;
            goto NEXT_ROW
        endif;

        go_on ptype to Type:
            Type PT Sql_P_boolean:
                fexacc :byte buf /== 0; goto Type end;
            Type PT Sql_P_bit:
                fexacc :byte buf;   goto Type end;
            Type PT Sql_P_sbyte:
                fexacc :sbyte buf;  goto Type end;
            Type PT Sql_P_byte:
                fexacc :byte buf;   goto Type end;
            Type PT Sql_P_short:
                fexacc :short buf;  goto Type end;
            Type PT Sql_P_ushort:
                fexacc :ushort buf; goto Type end;
            Type PT Sql_P_int:
                fexacc :int buf;    goto Type end;
            Type PT Sql_P_uint:
                fexacc :uint buf;   goto Type end;
            Type PT Sql_P_longlong:
                fexacc :longlong buf;   goto Type end;
            Type PT Sql_P_ulonglong:
                fexacc :ulonglong buf;  goto Type end;
            Type PT Sql_P_sfloat:
                fexacc :sfloat buf; goto Type end;
            Type PT Sql_P_dfloat:
                fexacc :dfloat buf; goto Type end;

            Type PT Sql_P_date:
                lvars sql_date = buf;
                consvector("sql_date",  fexacc sql_date.year,
                                        fexacc sql_date.month,
                                        fexacc sql_date.day, 4);
                goto Type end;

            Type PT Sql_P_time:
                lvars sql_time = buf;
                consvector("sql_time",  fexacc sql_time.hour,
                                        fexacc sql_time.minute,
                                        fexacc sql_time.second, 4);
                goto Type end;

            Type PT Sql_P_timestamp:
                lvars sql_timestamp = buf;
                consvector("sql_timestamp",
                        fexacc sql_timestamp.year,
                        fexacc sql_timestamp.month,
                        fexacc sql_timestamp.day,
                        fexacc sql_timestamp.hour,
                        fexacc sql_timestamp.minute,
                        fexacc sql_timestamp.second,
                        fexacc sql_timestamp.fraction, 8);
                goto Type end;

            Type PT Sql_P_string:
            Type PT Sql_P_binary:
                inits(nb) ->> str;
                move_bytes(1, buf, 1, str, nb);
                goto Type end;

            ;;; UNICODE TYPES
            Type PT Sql_P_wstring:
                inits16(nb/2) ->> str;
                move_bytes(1, buf, 1, str, nb);
            ;;; END UNICODE TYPES

        endgo_on;

ASSIGN_VAL:
        () -> if idvec then fast_subscrv(row,id) else idval(id) endif;

NEXT_ROW:
        row fi_+ 1 -> row;
        fexacc_@ :byte[] buf[bufmax fi_+ 1] -> buf
    endwhile
enddefine;


;;; --- PREPARING SQL REQUESTS --------------------------------------------

define lconstant unbind_params(hstmt);
    lvars res;
    CALL(SQLFreeStmt, (hstmt, SQL_RESET_PARAMS), res, , (false, 0, hstmt));
    garbage_pcvec_list(ST_PARAM_LIST, hstmt)
enddefine;

define lconstant unbind_cols(hstmt);
    lvars res, dvec = handle_data(hstmt);
    CALL(SQLFreeStmt, (hstmt, SQL_UNBIND), res, , (false, 0, hstmt));
    garbage_pcvec_list(ST_COL_LIST, hstmt);
    INIT_MAXROWS -> fast_subscrv(ST_COL_MAXROWS,dvec);
    false -> fast_subscrv(ST_COLS_BOUND,dvec)
enddefine;

define sql_prepare(hstmt, stmt);
    lvars res;
    check_handle(hstmt, STMT_HANDLE);
    check_string(stmt);
    unbind_params(hstmt);
    unbind_cols(hstmt);
    copy_fixed(stmt) -> stmt;
    CALL_ASYNC(SQLPrepare, (hstmt, stmt,SQL_NTS), res, ,
                                            (false, stmt, 1, hstmt));
    sys_grbg_fixed(stmt)
enddefine;

define sql_num_parameters(hstmt);
    lvars ptr, res;
    check_handle(hstmt, STMT_HANDLE);
    fast_subscrv(ST_WORK_PTR,handle_data(hstmt)) -> ptr;
    CALL_ASYNC(SQLNumParams, (hstmt, ptr), res, , (false, 0, hstmt));
    fexacc :short ptr
enddefine;

define sql_describe_parameter(hstmt, N) /* -> (sql_type, nullable) */;
    lvars stype, prec, scale, nullable, res, unsigned = false;
    if isboolean(N) then
        ;;; optional signed/unsigned indication (which can't be determined
        ;;; otherwise)
        ((), hstmt, N) -> (hstmt, N, unsigned)
    endif;
    check_handle(hstmt, STMT_HANDLE);
    checkinteger(N, 1, false);

    EXPTRINITSTR(:short)    -> stype;
    EXPTRINITSTR(:uint)     -> prec;
    EXPTRINITSTR(:short)    -> scale;
    fast_subscrv(ST_WORK_PTR, handle_data(hstmt)) -> nullable;

    CALL_ASYNC(SQLDescribeParam, (hstmt, N, stype, prec, scale, nullable),
                    res, , (false, 0, hstmt));

    sql_cons_sql_type(fexacc :short stype, fexacc :uint prec,
                            fexacc :short scale, unsigned),
    fexacc :short nullable;

    sys_grbg_fixed(stype, prec, scale, 3)
enddefine;

define sql_bind_parameter(/*hstmt,*/ N, id, sql_type);
    lvars   pop_type = false, param_type = SQL_PARAM_INPUT, nbytes, hstmt,
            ctype, ptype, buf, len_ptr, res, mode = false, fxd_vec = false,
            buf2 = false, stype, prec, scale, bufmax;

    if id == "*" and not(sql_type) then
        ;;; unbind all params
        check_handle(N, STMT_HANDLE);
        unbind_params(N);
        return
    endif;

    () -> hstmt;
    if isinteger(id) then
        if isinteger(N) then
            ;;; optional pop type and param type
            (), (), hstmt, N, id, sql_type
                    -> (hstmt, N, id, sql_type, pop_type, param_type);
            checkinteger(param_type, 0, false)
        else
            ;;; optional pop type
            (), hstmt, N, id, sql_type -> (hstmt, N, id, sql_type, pop_type)
        endif
    endif;
    if isinteger(hstmt) then
        ;;; mode supplied
        (), hstmt, N, id -> (hstmt, N, id, mode)
    endif;
    check_handle(hstmt, STMT_HANDLE);
    checkinteger(N, 1, false);
    unless isident(id) then mishap(id, 1, 'IDENTIFIER NEEDED') endunless;
    unless isident(mode) then check_mode_arg(mode) endunless;
    checkinteger(sql_type, false, false);
    if pop_type then
        checkinteger(pop_type, false, false)
    else
        default_pop_type(sql_type) -> pop_type
    endif;

    dest_pop_type(pop_type) -> (ptype, nbytes, ctype);
    fi_max(SIZEOFTYPE(:exptr), nbytes) -> bufmax;
    sql_dest_sql_type(sql_type) -> (stype, prec, scale, _);

    if fast_lmember(stype, long_stypes) then
        ;;; must use data-at-exec param
        exptr_init_fixed(3, vector_key) -> fxd_vec;
        if ptype fi_< PT Sql_P_string then initexptr_mem(bufmax) -> buf2 endif;
        ;;; This should really use sql_select_types to determine the max length
        ;;; of the type, but it's probably not worth it.
        16:7FFFFFFF -> prec
    endif;

    initexptr_mem(bufmax) -> buf;
    EXPTRINITSTR(:int) -> len_ptr;      ;;; needs array if multiple vals set

    CALL(SQLBindParameter, (hstmt, N, param_type, ctype, stype, prec, scale,
                                buf, bufmax, len_ptr),
                res, , (false, N, 1, hstmt));

    ;;; insert new param vec into ST_PARAM_LIST
    insert_pcvec(N, pop_type, buf, len_ptr, id, mode, fxd_vec, buf2,
                                    param_type, ST_PARAM_LIST, hstmt) ->
enddefine;

define sql_cursor_name(hstmt);
    lvars buf, res;
    check_handle(hstmt, STMT_HANDLE);
    EXPTRINITSTR(:byte[MAX_NAME_LEN]) -> buf;
    CALL(SQLGetCursorName, (hstmt, buf, MAX_NAME_LEN, short_ptr), res,
            if stmt_sqlstate(hstmt) = 'S1015' then
                ;;; no cursor
                sys_grbg_fixed(buf);
                return(false)
            endif,
            (true, 0, hstmt));
    exacc_ntstring(buf);
    sys_grbg_fixed(buf)
enddefine;
;;;
define updaterof sql_cursor_name(name, hstmt);
    lvars res;
    check_handle(hstmt, STMT_HANDLE);
    if name then
        check_string(name);
        CALL(SQLSetCursorName, (hstmt, name, SQL_NTS), res, ,
                                            (false, name, 1, hstmt))
    else
        ;;; close cursor
        CALL(SQLFreeStmt, (hstmt, SQL_CLOSE), res, , (false, 0, hstmt))
    endif
enddefine;

;;; --- SUBMITTING REQUESTS ------------------------------------------------

define sql_execute(hstmt);
    lvars stmt = false, res, params, vec, iotype, has_output = false;

    if isstring(hstmt) then
        (), hstmt -> (hstmt, stmt);
        copy_fixed(stmt) -> stmt
    endif;
    check_handle(hstmt, STMT_HANDLE);
    fast_subscrv(ST_PARAM_LIST,handle_data(hstmt)) -> params;

    ;;; set params from their identifiers
    fast_for vec in params do
        fast_subscrv(P_IO_TYPE,vec) -> iotype;
        if iotype /== SQL_PARAM_OUTPUT then
            data_input(hstmt, explode(vec) ->, 1, 1)
        endif;
        if iotype /== SQL_PARAM_INPUT then true -> has_output endif;
    endfor;

    l_typespec func :SQLExecDirect;
    lvars func = if stmt then SQLExecDirect else SQLExecute endif;

    ;;; execute
    CALL_ASYNC(func, (hstmt, stmt,SQL_NTS),
            res,
            if res == SQL_NEED_DATA     ;;; has data-at-execution params
            and (put_data_at_exec(hstmt) ->> res) == SQL_SUCCESS then
                goto SUCCESS
            endif,
            (false, 0, hstmt)
        );

SUCCESS:
    if stmt then sys_grbg_fixed(stmt) endif;
    returnunless(has_output);

    ;;; deal with output params
    fast_for vec in params do
        if fast_subscrv(P_IO_TYPE,vec) /== SQL_PARAM_INPUT then
            data_output(hstmt, explode(vec)->(_,_), 1, 1, false, true, false)
        endif
    endfor
enddefine;


define lconstant col_int_attr(hstmt, N, attr);
    lvars ptr = fast_subscrv(ST_WORK_PTR,handle_data(hstmt)), res;
    CALL_ASYNC(SQLColAttributes, (hstmt, N, attr, false, 0, false, ptr),
                                            res, , (false, 0, hstmt));
    fexacc :int ptr
enddefine;

define lconstant col_stype(hstmt, N) /* -> (islong, stype) */;
    lvars stype = if N == 0 then SQL(Sql_BOOKMARK)
                  else col_int_attr(hstmt, N, SQL(Sql_COLUMN_TYPE))
                  endif;
    fast_lmember(stype, long_stypes), stype;
enddefine;

define lconstant col_sql_type(hstmt, N, stype, prec, scale);
    sql_cons_sql_type(stype, prec, scale,
                fast_lmember(stype, signed_stypes)
                and col_int_attr(hstmt, N, SQL(Sql_COLUMN_UNSIGNED)) /== 0)
enddefine;

define sql_row_count(hstmt);
    lvars ptr, res;
    check_handle(hstmt, STMT_HANDLE);
    fast_subscrv(ST_WORK_PTR,handle_data(hstmt)) -> ptr;
    CALL(SQLRowCount, (hstmt, ptr), res, , (false, 0, hstmt));
    fexacc :int ptr
enddefine;

define sql_num_result_columns(hstmt);
    lvars ptr, res;
    check_handle(hstmt, STMT_HANDLE);
    fast_subscrv(ST_WORK_PTR,handle_data(hstmt)) -> ptr;
    CALL_ASYNC(SQLNumResultCols, (hstmt, ptr), res, , (false, 0, hstmt));
    fexacc :short ptr
enddefine;

define sql_describe_column(hstmt, N) /* -> (name, sql_type, nullable) */;
    lvars name, namelen, stype, prec, scale, nullable, res;

    check_handle(hstmt, STMT_HANDLE);
    checkinteger(N, 1, false);

    EXPTRINITSTR(:byte[MAX_NAME_LEN])   -> name;
    EXPTRINITSTR(:short)                -> namelen;
    EXPTRINITSTR(:short)                -> stype;
    EXPTRINITSTR(:uint)                 -> prec;
    EXPTRINITSTR(:short)                -> scale;
    fast_subscrv(ST_WORK_PTR,handle_data(hstmt)) -> nullable;

    CALL_ASYNC(SQLDescribeCol, (hstmt, N, name, MAX_NAME_LEN, namelen, stype,
                                                    prec, scale, nullable),
                    res, , (false, 0, hstmt));

    exacc_ntstring(name),
    col_sql_type(hstmt, N, fexacc :short stype, fexacc :uint prec,
                                                fexacc :short scale),
    fexacc :short nullable;

    sys_grbg_fixed(name, namelen, stype, prec, scale, 5)
enddefine;

define sql_column_attribute(hstmt, N, attr);
    lvars intbuf, res, dtype, buf, buflen;
    check_handle(hstmt, STMT_HANDLE);
    checkinteger(N, 1, false);
    fast_subscrv(ST_WORK_PTR,handle_data(hstmt)) -> intbuf;
    desc_prepare(attr, intbuf, MAX_NAME_LEN) -> (attr, dtype, buf, buflen);
    CALL_ASYNC(SQLColAttributes, (hstmt, N, attr, buf, buflen, short_ptr,
                                        intbuf), res, , (false, 0, hstmt));
    get_desc_value(dtype, buf)
enddefine;


;;; --- RETRIEVING RESULTS -----------------------------------------------

define lconstant do_bind_col(hstmt, vec);
    lvars   res, nbytes, ctype, bufmax, ptype, nrows,
            (N, pop_type, buf, len_ptr, id, _, _, _, _) = explode(vec);
    dest_pop_type(pop_type) -> (ptype, nbytes, ctype);

    if ptype fi_>= PT Sql_P_string and (col_stype(hstmt, N) -> ) then
        ;;; long data
        fast_subscrv(ST_LONG_DATA_MAX,handle_data(hstmt)) ->> nbytes
                                -> fast_subscrv(PC_FIXED_VEC,vec);
        sys_grbg_fixed(buf);
        if isvector(id) then datalength(id) else 1 endif -> nrows;
        initexptr_mem(nbytes fi_* nrows) ->> buf -> fast_subscrv(PC_BUF,vec)
    endif;

    fi_max(SIZEOFTYPE(:exptr), nbytes) -> bufmax;
    CALL(SQLBindCol, (hstmt, N, ctype, buf, bufmax, len_ptr),
                                        res, , (false, N, 1, hstmt))
enddefine;

define sql_bind_column(hstmt, N, id);
    lvars   pop_type = false, mode = false, stype, ctype, nbytes, buf,
            len_ptr, res, dvec, nrows, islong, new_vec, bufmax, chartype;

    define lconstant recalc_maxrows(dvec);
        lvars maxrows = INIT_MAXROWS, vec, id;
        fast_for vec in fast_subscrv(ST_COL_LIST,dvec) do
            fast_subscrv(PC_ID,vec) -> id;
            if isvector(id) then min(datalength(id),maxrows) else 1 endif
                            -> maxrows;
            quitif(maxrows == 1)
        endfor;
        maxrows -> fast_subscrv(ST_COL_MAXROWS,dvec)
    enddefine;

    if isinteger(id) then
        ;;; optional pop type supplied
        (), hstmt, N, id -> (hstmt, N, id, pop_type)
    endif;
    if isinteger(hstmt) then
        ;;; mode supplied
        (), hstmt, N, id -> (hstmt, N, id, mode)
    endif;
    check_handle(hstmt, STMT_HANDLE);
    handle_data(hstmt) -> dvec;

    if id then
        checkinteger(N, 0, false);          ;;; allow 0 for bookmark
        unless isident(id) or isvector(id) and datalength(id) fi_> 0 then
            mishap(id, 1, 'IDENTIFIER OR NON-NULL VECTOR NEEDED')
        endunless;
        unless isident(mode) then check_mode_arg(mode) endunless;
        if pop_type then
            checkinteger(pop_type, false, false);
            false -> stype      ;;; don't get this unless necesssary
        else
            col_stype(hstmt, N) -> (islong, stype);
            default_pop_type(col_sql_type(hstmt, N, stype, 0, 0)) -> pop_type
        endif;
        dest_pop_type(pop_type) -> (_, nbytes, ctype);
        false -> chartype;
        if ctype == SQL_C_CHAR or ctype == SQL_C_WCHAR then
            unless stype then col_stype(hstmt, N) -> (islong, stype) endunless;
            if islong then
                ;;; use SQL_C_BINARY instead of SQL_C_(W)CHAR to avoid
                ;;; problems with null-termination when reading long
                ;;; data
                (pop_type fi_&& 16:FFFF) fi_|| #_< SQL_C_BINARY << 16 >_#
                                                    -> pop_type;
                ;;; remember the original type
                ctype -> chartype;
            endif
        endif;

        if isvector(id) then datalength(id) else 1 endif -> nrows;
        fi_max(SIZEOFTYPE(:exptr), nbytes) -> bufmax;
        initexptr_mem(bufmax fi_* nrows) -> buf;
        initexptr_mem(SIZEOFTYPE(:int) fi_* nrows) -> len_ptr;

        ;;; insert new col vec into ST_COL_LIST
        insert_pcvec(N, pop_type, buf, len_ptr, id, mode, false, false,
                            chartype, ST_COL_LIST, hstmt) -> new_vec;

        if fast_subscrv(ST_COL_MAXROWS,dvec) fi_>= nrows then
            nrows -> fast_subscrv(ST_COL_MAXROWS,dvec)
        else
            recalc_maxrows(dvec)
        endif;

        if fast_subscrv(ST_COLS_BOUND,dvec) then
            ;;; using extended fetch -- bind it
            do_bind_col(hstmt, new_vec)
        endif

    else
        if N == "*" then
            ;;; unbind all columns
            unbind_cols(hstmt)
        else
            ;;; else unbinding one column
            checkinteger(N, 1, false);
            ncdelete(false, subscrv(ST_COL_LIST,dvec),
                        procedure(vec, _);
                            subscrv(PC_NUM,vec) == N
                        endprocedure, 1) -> subscrv(ST_COL_LIST,dvec);
            recalc_maxrows(dvec);
            if fast_subscrv(ST_COLS_BOUND,dvec) then
                CALL(SQLBindCol, (hstmt, N, 0, false, 0, false),
                                    res, , (false, N, 1, hstmt))
            endif
        endif
    endif
enddefine;

define lconstant refresh_rows(hstmt, row, lastrow, rowstat);
    lvars   vec, dvec = handle_data(hstmt),
            data_got = fast_subscrv(ST_COLS_BOUND,dvec),
            chartype;

    fast_for vec in fast_subscrv(ST_COL_LIST,dvec) do
        data_output(hstmt, explode(vec)->(_,chartype), row, lastrow, rowstat,
            data_got, chartype)
    endfor
enddefine;

define sql_fetch(hstmt) /* -> data_found */;
    lvars res;
    check_handle(hstmt, STMT_HANDLE);

    ;;; do the fetch
    CALL_ASYNC(SQLFetch, (hstmt), res,
                returnif(res == SQL_NO_DATA_FOUND) (false),
                (false, 0, hstmt));

    refresh_rows(hstmt, 1, 1, false);
    true
enddefine;

define sql_app_result_set(hstmt, select, procedure app_p);
    lvars N, id, colarg, n, free_id, col_list, nrescols, init_p = false;

    define get_colarg(N, select);
        lvars x, item;

        until select == [] do
            fast_front(select) ->> x -> item;
            if isvector(x) then subscrv(1,x) -> x endif;
            returnif(x == "*" or x == N
                     or ispair(x) and N >= (dest(x)->x) and N <= hd(x)
                    ) (item);
            fast_back(select) -> select
        enduntil;
        false
    enddefine;

    if isprocedure(select) then
        ;;; optional initial procedure
        (), hstmt, select -> (hstmt, select, init_p)
    endif;
    check_handle(hstmt, STMT_HANDLE);
    listlength(select) -> ;
    garbage_pcvec_list(ST_COL_LIST, hstmt);

    returnif((sql_num_result_columns(hstmt) ->> nrescols) == 0);

    [%  fast_for N to nrescols do
            nextunless(get_colarg(N, select) ->> colarg);
            if tmpclr_cache_prop("id") ->> id then
                fast_idval(id) -> tmpclr_cache_prop("id")
            else
                consident(0, 0, "lex") -> id
            endif;
            hstmt, N;
            if isvector(colarg) then
                destvector(colarg) -> n;
                id -> subscr_stack(n)   ;;; replaces col number
            else
                id
            endif;
            sql_bind_column();
            [% id, if init_p then fast_apply(hstmt, N, init_p) endif %]
        endfor
    %] -> col_list;
    if init_p then fast_apply(hstmt, false, init_p) endif;

    while sql_fetch(hstmt) do
        0 -> n;
        fast_for colarg in col_list do
            destlist(colarg) -> N;
            fast_idval(subscr_stack(N)) -> subscr_stack(N);
            n fi_+ N -> n
        endfor;
        app_p(n)
    endwhile;

    garbage_pcvec_list(ST_COL_LIST, hstmt);
    tmpclr_cache_prop("id") -> free_id;
    fast_for colarg in col_list do
compile_mode :vm -bjmpch;
        fast_front(colarg) -> id;
        free_id -> fast_idval(id), id -> free_id;
        sys_grbg_list(colarg)
    endfor;
    free_id -> tmpclr_cache_prop("id");
    sys_grbg_list(col_list)
enddefine;


;;; --- EXTENDED FETCH ---------------------------------------------------

define sql_extended_fetch(hstmt) /* -> nrows_or_false */;
    lvars   r, vec, dvec, fetchtype = SQL_FETCH_NEXT, rownum = 1,
            rsz, nr_ptr, size, rowstat, nrows, rowstat_vec = false;

    if isvector(hstmt) then
        ;;; optional row status vector
        (), hstmt -> (hstmt, rowstat_vec)
    endif;
    if isintegral(hstmt) then
        if isinteger(dup()) then
            ;;; optional fetch type and row number/offset/bookmark
            (), (), hstmt -> (hstmt, fetchtype, rownum)
        else
            ;;; optional fetch type
            (), hstmt -> (hstmt, fetchtype);
            checkinteger(fetchtype, false, false);
            if fetchtype fi_>= SQL_FETCH_ABSOLUTE then
                ;;; rownum must be specified
                mishap(0, 'ODBC: FETCH TYPE REQUIRES ROW NUMBER TO BE SPECIFIED')
            endif
        endif
    endif;

    check_handle(hstmt, STMT_HANDLE);
    handle_data(hstmt) -> dvec;
    unless fast_subscrv(ST_ROWSET_SIZE,dvec) ->> rsz then
        ;;; get this and cache it
        sql_statement_option(hstmt, Sql_ROWSET_SIZE) ->> rsz
                                    -> fast_subscrv(ST_ROWSET_SIZE,dvec)
    endunless;

    if rowstat_vec and datalength(rowstat_vec) fi_< rsz then
        mishap(datalength(rowstat_vec), 1,
                'ODBC: ROW STATUS VECTOR TOO SMALL FOR ROWSET SIZE')
    endif;
    if fast_subscrv(ST_COL_MAXROWS,dvec) fi_< rsz then
        mishap(rsz, 1, 'ODBC: ROWSET SIZE TOO LARGE FOR COLUMN BINDING(S)')
    endif;

    unless rsz == 1 or fast_subscrv(ST_COLS_BOUND,dvec) then
        ;;; bind all columns when there are multiple rows
        fast_for vec in subscrv(ST_COL_LIST,dvec) do
            do_bind_col(hstmt, vec)
        endfor;
        true -> fast_subscrv(ST_COLS_BOUND,dvec)
    endunless;

    rsz fi_* SIZEOFTYPE(:ushort) -> size;
    unless (fast_subscrv(ST_EF_ROWSTAT,dvec) ->> rowstat)
    and datasize(rowstat) fi_>= size then
        if rowstat then sys_grbg_fixed(rowstat) endif;
        initexptr_mem(size) ->> rowstat -> fast_subscrv(ST_EF_ROWSTAT,dvec)
    endunless;
    fast_subscrv(ST_WORK_PTR,dvec) -> nr_ptr;

    ;;; do the fetch
    CALL_ASYNC(SQLExtendedFetch, (hstmt, fetchtype, rownum, nr_ptr, rowstat),
                r,
                returnif(r == SQL_NO_DATA_FOUND) (false),
                (false, 0, hstmt)
        );

    if rowstat_vec then
        rowstat_vec -> fast_subscrv(ST_EF_ROWSTAT_VEC,dvec);
        fast_for r to rsz do
            fexacc :ushort[] rowstat[r] -> fast_subscrv(r,rowstat_vec)
        endfor
    endif;

    fexacc :uint nr_ptr -> nrows;
    refresh_rows(hstmt, 1, rsz, rowstat);
    nrows
enddefine;

define sql_set_position(hstmt, rownum);
    lvars   r, vec, dvec, rsz, row, lastrow, len, rowstat_vec, fxd_vec, buf2,
            rowstat, operation = SQL_POSITION, lock = SQL_LOCK_NO_CHANGE;

    define lconstant get_fxd_vec(hstmt, vec);
        lvars nbytes, ptype, bufmax, nrows, id;
        ;;; if col is long data, store fixed vec in PC_FIXED_VEC, else "undef"
        dest_pop_type(fast_subscrv(PC_POP_TYPE,vec)) -> (ptype, nbytes, _);
        if ptype fi_>= PT Sql_P_string
        and (col_stype(hstmt, fast_subscrv(PC_NUM,vec)) -> ) then
            fi_max(SIZEOFTYPE(:exptr), nbytes) -> bufmax;
            fast_subscrv(PC_ID,vec) -> id;
            if isvector(id) then datalength(id) else 1 endif -> nrows;
            exptr_init_fixed(nrows fi_* 3, vector_key),
            initexptr_mem(nrows fi_* bufmax)
        else
            "undef", false
        endif
    enddefine;

    if isinteger(hstmt) then
        if isinteger(dup()) then
            ;;; optional operation and lock type
            (), (), hstmt, rownum -> (hstmt, rownum, operation, lock)
        else
            ;;; optional operation
            (), hstmt, rownum -> (hstmt, rownum, operation)
        endif
    endif;

    check_handle(hstmt, STMT_HANDLE);
    handle_data(hstmt) -> dvec;
    unless fast_subscrv(ST_EF_ROWSTAT,dvec) ->> rowstat then
        mishap(0, 'ODBC: sql_extended_fetch NOT CALLED BEFORE sql_set_position')
    endunless;
    checkinteger(rownum, 0, false);
    if fast_subscrv(ST_COL_MAXROWS,dvec) fi_< rownum then
        mishap(rsz, 1, 'ODBC: ROW NUMBER TOO LARGE FOR COLUMN BINDING(S)')
    endif;
    fast_subscrv(ST_ROWSET_SIZE,dvec) -> rsz;

    if rownum == 0 then 1, rsz else rownum, rownum endif -> (row, lastrow);

    if operation == SQL_UPDATE or operation == SQL_ADD then
        fast_for vec in subscrv(ST_COL_LIST,dvec) do
            hstmt, explode(vec) -> (fxd_vec, buf2, _);
            unless fxd_vec then
                get_fxd_vec(hstmt, vec) ->> buf2 -> fast_subscrv(PC_BUF2,vec)
                            ->> fxd_vec -> fast_subscrv(PC_FIXED_VEC,vec)
            endunless;
            data_input((), fxd_vec /== "undef" and fxd_vec, buf2, row, lastrow)
        endfor
    endif;

    CALL_ASYNC(SQLSetPos, (hstmt, rownum, operation, lock),
            r,
            if r == SQL_NEED_DATA       ;;; has data-at-execution columns
            and (put_data_at_exec(hstmt) ->> r) == SQL_SUCCESS then
                goto SUCCESS
            endif,
            (false, 0, hstmt)
        );

SUCCESS:
    returnif(operation == SQL_POSITION);

    if fast_subscrv(ST_EF_ROWSTAT_VEC,dvec) ->> rowstat_vec then
        fast_for r from row to fi_min(rsz,lastrow) do
            fexacc :ushort[] rowstat[r] -> fast_subscrv(r,rowstat_vec)
        endfor
    endif;

    if operation == SQL_REFRESH then
        refresh_rows(hstmt, row, lastrow, rowstat)
    endif
enddefine;


;;; --- CATALOG FUNCTIONS -------------------------------------------------

define sql_select_types(hstmt /*,sql_type*/);
    lvars stype = Sql_ALL_TYPES, res;
    if isinteger(hstmt) then
        ;;; optional sql type
        (), hstmt -> (hstmt, stype)
    endif;
    sql_dest_sql_type(stype) -> (stype, _, _, _);
    check_handle(hstmt, STMT_HANDLE);
    CALL_ASYNC(SQLGetTypeInfo, (hstmt, stype), res, , (false, 0, hstmt))
enddefine;

define sql_select_columns(hstmt, tab_qual, tab_owner, tab_name, col_name);
    lvars res;
    check_handle(hstmt, STMT_HANDLE);
    if tab_qual then check_string(tab_qual) endif;
    if tab_owner then check_string(tab_owner) endif;
    if tab_name then check_string(tab_name) endif;
    if col_name then check_string(col_name) endif;
    CALL_ASYNC(SQLColumns, (hstmt, tab_qual,SQL_NTS, tab_owner,SQL_NTS,
                                        tab_name,SQL_NTS, col_name,SQL_NTS),
                    res, , (false, 0, hstmt))
enddefine;

define sql_select_special_columns(hstmt, col_type, tab_qual, tab_owner,
                                            tab_name, scope, nullable);
    lvars res;
    check_handle(hstmt, STMT_HANDLE);
    checkinteger(col_type, false, false);
    if tab_qual then check_string(tab_qual) endif;
    if tab_owner then check_string(tab_owner) endif;
    if tab_name then check_string(tab_name) endif;
    checkinteger(scope, false, false);
    checkinteger(nullable, false, false);
    CALL_ASYNC(SQLSpecialColumns, (hstmt, col_type, tab_qual,SQL_NTS,
                        tab_owner,SQL_NTS, tab_name,SQL_NTS, scope, nullable),
                    res, , (false, 0, hstmt))
enddefine;

define sql_select_column_privileges(hstmt, tab_qual, tab_owner, tab_name,
                                                                col_name);
    lvars res;
    check_handle(hstmt, STMT_HANDLE);
    if tab_qual then check_string(tab_qual) endif;
    if tab_owner then check_string(tab_owner) endif;
    if tab_name then check_string(tab_name) endif;
    if col_name then check_string(col_name) endif;
    CALL_ASYNC(SQLColumnPrivileges,
                (hstmt, tab_qual,SQL_NTS, tab_owner,SQL_NTS,
                tab_name,SQL_NTS, col_name,SQL_NTS),
                    res, , (false, 0, hstmt))
enddefine;

define sql_select_statistics(hstmt, tab_qual, tab_owner, tab_name, unique,
                                                            accuracy);
    lvars res;
    check_handle(hstmt, STMT_HANDLE);
    if tab_qual then check_string(tab_qual) endif;
    if tab_owner then check_string(tab_owner) endif;
    check_string(tab_name);         ;;; required
    checkinteger(unique, false, false);
    checkinteger(accuracy, false, false);
    CALL_ASYNC(SQLStatistics, (hstmt, tab_qual,SQL_NTS, tab_owner,SQL_NTS,
                                    tab_name,SQL_NTS, unique, accuracy),
                    res, , (false, 0, hstmt))
enddefine;

define sql_select_tables(hstmt, tab_qual, tab_owner, tab_name, tab_type);
    lvars res;
    check_handle(hstmt, STMT_HANDLE);
    if tab_qual then check_string(tab_qual) endif;
    if tab_owner then check_string(tab_owner) endif;
    if tab_name then check_string(tab_name) endif;
    if tab_type then check_string(tab_type) endif;
    CALL_ASYNC(SQLTables, (hstmt, tab_qual,SQL_NTS, tab_owner,SQL_NTS,
                                        tab_name,SQL_NTS, tab_type,SQL_NTS),
                res, , (false, 0, hstmt))
enddefine;

define sql_select_table_privileges(hstmt, tab_qual, tab_owner, tab_name);
    lvars res;
    check_handle(hstmt, STMT_HANDLE);
    if tab_qual then check_string(tab_qual) endif;
    if tab_owner then check_string(tab_owner) endif;
    if tab_name then check_string(tab_name) endif;
    CALL_ASYNC(SQLTablePrivileges, (hstmt, tab_qual,SQL_NTS, tab_owner,SQL_NTS,
                                        tab_name,SQL_NTS),
                res, , (false, 0, hstmt))
enddefine;

define sql_select_primary_keys(hstmt, tab_qual, tab_owner, tab_name);
    lvars res;
    check_handle(hstmt, STMT_HANDLE);
    if tab_qual then check_string(tab_qual) endif;
    if tab_owner then check_string(tab_owner) endif;
    check_string(tab_name);         ;;; required
    CALL_ASYNC(SQLPrimaryKeys, (hstmt, tab_qual,SQL_NTS, tab_owner,SQL_NTS,
                                        tab_name,SQL_NTS),
                res, , (false, 0, hstmt))
enddefine;

define sql_select_foreign_keys(hstmt, pktab_qual, pktab_owner, pktab_name,
                                fktab_qual, fktab_owner, fktab_name);
    lvars res;
    check_handle(hstmt, STMT_HANDLE);
    if pktab_qual then check_string(pktab_qual) endif;
    if pktab_owner then check_string(pktab_owner) endif;
    if pktab_name then check_string(pktab_name) endif;
    if fktab_qual then check_string(fktab_qual) endif;
    if fktab_owner then check_string(fktab_owner) endif;
    if fktab_name then check_string(fktab_name) endif;
    CALL_ASYNC(SQLForeignKeys, (hstmt,
                pktab_qual,SQL_NTS, pktab_owner,SQL_NTS, pktab_name,SQL_NTS,
                fktab_qual,SQL_NTS, fktab_owner,SQL_NTS, fktab_name,SQL_NTS),
                res, , (false, 0, hstmt))
enddefine;


constant odbc = true;

endexload_batch;
endsection;



/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Jun  8 1999
        Fix to sql_column_attribute
--- Robert Duncan, Mar  3 1999
        Further UNICODE support.
--- Julian Clinton, Feb 19 1999
        Added some UNICODE support (for SQL Server 7.0)
--- Robert Duncan, Jan 27 1999
        Changed return type for external SQL functions to RETCODE (short)
--- Robert Duncan, Jan  7 1997
        Changed exload to use autoloadable ODBC_EXLIBS
--- John Gibson, Aug  2 1996
        Stopped sql_alloc_statement getting the rowset size (done by
        sql_extended_fetch instead).
--- Robert Duncan, Jul 19 1996
        Added exload libraries for Win32
 */
