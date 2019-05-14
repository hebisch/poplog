/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.all/lib/include/odbc.ph
 > Purpose:         Definitions for lib odbc
 > Author:          John Gibson, May  1 1996 (see revisions)
 > Documentation:   REF * ODBC
 > Related Files:   LIB * ODBC
 */

#_TERMIN_IF DEF ODBC_INCLUDED

section;

    /* Internal */

iconstant macro (

    ;;; General
    SQL_SQLSTATE_SIZE       = 5,
    SQL_MAX_MESSAGE_LENGTH  = 512,
    SQL_MAX_DSN_LENGTH      = 32,

    ;;; Return Codes
    SQL_CANCELLED           = -99,      ;;; special for pop's use
    SQL_ERROR               = -1,
    SQL_SUCCESS             = 0,
    SQL_SUCCESS_WITH_INFO   = 1,
    SQL_STILL_EXECUTING     = 2,
    SQL_NEED_DATA           = 99,
    SQL_NO_DATA_FOUND       = 100,

    ;;; Special length values
    SQL_NULL_DATA           = -1,
    SQL_DATA_AT_EXEC        = -2,
    SQL_NTS                 = -3,
    SQL_NO_TOTAL            = -4,
    SQL_DEFAULT_PARAM       = -5,
    SQL_IGNORE              = -6,
    SQL_LEN_DATA_AT_EXEC_OFFSET = -100,

    ;;; SQLFreeStmt
    SQL_CLOSE               = 0,
    SQL_DROP                = 1,
    SQL_UNBIND              = 2,
    SQL_RESET_PARAMS        = 3,

    ;;; SQLTransact
    SQL_COMMIT              = 0,
    SQL_ROLLBACK            = 1,

    ;;; SQL datatypes
    SQL_BIT                 = -7,
    SQL_TINYINT             = -6,
    SQL_BIGINT              = -5,
    SQL_LONGVARBINARY       = -4,
    SQL_VARBINARY           = -3,
    SQL_BINARY              = -2,
    SQL_LONGVARCHAR         = -1,
    SQL_CHAR                =  1,
    SQL_NUMERIC             =  2,
    SQL_DECIMAL             =  3,
    SQL_INTEGER             =  4,
    SQL_SMALLINT            =  5,
    SQL_FLOAT               =  6,
    SQL_REAL                =  7,
    SQL_DOUBLE              =  8,
    SQL_DATE                =  9,
    SQL_TIME                = 10,
    SQL_TIMESTAMP           = 11,
    SQL_VARCHAR             = 12,
    SQL_ALL_TYPES           = 0,

    ;;; UNICODE TYPES (from sqlucode.h)
    SQL_WCHAR               = -8,
    SQL_WVARCHAR            = -9,
    SQL_WLONGVARCHAR        = -10,
    ;;; END UNICODE TYPES

    ;;; ODBC 3.5
    SQL_GUID                = -11,

    ;;; ODBC 3.0
    SQL_TYPE_DATE           = 91,
    SQL_TYPE_TIME           = 92,
    SQL_TYPE_TIMESTAMP      = 93,

    SQL_CODE_DATE           = 1,
    SQL_CODE_TIME           = 2,
    SQL_CODE_TIMESTAMP      = 3,

    SQL_CODE_YEAR               = 1,
    SQL_CODE_MONTH              = 2,
    SQL_CODE_DAY                = 3,
    SQL_CODE_HOUR               = 4,
    SQL_CODE_MINUTE             = 5,
    SQL_CODE_SECOND             = 6,
    SQL_CODE_YEAR_TO_MONTH          = 7,
    SQL_CODE_DAY_TO_HOUR            = 8,
    SQL_CODE_DAY_TO_MINUTE          = 9,
    SQL_CODE_DAY_TO_SECOND          = 10,
    SQL_CODE_HOUR_TO_MINUTE         = 11,
    SQL_CODE_HOUR_TO_SECOND         = 12,
    SQL_CODE_MINUTE_TO_SECOND       = 13,

    SQL_INTERVAL_YEAR                   = (100 + SQL_CODE_YEAR),
    SQL_INTERVAL_MONTH                  = (100 + SQL_CODE_MONTH),
    SQL_INTERVAL_DAY                    = (100 + SQL_CODE_DAY),
    SQL_INTERVAL_HOUR                   = (100 + SQL_CODE_HOUR),
    SQL_INTERVAL_MINUTE                 = (100 + SQL_CODE_MINUTE),
    SQL_INTERVAL_SECOND                 = (100 + SQL_CODE_SECOND),
    SQL_INTERVAL_YEAR_TO_MONTH          = (100 + SQL_CODE_YEAR_TO_MONTH),
    SQL_INTERVAL_DAY_TO_HOUR            = (100 + SQL_CODE_DAY_TO_HOUR),
    SQL_INTERVAL_DAY_TO_MINUTE          = (100 + SQL_CODE_DAY_TO_MINUTE),
    SQL_INTERVAL_DAY_TO_SECOND          = (100 + SQL_CODE_DAY_TO_SECOND),
    SQL_INTERVAL_HOUR_TO_MINUTE         = (100 + SQL_CODE_HOUR_TO_MINUTE),
    SQL_INTERVAL_HOUR_TO_SECOND         = (100 + SQL_CODE_HOUR_TO_SECOND),
    SQL_INTERVAL_MINUTE_TO_SECOND       = (100 + SQL_CODE_MINUTE_TO_SECOND),
    ;;; END ODBC 3.0

    ;;; C types
    SQL_SIGNED_OFFSET       = -20,
    SQL_UNSIGNED_OFFSET     = -22,

    SQL_C_BIT               = SQL_BIT,
    SQL_C_STINYINT          = SQL_TINYINT+SQL_SIGNED_OFFSET,
    SQL_C_UTINYINT          = SQL_TINYINT+SQL_UNSIGNED_OFFSET,
    SQL_C_SSHORT            = SQL_SMALLINT+SQL_SIGNED_OFFSET,
    SQL_C_USHORT            = SQL_SMALLINT+SQL_UNSIGNED_OFFSET,
    SQL_C_SLONG             = SQL_INTEGER+SQL_SIGNED_OFFSET,
    SQL_C_ULONG             = SQL_INTEGER+SQL_UNSIGNED_OFFSET,
    SQL_C_FLOAT             = SQL_REAL,
    SQL_C_DOUBLE            = SQL_DOUBLE,
    SQL_C_DATE              = SQL_DATE,
    SQL_C_TIME              = SQL_TIME,
    SQL_C_TIMESTAMP         = SQL_TIMESTAMP,
    SQL_C_CHAR              = SQL_CHAR,
    SQL_C_BINARY            = SQL_BINARY,
    SQL_C_BOOKMARK          = SQL_C_ULONG,

    ;;; UNICODE TYPES
    SQL_C_WCHAR             = SQL_WCHAR,
    ;;; END UNICODE TYPES

    ;;; ODBC 3.5
    SQL_C_GUID              = SQL_GUID,
    ;;; END ODBC 3.5

    ;;; ODBC 3.0
    SQL_C_TYPE_DATE                 = SQL_TYPE_DATE,
    SQL_C_TYPE_TIME                 = SQL_TYPE_TIME,
    SQL_C_TYPE_TIMESTAMP            = SQL_TYPE_TIMESTAMP,
    SQL_C_INTERVAL_YEAR             = SQL_INTERVAL_YEAR,
    SQL_C_INTERVAL_MONTH            = SQL_INTERVAL_MONTH,
    SQL_C_INTERVAL_DAY              = SQL_INTERVAL_DAY,
    SQL_C_INTERVAL_HOUR             = SQL_INTERVAL_HOUR,
    SQL_C_INTERVAL_MINUTE           = SQL_INTERVAL_MINUTE,
    SQL_C_INTERVAL_SECOND           = SQL_INTERVAL_SECOND,
    SQL_C_INTERVAL_YEAR_TO_MONTH    = SQL_INTERVAL_YEAR_TO_MONTH,
    SQL_C_INTERVAL_DAY_TO_HOUR      = SQL_INTERVAL_DAY_TO_HOUR,
    SQL_C_INTERVAL_DAY_TO_MINUTE    = SQL_INTERVAL_DAY_TO_MINUTE,
    SQL_C_INTERVAL_DAY_TO_SECOND    = SQL_INTERVAL_DAY_TO_SECOND,
    SQL_C_INTERVAL_HOUR_TO_MINUTE   = SQL_INTERVAL_HOUR_TO_MINUTE,
    SQL_C_INTERVAL_HOUR_TO_SECOND   = SQL_INTERVAL_HOUR_TO_SECOND,
    SQL_C_INTERVAL_MINUTE_TO_SECOND = SQL_INTERVAL_MINUTE_TO_SECOND,

    SQL_C_SBIGINT   = (SQL_BIGINT+SQL_SIGNED_OFFSET),     /* SIGNED BIGINT */
    SQL_C_UBIGINT   = (SQL_BIGINT+SQL_UNSIGNED_OFFSET),   /* UNSIGNED BIGINT */
    ;;; END ODBC 3.0

);

define :inline lconstant SQL_LEN_DATA_AT_EXEC(length);
    (SQL_LEN_DATA_AT_EXEC_OFFSET-(length))
enddefine;



    /* SQL_ output values */

iconstant macro (

    ;;; sql_connect_attribute values (SQLGetInfo)
    ;;; Sql_CONVERT_*
    SQL_CVT_CHAR                        = 2:1e0,
    SQL_CVT_NUMERIC                     = 2:1e1,
    SQL_CVT_DECIMAL                     = 2:1e2,
    SQL_CVT_INTEGER                     = 2:1e3,
    SQL_CVT_SMALLINT                    = 2:1e4,
    SQL_CVT_FLOAT                       = 2:1e5,
    SQL_CVT_REAL                        = 2:1e6,
    SQL_CVT_DOUBLE                      = 2:1e7,
    SQL_CVT_VARCHAR                     = 2:1e8,
    SQL_CVT_LONGVARCHAR                 = 2:1e9,
    SQL_CVT_BINARY                      = 2:1e10,
    SQL_CVT_VARBINARY                   = 2:1e11,
    SQL_CVT_BIT                         = 2:1e12,
    SQL_CVT_TINYINT                     = 2:1e13,
    SQL_CVT_BIGINT                      = 2:1e14,
    SQL_CVT_DATE                        = 2:1e15,
    SQL_CVT_TIME                        = 2:1e16,
    SQL_CVT_TIMESTAMP                   = 2:1e17,
    SQL_CVT_LONGVARBINARY               = 2:1e18,

    ;;; ODBC 3.0
    SQL_CVT_INTERVAL_YEAR_MONTH         = 2:1e19,
    SQL_CVT_INTERVAL_DAY_TIME           = 2:1e20,
    SQL_CVT_WCHAR                       = 2:1e21,
    SQL_CVT_WLONGVARCHAR                = 2:1e22,
    SQL_CVT_WVARCHAR                    = 2:1e23,
    ;;; END ODBC 3.0

    ;;; Sql_CONVERT_FUNCTIONS
    SQL_FN_CVT_CONVERT                  = 2:1e0,

    ;;; ODBC 3.0
    SQL_FN_CVT_CAST                     = 2:1e1,
    ;;; END ODBC 3.0

    ;;; Sql_STRING_FUNCTIONS
    SQL_FN_STR_CONCAT                   = 2:1e0,
    SQL_FN_STR_INSERT                   = 2:1e1,
    SQL_FN_STR_LEFT                     = 2:1e2,
    SQL_FN_STR_LTRIM                    = 2:1e3,
    SQL_FN_STR_LENGTH                   = 2:1e4,
    SQL_FN_STR_LOCATE                   = 2:1e5,
    SQL_FN_STR_LCASE                    = 2:1e6,
    SQL_FN_STR_REPEAT                   = 2:1e7,
    SQL_FN_STR_REPLACE                  = 2:1e8,
    SQL_FN_STR_RIGHT                    = 2:1e9,
    SQL_FN_STR_RTRIM                    = 2:1e10,
    SQL_FN_STR_SUBSTRING                = 2:1e11,
    SQL_FN_STR_UCASE                    = 2:1e12,
    SQL_FN_STR_ASCII                    = 2:1e13,
    SQL_FN_STR_CHAR                     = 2:1e14,
    SQL_FN_STR_DIFFERENCE               = 2:1e15,
    SQL_FN_STR_LOCATE_2                 = 2:1e16,
    SQL_FN_STR_SOUNDEX                  = 2:1e17,
    SQL_FN_STR_SPACE                    = 2:1e18,
    ;;; Sql_NUMERIC_FUNCTIONS
    SQL_FN_NUM_ABS                      = 2:1e0,
    SQL_FN_NUM_ACOS                     = 2:1e1,
    SQL_FN_NUM_ASIN                     = 2:1e2,
    SQL_FN_NUM_ATAN                     = 2:1e3,
    SQL_FN_NUM_ATAN2                    = 2:1e4,
    SQL_FN_NUM_CEILING                  = 2:1e5,
    SQL_FN_NUM_COS                      = 2:1e6,
    SQL_FN_NUM_COT                      = 2:1e7,
    SQL_FN_NUM_EXP                      = 2:1e8,
    SQL_FN_NUM_FLOOR                    = 2:1e9,
    SQL_FN_NUM_LOG                      = 2:1e10,
    SQL_FN_NUM_MOD                      = 2:1e11,
    SQL_FN_NUM_SIGN                     = 2:1e12,
    SQL_FN_NUM_SIN                      = 2:1e13,
    SQL_FN_NUM_SQRT                     = 2:1e14,
    SQL_FN_NUM_TAN                      = 2:1e15,
    SQL_FN_NUM_PI                       = 2:1e16,
    SQL_FN_NUM_RAND                     = 2:1e17,
    SQL_FN_NUM_DEGREES                  = 2:1e18,
    SQL_FN_NUM_LOG10                    = 2:1e19,
    SQL_FN_NUM_POWER                    = 2:1e20,
    SQL_FN_NUM_RADIANS                  = 2:1e21,
    SQL_FN_NUM_ROUND                    = 2:1e22,
    SQL_FN_NUM_TRUNCATE                 = 2:1e23,
    ;;; Sql_TIMEDATE_FUNCTIONS
    SQL_FN_TD_NOW                       = 2:1e0,
    SQL_FN_TD_CURDATE                   = 2:1e1,
    SQL_FN_TD_DAYOFMONTH                = 2:1e2,
    SQL_FN_TD_DAYOFWEEK                 = 2:1e3,
    SQL_FN_TD_DAYOFYEAR                 = 2:1e4,
    SQL_FN_TD_MONTH                     = 2:1e5,
    SQL_FN_TD_QUARTER                   = 2:1e6,
    SQL_FN_TD_WEEK                      = 2:1e7,
    SQL_FN_TD_YEAR                      = 2:1e8,
    SQL_FN_TD_CURTIME                   = 2:1e9,
    SQL_FN_TD_HOUR                      = 2:1e10,
    SQL_FN_TD_MINUTE                    = 2:1e11,
    SQL_FN_TD_SECOND                    = 2:1e12,
    SQL_FN_TD_TIMESTAMPADD              = 2:1e13,
    SQL_FN_TD_TIMESTAMPDIFF             = 2:1e14,
    SQL_FN_TD_DAYNAME                   = 2:1e15,
    SQL_FN_TD_MONTHNAME                 = 2:1e16,
    ;;; Sql_SYSTEM_FUNCTIONS
    SQL_FN_SYS_USERNAME                 = 2:1e0,
    SQL_FN_SYS_DBNAME                   = 2:1e1,
    SQL_FN_SYS_IFNULL                   = 2:1e2,
    ;;; Sql_TIMEDATE_ADD_INTERVALS/Sql_TIMEDATE_DIFF_INTERVALS
    SQL_FN_TSI_FRAC_SECOND              = 2:1e0,
    SQL_FN_TSI_SECOND                   = 2:1e1,
    SQL_FN_TSI_MINUTE                   = 2:1e2,
    SQL_FN_TSI_HOUR                     = 2:1e3,
    SQL_FN_TSI_DAY                      = 2:1e4,
    SQL_FN_TSI_WEEK                     = 2:1e5,
    SQL_FN_TSI_MONTH                    = 2:1e6,
    SQL_FN_TSI_QUARTER                  = 2:1e7,
    SQL_FN_TSI_YEAR                     = 2:1e8,
    ;;; Sql_ODBC_API_CONFORMANCE
    SQL_OAC_NONE                        = 0,
    SQL_OAC_LEVEL1                      = 1,
    SQL_OAC_LEVEL2                      = 2,
    ;;; Sql_ODBC_SAG_CLI_CONFORMANCE
    SQL_OSCC_NOT_COMPLIANT              = 0,
    SQL_OSCC_COMPLIANT                  = 1,
    ;;; Sql_ODBC_SQL_CONFORMANCE
    SQL_OSC_MINIMUM                     = 0,
    SQL_OSC_CORE                        = 1,
    SQL_OSC_EXTENDED                    = 2,
    ;;; Sql_CONCAT_NULL_BEHAVIOR
    SQL_CB_NULL                         = 0,
    SQL_CB_NON_NULL                     = 1,
    ;;; Sql_CURSOR_COMMIT_BEHAVIOR/Sql_CURSOR_ROLLBACK_BEHAVIOR
    SQL_CB_DELETE                       = 0,
    SQL_CB_CLOSE                        = 1,
    SQL_CB_PRESERVE                     = 2,
    ;;; Sql_IDENTIFIER_CASE
    SQL_IC_UPPER                        = 1,
    SQL_IC_LOWER                        = 2,
    SQL_IC_SENSITIVE                    = 3,
    SQL_IC_MIXED                        = 4,
    ;;; Sql_TXN_CAPABLE
    SQL_TC_NONE                         = 0,
    SQL_TC_DML                          = 1,
    SQL_TC_ALL                          = 2,
    SQL_TC_DDL_COMMIT                   = 3,
    SQL_TC_DDL_IGNORE                   = 4,
    ;;; Sql_SCROLL_OPTIONS
    SQL_SO_FORWARD_ONLY                 = 2:1e0,
    SQL_SO_KEYSET_DRIVEN                = 2:1e1,
    SQL_SO_DYNAMIC                      = 2:1e2,
    SQL_SO_MIXED                        = 2:1e3,
    SQL_SO_STATIC                       = 2:1e4,
    ;;; Sql_SCROLL_CONCURRENCY
    SQL_SCCO_READ_ONLY                  = 2:1e0,
    SQL_SCCO_LOCK                       = 2:1e1,
    SQL_SCCO_OPT_ROWVER                 = 2:1e2,
    SQL_SCCO_OPT_VALUES                 = 2:1e3,
    ;;; Sql_FETCH_DIRECTION
    SQL_FD_FETCH_NEXT                   = 2:1e0,
    SQL_FD_FETCH_FIRST                  = 2:1e1,
    SQL_FD_FETCH_LAST                   = 2:1e2,
    SQL_FD_FETCH_PRIOR                  = 2:1e3,
    SQL_FD_FETCH_ABSOLUTE               = 2:1e4,
    SQL_FD_FETCH_RELATIVE               = 2:1e5,
    SQL_FD_FETCH_RESUME                 = 2:1e6,
    SQL_FD_FETCH_BOOKMARK               = 2:1e7,
    ;;; Sql_TXN_ISOLATION_OPTION
    SQL_TXN_READ_UNCOMMITTED            = 2:1e0,
    SQL_TXN_READ_COMMITTED              = 2:1e1,
    SQL_TXN_REPEATABLE_READ             = 2:1e2,
    SQL_TXN_SERIALIZABLE                = 2:1e3,
    SQL_TXN_VERSIONING                  = 2:1e4,
    ;;; Sql_CORRELATION_NAME
    SQL_CN_NONE                         = 0,
    SQL_CN_DIFFERENT                    = 1,
    SQL_CN_ANY                          = 2,
    ;;; Sql_NULL_COLLATION
    SQL_NC_HIGH                         = 0,
    SQL_NC_LOW                          = 1,
    SQL_NC_START                        = 2,
    SQL_NC_END                          = 4,
    ;;; Sql_FILE_USAGE
    SQL_FILE_NOT_SUPPORTED              = 0,
    SQL_FILE_TABLE                      = 1,
    SQL_FILE_QUALIFIER                  = 2,
    ;;; Sql_GETDATA_EXTENSIONS
    SQL_GD_ANY_COLUMN                   = 2:1e0,
    SQL_GD_ANY_ORDER                    = 2:1e1,
    SQL_GD_BLOCK                        = 2:1e2,
    SQL_GD_BOUND                        = 2:1e3,
    ;;; Sql_ALTER_TABLE
    SQL_AT_ADD_COLUMN                   = 2:1e0,
    SQL_AT_DROP_COLUMN                  = 2:1e1,
    ;;; Sql_POSITIONED_STATEMENTS
    SQL_PS_POSITIONED_DELETE            = 2:1e0,
    SQL_PS_POSITIONED_UPDATE            = 2:1e1,
    SQL_PS_SELECT_FOR_UPDATE            = 2:1e2,
    ;;; Sql_GROUP_BY
    SQL_GB_NOT_SUPPORTED                = 0,
    SQL_GB_GROUP_BY_EQUALS_SELECT       = 1,
    SQL_GB_GROUP_BY_CONTAINS_SELECT     = 2,
    SQL_GB_NO_RELATION                  = 3,
    ;;; Sql_OWNER_USAGE
    SQL_OU_DML_STATEMENTS               = 2:1e0,
    SQL_OU_PROCEDURE_INVOCATION         = 2:1e1,
    SQL_OU_TABLE_DEFINITION             = 2:1e2,
    SQL_OU_INDEX_DEFINITION             = 2:1e3,
    SQL_OU_PRIVILEGE_DEFINITION         = 2:1e4,
    ;;; Sql_QUALIFIER_USAGE
    SQL_QU_DML_STATEMENTS               = 2:1e0,
    SQL_QU_PROCEDURE_INVOCATION         = 2:1e1,
    SQL_QU_TABLE_DEFINITION             = 2:1e2,
    SQL_QU_INDEX_DEFINITION             = 2:1e3,
    SQL_QU_PRIVILEGE_DEFINITION         = 2:1e4,
    ;;; Sql_SUBQUERIES
    SQL_SQ_COMPARISON                   = 2:1e0,
    SQL_SQ_EXISTS                       = 2:1e1,
    SQL_SQ_IN                           = 2:1e2,
    SQL_SQ_QUANTIFIED                   = 2:1e3,
    SQL_SQ_CORRELATED_SUBQUERIES        = 2:1e4,
    ;;; Sql_UNION
    SQL_U_UNION                         = 2:1e0,
    SQL_U_UNION_ALL                     = 2:1e1,
    ;;; Sql_BOOKMARK_PERSISTENCE
    SQL_BP_CLOSE                        = 2:1e0,
    SQL_BP_DELETE                       = 2:1e1,
    SQL_BP_DROP                         = 2:1e2,
    SQL_BP_TRANSACTION                  = 2:1e3,
    SQL_BP_UPDATE                       = 2:1e4,
    SQL_BP_OTHER_HSTMT                  = 2:1e5,
    SQL_BP_SCROLL                       = 2:1e6,
    ;;; Sql_STATIC_SENSITIVITY
    SQL_SS_ADDITIONS                    = 2:1e0,
    SQL_SS_DELETIONS                    = 2:1e1,
    SQL_SS_UPDATES                      = 2:1e2,
    ;;; Sql_LOCK_TYPESL
    SQL_LCK_NO_CHANGE                   = 2:1e0,
    SQL_LCK_EXCLUSIVE                   = 2:1e1,
    SQL_LCK_UNLOCK                      = 2:1e2,
    ;;; Sql_POS_OPERATIONS
    SQL_POS_POSITION                    = 2:1e0,
    SQL_POS_REFRESH                     = 2:1e1,
    SQL_POS_UPDATE                      = 2:1e2,
    SQL_POS_DELETE                      = 2:1e3,
    SQL_POS_ADD                         = 2:1e4,
    ;;; Sql_QUALIFIER_LOCATION
    SQL_QL_START                        = 1,
    SQL_QL_END                          = 2,
    ;;; Sql_OJ_CAPABILITIES
    SQL_OJ_LEFT                         = 2:1e0,
    SQL_OJ_RIGHT                        = 2:1e1,
    SQL_OJ_FULL                         = 2:1e2,
    SQL_OJ_NESTED                       = 2:1e3,
    SQL_OJ_NOT_ORDERED                  = 2:1e4,
    SQL_OJ_INNER                        = 2:1e5,
    SQL_OJ_ALL_COMPARISON_OPS           = 2:1e6,

    ;;; sql_connect_option values (SqlGet/SetConnectOption)
    ;;; Sql_ACCESS_MODE
    SQL_MODE_READ_WRITE             = 0,
    SQL_MODE_READ_ONLY              = 1,
    ;;; Sql_ODBC_CURSORS
    SQL_CUR_USE_IF_NEEDED           = 0,
    SQL_CUR_USE_ODBC                = 1,
    SQL_CUR_USE_DRIVER              = 2,

    ;;; sql_statement_option values (also sql_connect_option)
    ;;; (SQLGetSetStmtOption)
    SQL_MAX_OPTION_STRING_LENGTH    = 256,
    ;;; Sql_CURSOR_TYPE
    SQL_CURSOR_FORWARD_ONLY         = 0,
    SQL_CURSOR_KEYSET_DRIVEN        = 1,
    SQL_CURSOR_DYNAMIC              = 2,
    SQL_CURSOR_STATIC               = 3,
    ;;; Sql_CONCURRENCY
    SQL_CONCUR_READ_ONLY            = 1,
    SQL_CONCUR_LOCK                 = 2,
    SQL_CONCUR_ROWVER               = 3,
    SQL_CONCUR_VALUES               = 4,
    ;;; Sql_SIMULATE_CURSOR
    SQL_SC_NON_UNIQUE               = 0,
    SQL_SC_TRY_UNIQUE               = 1,
    SQL_SC_UNIQUE                   = 2,

    ;;; sql_column_attribute values (SQLColAttributes)
    ;;; Sql_COLUMN_NULLABLE
    SQL_NO_NULLS                    = 0,
    SQL_NULLABLE                    = 1,
    SQL_NULLABLE_UNKNOWN            = 2,
    ;;; Sql_COLUMN_UPDATABLE
    SQL_ATTR_READONLY               = 0,
    SQL_ATTR_WRITE                  = 1,
    SQL_ATTR_READWRITE_UNKNOWN      = 2,
    ;;; Sql_COLUMN_SEARCHABLE
    SQL_UNSEARCHABLE                = 0,
    SQL_LIKE_ONLY                   = 1,
    SQL_ALL_EXCEPT_LIKE             = 2,
    SQL_SEARCHABLE                  = 3,

    ;;; sql_select_foreign_keys (SQLForeignKeys)
    SQL_CASCADE                     = 0,
    SQL_RESTRICT                    = 1,
    SQL_SET_NULL                    = 2,
    SQL_NO_ACTION                   = 3,
    SQL_SET_DEFAULT                 = 4,
);


    /* SQL_ input arguments */

iconstant macro (

    ;;; sql_connect_functions (SQLGetFunctions). (Level 2 numbers only, since
    ;;; Level 1 is assumed.)
    SQL_API_ALL_FUNCTIONS           = 0,
    SQL_API_SQLBROWSECONNECT        = 55,
    SQL_API_SQLCOLUMNPRIVILEGES     = 56,
    SQL_API_SQLDATASOURCES          = 57,
    SQL_API_SQLDESCRIBEPARAM        = 58,
    SQL_API_SQLEXTENDEDFETCH        = 59,
    SQL_API_SQLFOREIGNKEYS          = 60,
    SQL_API_SQLMORERESULTS          = 61,
    SQL_API_SQLNATIVESQL            = 62,
    SQL_API_SQLNUMPARAMS            = 63,
    SQL_API_SQLPARAMOPTIONS         = 64,
    SQL_API_SQLPRIMARYKEYS          = 65,
    SQL_API_SQLPROCEDURECOLUMNS     = 66,
    SQL_API_SQLPROCEDURES           = 67,
    SQL_API_SQLSETPOS               = 68,
    SQL_API_SQLSETSCROLLOPTIONS     = 69,
    SQL_API_SQLTABLEPRIVILEGES      = 70,

    ;;; sql_connect (SQLDriverConnect)
    SQL_DRIVER_NOPROMPT             = 0,
    SQL_DRIVER_COMPLETE             = 1,
    SQL_DRIVER_PROMPT               = 2,
    SQL_DRIVER_COMPLETE_REQUIRED    = 3,

    ;;; sql_bind_parameter parameter types (SqlBindParameter)
    SQL_PARAM_TYPE_UNKNOWN          = 0,
    SQL_PARAM_INPUT                 = 1,
    SQL_PARAM_INPUT_OUTPUT          = 2,
    SQL_RESULT_COL                  = 3,
    SQL_PARAM_OUTPUT                = 4,
    SQL_RETURN_VALUE                = 5,

    ;;; sql_select_columns (SQLSpecialColumns)
    SQL_BEST_ROWID                  = 1,
    SQL_ROWVER                      = 2,
    SQL_SCOPE_CURROW                = 0,
    SQL_SCOPE_TRANSACTION           = 1,
    SQL_SCOPE_SESSION               = 2,
    SQL_PC_UNKNOWN                  = 0,
    SQL_PC_NOT_PSEUDO               = 1,
    SQL_PC_PSEUDO                   = 2,

    ;;; sql_select_statistics (SQLStatistics)
    SQL_INDEX_UNIQUE                = 0,
    SQL_INDEX_ALL                   = 1,
    SQL_QUICK                       = 0,
    SQL_ENSURE                      = 1,
    SQL_TABLE_STAT                  = 0,
    SQL_INDEX_CLUSTERED             = 1,
    SQL_INDEX_HASHED                = 2,
    SQL_INDEX_OTHER                 = 3,

    ;;; sql_extended_fetch (SQLExtendedFetch)
    ;;; FetchType values
    SQL_FETCH_NEXT                  = 1,
    SQL_FETCH_FIRST                 = 2,
    SQL_FETCH_LAST                  = 3,
    SQL_FETCH_PRIOR                 = 4,
    SQL_FETCH_ABSOLUTE              = 5,
    SQL_FETCH_RELATIVE              = 6,
    SQL_FETCH_BOOKMARK              = 8,
    ;;; RowStatus element values
    SQL_ROW_SUCCESS                 = 0,
    SQL_ROW_DELETED                 = 1,
    SQL_ROW_UPDATED                 = 2,
    SQL_ROW_NOROW                   = 3,
    SQL_ROW_ADDED                   = 4,
    SQL_ROW_ERROR                   = 5,

    ;;; ODBC 3.0
    SQL_ROW_SUCCESS_WITH_INFO       = 6,
    SQL_ROW_PROCEED                 = 0,
    SQL_ROW_IGNORE                  = 1,
    ;;; END ODBC 3.0

    ;;; sql_set_position (SQLSetPos)
    ;;; Operation
    SQL_POSITION                    = 0,
    SQL_REFRESH                     = 1,
    SQL_UPDATE                      = 2,
    SQL_DELETE                      = 3,
    SQL_ADD                         = 4,
    ;;; Lock options
    SQL_LOCK_NO_CHANGE              = 0,
    SQL_LOCK_EXCLUSIVE              = 1,
    SQL_LOCK_UNLOCK                 = 2,
);



    /* Sql_ input arguments */

    ;;; Type encoding for attribute & option descriptors
iconstant macro (
    SQL_dt_string   = 1,
    SQL_dt_YNstring = 2,
    SQL_dt_bool     = 3,
    SQL_dt_short    = 4,
    SQL_dt_ushort   = 5,
    SQL_dt_int      = 6,
    SQL_dt_uint     = 7,
    SQL_dt_handle   = 8,
);

define :inline iconstant SQL(Sql_val);
    #_< Sql_val >> 16 >_#
enddefine;

define :inline iconstant _ConsDesc(cval, type);
    (cval << 16) || type
enddefine;

define :inline iconstant _ConsDDesc(cval, dflt, type);
    (cval << 16) || (dflt<<5) || 2:1e4 || type
enddefine;

iconstant macro (

    ;;; sql_connect_attribute descriptors (SQLGetInfo)
    Sql_ACTIVE_CONNECTIONS          = _ConsDesc(0,  SQL_dt_ushort),
    Sql_ACTIVE_STATEMENTS           = _ConsDesc(1,  SQL_dt_ushort),
    Sql_DATA_SOURCE_NAME            = _ConsDesc(2,  SQL_dt_string),
    Sql_DRIVER_HDBC                 = _ConsDesc(3,  SQL_dt_handle),
    Sql_DRIVER_HENV                 = _ConsDesc(4,  SQL_dt_handle),
    Sql_DRIVER_HSTMT                = _ConsDesc(5,  SQL_dt_handle),
    Sql_DRIVER_NAME                 = _ConsDesc(6,  SQL_dt_string),
    Sql_DRIVER_VER                  = _ConsDesc(7,  SQL_dt_string),
    Sql_FETCH_DIRECTION             = _ConsDesc(8,  SQL_dt_uint),
    Sql_ODBC_API_CONFORMANCE        = _ConsDesc(9,  SQL_dt_short),
    Sql_ODBC_VER                    = _ConsDesc(10, SQL_dt_string),
    Sql_ROW_UPDATES                 = _ConsDesc(11, SQL_dt_YNstring),
    Sql_ODBC_SAG_CLI_CONFORMANCE    = _ConsDesc(12, SQL_dt_short),
    Sql_SERVER_NAME                 = _ConsDesc(13, SQL_dt_string),
    Sql_SEARCH_PATTERN_ESCAPE       = _ConsDesc(14, SQL_dt_string),
    Sql_ODBC_SQL_CONFORMANCE        = _ConsDesc(15, SQL_dt_short),
    Sql_DBMS_NAME                   = _ConsDesc(17, SQL_dt_string),
    Sql_DBMS_VER                    = _ConsDesc(18, SQL_dt_string),
    Sql_ACCESSIBLE_TABLES           = _ConsDesc(19, SQL_dt_YNstring),
    Sql_ACCESSIBLE_PROCEDURES       = _ConsDesc(20, SQL_dt_YNstring),
    Sql_PROCEDURES                  = _ConsDesc(21, SQL_dt_YNstring),
    Sql_CONCAT_NULL_BEHAVIOR        = _ConsDesc(22, SQL_dt_short),
    Sql_CURSOR_COMMIT_BEHAVIOR      = _ConsDesc(23, SQL_dt_short),
    Sql_CURSOR_ROLLBACK_BEHAVIOR    = _ConsDesc(24, SQL_dt_short),
    Sql_DATA_SOURCE_READ_ONLY       = _ConsDesc(25, SQL_dt_YNstring),
    Sql_DEFAULT_TXN_ISOLATION       = _ConsDesc(26, SQL_dt_uint),
    Sql_EXPRESSIONS_IN_ORDERBY      = _ConsDesc(27, SQL_dt_YNstring),
    Sql_IDENTIFIER_CASE             = _ConsDesc(28, SQL_dt_short),
    Sql_IDENTIFIER_QUOTE_CHAR       = _ConsDesc(29, SQL_dt_string),
    Sql_MAX_COLUMN_NAME_LEN         = _ConsDesc(30, SQL_dt_ushort),
    Sql_MAX_CURSOR_NAME_LEN         = _ConsDesc(31, SQL_dt_ushort),
    Sql_MAX_OWNER_NAME_LEN          = _ConsDesc(32, SQL_dt_ushort),
    Sql_MAX_PROCEDURE_NAME_LEN      = _ConsDesc(33, SQL_dt_ushort),
    Sql_MAX_QUALIFIER_NAME_LEN      = _ConsDesc(34, SQL_dt_ushort),
    Sql_MAX_TABLE_NAME_LEN          = _ConsDesc(35, SQL_dt_ushort),
    Sql_MULT_RESULT_SETS            = _ConsDesc(36, SQL_dt_YNstring),
    Sql_MULTIPLE_ACTIVE_TXN         = _ConsDesc(37, SQL_dt_YNstring),
    Sql_OUTER_JOINS                 = _ConsDesc(38, SQL_dt_YNstring),
    Sql_OWNER_TERM                  = _ConsDesc(39, SQL_dt_string),
    Sql_PROCEDURE_TERM              = _ConsDesc(40, SQL_dt_string),
    Sql_QUALIFIER_NAME_SEPARATOR    = _ConsDesc(41, SQL_dt_string),
    Sql_QUALIFIER_TERM              = _ConsDesc(42, SQL_dt_string),
    Sql_SCROLL_CONCURRENCY          = _ConsDesc(43, SQL_dt_uint),
    Sql_SCROLL_OPTIONS              = _ConsDesc(44, SQL_dt_uint),
    Sql_TABLE_TERM                  = _ConsDesc(45, SQL_dt_string),
    Sql_TXN_CAPABLE                 = _ConsDesc(46, SQL_dt_short),
    Sql_USER_NAME                   = _ConsDesc(47, SQL_dt_string),
    Sql_CONVERT_FUNCTIONS           = _ConsDesc(48, SQL_dt_uint),
    Sql_NUMERIC_FUNCTIONS           = _ConsDesc(49, SQL_dt_uint),
    Sql_STRING_FUNCTIONS            = _ConsDesc(50, SQL_dt_uint),
    Sql_SYSTEM_FUNCTIONS            = _ConsDesc(51, SQL_dt_uint),
    Sql_TIMEDATE_FUNCTIONS          = _ConsDesc(52, SQL_dt_uint),
    Sql_CONVERT_BIGINT              = _ConsDesc(53, SQL_dt_uint),
    Sql_CONVERT_BINARY              = _ConsDesc(54, SQL_dt_uint),
    Sql_CONVERT_BIT                 = _ConsDesc(55, SQL_dt_uint),
    Sql_CONVERT_CHAR                = _ConsDesc(56, SQL_dt_uint),
    Sql_CONVERT_DATE                = _ConsDesc(57, SQL_dt_uint),
    Sql_CONVERT_DECIMAL             = _ConsDesc(58, SQL_dt_uint),
    Sql_CONVERT_DOUBLE              = _ConsDesc(59, SQL_dt_uint),
    Sql_CONVERT_FLOAT               = _ConsDesc(60, SQL_dt_uint),
    Sql_CONVERT_INTEGER             = _ConsDesc(61, SQL_dt_uint),
    Sql_CONVERT_LONGVARCHAR         = _ConsDesc(62, SQL_dt_uint),
    Sql_CONVERT_NUMERIC             = _ConsDesc(63, SQL_dt_uint),
    Sql_CONVERT_REAL                = _ConsDesc(64, SQL_dt_uint),
    Sql_CONVERT_SMALLINT            = _ConsDesc(65, SQL_dt_uint),
    Sql_CONVERT_TIME                = _ConsDesc(66, SQL_dt_uint),
    Sql_CONVERT_TIMESTAMP           = _ConsDesc(67, SQL_dt_uint),
    Sql_CONVERT_TINYINT             = _ConsDesc(68, SQL_dt_uint),
    Sql_CONVERT_VARBINARY           = _ConsDesc(69, SQL_dt_uint),
    Sql_CONVERT_VARCHAR             = _ConsDesc(70, SQL_dt_uint),
    Sql_CONVERT_LONGVARBINARY       = _ConsDesc(71, SQL_dt_uint),
    Sql_TXN_ISOLATION_OPTION        = _ConsDesc(72, SQL_dt_uint),
    Sql_ODBC_SQL_OPT_IEF            = _ConsDesc(73, SQL_dt_YNstring),
    Sql_CORRELATION_NAME            = _ConsDesc(74, SQL_dt_short),
    Sql_NON_NULLABLE_COLUMNS        = _ConsDesc(75, SQL_dt_bool),
    Sql_DRIVER_HLIB                 = _ConsDesc(76, SQL_dt_handle),
    Sql_DRIVER_ODBC_VER             = _ConsDesc(77, SQL_dt_string),
    Sql_LOCK_TYPES                  = _ConsDesc(78, SQL_dt_uint),
    Sql_POS_OPERATIONS              = _ConsDesc(79, SQL_dt_uint),
    Sql_POSITIONED_STATEMENTS       = _ConsDesc(80, SQL_dt_uint),
    Sql_GETDATA_EXTENSIONS          = _ConsDesc(81, SQL_dt_uint),
    Sql_BOOKMARK_PERSISTENCE        = _ConsDesc(82, SQL_dt_uint),
    Sql_STATIC_SENSITIVITY          = _ConsDesc(83, SQL_dt_uint),
    Sql_FILE_USAGE                  = _ConsDesc(84, SQL_dt_short),
    Sql_NULL_COLLATION              = _ConsDesc(85, SQL_dt_short),
    Sql_ALTER_TABLE                 = _ConsDesc(86, SQL_dt_uint),
    Sql_COLUMN_ALIAS                = _ConsDesc(87, SQL_dt_YNstring),
    Sql_GROUP_BY                    = _ConsDesc(88, SQL_dt_short),
    Sql_KEYWORDS                    = _ConsDesc(89, SQL_dt_string),
    Sql_ORDER_BY_COLUMNS_IN_SELECT  = _ConsDesc(90, SQL_dt_YNstring),
    Sql_OWNER_USAGE                 = _ConsDesc(91, SQL_dt_uint),
    Sql_QUALIFIER_USAGE             = _ConsDesc(92, SQL_dt_uint),
    Sql_QUOTED_IDENTIFIER_CASE      = _ConsDesc(93, SQL_dt_short),
    Sql_SPECIAL_CHARACTERS          = _ConsDesc(94, SQL_dt_string),
    Sql_SUBQUERIES                  = _ConsDesc(95, SQL_dt_uint),
    Sql_UNION                       = _ConsDesc(96, SQL_dt_uint),
    Sql_MAX_COLUMNS_IN_GROUP_BY     = _ConsDesc(97, SQL_dt_ushort),
    Sql_MAX_COLUMNS_IN_INDEX        = _ConsDesc(98, SQL_dt_ushort),
    Sql_MAX_COLUMNS_IN_ORDER_BY     = _ConsDesc(99, SQL_dt_ushort),
    Sql_MAX_COLUMNS_IN_SELECT       = _ConsDesc(100,SQL_dt_ushort),
    Sql_MAX_COLUMNS_IN_TABLE        = _ConsDesc(101,SQL_dt_ushort),
    Sql_MAX_INDEX_SIZE              = _ConsDesc(102,SQL_dt_uint),
    Sql_MAX_ROW_SIZE_INCLUDES_LONG  = _ConsDesc(103,SQL_dt_YNstring),
    Sql_MAX_ROW_SIZE                = _ConsDesc(104,SQL_dt_uint),
    Sql_MAX_STATEMENT_LEN           = _ConsDesc(105,SQL_dt_uint),
    Sql_MAX_TABLES_IN_SELECT        = _ConsDesc(106,SQL_dt_ushort),
    Sql_MAX_USER_NAME_LEN           = _ConsDesc(107,SQL_dt_ushort),
    Sql_MAX_CHAR_LITERAL_LEN        = _ConsDesc(108,SQL_dt_uint),
    Sql_TIMEDATE_ADD_INTERVALS      = _ConsDesc(109,SQL_dt_uint),
    Sql_TIMEDATE_DIFF_INTERVALS     = _ConsDesc(110,SQL_dt_uint),
    Sql_NEED_LONG_DATA_LEN          = _ConsDesc(111,SQL_dt_YNstring),
    Sql_MAX_BINARY_LITERAL_LEN      = _ConsDesc(112,SQL_dt_uint),
    Sql_LIKE_ESCAPE_CLAUSE          = _ConsDesc(113,SQL_dt_YNstring),
    Sql_QUALIFIER_LOCATION          = _ConsDesc(114,SQL_dt_short),
    Sql_OJ_CAPABILITIES             = _ConsDesc(65003,SQL_dt_uint), ;;; temp

    ;;; ODBC 3.0
    Sql_CATALOG_LOCATION                    = Sql_QUALIFIER_LOCATION,
    Sql_CATALOG_NAME_SEPARATOR              = Sql_QUALIFIER_NAME_SEPARATOR,
    Sql_CATALOG_TERM                        = Sql_QUALIFIER_TERM,
    Sql_CATALOG_USAGE                       = Sql_QUALIFIER_USAGE,
    Sql_CONVERT_WCHAR                       = _ConsDesc(122, SQL_dt_uint),
    Sql_CONVERT_INTERVAL_DAY_TIME           = _ConsDesc(123, SQL_dt_uint),
    Sql_CONVERT_INTERVAL_YEAR_MONTH         = _ConsDesc(124, SQL_dt_uint),
    Sql_CONVERT_WLONGVARCHAR                = _ConsDesc(125, SQL_dt_uint),
    Sql_CONVERT_WVARCHAR                    = _ConsDesc(126, SQL_dt_uint),
    Sql_SCHEMA_TERM                         = Sql_OWNER_TERM,
    Sql_SCHEMA_USAGE                        = Sql_OWNER_USAGE,
    Sql_UNION_STATEMENT                     = Sql_UNION,
    ;;; END ODBC 3.0

    ;;; sql_connect_option descriptors (SQLGet/SetConnectOption)
    Sql_ACCESS_MODE             = _ConsDDesc(101, SQL_MODE_READ_WRITE, SQL_dt_int),
    Sql_AUTOCOMMIT              = _ConsDDesc(102, 1, SQL_dt_bool),
    Sql_LOGIN_TIMEOUT           = _ConsDDesc(103, 15, SQL_dt_uint),
    Sql_OPT_TRACE               = _ConsDDesc(104, 0, SQL_dt_bool),
    Sql_OPT_TRACEFILE           =  _ConsDesc(105,    SQL_dt_string),
    Sql_TRANSLATE_DLL           =  _ConsDesc(106,    SQL_dt_string),
    Sql_TRANSLATE_OPTION        =  _ConsDesc(107,    SQL_dt_uint),
    Sql_TXN_ISOLATION           =  _ConsDesc(108,    SQL_dt_uint),
    Sql_CURRENT_QUALIFIER       =  _ConsDesc(109,    SQL_dt_string),
    Sql_ODBC_CURSORS            = _ConsDDesc(110, SQL_CUR_USE_DRIVER, SQL_dt_int),
    Sql_QUIET_MODE              =  _ConsDesc(111,    SQL_dt_handle),
    Sql_PACKET_SIZE             =  _ConsDesc(112,    SQL_dt_uint),

    ;;; sql_statement_option (also sql_connect_option) descriptors
    ;;; (SQLGet/SetStmtOption)
    Sql_QUERY_TIMEOUT           = _ConsDDesc(0, 0,  SQL_dt_int),
    Sql_MAX_ROWS                = _ConsDDesc(1, 0,  SQL_dt_int),
    Sql_NOSCAN                  = _ConsDDesc(2, 0,  SQL_dt_bool),
    Sql_MAX_LENGTH              = _ConsDDesc(3, 0,  SQL_dt_int),
    Sql_ASYNC_ENABLE            = _ConsDDesc(4, 0,  SQL_dt_bool),
    Sql_BIND_TYPE               = _ConsDDesc(5, 0,  SQL_dt_int),
    Sql_CURSOR_TYPE             = _ConsDDesc(6, SQL_CURSOR_FORWARD_ONLY, SQL_dt_int),
    Sql_CONCURRENCY             = _ConsDDesc(7, SQL_CONCUR_READ_ONLY, SQL_dt_int),
    Sql_KEYSET_SIZE             = _ConsDDesc(8, 0,  SQL_dt_int),
    Sql_ROWSET_SIZE             = _ConsDDesc(9, 1,  SQL_dt_int),
    Sql_SIMULATE_CURSOR         =  _ConsDesc(10,    SQL_dt_int),
    Sql_RETRIEVE_DATA           = _ConsDDesc(11, 1, SQL_dt_bool),
    Sql_USE_BOOKMARKS           = _ConsDDesc(12, 0, SQL_dt_bool),
    Sql_GET_BOOKMARK            =  _ConsDesc(13,    SQL_dt_uint),   ;;; Get only
    Sql_ROW_NUMBER              =  _ConsDesc(14,    SQL_dt_int),    ;;; Get only
    ;;; special for pop
    Sql_LONG_DATA_MAX           = _ConsDDesc(-1, 16:7FF, SQL_dt_uint),

    ;;; sql_column_attribute descriptors (SQLColAttributes)
    Sql_COLUMN_COUNT            = _ConsDesc(0,  SQL_dt_uint),
    Sql_COLUMN_NAME             = _ConsDesc(1,  SQL_dt_string),
    Sql_COLUMN_TYPE             = _ConsDesc(2,  SQL_dt_int),
    Sql_COLUMN_LENGTH           = _ConsDesc(3,  SQL_dt_uint),
    Sql_COLUMN_PRECISION        = _ConsDesc(4,  SQL_dt_uint),
    Sql_COLUMN_SCALE            = _ConsDesc(5,  SQL_dt_uint),
    Sql_COLUMN_DISPLAY_SIZE     = _ConsDesc(6,  SQL_dt_uint),
    Sql_COLUMN_NULLABLE         = _ConsDesc(7,  SQL_dt_int),
    Sql_COLUMN_UNSIGNED         = _ConsDesc(8,  SQL_dt_bool),
    Sql_COLUMN_MONEY            = _ConsDesc(9,  SQL_dt_bool),
    Sql_COLUMN_UPDATABLE        = _ConsDesc(10, SQL_dt_int),
    Sql_COLUMN_AUTO_INCREMENT   = _ConsDesc(11, SQL_dt_bool),
    Sql_COLUMN_CASE_SENSITIVE   = _ConsDesc(12, SQL_dt_bool),
    Sql_COLUMN_SEARCHABLE       = _ConsDesc(13, SQL_dt_int),
    Sql_COLUMN_TYPE_NAME        = _ConsDesc(14, SQL_dt_string),
    Sql_COLUMN_TABLE_NAME       = _ConsDesc(15, SQL_dt_string),
    Sql_COLUMN_OWNER_NAME       = _ConsDesc(16, SQL_dt_string),
    Sql_COLUMN_QUALIFIER_NAME   = _ConsDesc(17, SQL_dt_string),
    Sql_COLUMN_LABEL            = _ConsDesc(18, SQL_dt_string),
);



    ;;; Pop SQL datatypes

define :inline iconstant _ConsSQL(stype, precision, scale);
    (stype << 16) || precision || (scale << 8)
enddefine;

iconstant macro (
    Sql_BIT             = _ConsSQL(SQL_BIT,         1,  0),
    Sql_TINYINT         = _ConsSQL(SQL_TINYINT,     3,  0),
    Sql_BIGINT          = _ConsSQL(SQL_BIGINT,      19, 0),
    Sql_LONGVARBINARY   = _ConsSQL(SQL_LONGVARBINARY,0,  0),
    Sql_LONGVARCHAR     = _ConsSQL(SQL_LONGVARCHAR, 0,  0),
    Sql_INTEGER         = _ConsSQL(SQL_INTEGER,     10, 0),
    Sql_SMALLINT        = _ConsSQL(SQL_SMALLINT,    5,  0),
    Sql_FLOAT           = _ConsSQL(SQL_FLOAT,       15, 0),
    Sql_REAL            = _ConsSQL(SQL_REAL,        7,  0),
    Sql_DOUBLE          = _ConsSQL(SQL_DOUBLE,      15, 0),
    Sql_DATE            = _ConsSQL(SQL_DATE,        10, 0),
    Sql_TIME            = _ConsSQL(SQL_TIME,        8,  0),
    Sql_TIMESTAMP       = _ConsSQL(SQL_TIMESTAMP,   23, 3),
    ;;; special for internal use
    Sql_BOOKMARK        = _ConsSQL(999,             10, 0),

    ;;; unsigned versions of int types (bit 15 set)
    Sql_UTINYINT        = Sql_TINYINT  || 2:1e15,
    Sql_USMALLINT       = Sql_SMALLINT || 2:1e15,
    Sql_UINTEGER        = Sql_INTEGER  || 2:1e15,
    Sql_UBIGINT         = _ConsSQL(SQL_BIGINT, 20, 0) || 2:1e15,

    Sql_ALL_TYPES       = _ConsSQL(SQL_ALL_TYPES,  0,  0),

    ;;; UNICODE TYPES
    Sql_WLONGVARCHAR    = _ConsSQL(SQL_WLONGVARCHAR, 0,  0),
    ;;; END UNICODE TYPES
);

define :inline iconstant Sql_VARBINARY(n);
    #_< (SQL_VARBINARY<<16) || fi_check(n,1,255) >_#
enddefine;
define :inline iconstant Sql_BINARY(n);
    #_< (SQL_BINARY<<16) || fi_check(n,1,255) >_#
enddefine;
define :inline iconstant Sql_CHAR(n);
    #_< (SQL_CHAR<<16) || fi_check(n,1,254) >_#
enddefine;
define :inline iconstant Sql_NUMERIC(p,s);
    #_< (SQL_NUMERIC<<16) || fi_check(p,1,15) || (fi_check(s,0,p)<<8) >_#
enddefine;
define :inline iconstant Sql_DECIMAL(p,s);
    #_< (SQL_DECIMAL<<16) || fi_check(p,1,15) || (fi_check(s,0,p)<<8) >_#
enddefine;
define :inline iconstant Sql_VARCHAR(n);
    #_< (SQL_VARCHAR<<16) || fi_check(n,1,254) >_#
enddefine;

;;; UNICODE TYPES
define :inline iconstant Sql_WCHAR(n);
    #_< (SQL_WCHAR<<16) || fi_check(n,1,254) >_#
enddefine;
define :inline iconstant Sql_WVARCHAR(n);
    #_< (SQL_WVARCHAR<<16) || fi_check(n,1,254) >_#
enddefine;
;;; END UNICODE TYPES


i_typespec
    sql_date        {year :short, month :ushort, day :ushort},
    sql_time        {hour :ushort, minute :ushort, second :ushort},
    sql_timestamp   {year :short, month :ushort, day :ushort,
                        hour :ushort, minute :ushort, second :ushort,
                        fraction :uint},
    ;


    ;;; Pop datatypes

define :inline iconstant _ConsPop(ptype, tspec=typespec, ctype);
    (ctype << 16) || (SIZEOFTYPE(tspec) << 5) || ptype
enddefine;

iconstant macro (
    Sql_P_boolean       = _ConsPop(1,   :byte,      SQL_C_BIT),
    Sql_P_bit           = _ConsPop(2,   :byte,      SQL_C_BIT),
    Sql_P_sbyte         = _ConsPop(3,   :sbyte,     SQL_C_STINYINT),
    Sql_P_byte          = _ConsPop(4,   :byte,      SQL_C_UTINYINT),
    Sql_P_short         = _ConsPop(5,   :short,     SQL_C_SSHORT),
    Sql_P_ushort        = _ConsPop(6,   :ushort,    SQL_C_USHORT),
    Sql_P_int           = _ConsPop(7,   :int,       SQL_C_SLONG),
    Sql_P_uint          = _ConsPop(8,   :uint,      SQL_C_ULONG),
    Sql_P_longlong      = _ConsPop(9,   :byte[21],  SQL_C_CHAR),
    Sql_P_ulonglong     = _ConsPop(10,  :byte[21],  SQL_C_CHAR),
    Sql_P_sfloat        = _ConsPop(11,  :sfloat,    SQL_C_FLOAT),
    Sql_P_dfloat        = _ConsPop(12,  :dfloat,    SQL_C_DOUBLE),
    Sql_P_date          = _ConsPop(13,  :sql_date,  SQL_C_DATE),
    Sql_P_time          = _ConsPop(14,  :sql_time,  SQL_C_TIME),
    Sql_P_timestamp     = _ConsPop(15,  :sql_timestamp, SQL_C_TIMESTAMP),
    Sql_P_bookmark      = _ConsPop(16,  :uint,      SQL_C_BOOKMARK),
    Sql_P_string        = _ConsPop(17,  :byte[255], SQL_C_CHAR),
    Sql_P_binary        = _ConsPop(18,  :byte[255], SQL_C_BINARY),

    ;;; UNICODE TYPES
    Sql_P_wstring       = _ConsPop(19,  :ushort[255], SQL_C_WCHAR),
    ;;; END UNICODE TYPES
);

iconstant ODBC_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- Julian Clinton, Feb 19 1999
        Added some ODBC 3.0/5 and UNICODE symbols.
 */
