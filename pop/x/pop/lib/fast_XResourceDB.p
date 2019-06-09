/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/fast_XResourceDB.p
 > Purpose:         Resource Database Management
 > Author:          Adrian Howard, Jul  8 1993
 > Documentation:   REF * XResourceDB
 > Related Files:   LIB * XResourceDB
 */

compile_mode: pop11 +strict;
section;
exload_batch;

include xpt_xtypes;
include xpt_coretypes;

XptLoadProcedures fast_XResourceDB
    lvars
        XrmDestroyDatabase(database),
        XrmGetFileDatabase(filename) :XptXrmDatabase,
        XrmGetResource(database, str_n, str_c, str_t, value) :XptBoolean,
        XrmGetStringDatabase(data) :XptXrmDatabase,
        XrmInitialize(),
        XrmMergeDatabases(source, target),
        XrmParseCommand(db, table, count, name, argc, argv),
        XrmPutFileDatabase(db, stored),
        XrmPutLineResource(database, line),
        XrmPutResource(db, specifier, type, value),
        XrmPutStringResource(db, resource, value),
        XrmQGetResource(db, qname, qclass, qtype, value) :XptBoolean,
        XrmQGetSearchList(db, names, classes, list, len),
        XrmQGetSearchResource(list, name, class, type, value) :XptBoolean,
        XrmQPutResource(db, bindings, quarks, type, value),
        XrmQPutStringResource(db, bindings, quarks, value),
        XrmQuarkToString(quark) :XptString,
        XrmStringToBindingQuarkList(string, bindings, quarks),
        XrmStringToQuark(string) :XptXrmQuark,
        XrmStringToQuarkList(string, quarks),
        XrmUniqueQuark() :XptXrmQuark,
;


define fast_XrmDestroyDatabase() with_nargs 1;
    exacc raw_XrmDestroyDatabase();
enddefine;

define fast_XrmGetFileDatabase() with_nargs 1;
    exacc raw_XrmGetFileDatabase();
enddefine;

define fast_XrmGetResource() with_nargs 5;
    exacc raw_XrmGetResource();
enddefine;

define fast_XrmGetStringDatabase() with_nargs 1;
    exacc raw_XrmGetStringDatabase();
enddefine;

define fast_XrmInitialize();
    exacc raw_XrmInitialize();
enddefine;

define fast_XrmMergeDatabases() with_nargs 2;
    exacc raw_XrmMergeDatabases();
enddefine;

define fast_XrmParseCommand() with_nargs 6;
    exacc raw_XrmParseCommand();
enddefine;

define fast_XrmPutFileDatabase() with_nargs 2;
    exacc raw_XrmPutFileDatabase();
enddefine;

define fast_XrmPutLineResource() with_nargs 2;
    exacc raw_XrmPutLineResource();
enddefine;

define fast_XrmPutResource() with_nargs 4;
    exacc raw_XrmPutResource();
enddefine;

define fast_XrmPutStringResource() with_nargs 3;
    exacc raw_XrmPutStringResource();
enddefine;

define fast_XrmQGetResource() with_nargs 5;
    exacc raw_XrmQGetResource();
enddefine;

define fast_XrmQGetSearchList() with_nargs 5;
    exacc raw_XrmQGetSearchList();
enddefine;

define fast_XrmQGetSearchResource() with_nargs 5;
    exacc raw_XrmQGetSearchResource();
enddefine;

define fast_XrmQPutResource() with_nargs 5;
    exacc raw_XrmQPutResource();
enddefine;

define fast_XrmQPutStringResource() with_nargs 4;
    exacc raw_XrmQPutStringResource();
enddefine;

define fast_XrmQuarkToString() with_nargs 1;
    exacc raw_XrmQuarkToString();
enddefine;

define fast_XrmStringToBindingQuarkList() with_nargs 3;
    exacc raw_XrmStringToBindingQuarkList();
enddefine;

define fast_XrmStringToQuark() with_nargs 1;
    exacc raw_XrmStringToQuark();
enddefine;

define fast_XrmStringToQuarkList() with_nargs 2;
    exacc raw_XrmStringToQuarkList();
enddefine;

define fast_XrmUniqueQuark();
    exacc raw_XrmUniqueQuark();
enddefine;

constant fast_XResourceDB = true;

endexload_batch;
endsection;
