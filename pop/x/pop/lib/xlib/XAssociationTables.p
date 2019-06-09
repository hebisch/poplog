/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XAssociationTables.p
 > Purpose:
 > Author:          Gareth Palmer,  4 July 1989 (see revisions)
 > Documentation:   REF *XAssociationTables
 > Related Files:
 */

compile_mode: pop11 +strict;
section;

include xdefs.ph;
#_IF not(DEF XHASOLDX)
mishap(0, 'XAssociationTables: liboldX.a library not available');
#_ENDIF

uses XConstants;

external declare XAssociationTables in c;
    (external_import_procedure XptImportProcedure)

    /*
     * XAssoc - Associations used in the XAssocTable data structure.  The
     * associations are used as circular queue entries in the association table
     * which is contains an array of circular queues (buckets).
     */
    typedef struct _XAssoc {
        struct _XAssoc *next;   ;;; Next object in this bucket. */
        struct _XAssoc *prev;   ;;; Previous obejct in this bucket. */
        Display *display;   ;;; Display which ownes the id. */
        XID x_id;       ;;; X Window System id. */
        char *data;     ;;; Pointer to untyped memory. */
    } XAssoc;

    /*
     * XAssocTable - X Window System id to data structure pointer association
     * table.  An XAssocTable is a hash table whose buckets are circular
     * queues of XAssoc's.  The XAssocTable is constructed from an array of
     * XAssoc's which are the circular queue headers (bucket headers).
     * An XAssocTable consists an XAssoc pointer that points to the first
     * bucket in the bucket array and an integer that indicates the number
     * of buckets in the array.
     */
    typedef struct {
        XAssoc *buckets;        ;;; Pointer to first bucket in bucket array.*/
        int size;           ;;; Table size (number of buckets). */
    } XAssocTable;


XAssocTable *XCreateAssocTable(size)
int size;
{}

void XDeleteAssoc(display, table, x_id)
Display *display;
XAssocTable *table;
XID x_id;
{}

void XDestroyAssocTable(table)
XAssocTable *table;
{}

caddr_t XLookUpAssoc(display, table, x_id)
Display *display;
XAssocTable *table;
XID x_id;
{}

void XMakeAssoc(display, table, x_id, data)
Display *display;
XAssocTable *table;
XID x_id;
caddr_t data;
{}

endexternal;

xlib_external_require XAssociationTables;

global vars XAssociationTables = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun  7 1993
        Sectioned and made +strict
--- Simon Nichols, Oct 29 1991
        Changed test from #_IF not(XHASOLDX) to #_IF not(DEF XHASOLDX) as
        XHASOLDX may not be defined. See bugreport ianr.27.
--- Jonathan Meyer, Jan 25 1991
        Changed to use xlib_external_require;
        Added guard for XHASOLDX
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
