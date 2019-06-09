/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 * File:        C.x/x/Xpw/Assoc.h
 * Purpose:     C Hashed Association tables - header file.
 * Author:      Jonathan Meyer, Jan 1990 (see revisions)
 * Documentation:
 * Related Files:
 */


#ifndef NULL
#define NULL    0   /* Needed for VMS */
#define FALSE   0
#define TRUE    1
#endif

/* provide definition for caddr_t */
#ifdef VMS          /* This VMS code taken from DECW$INCLUDE:XOS.H */
#include <types.h>
#if !defined(CADDR_T) && !defined(__alpha)
typedef char *caddr_t;
#define CADDR_T
#endif /* CADDR_T */
#else
#include <sys/types.h>
#endif /* VMS */


typedef unsigned long XpwAssocID;

typedef struct _XpwAssoc {
    struct _XpwAssoc *next; /* Next object in this bucket. */
    struct _XpwAssoc *prev; /* Previous obejct in this bucket. */
    XpwAssocID id;          /* ID for entry in table */
    char *data;                 /* Pointer to untyped memory. */
} XpwAssoc;

typedef struct {
    XpwAssoc *buckets;/* Pointer to first bucket in bucket array.*/
    int size;           /* Table size (number of buckets). */
} XpwAssocTable;

extern XpwAssocTable *XpwCreateAssocTable();
extern void XpwMakeAssoc();
caddr_t XpwLookupAssoc();

/* not implemented:
extern void XpwDestAssocTable();
extern void XpwDeleteAssoc();
*/


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jun  7 1993
        Removed dubious SYS*V stuff
--- John Gibson, Oct 31 1991
    VMS mods
--- Robert John Duncan, Jun 24 1991
        Added sgi (SG IRIX) to list of non-System V systems.
--- Jonathan Meyer, Aug 23 1990
    Checked caddr_t defined, and #included <sys/types.h> if it isn't.
    Replaced XID type with "unsigned long".
--- Jonathan Meyer, Aug 21 1990 Renamed file Assoc.h
--- Andreas Schoter, Jul 20 1990 changed all Pop* to Xpw*
--- Simon Nichols, Jun 19 1990
        Added hpux to list of systems which aren't System V but define
        L_ctermid.
--- Simon Nichols, Jun 18 1990
        Added SYSTYPE_BSD (MIPS RISC/os in BSD compatibility mode) to list
        of systems which aren't System V but define L_ctermid.
--- Simon Nichols, Jun  4 1990
        Added Ultrix to list of systems which aren't System V but define
        L_ctermid.
 */
