/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 * File:        C.x/x/Xpw/Assoc.c
 * Version:     Revision 5.0
 * Purpose:         Hash Association tables in C.
 * Author:      Jonathan Meyer 12 Jan 1989 (see revisions)
 */

/* XpwCreateAssocTable(), XpwMakeAssoc(), XpwLookupAssoc() - C hash tables.

   This is based directly on the MIT XCreateAssoc, XMakeAssoc and
   XLookupAssoc functions, only it does not use XID's or display information.
   Some copyright notice from MIT should probably go here, but hash tables
   are hardly a patented technique!

   Synopsis:

   #include <X11/Xpw/Assoc.h>

   XpwAssocTable *table;
   table = XpwCreateAssocTable(size);
   int size;

   XpwMakeAssoc(table, key, value)
   XpwAssocTable *table;
   XpwAssocID key;
   caddr_t value;

   caddr_t XpwLookupAssoc(table, key)
   XpwAssocTable *table;
   XpwAssocId key;

   Used for many things in Xpw -method lookup, caching, resource conversion...

*/

#include <X11/Intrinsic.h>
#include "Assoc.h"


XpwAssocTable *XpwCreateAssocTable(size)
    register int size;      /* Desired size of the table. */
{
    register XpwAssocTable *table;  /* XpwAssocTable to be initialized. */
    register XpwAssoc *buckets; /* pointer to the first bucket in */
                                    /* the bucket array. */

    table = (XpwAssocTable *) XtMalloc(sizeof(XpwAssocTable));

    /* XtMalloc the buckets (actually just their headers). */
    buckets = (XpwAssoc *)XtCalloc((unsigned)size,
                        (unsigned)sizeof(XpwAssoc));
    /* Insert table data into the XpwAssocTable structure. */
    table->buckets = buckets;
    table->size = size;

    while (--size >= 0) {
        /* Initialize each bucket. */
        buckets->prev = buckets;
        buckets->next = buckets;
        buckets++;
    }

    return(table);
}

static void
insert(elem, pred)
XpwAssoc *elem, *pred;
{   XpwAssoc *succ = pred->next;
    pred->next = elem;
    elem->prev = pred;
    elem->next = succ;
    if (succ) succ->prev = elem;
}

void XpwMakeAssoc(table, id, data)
    register XpwAssocTable *table;
    register XpwAssocID id;
    register caddr_t data;
{
    int hash;
    register XpwAssoc *bucket;
    register XpwAssoc *Entry;
    register XpwAssoc *new_entry;

    /* Hash the Id to get the bucket number. */
    hash = id % table->size;
    /* Look up the bucket to get the entries in that bucket. */
    bucket = &table->buckets[hash];
    /* Get the first entry in the bucket. */
    Entry = bucket->next;

    if (Entry != bucket) {
        /* The bucket isn't empty, begin searching. */
        /* If we leave the for loop then we have either passed */
        /* where the entry should be or hit the end of the bucket. */
        /* In either case we should then insert the new entry */
        /* before the current value of "Entry". */
        for (; Entry != bucket; Entry = Entry->next) {
            if (Entry->id == id) {
                /* Entry has the same Id... */
                Entry->data = data;
                return;
            }
            /* If the current entry's Id is greater than the */
            /* Id of the entry to be inserted then we have */
            /* passed the location where the new Id should */
            /* be inserted. */
            if (Entry->id > id) break;
        }
        }

    /* If we are here then the new entry should be inserted just */
    /* before the current value of "Entry". */
    /* Create a new XpwAssoc and load it with new provided data. */

    new_entry = (XpwAssoc *)XtMalloc(sizeof(XpwAssoc));
    new_entry->id = id;
    new_entry->data = data;

    /* Insert the new entry. */
    insert(new_entry, Entry->prev);
}

caddr_t XpwLookupAssoc(table, id)
    register XpwAssocTable *table;  /* XpwAssocTable to search in. */
    register XpwAssocID id;         /* XId to search for. */
{
    int hash;
    register XpwAssoc *bucket;
    register XpwAssoc *Entry;

    /* Hash the Id to get the bucket number. */
    hash = id % table->size;
    /* Look up the bucket to get the entries in that bucket. */
    bucket = &table->buckets[hash];
    /* Get the first entry in the bucket. */
    Entry = bucket->next;

    /* Scan through the entries in the bucket for the right Id. */
    for (; Entry != bucket; Entry = Entry->next) {
        if (Entry->id == id) {
            /* We have the right Id. */
            return(Entry->data);
        }
        if (Entry->id > id) {
            /* We have gone past where it should be. */
            /* It is apparently not in the table. */
            return(NULL);
        }
    }
    /* It is apparently not in the table. */
    return(NULL);
}

/* copy one XpwAssocTable's entries into anothers */

void XpwCopyAssoc(src_table, dest_table)
    register XpwAssocTable *src_table, *dest_table;
{
    int s = src_table->size;
    register XpwAssoc *buckets = src_table->buckets;
    register XpwAssoc *Entry;

    /* must use XpwMakeAssoc since dest_table may have Assoc's in it */

    while (--s >=0  )
        for (Entry=buckets[s].next; Entry != &buckets[s]; Entry = Entry->next)
            XpwMakeAssoc(dest_table, Entry->id, (caddr_t)Entry->data);
}


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jun  7 1993
        Removed dubious SYS*V stuff
--- John Gibson, Nov  3 1992
        Extended #if for defining insque to include hpux
--- Adrian Howard, Aug 18 1992 (c/o JonM)
        Fixed hash function.
--- John Gibson, Oct 30 1991
        Added VMS definition for insque
--- Jonathan Meyer, Aug 23 1990
    Removed all #include's except "Assoc.h" - they were redundant.
--- Jonathan Meyer, Aug 21 1990
    Renamed Assoc.c, Added "X11/" prefix again.
    Added XpwNotInstalled define.
--- Andreas Schoter, Jul 23 1990 Changed all pop* to xpw*
--- Andreas Schoter, Jul 20 1990 changed all Pop* to Xpw*
--- Simon Nichols, Jun 19 1990
        Removed "X11/" prefix from #include file names.
 */
