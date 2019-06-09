/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XHostAccess.p
 > Purpose:
 > Author:          Gareth Palmer,  4 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;

global constant macro (

/* for ChangeHosts */

 HostInsert      = 0,
 HostDelete      = 1,

/* for ChangeAccessControl */

 EnableAccess        = 1,
 DisableAccess       = 0,

);


external declare XHostAccess in c;
    (external_import_procedure XptImportProcedure)



    /*
     * Data structure for host setting; getting routines.
     *
     */

    typedef struct {
        int family;     ;;; for example AF_DNET */
        int length;     ;;; length of address, in bytes */
        char *address;      ;;; pointer to where to find the bytes */
    } XHostAddress;





void XAddHost(display, host)
Display *display;
XHostAddress *host;
{}

void XAddHosts(display, hosts, num_hosts)
Display *display;
XHostAddress *hosts;
int num_hosts;
{}

XHostAddress *XListHosts(display, nhosts, state)
Display *display;
int *nhosts;                ;;; RETURN
Bool *state;                ;;; RETURN
{}

void XRemoveHost(display, host)
Display *display;
XHostAddress *host;
{}

void XRemoveHosts(display, hosts, num_hosts)
Display *display;
XHostAddress *hosts;
int num_hosts;
{}

void XDisableAccessControl(display)
Display *display;
{}

void XEnableAccessControl(display)
Display *display;
{}

void XSetAccessControl(display, mode)
Display *display;
int mode;
{}


endexternal;


xlib_external_require XHostAccess;


global vars XHostAccess = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
