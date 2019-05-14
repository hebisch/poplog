/* --- Copyright University of Sussex 1992.  All rights reserved. ---------
 > File:           C.unix/lib/lib/mux.p
 > Purpose:        Routines for multiplexing input sources
 > Author:         Roger Evans, Dec 10 1986 (see revisions)
 > Documentation:  HELP * MUX
 > Related Files:  LIB * VEDMUX, LIB * VSH
 */

#_TERMIN_IF DEF POPC_COMPILING

/* N.B. SEE DISCLAIMER IN HELP MUX */

/*  LIB MUX         R.Evans November 1986

   This library uses the SELECT Unix system call to multiplex input channels
   This lets poplog service input from several channels simultaneously,
   rather than having to poll or block. See lib VED_VSH for an example of use.
*/


section $-library => mux_select mux_table mux_input mux_entry mux;

/* -- multiplex select on mask given -------------------------------------- */

/* load up select system call */
exload 'mux/select'
(language C)
    lconstant
        sysSELECT(width,readfds,writefds,exceptfds,timeout):int <- select,
    ;
endexload;

/*  given input mask for select call, and table of devices, do a select
    and return a list of table entries corresponding to device(s) that cause
    select to return, [] means abnormal exit from select  */
define global mux_select(imask,table);
    lvars imask, table, i, l = datalength(table);

    l_typespec imaskptr :uint;
    lconstant imaskptr = EXPTRINITSTR(:imaskptr);

    imask -> exacc imaskptr;
    if exacc sysSELECT(l,imaskptr,0,0,0) fi_< 0 then
        []
    else
        exacc imaskptr -> imask;
        [%  0 -> i;
            repeat
                if testbit(imask,i) then
                    subscrv(i fi_+ 1,table);
                endif;
                quitif((i fi_+ 1 ->> i) == l);
            endrepeat;
        %]
    endif;
enddefine;

/* -- Support for standard mux table -------------------------------------- */

/* these utilities manipulate a standard table for mux_select, muxtable,
   which can be assigned entries by pop device.
*/
lvars muxmask = 0;  ;;; mask for currently defined table entries
global vars muxtable = {% repeat 32 times; false; endrepeat %};

/* do select on standard table */
define global mux_input;
    mux_select(muxmask,muxtable);
enddefine;

/* looking up table entries */
define global mux_entry(device);
    lvars device;
    unless isinteger(device) then
        device_os_channel(device) -> device;
    endunless;
    muxtable(device+1);
enddefine;

/* adding entries to tables, and updating mask */
define updaterof mux_entry(entry,device);
    lvars entry device;
    unless isinteger(device) then
        device_os_channel(device) -> device;
    endunless;
    if entry then
        /* set mask bit */
        muxmask || (1 << device) -> muxmask;
    else
        /* clear mask bit */
        muxmask &&~~ (1 << device) -> muxmask;
    endif;
    entry -> muxtable(device+1);
enddefine;

global constant mux = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  1 1992
        Made it use exacc instead of vectorclass
--- Robert John Duncan, Jun 29 1992
        Moved to C.unix
--- Robert John Duncan, Nov 26 1990
        Changed to use -exload/exacc- syntax.
--- Aaron Sloman, Oct 24 1988
    Put in main library, but not autoloadable.
--- Roger Evans, Dec 11 1986 added check for presence of device_os_channel,
    and suitable (if rude) definition for it if absent (ie pre 12.4 poplog)
*/
