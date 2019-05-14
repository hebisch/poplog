/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/auto/io_handler.p
 > Purpose:         multi-device access to -sys_async_io-
 > Author:          Roger Evans, Jun 20 1990 (see revisions)
 > Documentation:   REF *sysio
 > Related Files:
 */

/* NB:  this library will become obsolete in v14.1 with the introduction of
        procedural handlers for sys_async_input itself
*/

section;

include sigdefs.ph;

lconstant io_handler_table = newproperty([],8,false,"tmparg");

lvars io_dev_list = [];

define global procedure io_handler with_nargs 1;
    io_handler_table();
enddefine;

define updaterof io_handler(proc,dev);
    lvars proc, dev;
    if proc then
        unless isprocedure(proc) then
            mishap(proc,1,'PROCEDURE NEEDED');
        endunless;
        unless lmember(dev,io_dev_list) then
            dev :: io_dev_list -> io_dev_list;
        endunless;
        true;
    else
        ncdelete(dev,io_dev_list, nonop ==) -> io_dev_list;
        false;
    endif -> sys_async_io(dev, 0);
    proc -> io_handler_table(dev);
enddefine;

define lconstant io_handler_interrupt;
    lvars list dev;
    sys_input_waiting(io_dev_list) -> list;
    for dev in list do
        io_handler_table(dev)(dev);
    endfor;
enddefine;

io_handler_interrupt -> sys_signal_handler(SIG_IO);

endsection;

/* --- Revision History ---------------------------------------------------
--- Roger Evans, Feb  1 1991 added 'obsolete' warning
 */
