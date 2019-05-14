/* --- Copyright University of Birmingham 1995. All rights reserved. ------
 > File:            $poplocal/local/auto/sys_obey_linerep.p
 > Purpose:         Run a Unix command
 >                  and produce a line repeater for the output
 > Author:          Aaron Sloman, May  5 1993 (see revisions)
 > Documentation:   Below
 > Related Files:
 */

/*
sys_obey_linerep(string) -> repeater;
sys_obey_linerep(string, shell) -> repeater;

The first form takes a string which is a unix command and runs
it returning a repeater for the output lines.

The second form is similar, except that the extra argument, which is one of
the characters `$`, `%`, `!`, determines which shell should be used,
as with * SYSOBEY, i.e.

                `$`     /bin/sh
                `%`     /bin/csh
                `!`     the value of systranslate('SHELL')

Example:

    vars rep = sys_obey_linerep('ls -ld a*');
    vars file, files = [% for file from_repeater rep do file endfor %];
    files ==>
    ** [drwxr-xr-x  5 axs           512 Feb 16 20:01 a2ps.dir
        -rw-r--r--  1 axs          3716 Jun 30  1992 a2psp.man
        -rw-r--r--  1 axs          1535 Jul  3 18:12 absence
        -rw-r--r--  1 axs         59483 Jul  1 17:44 addr
        -rw-r--r--  1 axs         59386 Jun 28 17:07 addr-
        -rw-r--r--  1 axs         59348 Jun 25 07:21 addr--
        drwxr-xr-x  8 axs          6656 Jul  3 22:25 adm:
        -rw-r--r--  1 axs          9535 Nov 16  1992 advent.objectclass
        .... etc.
*/


section;


global vars pipein_child_pid = false;       ;;; not used since V15

compile_mode:pop11 +varsch +defpdr -lprops +constr
        :vm +prmfix  :popc -wrdflt -wrclos;

lconstant
    arglist = [0 '-ce' 0];


define lconstant sub_repeater(dev, buffer, lim, pid) -> line with_props line_repeater;
    ;;; This will be partially applied to its arguments, to create
    ;;; a repeater. "dev" is a file device record.

    lvars len, buffer, dev, lim, line, pid;

    fast_sysread(dev, 1, buffer, lim) -> len;
    if len == 0 then termin
    else substring(1, len fi_- 1, buffer)
    endif -> line;
    if pid and line == termin then
        ;;; wait for child to finish
        until syswait() == pid do enduntil;
    endif;
enddefine;

define sys_obey_linerep(string, /*shell*/) -> repeater;
    lvars shell, string, pipe_device, repeater;

    if isinteger(string) then
        ;;; extra argument on stack
        string -> (string, shell)
    else
        false -> shell
    endif;

    if  shell == `$` or not(shell) then '/bin/sh'
    elseif shell == `%` then '/bin/csh'
    elseif shell == `!` then systranslate('SHELL')
    else shell
    endif -> shell;

    unless isstring(shell) then
        mishap(shell,1,'SHELL PATH NAME NEEDED')
    endunless;

    shell -> arglist(1);
    string -> arglist(3);

    pipein(shell, arglist, false) -> pipe_device;

    ;;; make a string repeater from the device
    sub_repeater(%pipe_device,
                    copy(sysstring),
                    sysstringlen,
                    ;;; next bit needed for Poplog V14.5 only
                    pipein_child_pid%) -> repeater;

enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Dec 10 1995
    Altered for Poplog V15.0 wehre pipein_child_pid is no longer used.
 */
