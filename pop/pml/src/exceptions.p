/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/pml/src/exceptions.p
 > Purpose:         PML: Exception handling
 > Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 */


section $-ml;

;;; Forward:
constant procedure (mlEXECUTE);

vars
    exn_pdr = false,
        ;;; a procedure to evaluate <exp> in an expression:
        ;;;     <exp> handle <handler>
    exn_stacklength,
        ;;; the user stacklength when -exn_pdr- was entered
    exn_traceback = false,
        ;;; enables exception traceback
;

;;; Predefined exceptions:
;;;     (initialised by external exception declarations)

#_IF not(DEF exception)
define exception =
    newassoc([]);
enddefine;
#_ENDIF

;;; exception packets:
defclass packet [writeable] {
    packet_value    : full,     ;;; value carried by the packet
    packet_id       : full,     ;;; unique exception identifier
    packet_name     : full,     ;;; the exception name
    packet_printer  : full,     ;;; procedure for printing the value
};

procedure(pkt);
    lvars pkt;
    unless pr == ml_pr then printf('<packet ') endunless;
    sys_syspr(packet_name(pkt));
    if packet_printer(pkt) then
        packet_printer(pkt)(packet_value(pkt), true);
    endif;
    unless pr == ml_pr then printf('>') endunless;
endprocedure -> class_print(packet_key);


;;; handle:
;;;     handles exceptions which escape to top-level.

define handle(pkt);
    lvars   pkt;
    dlocal  pop_pr_exception = sys_pr_exception, pr = ml_pr;
    printf('\n;;; Uncaught exception: %p\n', [^pkt]);
    interrupt();
enddefine;

;;; raise_and_trace:
;;;     raises an exception, printing a backtrace

define lconstant raise_and_trace(pkt);
    lvars pkt;

    define lconstant prcaller(code, p, n);
        lvars code, p, n;
        returnif(p == sysEXECUTE or p == mlEXECUTE);
        dlocal pr = ml_pr, cucharout = cucharerr;
        if isprocedure(p) then
            unless pdprops(p) ->> p then "'<fn>'" -> p endunless;
        endif;
        if trace_depth > 0 then
            ;;; merge with trace output
            printf('[');
            pr_field(trace_depth, 4, ` `, false, sys_syspr);
            printf(']');
        endif;
        printf('%c %p', [^code ^p]);
        if n > 1 then printf(' (*%p)', [^n]) endif;
        printf('\n');
    enddefine;

    define lconstant unwind();
        dlocal exn_traceback = false;
        define lconstant is_exn_pdr(p);
            lvars p, ep = exn_pdr;
            while isclosure(ep) do pdpart(ep) -> ep endwhile;
            p == ep;
        enddefine;
        lvars p = caller(1);
        if is_exn_pdr(p) then
            prcaller(`H`, caller(2), 1);
            ;;; exitfrom -exn_pdr-, returning <false> to indicate exception
            ;;; raised
            chain(false, identfn, chain);
        else
            lvars i = 2, q;
            while caller(i) == p do
                i fi_+ 1 -> i;
            endwhile;
            i fi_- 1 -> i;
            prcaller(`X`, p, i);
            chainfrom(callstacklength(i), unwind);
        endif;
    enddefine;

    prcaller(`E`, pkt, 1);
    chain(pkt, unwind);
enddefine;

;;; raise:
;;;     raises an exception.

define raise(pkt);
    lvars pkt;
    if exn_pdr then
        ;;; inside a handler: chain to the handler code
        erasenum(stacklength() fi_- exn_stacklength);
        if trace_names or exn_traceback then
            chain(pkt, raise_and_trace);
        else
            exitfrom(pkt, false, exn_pdr);
        endif;
    else
        ;;; not in a handler: mishap
        mishap(pkt, 1, 'UNCAUGHT EXCEPTION');
    endif;
enddefine;


;;; new_exn_value:
;;;     returns a unique (integer) exception value

lvars
    exn_cnt = 0,
;

define new_exn_value();
    exn_cnt + 1 ->> exn_cnt;
enddefine;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Apr 26 1996
        Changes for new (Poplog) exception printing
--- Robert John Duncan, Jul  4 1995
        Made packet class writeable by default
--- Robert John Duncan, Dec 20 1994
        Changed traceback printing to fit in with function call tracing
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Mar  1 1991
        Added print procedure to packet key.
--- Robert John Duncan, Feb  4 1991
        Changed to use -defclass-.
--- Robert John Duncan, Jan 21 1991
        Removed -exn*handler- procedure: handler code now planted inline
        and -raise- returns <false> to indicate that it should be executed.
--- Robert John Duncan, Nov  6 1990
        Changed -unwind- to do an explicit chain-from-caller rather than a
        -chainfrom-: guards against infinite loops if an interrupt arrives
        while inside -unwind-.
        Also localised the traceback flag to <false> so that an interrupt
        will abort the traceback.
--- Robert John Duncan, Oct  8 1990
        Added exception traceback.
--- Robert John Duncan, Aug  9 1990
        Deleted pop exception table: all trapped mishaps now raise either
        -Overflow- or -Div- exceptions.
--- Robert John Duncan, Jul 24 1990
        Change to list of pop exceptions: the ML -mod- function now has
        pdprops "mod" rather than "rem".
--- Rob Duncan, Mar 13 1990
        Simplified exception handling interface: -raise- now always assumes
        there's a handler present, and mishaps if not (there may not be if
        we're calling an ML function from POP-11). -raise- also resets the
        stack length, so this no longer has to be done by the handler itself.
            Exported the top-level handler and changed it to call -interrupt-
        rather than -setpop-.
 */
