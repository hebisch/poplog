/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/pml/src/io.p
 > Purpose:         PML: IO streams
 > Author:          Rob Duncan & Simon Nichols, May 22 1989 (see revisions)
 */


section $-ml =>
    ml_instream
    ml_instream_producer
    ml_outstream
    ml_outstream_consumer
;

/*
 *  Instream and Oustream Types
 */

defclass instream [writeable] {
    instream_name   : full,     ;;; stream name; a string
    instream_proc   : full,     ;;; character repeater + put-back buffer
};

defclass outstream [writeable] {
    outstream_name  : full,     ;;; stream name; a string
    outstream_proc  : full,     ;;; character consumer
};

procedure(s);
    lvars s;
    if instream_name(s) = nullstring then
        appdata('<instream>', cucharout);
    else
        appdata('<instream ', cucharout);
        appdata(instream_name(s), cucharout);
        cucharout(`>`);
    endif;
endprocedure -> class_print(instream_key);

procedure(s);
    lvars s;
    if outstream_name(s) = nullstring then
        appdata('<outstream>', cucharout);
    else
        appdata('<outstream ', cucharout);
        appdata(outstream_name(s), cucharout);
        cucharout(`>`);
    endif;
endprocedure -> class_print(outstream_key);

;;; closed_instream:
;;;     character repeater for closed instreams: returns <termin> forever

define closed_instream();
    termin;
enddefine;

;;; closed_outstream:
;;;     character consumer for closed outstreams: mishaps on anything
;;;     except <termin>

define closed_outstream(c);
    lvars c;
    unless c == termin then
        mishap(0, 'OUTPUT STREAM IS CLOSED');
    endunless;
enddefine;

;;; stream_device:
;;;     returns the device underlying an input or output stream, or <false>
;;;     if inaccessible.

define stream_device(s);
    lvars s;
    repeat fast_back(s) -> s; quitunless(ispair(s)) endrepeat;
    if s == charin then
        pop_charin_device;
    elseif s == charout then
        pop_charout_device;
    elseif s == charerr then
        pop_charerr_device;
    elseif isclosure(s) and isdevice(frozval(1,s) ->> s) then
        s;
    else
        false;
    endif;
enddefine;

;;; stream_full_name:
;;;     returns the full name of the file underlying an input or output
;;;     stream, or <nullstring> if no name can be found

define stream_full_name(s);
    lvars s;
    repeat fast_back(s) -> s; quitunless(ispair(s)) endrepeat;
    if isclosure(s) and isdevice(frozval(1,s)) then
        device_full_name(frozval(1,s)) or nullstring;
    elseif isstring(pdprops(s)) then
        pdprops(s);
    else
        nullstring;
    endif;
enddefine;


/*
 *  Standard IO Streams
 */

constant
    std_in  = consinstream('std_in',   closed_instream),
    std_out = consoutstream('std_out', closed_outstream),
    std_err = consoutstream('std_err', closed_outstream),
;

;;; setup_std_io:
;;;     give sensible values to the standard IO streams

define setup_std_io();
    charin  -> instream_proc(std_in);
    charout -> outstream_proc(std_out);
    charerr -> outstream_proc(std_err);
enddefine;

;;; GUARD_IO:
;;;     stops characters being lost from the input when swapping between
;;;     instances of the compiler

constant macro GUARD_IO = [dlocal %"%"% instream_proc(std_in) %"%"%;];


/*
 *  Operations on Input Streams
 */

constant procedure input;       ;;; forward

;;; instream:
;;;     construct an instream from a string, device or character repeater.
;;;     Mishaps if the file isn't readable.

define instream(f);
    lvars f, p;
    if isinstream(f) then
        f;
    elseif isstring(f) then
        consinstream(f, discin(f));
    elseif isword(f) then
        consinstream(f sys_>< nullstring, discin(f));
    elseif isdevice(f) then
        consinstream(device_open_name(f), discin(f));
    elseif isprocedure(f) then
        if f == charin then
            std_in;
        elseif isclosure(f) and pdpart(f) == input then
            ;;; value constructed by -instream_producer- below
            frozval(1, f);
        else
            unless recursive_front(pdprops(f)) ->> p then
                nullstring -> p;
            endunless;
            consinstream(
                if isstring(p) then p else p sys_>< nullstring endif,
                f);
        endif;
    else
        mishap(f, 1, 'STRING NEEDED');
    endif;
enddefine;

;;; instream_producer:
;;;     constructs a character repeater from an instream

define instream_producer(s) -> s;
    lvars s, f;
    instream_name(s) -> f;
    input(% s %) -> s;
    unless f = nullstring then consword(f) -> pdprops(s) endunless;
enddefine;

;;; close_instream:
;;;     "close" an instream so that subsequent inputs will return -termin-.
;;;     The standard input stream can't be closed.

define close_instream(s);
    lvars s;
    unless s == std_in then
        closed_instream -> instream_proc(s);
    endunless;
enddefine;

;;; input:
;;;     reads the next character from instream -s-, or <termin> if -s- is
;;;     finished.
;;;     As an updater, -input- puts a character or string back on the
;;;     input stream; <termin> has the effect of closing the stream.

define input(s) -> c;
    lvars p, c, s;
    fast_back(s) -> p;
    if ispair(p) then
        fast_destpair(p) -> fast_back(s) -> c;
    elseif (p() ->> c) == termin then
        close_instream(s);
    endif;
enddefine;

define updaterof input(c, s);
    lvars c, s, p;
    if isinteger(c) and 0 fi_<= c and c fi_<= 255 then
        conspair(c, fast_back(s)) -> fast_back(s);
    elseif isstring(c) then
        fast_back(s) -> p;
        explode(c);
        fast_repeat datalength(c) times conspair(p) -> p endrepeat;
        p -> fast_back(s);
    elseif c == termin then
        close_instream(s);
    else
        mishap(c, 1, 'CHARACTER NEEDED');
    endif;
enddefine;

;;; input_n:
;;;     reads at most -n- characters from instream -s-, returning them in
;;;     a string

define input_n(n, s);
    lvars c, p, ngot = 0, n, s;
    returnif(n == 0)(nullstring);
    fast_back(s) -> p;
    while ispair(p) do
        fast_destpair(p) -> p;
        p -> fast_back(s);
        ngot fi_+ 1 -> ngot;
        returnif(ngot == n)(mlstring(ngot));
    endwhile;
    repeat
        returnif((p() ->> c) == termin)(close_instream(s), mlstring(ngot));
        c; ngot fi_+ 1 -> ngot;
        returnif(ngot == n)(mlstring(ngot));
    endrepeat;
enddefine;

;;; lookahead:
;;;     returns the next character from -s- without removing it

define lookahead(s) -> c;
    lvars p, c, s;
    fast_back(s) -> p;
    if ispair(p) then
        fast_front(p) -> c;
    elseif (p() ->> c) == termin then
        close_instream(s);
    else
        conspair(c, p) -> fast_back(s);
    endif;
enddefine;

;;; input_line:
;;;     reads the next line of input from instream -s-.

define input_line(s);
    lvars c, p, ngot = 0, s;
    fast_back(s) -> p;
    while ispair(p) do
        fast_destpair(p) -> p -> c;
        p -> fast_back(s);
        c; ngot fi_+ 1 -> ngot;
        returnif(c == `\n`)(mlstring(ngot));
    endwhile;
    repeat
        returnif((p() ->> c) == termin)(close_instream(s), mlstring(ngot));
        c; ngot fi_+ 1 -> ngot;
        returnif(c == `\n`)(mlstring(ngot));
    endrepeat;
enddefine;


/*
 *  Operations on Output Streams
 */

constant procedure output;      ;;; forward

;;; outstream:
;;;     constructs an outstream from a string, device or character consumer.

define outstream(f);
    lvars f, p;
    if isstring(f) then
        consoutstream(f, discout(f));
    elseif isdevice(f) then
        consoutstream(device_open_name(f), discout(f));
    elseif isprocedure(f) then
        if f == charout then
            std_out;
        elseif f == charerr then
            std_err;
        elseif isclosure(f) and pdpart(f) == output then
            ;;; value constructed by -outstream_consumer- below
            frozval(1, f);
        else
            unless pdprops(f) ->> p then nullstring -> p endunless;
            consoutstream(
                if isstring(p) then p else p sys_>< nullstring endif,
                f);
        endif;
    else
        mishap(f, 1, 'STRING NEEDED');
    endif;
enddefine;

;;; appendstream:
;;;     constructs an outstream for appending to a file. Works on a string
;;;     only.

define appendstream(f);
    lvars f;
    if isstring(f) then
        consoutstream(f, discappend(f));
    else
        mishap(f, 1, 'STRING NEEDED');
    endif;
enddefine;

;;; outstream_consumer:
;;;     constructs a character consumer from an outstream

define outstream_consumer(s) -> s;
    lvars s, f;
    outstream_name(s) -> f;
    output(% s %) -> s;
    unless f = nullstring then consword(f) -> pdprops(s) endunless;
enddefine;

;;; close_outstream:
;;;     "closes" an outstream so that nothing else can be written on it.
;;;     The standard output and error streams can't be closed.

define close_outstream(s);
    lvars s;
    unless s == std_out or s == std_err then
        outstream_proc(s)(termin);
        closed_outstream -> outstream_proc(s);
    endunless;
enddefine;

;;; output:
;;;     writes a character or a string on outstream -s-; <termin> closes
;;;     the stream. Mishaps if the stream is already closed.

define output(c, s);
    lvars c, s;
    if isstring(c) then
        appdata(c, fast_back(s));
    elseif c == termin then
        close_outstream(s);
    else
        fast_back(s)(c);
    endif;
enddefine;

;;; Exported stream functions:

constant procedure (
    ml_instream             = instream,
    ml_instream_producer    = instream_producer,
    ml_outstream            = outstream,
    ml_outstream_consumer   = outstream_consumer,
);

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul  4 1995
        Made stream classes writeable by default
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Feb  4 1991
        Changed to use -defclass-
--- Robert John Duncan, Nov  6 1990
        Changed -popdevX- to -pop_charX_device-
--- Rob Duncan, Jan 31 1990
        Made standard IO streams constant.
        Exported stream construction and access procedures for POP-11 users.
        Fixed -close_outstream- to send <termin> to the output consumer.
 */
