(* --- Copyright University of Sussex 1996.  All rights reserved. ---------
 * File:            C.all/pml/src/StdIO.ml
 * Purpose:         PML: IO primitives
 * Author:          Robert Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 * Documentation:   HELP * STDIO
 *)


signature StdIO = sig

;;; IO stream types

type instream
type outstream

;;; IO exception

exception Io of string

;;; Standard IO streams

val std_in          : instream
val std_out         : outstream
val std_err         : outstream

;;; Functions on input streams

val open_in         : string -> instream
val close_in        : instream -> unit
val input           : instream * int -> string
val input_line      : instream -> string
val lookahead       : instream -> string
val end_of_stream   : instream -> bool
val is_term_in      : instream -> bool

;;; Functions on output streams

val open_out        : string -> outstream
val open_append     : string -> outstream
val close_out       : outstream -> unit
val output          : outstream * string -> unit
val flush_out       : outstream -> unit
val is_term_out     : outstream -> bool

end;    (* signature StdIO *)


;;; Everything is defined in POP-11

pop11

section $-ml;

ml_structure StdIO : StdIO = struct

vars
    ml_readprompt = nullstring,     ;;; prompt for interactive reads
;


;;; IO stream types:

ml_type instream;
ml_type outstream;


;;; IO Exception

ml_exception Io of string;

;;; io_failure:
;;;     raises an io_failure exception

define lconstant io_failure(msg);
    lconstant procedure io_exn = exception("Io");
    raise(io_exn(msg));
enddefine;

;;; io_exception_handler:
;;;     mishap handler during I/O operations (calls -io_failure-)

lvars opening_file = false;

define lconstant io_exception_handler(n, msg, idstring, severity);
    if severity == `E` or severity == `R` then
        if opening_file then
            ;;; supply message required by the Definition
            erasenum(n);
            format_message(opening_file, 1, '%Cannot open %S (%M)') -> (msg, _);
        else
            format_message(n, msg) -> (msg, _);
            unless msg = nullstring then
                mapdata(msg, uppertolower) -> msg;
                lowertoupper(msg(1)) -> msg(1);
            endunless;
        endif;
        io_failure(msg);
    endif;
    false;
enddefine;


;;; Standard IO streams:

ml_val std_in   : instream  = std_in;
ml_val std_out  : outstream = std_out;
ml_val std_err  : outstream = std_err;


;;; Functions on instreams:

ml_val open_in : string -> instream =
procedure(f) with_props open_in;
    dlocal opening_file = f, pop_exception_handler = io_exception_handler;
    instream(f);
endprocedure;

ml_val close_in : instream -> unit =
procedure(/* s */) with_props close_in with_nargs 1;
    dlocal pop_exception_handler = io_exception_handler;
    close_instream(/* s */);
    ml_unit;
endprocedure;

ml_val input : instream * int -> string =
procedure(is, n) with_props input;
    dlocal pop_exception_handler = io_exception_handler;
    dlocal popprompt = ml_readprompt;
    input_n(n, is);
endprocedure;

ml_val input_line : instream -> string =
procedure(/* s */) with_props input_line with_nargs 1;
    dlocal pop_exception_handler = io_exception_handler;
    dlocal popprompt = ml_readprompt;
    input_line(/* s */);
endprocedure;

ml_val lookahead : instream -> string =
procedure(/* s */) with_props lookahead with_nargs 1;
    dlocal pop_exception_handler = io_exception_handler;
    dlocal popprompt = ml_readprompt;
    lvars c = lookahead(/* s */);
    if c == termin then nullstring else mlstring(c, 1) endif;
endprocedure;

ml_val end_of_stream : instream -> bool =
procedure(/* s */) with_props end_of_stream with_nargs 1;
    dlocal pop_exception_handler = io_exception_handler;
    dlocal popprompt = ml_readprompt;
    lookahead(/* s */) == termin;
endprocedure;

ml_val is_term_in : instream -> bool =
procedure(s) with_props is_term_in;
    dlocal pop_exception_handler = io_exception_handler;
    if stream_device(s) ->> s then systrmdev(s) else false endif;
endprocedure;


/*
 *  Operations on output streams
 */

ml_val open_out : string -> outstream =
procedure(f) with_props open_out;
    dlocal opening_file = f, pop_exception_handler = io_exception_handler;
    outstream(f);
endprocedure;

ml_val open_append : string -> outstream =
procedure(f) with_props open_append;
    dlocal opening_file = f, pop_exception_handler = io_exception_handler;
    appendstream(f);
endprocedure;

ml_val close_out : outstream -> unit =
procedure(/* s */) with_props close_out with_nargs 1;
    dlocal pop_exception_handler = io_exception_handler;
    close_outstream(/* s */);
    ml_unit;
endprocedure;

ml_val output : outstream * string -> unit =
procedure(os, s) with_props output;
    dlocal pop_exception_handler = io_exception_handler;
    output(s, os);
    ml_unit;
endprocedure;

ml_val flush_out : outstream -> unit =
procedure(s) with_props flush_out;
    dlocal pop_exception_handler = io_exception_handler;
    if stream_device(s) ->> s then sysflush(s) endif;
    ml_unit;
endprocedure;

ml_val is_term_out : outstream -> bool =
procedure(s) with_props is_term_out;
    dlocal pop_exception_handler = io_exception_handler;
    if stream_device(s) ->> s then systrmdev(s) else false endif;
endprocedure;

ml_endstructure;

endsection; /* $-ml */

ml

pervasive StdIO;

(* --- Revision History ---------------------------------------------------
--- Robert Duncan, Apr 26 1996
        Changes for new Poplog exception handling
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Jan 21 1991
        Removed redundant optimisations for input & output (now done
        automatically by tupled-function optimisation).
--- Rob Duncan, Jan 31 1990
        Made standard IO streams constant
 *)
