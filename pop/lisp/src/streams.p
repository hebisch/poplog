/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lisp/src/streams.p
 > Purpose:         Common Lisp Streams
 > Author:          John Williams, May 29 1987 (see revisions)
 > Documentation:   CLtL p327-332
 > Related Files:
 */

lisp_compile_mode;

section $-lisp;


/* Streams are four element records (the key is defined in types.p).

The four fields are:

    stream_read_p
        Procedure (of 0 args) for reading next character from the stream
    stream_write_p
        Procedure (of 1 arg) for writing a single character to the stream
    stream_source
        The source of the characters returned by the stream_read_p procedure.
    stream_dest
        The destination of characters given to the stream_write_p procedure.

The stream_source and stream_dest fields may be one of the following types:

    ident           pointer to device
    device          stream reads from/writes to that device
    stream          if the stream is a two-way or echo stream
    symbol          if the stream is a synonym stream
    string          stream reads from/writes to that string
    list (of streams) if the stream is a concatenated or broadcast stream
    pop_undef       source/dest is the procedure, nowhere else
    false           stream does not support input/output

*/

define_fast_accessors(stream_key);

fastprocs cont, front, back, destpair, for, frozval;


define is_input_stream() with_nargs 1;
    stream_source() and true
enddefine;


define is_output_stream() with_nargs 1;
    stream_dest() and true
enddefine;


built_in_class(@POPLOG:INPUT-STREAM, [^@STREAM], is_input_stream);
built_in_class(@POPLOG:OUTPUT-STREAM, [^@STREAM], is_output_stream);


/* Stream errors */

constant Err_out;

define lconstant Check_stream(s);
    unless isstream(s) do
        type_error(s, @STREAM)
    endunless
enddefine;


define stream_error(message, involving, stream);
    if stream == error_output then
        Err_out -> error_output
    endif;
    fast_chain(
        @STREAM-ERROR,
        {^@:MESSAGE ^message ^@:INVOLVING ^involving ^@:STREAM ^stream},
        lisp_error)
enddefine;


define lconstant Read_error(stream);
    stream_error('Cannot read from ~S', [^stream], stream)
enddefine;


define updaterof Read_error(char, stream);
    if isinteger(char) then
        (CHARACTER char) -> char
    endif;
    stream_error('Cannot un-read ~S to ~S', [^char ^stream], stream)
enddefine;


define lconstant Write_error(char, stream);
    if isinteger(char)
    and char fi_>= 0 and char fi_<= 255 then
        fast_code_char(char) -> char
    endif;
    stream_error('Cannot write ~S to ~S', [^char ^stream], stream)
enddefine;


define lconstant Closed_error(stream, op);
    lisp_error(@POPLOG:CLOSED-STREAM, {^@:OPERATION ^op ^@:STREAM ^stream})
enddefine;


define updaterof Closed_error(char, stream, op);
    returnif(char == termin);
    (CHARACTER char) -> char;
    lisp_error(@POPLOG:CLOSED-STREAM,
               {^@:MESSAGE 'Cannot un-read ~S to ~S'
                ^@:INVOLVING [^char ^stream]
                ^@:OPERATION ^@:UN-READ
                ^@:STREAM ^stream})
enddefine;


/* Making streams from character repeaters and consumers */

constant procedure (
    Discin      =   pdpart(discin(popdevin)),
    Stringin    =   pdpart(stringin(nullstring)),
    Read_string,        /* defined below */
    );


define buffercharin(pdr, buffer) -> char;
    dlocal
        closed_read_ok = true,
        pop_charin_escapes = [],
        /* the following variables are set because
            Xved is invoked via -charin-.
            See also -apply_in_pop11_environment- in
            C.all/lisp/src/compile.p
        */
        pr = syspr,
        popradians = false,
        popdprecision = false,
        ;
    if (cont(buffer) ->> char) then
        false -> cont(buffer)
    else
        fast_apply(pdr) -> char
    endif
enddefine;


define make_stream(input, output) -> stream;
    lvars buffer;

    define lconstant Get_device(pdr);
        while isclosure(pdr) and datalength(pdr) /== 0 do
            frozval(1, pdr) -> pdr
        endwhile;
        if isdevice(pdr) then pdr else pop_undef endif
    enddefine;

    consstream(0, 0, false, false) -> stream;

    if input then
        if input is_closure_of Stringin then
            Read_string(% frozval(1, input) %) -> input;
            SLOW front(frozval(1, input))
        else
            Get_device(input)
        endif -> stream_source(stream);

        unless updater(input) do
            consref(false) -> buffer;
            pdprops(input);
            buffercharin(% input, buffer %) -> input;
            -> pdprops(input);
            updater(cont)(% buffer %) -> updater(input)
        endunless
    endif;
    if output then
        Get_device(output) -> stream_dest(stream)
    endif;
    input or Read_error(% stream %)  -> stream_read_p(stream);
    output or Write_error(% stream %) -> stream_write_p(stream)
enddefine;


/* Two-way streams */

define make_io_stream(input, output);
    Check_stream(input);
    Check_stream(output);
    consstream(stream_read_p(input), stream_write_p(output), input, output)
enddefine;


define is_io_stream(s);
    isstream(s) and isstream(stream_source(s)) and isstream(stream_dest(s))
enddefine;


define io_stream_input_stream(s);
    if is_io_stream(s) then
        stream_source(s)
    else
        type_error(s, @TWO-WAY-STREAM)
    endif
enddefine;


define io_stream_output_stream(s);
    if is_io_stream(s) then
        stream_dest(s)
    else
        type_error(s, @TWO-WAY-STREAM)
    endif
enddefine;


built_in_class(@TWO-WAY-STREAM, [^@STREAM], is_io_stream);


/* Echo streams */

vars peek_mode = false;     ;;; See also peek_char in lisp/src/streams.p

define lconstant Read_echo_stream(read_p, write_pair) -> char;
    fast_apply(read_p) -> char;
    unless peek_mode == nil or char == termin do
        if back(write_pair) then
            fast_apply(char, front(write_pair))
        else
            true -> back(write_pair)
        endif
    endunless;
enddefine;


define updaterof Read_echo_stream(char, read_p, write_pair);
    char -> fast_apply(read_p);
    unless peek_mode do
        false -> back(write_pair)
    endunless
enddefine;


define lconstant Write_echo_stream() with_nargs 2;
    /* args are char, write_p */
    fast_apply()
enddefine;


define make_echo_stream(input, output);
    Check_stream(input);
    Check_stream(output);
    consstream(
        Read_echo_stream(% stream_read_p(input),
                           writeable conspair(stream_write_p(output), true)
                         %),
        Write_echo_stream(% stream_write_p(output) %),
        input,
        output)
enddefine;


define is_echo_stream(s);
    lvars w;
    isstream(s)
        and
    (stream_read_p(s) is_closure_of Read_echo_stream
        or ((stream_write_p(s) ->> w) is_closure_of Closed_error
             and updater(w) is_closure_of Write_echo_stream))
enddefine;


define echo_stream_input_stream(s);
    if is_echo_stream(s) then
        stream_source(s)
    else
        type_error(s, @ECHO-STREAM)
    endif
enddefine;


define echo_stream_output_stream(s);
    if is_echo_stream(s) then
        stream_dest(s)
    else
        type_error(s, @ECHO-STREAM)
    endif
enddefine;


built_in_class(@ECHO-STREAM, [^@STREAM], is_echo_stream);


/* Concatenated streams */

define lconstant Read_streams(s_ref) -> char;
    lvars streams;
    termin -> char;
    cont(s_ref) -> streams;
    until streams == [] do
        fast_apply(stream_read_p(front(streams))) -> char;
        quitunless(char == termin);
        back(streams) ->> streams -> cont(s_ref)
    enduntil
enddefine;


define updaterof lconstant Read_streams(char, s_ref);
    lvars streams;
    cont(s_ref) -> streams;
    if streams == [] and char /== termin then
        char -> Read_error(streams)
    else
        char -> fast_apply(stream_read_p(front(streams)))
    endif
enddefine;


define make_conc_stream(streams) -> result;
    applist(streams, Check_stream);
    consstream(
        Read_streams(% consref(streams) %),
        Write_error(% 0 %),
        streams,
        false) ->> result -> frozval(1, stream_write_p(result))
enddefine;


define is_conc_stream(s);
    isstream(s) and stream_read_p(s) is_closure_of Read_streams
enddefine;


define conc_stream_streams(s);
    if is_conc_stream(s) then
        cont(frozval(1, stream_read_p(s)))
    else
        type_error(s, @CONCATENATED-STREAM)
    endif
enddefine;


built_in_class(@CONCATENATED-STREAM, [^@STREAM], is_conc_stream);


/* Broadcast streams */

define make_broadcast_stream(streams) -> result;
    lvars outputs;

    define lconstant Write_streams(char);
        lvars c, output;
        if isstring(char) then
            for c in_string char do
                for output in outputs do
                    fast_apply(c, output)
                endfor
            endfor
        else
            for output in outputs do
                fast_apply(char, output)
            endfor
        endif
    enddefine;

    maplist(streams, SLOW stream_write_p) -> outputs;

    consstream(
        Read_error(% 0 %),
        Write_streams,
        false,
        streams) ->> result -> frozval(1, stream_read_p(result))
enddefine;


define is_broadcast_stream(s);
    isstream(s) and listp(stream_dest(s))
enddefine;


define broadcast_stream_streams(s);
    if is_broadcast_stream(s) then
        stream_dest(s)
    else
        type_error(s, @BROADCAST-STREAM)
    endif
enddefine;


built_in_class(@BROADCAST-STREAM, [^@STREAM], is_broadcast_stream);


/* Synonym streams */

define lconstant Read_synonym_stream() with_nargs 1;
    fast_apply(SLOW stream_read_p(valof()))
enddefine;


define updaterof Read_synonym_stream(vtok) with_nargs 2;
    -> fast_apply(SLOW stream_read_p(valof(vtok)))
enddefine;


define lconstant Write_synonym_stream(vtok) with_nargs 2;
    fast_apply(SLOW stream_write_p(valof(vtok)))
enddefine;


define make_synonym_stream(sym);
    lvars vtok;
    sv_token(sym) -> vtok;
    consstream(
        Read_synonym_stream(% vtok %),
        Write_synonym_stream(% vtok %),
        vtok,
        vtok)
enddefine;


define is_synonym_stream(s);
    isstream(s) and issymbol(stream_source(s))
enddefine;


define synonym_stream_symbol(s);
    if is_synonym_stream(s) then
        stream_source(s)
    else
        type_error(s, @SYNONYM-STREAM)
    endif
enddefine;


built_in_class(@SYNONYM-STREAM, [^@STREAM], is_synonym_stream);


define recursive_str_input(stream) -> input;
    while issymbol(stream_source(stream) ->> input) do
        quitif(input == nil);
        valof(input) -> stream
    endwhile;
    stream_read_p(stream) -> input
enddefine;


define recursive_stream_write_p(stream);
    lvars item;
    while issymbol(stream_dest(stream) ->> item) do
        quitif(item == nil);
        valof(item) -> stream
    endwhile;
    stream_write_p(stream)
enddefine;


/* String streams */

define Read_string() with_nargs 1;
    Stringin()
enddefine;


define updaterof Read_string(char, si_pair);
    lvars string, index;
    returnunless(char);     ;;; false means flush stream_read_p
    destpair(si_pair) -> (string, index);
    index fi_- 1 -> index;
    if index fi_> 0 and char == fast_subscrs(index, string) then
        index -> back(si_pair)
    elseunless char == termin and index == fast_vector_length(string) do
        char -> Read_error(string)
    endif
enddefine;


define lconstant Read_sequence(sil_vec);    /* Used for adjustable strings */
    lvars seq, index, lim;
    explode(sil_vec) -> (seq, index, lim);
    defaults lim seq_length(seq);
    if index fi_< lim then
        char_code(elt(seq, index));
        index fi_+ 1 -> fast_subscrv(2, sil_vec)
    else
        termin
    endif
enddefine;


define updaterof Read_sequence(char, sil_vec);
    lvars seq, index, lim;
    returnunless(char);                 ;;; false means flush stream_read_p
    explode(sil_vec) -> (seq, index, lim);
    defaults lim seq_length(seq);
    if index fi_> 0
    and index fi_<= lim
    and char == char_code(elt(seq, index fi_- 1)) then
        index fi_- 1 -> back(sil_vec)
    elseunless char == termin and index == lim do
        char -> Read_error(seq)
    endif
enddefine;


define make_stringin_stream(string, lo, hi) -> stream;
    lvars input;

    ;;; defaults for lo
    if pop_true(lo) then
        check_positive(lo)
    else
        0
    endif -> lo;

    if isstring(string)
    and (not(pop_true(hi)) or hi == fast_vector_length(string)) then
        Read_string(% conspair(string, lo fi_+ 1) %)
    else
        ;;; defaults for hi
        if pop_true(hi) then
            check_positive(hi)
        elseif adjustable_array_p(string) then
            false
        else
            seq_length(string)
        endif -> hi;

        Read_sequence(% {^string ^lo ^hi} %)
    endif -> input;

    consstream(
        input,
        Write_error(% 0 %),
        string,
        false) ->> stream -> frozval(1, stream_write_p(stream))
enddefine;


define lconstant Write_string(char, si_pair);
    dlvars index, string, len;

    define lconstant Do_char(c);
        lvars new;
        if index fi_>= len then
            inits(len fi_+ 64) -> new;
            move_bytes(1, string, 1, new, len);
            new -> string;
            fast_vector_length(string) -> len
        endif;
        c -> fast_subscrs(index fi_+ 1 ->> index, string)
    enddefine;

    returnif(char == termin);
    destpair(si_pair) -> (string, index);
    fast_vector_length(string) -> len;
    if isstring(char) then
        appdata(char, Do_char)
    else
        Do_char(char)
    endif;
    string -> front(si_pair);
    index -> back(si_pair)
enddefine;


define make_stringout_stream(string) -> stream;
    lvars output;

    if pop_true(string) then
        unless is_string_array(string) and is_fp_array(string) do
            lisp_error(
                @SIMPLE-TYPE-ERROR,
                {^@:DATUM ^string
                 ^@:EXPECTED-TYPE [^@AND ^@STRING
                                   [^@SATISFIES ^@ARRAY-HAS-FILL-POINTER-P]]
                 ^@:FORMAT-STRING 'String with fill-pointer needed: got ~S'
                 ^@:FORMAT-ARGUMENTS [^string]
                })
        endunless;
        if isadjustable(string) then
            procedure(char);
                vector_push_extend(fast_code_char(char), string, false) ->
            endprocedure
        else
            procedure(char);
                vector_push(fast_code_char(char), string) ->
            endprocedure
        endif
    else
        initvectorclass(64, 0, string_key) -> string;
        Write_string(% conspair(string, 0) %)
    endif -> output;

    consstream(Read_error(% 0 %), output, false, string)
        ->> stream -> frozval(1, stream_read_p(stream))
enddefine;


define is_stringin_stream() with_nargs 1;
    lvars p;
    recursive_str_input() -> p;
    if (p is_closure_of Read_string)
    or (p is_closure_of Read_sequence) then
        frozval(1, p)
    else
        false
    endif
enddefine;


define is_stringout_stream() with_nargs 1;
    lvars p, u;
    recursive_stream_write_p() -> p;
    if p is_closure_of Write_string then
        frozval(1, p)
    elseif p is_closure_of Closed_error
    and (updater(p) ->> u) is_closure_of Write_string then
        frozval(1, u)
    else
        false
    endif
enddefine;


define get_stringin_index(stream);
    lvars item;
    if (is_stringin_stream(stream) ->> item) then
        if ispair(item) then
            back(item) fi_- 1
        else
            item(2)
        endif
    else
        type_error(stream, [^@AND ^@STRING-STREAM ^@POPLOG:INPUT-STREAM])
    endif
enddefine;


define get_stringout_string(stream);
    lvars si_pair;
    if (is_stringout_stream(stream) ->> si_pair) then
        substring(1, back(si_pair), front(si_pair));
        0 -> back(si_pair)
    else
        type_error(stream, [^@AND ^@STRING-STREAM ^@POPLOG:OUTPUT-STREAM])
    endif
enddefine;


define is_string_stream(s);
    isstream(s) and stringp(stream_source(s)) or stringp(stream_dest(s))
enddefine;


built_in_class(@STRING-STREAM, [^@STREAM], is_string_stream);


/* Create standard streams */

constant
    Std_in      =   make_stream(charin, false),
    Std_out     =   make_stream(false, charout),
    Err_out     =   make_stream(false, charerr),
    Trace_out   =   make_stream(false, charout),
    Raw_io      =   make_stream(rawcharin, rawcharout),
    ;


ident pop_charin_device     ->  stream_source(Std_in);
ident pop_charout_device    ->  stream_dest(Std_out);
ident pop_charerr_device    ->  stream_dest(Err_out);
ident pop_charout_device    ->  stream_dest(Trace_out);
ident poprawdevin           ->  stream_source(Raw_io);
ident poprawdevout          ->  stream_dest(Raw_io);


constant
    Debug_io    =   make_io_stream(Std_in, Std_out),
    Query_io    =   make_io_stream(Std_in, Std_out),
    Term_io     =   make_io_stream(Std_in, Std_out),
    Null_io     =   make_stream(procedure(); termin endprocedure, erase),
    ;


/* Active variables standard_input, standard_output, & error_output */

lvars Std_in_var, Std_out_var, Err_out_var;

vars procedure (str_input = stream_read_p, str_output = stream_write_p);


define active standard_input();
    Std_in_var
enddefine;


define updaterof active standard_input(item);
    returnunless(item);
    returnif(item == nil);
    returnif(item == Std_in_var);
    if item == true then
        terminal_io -> item
    endif;
    unless is_input_stream(item) do
        type_error(item, @POPLOG:INPUT-STREAM)
    endunless;
    item -> Std_in_var
enddefine;


define make_standard_input(input);
    returnif (input == stream_read_p(Std_in_var));
    if input == charin
    or input == str_input(Std_in) then
        Std_in
    else
        make_stream(input, false)
    endif -> Std_in_var
enddefine;


define read_standard_input();
    fast_apply(str_input(Std_in_var))
enddefine;


define updaterof read_standard_input() with_nargs 1;
    -> fast_apply(str_input(Std_in_var))
enddefine;


define active standard_output();
    Std_out_var
enddefine;


define updaterof active standard_output(item);
    returnunless(item);
    returnif(item == nil);
    if item == true then
        terminal_io -> item
    endif;
    returnif(item == Std_out_var);
    unless is_output_stream(item) do
        type_error(item, @POPLOG:OUTPUT-STREAM)
    endunless;
    item -> Std_out_var
enddefine;


vars pc_phase;      /* See C.all/lisp/src/printcore.p */


define make_standard_output(output);
    /* This procedure should not be used if output is
       (directly or otherwise) a closure of pp_charout.
    */
    returnif (output == stream_write_p(Std_out_var) or pc_phase == @:MARK);
    if output == charout then
        Std_out
    elseif output == charerr then
        Err_out
    else
        make_stream(false, output)
    endif -> Std_out_var
enddefine;


define write_standard_output() with_nargs 1;
    fast_apply(str_output(Std_out_var))
enddefine;


define active error_output;
    Err_out_var
enddefine;


define updaterof active error_output(item);
    unless is_output_stream(item) do
        type_error(item, @POPLOG:OUTPUT-STREAM)
    endunless;
    item -> Err_out_var
enddefine;


define write_error_output() with_nargs 1;
    fast_apply(str_output(Err_out_var))
enddefine;


constant macro
    (SET_CUCHARIN =  [dlocal cucharin  = str_input(standard_input);],
     SET_CUCHAROUT = [dlocal cucharout = str_output(standard_output);],
     SET_CUCHARERR = [dlocal cucharerr = str_output(error_output);],
    );


define reset_streams();
    /* Called by the SS_RESET procedure in the Lisp subsystem entry */

    Std_in -> standard_input;
    Std_out -> standard_output;
    Err_out -> error_output;
    Trace_out -> trace_output;
    Raw_io -> raw_io;
    Debug_io -> debug_io;
    Query_io -> query_io;
    Term_io -> terminal_io;

    /* Clear input buffers in Std_in and Raw_io
        (and hence all the other input streams which use Std_in)
    */

    false -> SLOW stream_read_p(Std_in)();          /* Use SLOW to be safe */
    false -> SLOW stream_read_p(Raw_io)();          /* Use SLOW to be safe */
enddefine;


/* Call reset_streams so that we can load the Lisp source files */

reset_streams();


/* Closing streams, and recognising open ones */

define lconstant Close_dev(dev);
    if isdevice(dev) then
        if isclosed(dev) then
            nil
        else
            sysclose(dev);
            true
        endif
    elseif dev and not(isident(dev)) then
        1
    else
        false
    endif
enddefine;


define close_stream(stream);
    lvars s, w, d;
    if (Close_dev(stream_source(stream)) ->> s) == 1 then
        if stream_read_p(stream) is_closure_of Closed_error then
            nil
        else
            Closed_error(% stream, @READ %) -> stream_read_p(stream);
            true
        endif -> s
    endif;
    if (Close_dev(stream_dest(stream)) ->> d) then
        if (stream_write_p(stream) ->> w) is_closure_of Closed_error then
            nil
        else
            Closed_error(% stream, @WRITE %) ->> d
                -> stream_write_p(stream);
            w -> updater(d);
            true
        endif -> d
    endif;
    s or d or true
enddefine;


define is_open_stream(stream);
    lvars item;
    stream_source(stream) or stream_dest(stream) -> item;
    if isident(item) then
        idval(item) -> item
    endif;
    if isdevice(item) then
        not(isclosed(item))
    else
        if stream_source(stream) then
            stream_read_p
        else
            stream_write_p
        endif (stream) -> item;
        item and not(item is_closure_of Closed_error)
    endif
enddefine;


/* General stream utilities */

define stream_element_type(stream) -> type1;
    lvars type2;
    if recursive_str_input(stream) is_closure_of read_bits then
        @INTEGER
    else
        @CHARACTER
    endif -> type1;
    if recursive_stream_write_p(stream) is_closure_of write_bits then
        @INTEGER
    else
        @CHARACTER
    endif -> type2;
    unless type1 == type2 do
        [^@OR ^type1 ^type2] -> type1
    endunless
enddefine;


define streamdev(stream);
    lvars err = true, In, Out, item;

    unless stream do
        /* Optional argument false means don't signal error */
        false -> err;
        -> stream
    endunless;

    stream_source(stream) -> In;
    stream_dest(stream) -> Out;
    if (In and Out) and In /== Out then
        if err then
            stream_error('Stream has different source and destination',
                         [^stream], stream)
        else
            return(false)
        endif
    else
        In or Out
    endif -> item;
    if isident(item) then
        idval(item) -> item
    endif;
    if isdevice(item)
    and not(systrmdev(item)) and device_full_name(item) then
        item
    elseif issymbol(item) then
        streamdev(valof(item), if err == false then err endif)
    elseif err then
        type_error(stream, @FILE-STREAM)
    else
        false
    endif
enddefine;


define is_file_stream() with_nargs 1;
    streamdev(false) and true
enddefine;

built_in_class(@FILE-STREAM, [^@STREAM], is_file_stream);


define app_str_devs(stream, field, pdr);
    lvars item;
    fast_apply(stream, field) -> item;
    if isstream(item) then
        app_str_devs(item, field, pdr)
    elseif listp(item) then
        until endp(item) do
            app_str_devs(destpair(item) -> item, field, pdr)
        enduntil
    elseif issymbol(item) then
        app_str_devs(valof(item), field, pdr)
    else
        if isident(item) then
            idval(item) -> item
        endif;
        if isdevice(item) then
            fast_apply(item, pdr)
        endif
    endif
enddefine;


define str_input_name() with_nargs 1;
    app_str_devs(
        stream_source,
        procedure() with_nargs 1;
            exitfrom(device_full_name(), str_input_name)
        endprocedure);
    false
enddefine;


define lconstant Apply_if_open(dev, p);
    unless isclosed(dev) do
        fast_apply(dev, p)
    endunless
enddefine;


define clear_input(standard_input);
    dlocal standard_input;
    false -> read_standard_input();     /* Flush itemiser buffer */
    app_str_devs(
        standard_input,
        stream_source,
        #_< Apply_if_open(% sys_clear_input %) >_#
        );
    nil
enddefine;


constant
    procedure (is_pp_stream, make_pp_event),
    PPE_FRESH_LINE, PPE_CLEAR_OUTPUT, PPE_FINISH_OUTPUT, PPE_FORCE_OUTPUT,
    ;


define clear_output(standard_output);
    dlocal standard_output;
    if is_pp_stream(standard_output) then
        make_pp_event(PPE_CLEAR_OUTPUT)
    else
        /* Do nothing because there's no Poplog procedure to "clear output" */
    endif;
    nil
enddefine;


define finish_output(standard_output);
    dlocal standard_output;
    if is_pp_stream(standard_output) then
        make_pp_event(PPE_FINISH_OUTPUT)
    else
        app_str_devs(
            standard_output,
            stream_dest,
            #_< Apply_if_open(% sysflush(% true %) %) >_#
            )
    endif;
    nil
enddefine;


define force_output(standard_output);
    dlocal standard_output;
    if is_pp_stream(standard_output) then
        make_pp_event(PPE_FORCE_OUTPUT)
    else
        app_str_devs(
            standard_output,
            stream_dest,
            #_< Apply_if_open(% sysflush(% false %) %) >_#
            )
    endif;
    nil
enddefine;


/*  listen (below) returns:
        true if input is definitely available,
        nil if input is definitely unavailable,
        false if availability of input cannot be determined

    See also read_char_no_hang in C.all/lisp/src/read.p
*/


define listen(standard_input);
    lvars input, x;
    dlocal standard_input;

    /* First try itemiser buffer, or string-standard_input */

    recursive_str_input(standard_input) -> input;

    if input is_closure_of buffercharin
    and (cont(frozval(1, updater(input))) ->> x) then
        return(lisp_true(x /== termin))
    elseif input is_closure_of Read_string then
        frozval(1, input) -> x;
        return(lisp_true(back(x) fi_<= fast_vector_length(front(x))))
    elseif input is_closure_of Read_sequence then
        lblock;
            lvars seq, index, lim;
            explode(frozval(1, input)) -> (seq, index, lim);
            defaults lim seq_length(seq);
            return(lisp_true(index fi_< lim))
        endlblock
    endif;

    /* Now look at first input device, return false if not found */

    app_str_devs(
        standard_input,
        stream_source,
        procedure(dev);
            if isclosed(dev) then
                nil
            else
                sys_input_waiting(dev)
            endif;
            exitfrom(listen)
        endprocedure);
    false
enddefine;


/* Recognising interactive streams */

define is_interactive_repeater(pdr);
    lvars dev;
    if pdr is_closure_of buffercharin then
        frozval(1, pdr) -> pdr
    endif;
    if pdr == charin then
        true
 #_IF VED_LOADED
    elseif is_ved_lmr_stream(pdr) then
        true
 #_ENDIF
    else
        if pdr == rawcharin then
            poprawdevin
        elseif pdr is_closure_of Discin then
            frozval(1, pdr)
        else
            return(false)
        endif -> dev;
        dev == pop_charin_device or systrmdev(dev)
    endif
enddefine;


define is_interactive_stream =
    recursive_str_input <> is_interactive_repeater
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Feb 15 1996
        Changes for new exception handler.
--- John Williams, Aug 25 1995
        Echo streams now handle peek_char and unread_char correctly.
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Jun 20 1995
        Removed erroneous 'lconstant' in definition of Read_string.
--- John Williams, Jun  1 1995
        Ved identifiers now guarded with #_IF VED_LOADED.
--- John Williams, Mar 30 1995
        Upgraded to CLtL 2 (ch 21). standard_input, standard_output, and
        error_output are now active variables. lisp_charin/out replaced
        by read_standard_input and write_standard_output.
--- John Williams, Oct 14 1993
        finish_output and force_output now use the new sync argument to
        sysflush.
--- John Williams, Aug 31 1993
        Extra argument to checkr_string_ends.
--- John Williams, Jul 12 1993
        Uses consclosure instead of partapply.
--- John Williams, Feb 21 1992
        -buffercharin- now dlocal's -pr- etc. See comments.
--- John Williams, Feb 21 1992
        Changed -check_string- to -checkr_string_ends- (cf BR isl-fr.4412)
--- John Williams, Nov 21 1990
        -buffercharin- locally sets -closed_read_ok- true
--- John Williams, Jan  2 1990
        -buffercharin- locally sets -pop_charin_escapes- to []
--- John Williams, Nov  3 1988
        Fixed -is_interactive_repeater-
 */
