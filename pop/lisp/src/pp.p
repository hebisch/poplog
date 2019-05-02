/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/pp.p
 > Purpose:         Pretty printing
 > Author:          John Williams, Oct 16 1995
 > Documentation:   CltL ch 27
 > Related Files:
 */

lisp_compile_mode;

section $-lisp;

fastprocs for, repeat, front, back, destpair, ncrev, lmember, subscrs,
          +, -, *, //, rem, <, <=, >, >=, min;


defclass pp_state
    {   pps_buffer,
        pps_index,
        pps_events,
        pps_charout,
        pps_current_lb,
    };

define_fast_accessors(pp_state_key);

fastprocs pps_buffer, pps_index, pps_events, pps_charout, pps_current_lb;


vars pp_current_state = false;

constant macro (
    PP_BUFFER       =   [ pps_buffer(pp_current_state) ],
    PP_BUFFER_I     =   [ pps_index(pp_current_state) ],
    PP_EVENTS       =   [ pps_events(pp_current_state) ],
    PP_CHAROUT      =   [ pps_charout(pp_current_state) ],
    PP_CURRENT_LB   =   [ pps_current_lb(pp_current_state) ],
    );


define add_pp_event(e);
    conspair(e, PP_EVENTS) -> PP_EVENTS
enddefine;


constant
    /* event codes */
    PPE_NEWLINE         =   1,
    PPE_FRESH_LINE      =   2,
    PPE_TAB             =   3,
    PPE_BLOCK_COL       =   4,
    PPE_ENDBLOCK        =   5,
    PPE_FINISH_OUTPUT   =   6,
    PPE_FORCE_OUTPUT    =   7,
    PPE_CLEAR_OUTPUT    =   8,
    ;


/* Character consumer that prints into a pp_state */


define pp_charout(c, pp_current_state);
    lvars i, buff, len, l;
    dlocal pp_current_state;

    define Extend_buff();
        lvars b;
        if i > len then
            inits(len + 256) -> b;
            move_bytes(1, buff, 1, b, len);
            b ->> buff -> PP_BUFFER
        endif
    enddefine;

    PP_BUFFER -> buff;
    PP_BUFFER_I -> i;
    fast_vector_length(buff) -> len;

    if c == `\n` then
        add_pp_event(conspair(PPE_NEWLINE, i - 1))
    elseif c == `\t` then
        add_pp_event(conspair(PPE_TAB, i - 1))
    elseif isinteger(c) then
        if i > len then
            Extend_buff()
        endif;
        c -> subscrs(i, buff);
        i + 1 -> PP_BUFFER_I
    else
        fast_vector_length(c) -> l;
        while l > (len - i + 1) do
            Extend_buff();
            fast_vector_length(buff) -> len
        endwhile;
        move_bytes(1, c, i, buff, l);
        i + l -> PP_BUFFER_I
    endif
enddefine;


/* Mapping streams to pp_states */

global vars
    pp_states = [],     ;;; Assoc list of stream_write_p -> pp_state mappings
    pp_streams = [],    ;;; List of streams for output_all_events
    ;

define pp_state(write_p);
    list_assoc(write_p, pp_states)
enddefine;


define updaterof pp_state(state, write_p);
    acons(write_p, state, pp_states) -> pp_states
enddefine;


define global is_pp_stream() with_nargs 1;
    pp_state(recursive_stream_write_p())
enddefine;


define lconstant Get_pp_state(stream) -> state;
    lvars write_p, state;
    unless (pp_state(recursive_stream_write_p(stream) ->> write_p) ->> state)
    do
        conspp_state(inits(256), 1, [], erase, false)
            ->> state -> pp_state(write_p);
        pp_charout(% state %) -> pps_charout(state);
        conspair(stream, pp_streams) -> pp_streams
    endunless
enddefine;


constant macro SET_PP_CURRENT_STATE =
    [returnunless(is_pp_stream(standard_output) ->> pp_current_state);];


define peek_pp_state(stream, Max, write_p);      /* Called by debugger option :pp */
    lvars state, b, i;
    check_positive(Max) ->;
    if (is_pp_stream(stream) ->> state) then
        pps_buffer(state) -> b;
        for i from 1 to min(pps_index(state) - 1, Max) do
            fast_apply(subscrs(i, b), write_p)
        endfor;
        fast_apply(`\n`, write_p)
    endif
enddefine;


/* Logical blocks */

defclass logical_block
    {   lb_level,
        lb_start,
        lb_end,
        lb_prefix,
        lb_suffix,
        lb_line_prefix,
        lb_start_col,
        lb_block_col,
        lb_indent,
        lb_miser,
        lb_prev_cn,
    };


vars pp_top_level = true;       /* false inside pp_logical_block */


define macro CHECK_PP_LEVEL;
    [ if pp_top_level then
          mishap(0, 'Not inside a logical block')
      endif;
    ].dl
enddefine;


define macro CHECK_PC_MARK;
    "returnif", "(", "pc_phase", "==", @:MARK, ")", ";"
enddefine;


vars pp_item;               /* Item given to pp_logical_block */


define pp_logical_block(standard_output, pp_item,
                              prefix, suffix, line_prefix, print_p);
    lvars sl, level, lb;
    dlocal
        cucharout,
        pp_current_state,
        pp_top_level = false,
        depth_printed,
        length_printed  =   0,
        popgctrace      =   false,
        pp_item,
        standard_output
        ;
    stacklength() -> sl;

    if pc_phase == @:MARK then
        unless pp_item and print_by_key_hook(pp_item) do
            lisp_apply(print_p, 0, 0)
        endunless;
        /* pprint_exit comes out here */
        setstacklength(sl);
        return
    endif;

    Get_pp_state(standard_output) -> pp_current_state;

    define dlocal str_output(s) -> p;
        lvars state;
        /* No need to check pc_phase = @:MARK because already done above */
        recursive_stream_write_p(s) -> p;
        if (pp_state(p) ->> state) then
            pps_charout(state) -> p
        endif
    enddefine;

    str_output(standard_output) -> cucharout;

    if pp_item then
        CHECK_PR_LEVEL;
        returnif(pc_phase == @:PRINT and print_by_key_hook(pp_item))
    endif;

    (pop_true(prefix) and get_simple_string(prefix)) -> prefix;
    (pop_true(suffix) and get_simple_string(suffix)) -> suffix;
    (pop_true(line_prefix) and get_simple_string(line_prefix)) -> line_prefix;

    if PP_CURRENT_LB then
        lb_level(PP_CURRENT_LB) + 1
    else
        1
    endif -> level;

    conslogical_block
        (level, PP_BUFFER_I, false,
         prefix, suffix, line_prefix,
         false, false, false, false, false
        ) -> lb;

    add_pp_event(lb);

    procedure();
        dlocal
            % PP_CURRENT_LB % = lb,
            0 % (), (PP_BUFFER_I - 1 -> lb_end(lb)) %,
            ;
        if prefix then
            appdata(prefix, cucharout)
        elseif line_prefix then
            appdata(line_prefix, cucharout)
        endif;
        add_pp_event(conspair(PPE_BLOCK_COL, PP_BUFFER_I - 1));
        lisp_apply(print_p, 0, 0);
        if suffix then
            appdata(suffix, cucharout)
        endif
    endprocedure();

    /* pprint_exit comes out here */
    setstacklength(sl);
    add_pp_event(conspair(PPE_ENDBLOCK, PP_BUFFER_I - 1))
enddefine;


vars procedure output_pp_events;


define pprint_logical_block(standard_output, item, pre, suff, lp, body);
    dlocal standard_output;
    lvars p;

    if pp_top_level then
        procedure() with_nargs 6;
            lvars stream;
            dlocal pp_streams = [], pp_states = [];
            pp_logical_block();
            for stream in rev(pp_streams) do
                output_pp_events(stream, is_pp_stream(stream))
            endfor
        endprocedure
    else
        pp_logical_block
    endif -> p;

    if print_circle /== nil then
        pc_apply(p(% standard_output, item, pre, suff, lp, body %))
    else
        p(standard_output, item, pre, suff, lp, body)
    endif
enddefine;


/* Conditional newlines, tabs and indents */

defclass cond_newline
    {   cn_left_start,
        cn_right_start,
        cn_right_end,
        cn_kind,
        cn_lb,
    };


procedure(cn);
    lvars i;
    cucharout(`<`);
    spr(dataword(cn));
    for i from 1 to 4 do
        spr(fast_record_access(i, cn))
    endfor;
    pr(lb_level(cn_lb(cn)));
    cucharout(`>`)
endprocedure -> class_print(cond_newline_key);



define cn_left_end() with_nargs 1;
    cn_right_start() - 1
enddefine;


defclass pp_indent
    {   ppi_rel,
        ppi_val,
        ppi_pos,
    };


defclass pp_tab
    {   ppt_kind,
        ppt_col,
        ppt_inc,
        ppt_pos,
    };


define global pprint_newline(kind, standard_output);
    lvars i, lb, prev, cn;
    dlocal pp_current_state, standard_output;

    CHECK_PP_LEVEL;
    CHECK_PC_MARK;
    SET_PP_CURRENT_STATE;
    PP_BUFFER_I -> i;
    PP_CURRENT_LB -> lb;

    /* Get previous cond_newline in this block (if any).
        Set cn_right_end at same time.
    */

    if (lb_prev_cn(lb) ->> prev) then
        i - 1 -> cn_right_end(prev)
    endif;

    conscond_newline
        ( /* Start index of section to left */
            if prev then
                cn_right_start(prev)
            else
                lb_start(lb)
            endif,

            /* Start and end of section to right */
            i, false,

            /* Kind & containing logical block */
            kind, lb
        ) -> cn;

    cn -> lb_prev_cn(lb);
    add_pp_event(cn)
enddefine;


define global pprint_indent(rel, n, standard_output);
    dlocal pp_current_state, standard_output;
    CHECK_PP_LEVEL;
    CHECK_PC_MARK;
    SET_PP_CURRENT_STATE;
    add_pp_event(conspp_indent(rel, n, PP_BUFFER_I - 1))
enddefine;


define global pprint_tab(kind, colnum, colinc, standard_output);
    dlocal pp_current_state, standard_output;
    CHECK_PP_LEVEL;
    CHECK_PC_MARK;
    SET_PP_CURRENT_STATE;
    add_pp_event(conspp_tab(kind, colnum, colinc, PP_BUFFER_I - 1))
enddefine;


define pprint_exit();
    lvars s;
    CHECK_PP_LEVEL;
    unless pc_phase == @:MARK do
        if (lb_suffix(PP_CURRENT_LB) ->> s) then
            appdata(s, cucharout)
        endif
    endunless;
    exitto(pp_logical_block)
enddefine;


define pprint_test_exit();
    CHECK_PP_LEVEL;
    if ispair(pp_item) then
        nil
    else
        pprint_exit()
    endif
enddefine;


define pprint_pop() -> next;
    CHECK_PP_LEVEL;
    if listp(pp_item) then
        if isinteger(print_length)
        and length_printed >= print_length
        and print_readably == nil
        then
           cucharout(`.`);
           cucharout(`.`);
           cucharout(`.`);
           pprint_exit()
        else
           length_printed + 1 -> length_printed
        endif;
        destpair(pp_item) -> (next, pp_item);
        if ispair(pp_item) and pc_list_tail_hook(pp_item) then
            pprint_exit()
        endif
    else
        cucharout(`.`);
        cucharout(`\s`);
        _lisppr(pp_item ->> next)
    endif
enddefine;


define make_pp_event(kind);
    dlocal pp_current_state;
    CHECK_PP_LEVEL;
    CHECK_PC_MARK;
    SET_PP_CURRENT_STATE;
    add_pp_event(conspair(kind, PP_BUFFER_I - 1))
enddefine;


/* Deciding conditional newlines */


define lconstant Exceeds_line(col, i, j);     /* col is number of columns used so far */
    (j - i + 1) > (print_right_margin - col)
enddefine;


define lconstant Skip_back_trailing_spaces(buffer, i, j) -> n;
    for n from j by -1 to i do
        quitunless(subscrs(n, buffer) == `\s`)
    endfor
enddefine;


define lconstant Set_cn_right_end(cn, events, lim);
    lvars e;
    unless cn_right_end(cn) do
        for e in events do
            nextunless(iscond_newline(e));
            if lb_level(cn_lb(e)) <= lb_level(cn_lb(cn)) then
                cn_left_end(e) -> cn_right_end(cn);
                return
            endif
        endfor;
        lim -> cn_right_end(cn)
    endunless
enddefine;


define lconstant Decide_newline(cn, buffer, current_col, last_nl_pos);
    lvars kind, lb;
    cn_kind(cn) -> kind;
    cn_lb(cn) -> lb;
    if kind == @:LINEAR then
        LINEAR:
        Exceeds_line(lb_start_col(lb), lb_start(lb), lb_end(lb))
    elseif kind == @:FILL then
        if last_nl_pos >= cn_left_start(cn) then
            true
        elseif Exceeds_line(
                current_col,
                cn_right_start(cn),
                Skip_back_trailing_spaces
                    (buffer, cn_right_start(cn), cn_right_end(cn)))
        then
            true
        elseif lb_miser(lb) then
            goto LINEAR
        else
            false
        endif
    elseif kind == @:MISER then
        if lb_miser(lb) then goto LINEAR else false endif
    elseif kind == @:MANDATORY then
        true
    else
        mishap(kind, 1, 'Unrecognised conditional newline type')
    endif
enddefine;


define lconstant Do_pp_tab(kind, colnum, colinc, col, sect);

/* col is current output column */
/* sect is start col for current section */

    lvars r, q;

    if kind == @:LINE or kind == @:SECTION then
        if kind == @:SECTION then
            colnum + sect -> colnum
        endif;
        if col >= colnum then
            if colinc == 0 then
                0
            else
                while colnum < col do
                    colnum + colinc -> colnum
                endwhile;
                colnum - col
            endif
        else
            colnum - col
        endif
    elseif kind == @:LINE-RELATIVE or kind == @:SECTION-RELATIVE then
        if kind == @:SECTION-RELATIVE then
            col - sect -> col
        endif;
        (col + colnum) // colinc -> (r, q);
        if r == 0 then
            colnum
        else
            ((q + 1) * colinc) - col
        endif
    else
        mishap(kind, 1, 'Unrecognised tab type')
    endif
enddefine;


/* Procedure to emit the final printed output */

define vars output_pp_events(standard_output, pp_current_state);
    lvars buffer, events, lim, le, x;
    dlvars e, i = 1, col, sect, pptab, lb = false, lb_stack = [],
            line = 1, line_prefix = false, last_nl_pos = -1, prev_c = 0,
        ;
    dlocal
        pp_current_state, standard_output,
        pp_states = [], pp_streams = [],
        poplinewidth = false, print_right_margin,
        ;

    PP_BUFFER -> buffer;
    rev(PP_EVENTS) -> events;
    PP_BUFFER_I - 1 -> lim;

    SET_CUCHAROUT;
    if cucharout == charout then
        pop_charout_col
    elseif cucharout == charerr then
        pop_charerr_col
    elseif cucharout == fpr$-f_charout then
        fpr$-f_column
    else
        0
    endif ->> col -> sect;

    if col == 0 then
        `\n` -> prev_c
    endif;

    if vedinvedprocess then
        vedindentstep
    else
        8
    endif -> pptab;

    unless isinteger(print_right_margin) do
        if vedinvedprocess then vedlinemax else 76 endif -> print_right_margin
    endunless;

    define Do_indent();
        lvars i;
        returnunless(lb);
        lb_indent(lb) -> i;
        while col < i do
            cucharout(`\s`);
            col + 1 -> col;
            `\s` -> prev_c
        endwhile
    enddefine;

    define Prefix_len(lb);
        lvars p;
        if (lb_prefix(lb) ->> p) or (lb_line_prefix(lb) ->> p) then
            datalength(p)
        else
            0
        endif
    enddefine;

    define Suffixes_len() -> len;
        lvars len = 0, s;
        dlocal lb, lb_stack;
        while lb do
            if (lb_suffix(lb) ->> s) then
                datalength(s) + len -> len
            endif;
            destpair(lb_stack) -> (lb, lb_stack)
        endwhile
    enddefine;

    define Do_print_lines_exit();
        lvars s;
        unless prev_c == `\s` or prev_c == `\n` do
            cucharout(`\s`)
        endunless;
        cucharout(`.`);
        cucharout(`.`);
        while lb do
            if (lb_suffix(lb) ->> s) then
                appdata(s, cucharout)
            endif;
            destpair(lb_stack) -> (lb, lb_stack)
        endwhile;
        exitfrom(output_pp_events)
    enddefine;

    define Output_newline(pos);
        if isinteger(print_lines)
        and line >= print_lines then
            Do_print_lines_exit()
        endif;
        pos -> last_nl_pos;
        cucharout(`\n`);
        `\n` -> prev_c;
        line + 1 -> line;
        if line_prefix then
            appdata(line_prefix, cucharout);
            datalength(line_prefix)
        else
            0
        endif -> col
    enddefine;

    define Outchar(c);
        cucharout(c);
        col + 1 -> col;
        c -> prev_c
    enddefine;

    define Outchars_to(j, check);
        lvars slen = 0;
        returnif(i > j);
        if check then
            if line == print_lines then
                Suffixes_len() + 3 -> slen      ;;; 3 for <space>..
            endif;
            if Exceeds_line(col, i, j + slen) then
                repeat (print_right_margin - slen) - col times
                    Outchar(subscrs(i, buffer));
                    i + 1 -> i
                endrepeat;
                if slen /== 0 then
                    Do_print_lines_exit()
                else
                    ;;; cucharout(`!`);
                    Output_newline(i);
                    Do_indent();
                    fast_chain(j, true, Outchars_to) ;;; infinite loop poss??
                endif
            endif
        endif;
        for i from i to j do
            Outchar(subscrs(i, buffer))
        endfor
    enddefine;


    /* Main output loop starts here */

    until events == [] do
        destpair(events) -> (e, events);
        if islogical_block(e) then
            if lb then
                lb_indent(lb) -> lb_indent(e);
                Outchars_to(lb_start(e) - 1, true)
            else
                if col + Prefix_len(e) >= print_right_margin then
                    Output_newline(-1)
                endif;
                col -> lb_indent(e);
                lb_start(e) -> i
            endif;
            conspair(lb, lb_stack) -> lb_stack;
            e -> lb;
            col ->> sect -> lb_start_col(lb);
            if isinteger(print_miser_width) then
                col >= (print_right_margin - print_miser_width)
                    -> lb_miser(lb)
            endif;
            if (lb_line_prefix(lb) ->> x) then
                x -> line_prefix
            endif
        elseif iscond_newline(e) then
            Outchars_to(cn_left_start(e) - 1, true);
            Set_cn_right_end(e, events, lim);
            cn_left_end(e) -> le;
            Skip_back_trailing_spaces(buffer, i, le) -> x;
            Outchars_to(x, true);
            if Decide_newline(e, buffer, col + (le - x), last_nl_pos) then
                Output_newline(le);
                Do_indent()
            else
                Outchars_to(le, false)
            endif;
            cn_right_start(e) -> i;
            col -> sect
        elseif ispp_indent(e) then
            Outchars_to(ppi_pos(e), true);
            unless lb_miser(lb) do
                /* Set target column for indentation */
                ppi_val(e) -> x;
                if ppi_rel(e) == @:BLOCK then
                    x + (lb_block_col(lb) or lb_start_col(lb)) -> x
                elseif ppi_rel(e) == @:CURRENT then
                    x + col -> x
                endif;
                if x < print_right_margin then x -> lb_indent(lb) endif
            endunless
        elseif ispp_tab(e) then
            Outchars_to(ppt_pos(e), true);
            repeat min(Do_pp_tab(explode(e) ->, col, sect),
                       print_right_margin - col)
            times
                Outchar(`\s`)
            endrepeat
        elseif ispair(e) then
            Outchars_to(back(e), true);
            go_on front(e) to
             NEWLINE
             FRESH_LINE
             TAB
             BLOCK_COL
             ENDBLOCK
             FINISH_OUTPUT
             FORCE_OUTPUT
             CLEAR_OUTPUT
            else ERR;

            NEWLINE:
                Output_newline(i - 1);
                nextloop;

            FRESH_LINE:
                unless prev_c == `\n` do Output_newline(i - 1) endunless;
                nextloop;

            TAB:
                repeat pptab - ((col - 1) rem pptab) times
                    Outchar(`\s`)
                endrepeat;
                nextloop;

            BLOCK_COL:
                col ->> sect ->> lb_block_col(lb) -> lb_indent(lb);
                nextloop;

            ENDBLOCK:
                Outchars_to(lb_end(lb), true);
                if lb_line_prefix(lb) then
                    false -> line_prefix
                endif;
                unless ispair(lb_stack) do
                    mishap(e, 1, 'System error: unexpected pp event')
                endunless;
                destpair(lb_stack) -> (lb, lb_stack);
                if lb and (lb_line_prefix(lb) ->> x) then
                    x -> line_prefix
                endif;
                nextloop;

            FINISH_OUTPUT:
                finish_output(standard_output) ->;
                nextloop;

            FORCE_OUTPUT:
                force_output(standard_output) ->;
                nextloop;

            CLEAR_OUTPUT:
                return;
        else
            ERR:
            mishap(e, 1, 'System error: unrecognised pp event')
        endif
    enduntil
enddefine;


/* Interface to pretty printer used by individual lisp_class_print
    procedures (in C.all/lisp/src/print.p)
*/

define print_logical_block(prefix, suffix, print_p);
    if print_pretty == nil then
        if prefix then appdata(prefix, cucharout) endif;
        fast_apply(print_p);
        if suffix then appdata(suffix, cucharout) endif
    else
        pprint_logical_block(standard_output, false,
                         prefix, suffix, false, print_p)
    endif
enddefine;


define print_newline(kind);
    if print_pretty /== nil then
        pprint_newline(kind, standard_output)
    elseif kind == @:MANDATORY then
        write_standard_output(`\n`)
    endif
enddefine;


define print_indent(rel, val);
    if print_pretty /== nil then
        pprint_indent(rel, val, standard_output)
    endif
enddefine;


endsection;
