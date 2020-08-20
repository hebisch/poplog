/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/lr_parser/lib/lr_report.p
 > Purpose:         LALR(1) Parser Generator: the parser report file
 > Author:          Robert John Duncan, Nov 27 1992
 > Documentation:   HELP * LR_PARSER, REF * LR_PARSER
 > Related Files:
 */

compile_mode:pop11 +strict;

section $-LR =>
    lr_report,
;

uses lr_core;
uses lr_info;

;;; =========================================================================

;;; lr_report:
;;;     prints a summary report on the parser to the specified output.
;;;     output can be an output device, a character consumer or a file
;;;     name.

define global lr_report(parser, output);
    lvars   parser, output, info, nchars;
    dlocal  cucharout;

    define lconstant print_header();
        printf('Parser %p\n', [%
            parser_name(parser)
        %]);
        printf('%p states\n', [%
            datalength(parser_states(info))
        %]);
        printf('%p shift/reduce conflicts\n', [%
            parser_sr_conflicts(info)
        %]);
        printf('%p reduce/reduce conflicts\n', [%
            parser_rr_conflicts(info)
        %]);
    enddefine;

    define lconstant print_state(state);
        lvars state;

            ;;; print a symbol, in a field of at least 8 characters
        define lconstant print_symbol(symbol);
            lvars symbol;
            symbol_name(symbol) -> symbol;
            pr(symbol);
            fast_repeat max(0, 8 fi_- printlength(symbol)) times
                pr(space);
            endrepeat;
        enddefine;

            ;;; display an item
        define lconstant print_item(item);
            lvars item, symbol, dot = item_number(item);
            ;;; get the rule to which it belongs
            until isrule(item_next(item) ->> item) do /**/ enduntil;
            printf('\t[');
            pr_field(rule_number(item), nchars, `\s`, false);
            printf(']');
            printf('\s\s%p -->', [% symbol_name(rule_lhs(item)) %]);
            rule_rhs(item) -> item;
            repeat
                if item_number(item) == dot then
                    ;;; found the dot
                    printf('\s<.>');
                endif;
            quitunless(item_symbol(item) ->> symbol);
                printf('\s%p', [% symbol_name(symbol) %]);
                item_next(item) -> item;
            endrepeat;
            printf('\n');
        enddefine;

            ;;; print a shift or reduce action
        define lconstant print_action(action);
            lvars token, action, rest = [];

            define lconstant pr_action(conflict, token, action);
                lvars conflict, token, action;
                if conflict then printf('!') endif;
                printf('\t');
                print_symbol(token);
                if isstate(action) then
                    printf('\tshift\t%p', [% state_number(action) %]);
                else
                    printf('\treduce\t%p', [% rule_number(action) %]);
                endif;
                printf('\n');
            enddefine;

            destpair(action) -> (token, action);
            if ispair(action) then destpair(action) -> (action, rest) endif;
            pr_action(rest /== [], token, action);
            for action in rest do
                pr_action(true, token, action);
            endfor;
        enddefine;

            ;;; display the default action
        define lconstant print_default(rule);
            lvars rule;
            if rule == "accept" then
                printf('\tFINISHED\n');
            elseif rule then
                printf('\t$default$\treduce\t%p\n', [% rule_number(rule) %]);
            endif;
        enddefine;

            ;;; display a goto transition
        define lconstant print_goto(trans);
            lvars symbol, state, trans;
            destpair(trans) -> (symbol, state);
            printf('\t');
            print_symbol(symbol);
            printf('\tgoto\t%p\n', [% state_number(state) %]);
        enddefine;

        printf('State %p\n\n', [% state_number(state) %]);
        applist(state_kernel_items(state), print_item);
        printf('\n');
        applist(state_actions(state), print_action);
        print_default(state_default_action(state));
        unless state_actions(state) == [] and not(state_default_action(state))
        then
            printf('\n');
        endunless;
        applist(state_gotos(state), print_goto);
        unless state_gotos(state) == [] then printf('\n') endunless;
    enddefine;

    lvars close_output = false;
    Checkr_parser(parser) -> parser;
    Checkr_parser_info(parser) -> info;
    ;;; assign to -cucharout- from -output-
    unless isprocedure(output) then
        if isword(output) then
            output sys_><
                if hd(sys_os_type) == "vms"
                and sys_fname_extn(output) = nullstring
                then
                    '.lis'
                else
                    nullstring
                endif -> output;
        endif;
        if isstring(output) then
            true -> close_output;
        endif;
        discout(output) -> output;
    endunless;
    output -> cucharout;
    ;;; print the header
    print_header();
    printf('\n');
    ;;; print the states
    datalength(datalength(parser_rules(info)) sys_>< nullstring) -> nchars;
    appdata(parser_states(info), print_state);
    ;;; close the output if we opened it
    if close_output then cucharout(termin) endif;
enddefine;

endsection;     /* $-LR */
