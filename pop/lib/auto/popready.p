/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:           C.all/lib/auto/popready.p
 > Purpose:        provide a break proint into POP-11
 > Author:         S Hardy, March 1982 (see revisions)
 > Documentation:  HELP * POPREADY
 */

compile_mode:pop11 +strict;

section;

global vars
    popready_stack,
    popready_level  = 0,
    ;

define global vars popready();
    lvars temp;
    dlvars char_read = false, prompt;
    dlocal popnewline, proglist,
            popready_stack, popready_level = popready_level + 1,
            pop_pas_mode = false,
        ;

    consvector(stacklength()) -> popready_stack;
    ':' sys_>< popready_level sys_>< ': ' -> prompt;

    define lconstant my_interrupt();
        if char_read then
            if popexecute then popready() endif;
            exitto(popready)
        elseif popready_level <= 1 then
            setpop()
        else
            exitfrom(popready)
        endif
    enddefine;

    dlocal interrupt = my_interrupt;

    repeat
        procedure();
            false -> popnewline;
            false -> char_read;
            nprintf('Popready');
            sys_clear_input(pop_charin_device);
            subsystem_compile(
                    procedure();
                        dlocal popprompt = prompt;
                        charin();
                        true -> char_read
                    endprocedure, "pop11");
            ;;; deal with normal exits here - caused by typing EOF
            consvector(stacklength()) -> temp;
            explode(popready_stack);
            explode(temp);
            exitfrom(if caller(2) == my_interrupt then
                        ;;; carry on after the interrupt
                        my_interrupt
                     else
                        popready
                     endif)
        endprocedure()
        ;;; exitto comes out here - restart loop
    endrepeat
enddefine;


endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Aug 16 1993
        pop_pas_mode now an active var
--- John Williams, May  6 1993
        Now sets popprompt inside the character repeater passed to
        subsystem_compile (to bypass pop11_compile's assignment of
        ': ' to popprompt). Also removed dlocal of cucharout.
--- John Gibson, Jul  4 1989
        Added +strict
--- John Gibson, Apr 28 1989
        Simplified, tidied up. Made the call of -popready- from inside
        -my_interrupt- jsut carry on after the interrupt if terminated
        with Ctrl-Z.
--- John Williams, Oct  6 1988
        Now clears -pop_charin_device- instead of -popdevin-
--- John Gibson, Nov 11 1987
        Replaced -sys_purge_terminal- with -sys_clear_input-
--- Mark Rubinstein, Feb 1985 - Modified to work in ved on version 10
--- Aaron Sloman, July 1982 - Modified to add levels
 */
