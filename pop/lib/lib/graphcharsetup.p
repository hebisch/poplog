/* --- Copyright University of Sussex 1992.  All rights reserved. ---------
 > File:           C.all/lib/lib/graphcharsetup.p
 > Purpose:        set up variable names for graphic chars for diff terminals
 > Author:         Jonathan Laventhol, July 29 1983 (see revisions)
 > Documentation:  None (now defunct).
 */

#_TERMIN_IF DEF POPC_COMPILING

   /****************************************************************
    *                                                              *
    *           N.B. THIS FILE IS NOW REDUNDANT                    *
    *   There is now a  standard set of VED graphics chars         *
    *   containing all the line-drawing characters plus a few      *
    *   others, all of  which have string/character constant       *
    *   syntax.                                                    *
    *                                                              *
    *   However, the  standard set  does  not support  the  other  *
    *   chars defined below (but you are ill-advised to use        *
    *   these anyway, because they don't have acceptable default   *
    *   substitute characters).                                    *
    *                                                              *
    ****************************************************************/

printf(';;; NOTE: lib graphcharsetup is now superseded by a standard set\n');
printf(';;; of graphics characters.\n');
printf(';;; Please see \'VED Standard Graphics Characters\' in REF VEDPROCS.\n');


;;; currently only works for V200, V55 and VT100-type terminals.
;;; any unknown terminal will give printable characters.
;;;
;;; Procedures:
;;;     only graphcharsetup, isgraphiccode, and the
;;;     variables for the characters are exported.  Graphcharsetup and the
;;;     variable graphcharsetupdone are effectively exported
;;;
;;;     Graphcharsetup(terminal)
;;;         terminal can be either "vi200", "vt52", "vt100" , "v55" or "print".
;;;
;;;     isgraphiccode(c)
;;;         true if c is a valid graphic code

section $-graphchars => isgraphiccode graphcharsetup grapcharsetuptrap
    graph_horz graph_vert graph_cross graph_topleft graph_topright
    graph_botleft graph_botright graph_teeup graph_teedown graph_teeleft
    graph_teeright graph_degree graph_plusminus graph_rightarrow
    graph_leftarrow graph_uparrow graph_downarrow graph_dotdot graph_divide
    graph_yen graph_cent graph_pound graph_sub0 graph_sub1 graph_sub2
    graph_sub3 graph_sub4 graph_sub5 graph_sub6 graph_sub7 graph_sub8
    graph_sub9 graph_para graph_diamond graph_checkerboard graph_ht graph_ff
    graph_cr graph_lf graph_nl graph_vt graph_lesseq graph_greateq graph_pi
    graph_noteq graph_dot
    ;

global vars
    graphcharsetupdone = false,
    graph_horz graph_vert graph_cross graph_topleft graph_topright
    graph_botleft graph_botright graph_teeup graph_teedown graph_teeleft
    graph_teeright graph_degree graph_plusminus graph_rightarrow
    graph_leftarrow graph_uparrow graph_downarrow graph_dotdot graph_divide
    graph_yen graph_cent graph_pound graph_sub0 graph_sub1 graph_sub2
    graph_sub3 graph_sub4 graph_sub5 graph_sub6 graph_sub7 graph_sub8
    graph_sub9 graph_para graph_diamond graph_checkerboard graph_ht graph_ff
    graph_cr graph_lf graph_nl graph_vt graph_lesseq graph_greateq graph_pi
    graph_noteq graph_dot
procedure
    grapcharsetuptrap = erase,
    ;

define global Graphcharsetup(terminal);
    lvars entry, table_index, terminal, terminal_name, terminal_type;

    lconstant
        VT52    = 1,
        VT100   = 2,
        PRINT   = 3,
    ;

    lconstant terminal_types = [
        [vt52   ^VT52 ]
        [vi200  ^VT52 ]
        [v55    ^VT52 ]
        [vi55   ^VT52 ]
        ['vt1'  ^VT100]
        ['vt2'  ^VT100]
        ['vt3'  ^VT100]
        [xterm  ^VT100]
        [dxterm ^VT100]
        [xved   ^VT100]
        [print  ^PRINT]
    ];

    define lconstant macro GRAPH c;
        lvars c;
        c || 16:80;
    enddefine;

    lconstant graphic_chars = [
        /* name                  VT52        VT100           PRINT  */

        ;;; These are all in the standard set, and have character constant
        ;;; notation (see REF *ITEMISE). They are translated on output by
        ;;; -vedscreengraphtrans-.
        {graph_horz           %  `\G-`,      `\G-`,          `\G-`  %}
        {graph_vert           %  `\G|`,      `\G|`,          `\G|`  %}
        {graph_cross          %  `\G+`,      `\G+`,          `\G+`  %}
        {graph_topleft        %  `\Gtl`,     `\Gtl`,         `\Gtl` %}
        {graph_topright       %  `\Gtr`,     `\Gtr`,         `\Gtr` %}
        {graph_botleft        %  `\Gbl`,     `\Gbl`,         `\Gbl` %}
        {graph_botright       %  `\Gbr`,     `\Gbr`,         `\Gbr` %}
        {graph_teeup          %  `\Gtt`,     `\Gtt`,         `\Gtt` %}
        {graph_teedown        %  `\Gbt`,     `\Gbt`,         `\Gbt` %}
        {graph_teeleft        %  `\Glt`,     `\Glt`,         `\Glt` %}
        {graph_teeright       %  `\Grt`,     `\Grt`,         `\Grt` %}
        {graph_degree         %  `\Go`,      `\Go`,          `\Go`  %}
        {graph_diamond        %  `\G#`,      `\G#`,          `\G#`  %}
        {graph_dot            %  `\G.`,      `\G.`,          `\G.`  %}

        ;;; Rest are not in the standard set: they will only work when
        ;;; -pop_character_set- is false (set false below), which
        ;;; prevents the use of character codes 16:A1 -  16:FF in the
        ;;; ISO Latin character set. In any case, you shouldn't use the
        ;;; characters below because their default substitutes are mostly
        ;;; unacceptable (e.g. = for /= is utterly hopeless).
        {graph_plusminus      %  GRAPH `g`,  GRAPH `g`,        `+`  %}
        {graph_rightarrow     %  GRAPH `h`,        `>`,        `>`  %}
        {graph_leftarrow      %        `<`,        `<`,        `<`  %}
        {graph_uparrow        %        `^`,        `^`,        `^`  %}
        {graph_downarrow      %  GRAPH `k`,        `V`,        `V`  %}
        {graph_dotdot         %  GRAPH `i`,        `.`,        `.`  %}
        {graph_divide         %  GRAPH `j`,        `/`,        `/`  %}
        {graph_yen            %  GRAPH `p`,        `Y`,        `Y`  %}
        {graph_cent           %  GRAPH `q`,        `c`,        `c`  %}
        {graph_pound          %  GRAPH `r`,        `P`,        `P`  %}
        {graph_sub0           %  GRAPH `t`,        `0`,        `0`  %}
        {graph_sub1           %  GRAPH `u`,        `1`,        `1`  %}
        {graph_sub2           %  GRAPH `v`,        `2`,        `2`  %}
        {graph_sub3           %  GRAPH `w`,        `3`,        `3`  %}
        {graph_sub4           %  GRAPH `x`,        `4`,        `4`  %}
        {graph_sub5           %  GRAPH `y`,        `5`,        `5`  %}
        {graph_sub6           %  GRAPH `z`,        `6`,        `6`  %}
        {graph_sub7           %  GRAPH `{`,        `7`,        `7`  %}
        {graph_sub8           %  GRAPH `|`,        `8`,        `8`  %}
        {graph_sub9           %  GRAPH `}`,        `9`,        `9`  %}
        {graph_para           %  GRAPH `~`,        `P`,        `P`  %}
        {graph_checkerboard   %        `C`,  GRAPH `a`,        `C`  %}
        {graph_ht             %        `H`,  GRAPH `b`,        `H`  %}
        {graph_ff             %        `F`,  GRAPH `c`,        `F`  %}
        {graph_cr             %        `C`,  GRAPH `d`,        `C`  %}
        {graph_lf             %        `L`,  GRAPH `e`,        `L`  %}
        {graph_nl             %        `N`,  GRAPH `h`,        `N`  %}
        {graph_vt             %        `V`,  GRAPH `i`,        `V`  %}
        {graph_lesseq         %        `<`,  GRAPH `y`,        `<`  %}
        {graph_greateq        %        `>`,  GRAPH `z`,        `>`  %}
        {graph_pi             %        `p`,  GRAPH `{`,        `p`  %}
        {graph_noteq          %        `=`,  GRAPH `|`,        `=`  %}
    ];

    PRINT + 1 -> table_index;   ;;; default to printable characters
    unless vvedscreengraphic = nullstring then
        for entry in terminal_types do
            dl(entry) -> terminal_type -> terminal_name;
            if terminal_name == terminal
            or isstring(terminal_name)
            and isstartstring(terminal_name, terminal)
            then
                terminal_type + 1 -> table_index;
                quitloop;
            endif;
        endfor;
    endunless;

    for entry in graphic_chars do
        if table_index > datalength(entry) then
            mishap(terminal, 1, 'Unknown terminal type')
        else
            entry(table_index) -> valof(entry(1))
        endif
    endfor;
    ;;; run user assignable trap procedure
    grapcharsetuptrap(terminal);
enddefine;

define global graphcharsetup();
    if (systrmdev(popdevin) or vedusewindows == "x")
    and not(graphcharsetupdone) then
        vedsetup();
        Graphcharsetup(vedterminalname);    /* do it */
        true -> graphcharsetupdone;
        if pop_character_set then
            sysprmessage(0, 'graphcharsetup: SETTING pop_character_set FALSE', 'WARNING -', 1);
            ;;; This allows chars >= 16:A1 to be treated as graphic
            false -> pop_character_set
        endif
    endif;
enddefine;

define global isgraphiccode(c);
lvars c;
    c.isinteger and c < 256 and c >= 16:80
enddefine;

endsection; /* $-grapchars */

;;; now effectively export Graphcharsetup and grapcharsetupdone without
;;; removing them from the section;
syssynonym("graphcharsetupdone",
    word_identifier("graphcharsetupdone",
        section_subsect("graphchars", pop_section, false),
        false));
syssynonym("Graphcharsetup",
    word_identifier("Graphcharsetup",
        section_subsect("graphchars", pop_section, false),
        false));

/*  --- Revision History ---------------------------------------------------
--- John Gibson, Feb 10 1992
        Replaced chars now in standard VED graphics set with standard
        ones and added note at top of file, etc.
--- John Gibson, Dec 24 1991
        Added test for vedusewindows == "x" in graphcharsetup
--- John Gibson, Jul 12 1991
        Added xved to terminal_types
--- Rob Duncan, Jun 12 1990
        Tidied up -Graphcharsetup- and extended it to allow for other
        VTx00-type terminals (all treated as VT100) and lookalikes.
--- Rob Duncan, Jan 26 1990
        Deleted call to -vedtermsetup-
--- Aaron Sloman, Jan 22 1990
    Added vi55 option
--- John Gibson, Nov 19 1989
        Changed terminal name "v200" to "vi200"
--- Ben Rubinstein, Dec 16 1986 - made this check for vednographic having
    been loaded, so that this will pick up on it. eg because LIB BBC says
    its a vt52, but it doesn't have the graphic characters.
--- Mark Rubinstein, May 13 1986 - moved ved_inc and ved_graph_test to seperate
    files.  Made -graphcharsetupdone- and -Graphcharsetup- effectively
    exported, and added a new user definable procedure grapcharsetuptrap,
    called by Grapcharsetup with the name of the terminal.
--- Mark Rubinstein, Nov  7 1985 - ved_inc made global and exported.  Changed
    printable corners to slashes (/ and \) instead of all being asterisk (*).
--- Sak Wathanasin, Nov  7 1985 - merged with graphchars.p.
--- Aaron Sloman, Nov  7 1985 - modified to call vedsetup before vedtermsetup.
 */
