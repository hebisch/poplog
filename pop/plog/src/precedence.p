/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/precedence.p
 > Purpose:         Prolog: operator tables with core operator precedences
 > Author:          Robert John Duncan, May 10 1993
 > Documentation:
 > Related Files:   C.all/plog/src/operators.p
 */


section prolog;

define lconstant precedence_table(entries);
    lvars entries;
    newproperty(entries, 64, NOPREC, "perm");
enddefine;

define prefix_prec =
    precedence_table([
        [:-         255]
        [?-         255]
        [\\\+       249]
        [module     249]
        [endmodule  249]
        [$-           8]
    ]);
enddefine;

define prefix_rprec =
    precedence_table([
        [:-         254]
        [?-         254]
        [\\\+       249]
        [module     248]
        [endmodule  248]
        [$-           8]
    ]);
enddefine;

define postfix_prec =
    precedence_table([
    ]);
enddefine;

define postfix_lprec =
    precedence_table([
    ]);
enddefine;

define infix_prec =
    precedence_table([
        [:-     255]
        [;      254]
        [->     253]
        [,      252]
        [$-       8]
    ]);
enddefine;

define infix_lprec =
    precedence_table([
        [:-     254]
        [;      253]
        [->     252]
        [,      251]
        [$-       7]
    ]);
enddefine;

define infix_rprec =
    precedence_table([
        [:-     254]
        [;      254]
        [->     253]
        [,      252]
        [$-       8]
    ]);
enddefine;

endsection;     /* prolog */
