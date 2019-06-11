/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/ved/term/popc_declare.ph
 > Purpose:         Identifier declarations for POPC
 > Author:          John Gibson, Nov 13 1992
 */

library_declare_section '$popvedlib/term/'

section;

weak global constant procedure (
        vedansikeys,
        vedansiscreen,
        vedbbckeys,
        vedbbcscreen,
        vedbbcvt100keys,
        vedbbcvt100screen,
        vedciferkeys,
        vedciferscreen,
        vedcon80x25keys,
        veddxtermkeys,
        veddxtermscreen,
        vedhpscreen,
        vedhptermkeys,
        vedhptermscreen,
        vedhpxtermkeys,
        vediris_ansikeys,
        vedncdxtermkeys,
        vedsun_cmdkeys,
        vedsun_cmdscreen,
        vedsunkeys_NOSUNVIEW,
        vedsunkeys_SUNVIEW,
        vedsunscreen,
        vedsunxtermkeys,
        vedtvi925keys,
        vedtvi925screen,
        vedtvikeys,
        vedtviscreen,
        vedvi200keys,
        vedvi200screen,
        vedvi500keys,
        vedvi500screen,
        vedvi550keys,
        vedvi550screen,
        vedvi55keys,
        vedvi55screen,
        vedvt200keys,
        vedvt200screen,
        vedvt220keys,
        vedvt220screen,
        vedvt300keys,
        vedvt300screen,
        vedvt320keys,
        vedvt320screen,
        vedvt52keys,
        vedvt52screen,
        vedvt52screengraphtrans,
        vedvt52screenxy,
        vedxtermkeys,
        vedxtermscreen,
    );

weak global vars procedure (
        vedsun,
        vedsunkeys,
        vedsunrefresh,
        vedsunwindowsize,
    );

weak global vars
        vedsuninwindow,
    ;

endsection;

end_library_declare_section;
