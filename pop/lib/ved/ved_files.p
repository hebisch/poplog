/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_files.p
 > Purpose:         List files
 > Author:          Aaron Sloman
 > Documentation:   REF *VEDCOMMS
 */
compile_mode :pop11 +strict;

section;

define vars ved_files;

    define Report_name;
        ;;; for use in ved_files
        printf( '\n%s : %p lines, %s%s%s', [%
                vedpathname,
                vvedbuffersize,
                if vedchanged then vedchanged sys_>< ' changes.'
                else 'unchanged.'
                endif,
                if vedcompileable and vedneedscompiling then ' needs compiling.'
                else nullstring
                endif,
                if (vedwriteable or vedwriteallfiles) and vedchanged then
                    ' needs writing.'
                else nullstring
                endif
                %] )
    enddefine;

    printf('\n\nFILES CURRENTLY BEING EDITED:');
    vedappfiles(Report_name);
    cucharout(`\n`);
enddefine;

endsection;
