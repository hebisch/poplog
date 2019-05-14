/* --- Copyright University of Birmingham 2000. All rights reserved. ------
 > File:            $poplocal/local/auto/compilehere.p
 > Purpose:         Make it easy to set up a package with one file
                    that compiles others
 > Author:          Aaron Sloman, Mar  2 2000
 > Documentation:
 > Related Files:
 */


section;

define macro compilehere();

            ;;; Get pathname for THIS directory
    lvars
        file,
        thisdir =   sys_fname_path(popfilename);

    dlocal popnewline = true;
    ;;; ignore rest of line
    rdstringto([; ^newline]) -> file;

    ;;; Now read file names, one per line, and plant code to compile them.
    repeat;
        rdstringto([; ^newline]) -> file;
        ;;; Veddebug(file);
        quitif(file = nullstring);
        ;;; Veddebug(thisdir dir_>< file);
        "pop11_compile","(",thisdir dir_>< file,")",";"
    endrepeat;



enddefine;

endsection;
