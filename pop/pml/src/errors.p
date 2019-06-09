/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/pml/src/errors.p
 > Purpose:         PML: Error reporting
 > Author:          Robert John Duncan, Oct 21 1994 (see revisions)
 */

section $-ml;

weak constant procedure ($-vededit);

vars ml_compiled_from_ved;  ;;; forward

vars
    ml_errors_in_ved = false,
        ;;; set <true> for special Ved treatment of errors (experimental)
    ml_errors_printed = false,
        ;;; count of number of messages printed for this topdec
    ml_errors_raised = false,
        ;;; count of number of errors raised in this topdec
    ml_errors_continue = false,
        ;;; whether to continue after an error
;

;;; ml_warning, ml_error:
;;;     general error reporting

define lconstant error_report(msg, file, line, kind);
    lvars msg, file, line, kind;
    dlocal cucharout = cucharerr, pop_pr_quotes = false;
    define dlocal pr(item);
        lvars item;
        if isstring(item) then
            ;;; so that Ved character attributes show up
            appdata(item, cucharout);
        else
            ml_pr(item);
        endif;
    enddefine;
    if testdef vededit
    and ml_errors_in_ved
    and ml_compiled_from_ved
    and isstring(vedlmr_errs_in_file)
    then
        if isstring(file) and isinteger(line) then
            ;;; display location of error (this doesn't work, because
            ;;; LMR always sets the file position to be wherever it read
            ;;; to as the very last thing it does; makes it all a bit
            ;;; pointless, really)
            weakref vededit(file); vedjumpto(line, vedcolumn);
        endif;
        weakref vededit(vedlmr_errs_in_file,
            procedure();
                false -> vedwriteable;
                vedveddefaults();
                false -> vedcompileable;
            endprocedure);
        vedendfile(); vedlinebelow(); vedpositionpush();
        vedcharinsert -> cucharout;
    else
        printf('\n');
    endif;
    if file then
        lvars dir = current_directory;
        if isstartstring(dir, file) then
            allbutfirst(datalength(dir), file) -> file;
        endif;
        printf('In file %p', [^file]);
        if line then printf(', line %p', [^line]) endif;
        printf(':\n');
    endif;
    printf('%S: ', [^kind]);
    if islist(msg) then
        ;;; list of culprits -- format string is below on the stack
        printf(/*format,*/ msg);
    elseif isprocedure(msg) then
        ;;; special routine -- extra args may be on the stack
        msg();
    else
        ;;; should be a string, but allow anything
        npr(msg);
    endif;
    if cucharout == vedcharinsert then
        cucharerr -> cucharout;
        vedcheck(); vedpositionpop(); vedcheck();
    endif;
    if isinteger(ml_errors_printed) then
        ml_errors_printed + 1 -> ml_errors_printed;
    endif;
enddefine;

define ml_warning =
    error_report(%'Warning'%);
enddefine;

define ml_error() with_nargs 3;
    error_report((), 'Error');
    if isinteger(ml_errors_raised) then
        ml_errors_raised + 1 -> ml_errors_raised;
    endif;
    if ml_errors_continue and popfilename then
        ;;; continue after this error, if compiling from a file (i.e.
        ;;; not interactive)
        false -> ml_errors_continue;
    else
        interrupt();
    endif;
enddefine;

;;; format_message:
;;;     convert Pop-style exception message

define format_message(n, msg) -> (msg, args);
    lvars args = conslist(n);
    if isvector(msg) then
        msg(1) -> msg;
    endif;
    if isstring(msg) and isstartstring('%', msg) then
        define Rewrite_message(msg, args) -> (msg, args);
            lvars index = 0, len = 40;
            lvars buffer = inits(len);
            define dlocal cucharout(c);
                if index >= len then
                    buffer <> buffer -> buffer;
                    datalength(buffer) -> len;
                endif;
                index + 1 -> index;
                c -> buffer(index);
            enddefine;
            rev([% printf(dl(rev(args)), msg) %]) -> args;
            substring(1, index, buffer) -> msg;
        enddefine;
        Rewrite_message(allbutfirst(1, msg), args) -> (msg, args);
    endif;
enddefine;

;;; ml_pr_exception:
;;;     display Poplog exception messages in PML style

define ml_pr_exception(n, msg, idstring, severity);
    error_report(
        format_message(n, msg),
        procedure(msg, culprits);
            printf('%P\n', [^msg]);
            unless culprits == [] then
                printf('\t'); applist(culprits, spr); printf('\n');
            endunless;
        endprocedure,
        popfilename, poplinenum,
        if severity == `I` then
            'Note'
        elseif severity == `W` then
            'Warning'
        else
            'Error'
        endif);
enddefine;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Apr 26 1996
        Changes for new exception printing
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Nov  8 1994
        Added ml_errors_continue so that errors needn't cause an immediate
        abort
 */
