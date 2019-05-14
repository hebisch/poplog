/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 *  File:           $usepop/master/C.all/lib/auto/yesno.p
 *  Purpose:        keep asking question until reply is yes or no
 *  Author:         Steven Hardy, September 1982 (see revisions)
 *  Documentation:  HELP POPPROCS /yesno
 *  Related Files:
 */

;;;     This procedure takes as argument a 'question' which it prints
;;;     on the terminal and then it reads a reply which must be
;;;     either YES or NO. YESNO returns 'true' if the answer is YES and
;;;     'false' if the answer is 'no'.

section;

define global procedure yesno(question);
lvars input question;
    question =>
    readline() -> input;
    if input = [yes] or input = [y] then
        return(true)
    elseif input = [no] or input = [n] then
        return(false)
    else
        [please answer either yes or no] =>
        yesno(question)
    endif
enddefine;

endsection;
