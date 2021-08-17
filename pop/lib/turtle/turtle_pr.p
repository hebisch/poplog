/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           C.all/lib/turtle/turtle_pr.p
 |  Purpose:        printing on the turtle picture
 |  Author:         Unknown, ??? (see revisions)
 |  Documentation:
 |  Related Files:
 */

;;; turtle_pr is used just like pr to print out pop11 objects (numbers,
;;; words,lists etc) on the turtle picture itself. The printing will be
;;; done in a straight line starting where the turtle is and going in the
;;; direction the turtle is facing (any angle will work)

uses turtle;
uses Markhere;

section $-turtle => turtle_pr;

define global turtle_pr(item);
    vars sl image ctr _old_Markhere Markhere;

    stacklength() -> sl;    ;;; save stacklength - restore at end

    ;;; redefine cucharout locally to collect up the characters that would have
    ;;; been printed in the list image
    [] -> image;
    0 -> ctr;
    procedure;
        define cucharout(char);
            consword(char,1) :: image -> image;
            ctr+1 -> ctr;
        enddefine;
        ;;; use spr because cursor gets left ON last char (ie not AFTER it)
        spr(item);
    endprocedure();

    ;;; image now has print image of item

    Markhere -> _old_Markhere;
    ;;; Markhere is used by turtle to actually mark the picture
    ;;; Plotto optimises so Markhere is not called twice in one picture
    ;;; cell (even if rounding of sloping lines calls for it).
    ;;; Modify so that it prints successive chars of print image and
    ;;; checks stack and exits from turtle_pr when it has finished
    procedure(sl);
        vars paint;
        if ctr = 0 then
            ;;; check stack is right length
            repeat stacklength() - sl times erase(); endrepeat;
            exitfrom(turtle_pr);
        else
            image(ctr)-> paint;
            _old_Markhere();
            ctr - 1 -> ctr;
        endif;
    endprocedure(%sl%) -> Markhere;
    unless ctr < 1 then
        draw(ctr * 2 + 2);
    endunless;
enddefine;

endsection;
