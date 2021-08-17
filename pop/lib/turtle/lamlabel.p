/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           C.all/lib/turtle/lamlabel.p
 |  Purpose:        line labelling package ?
 |  Author:         Unknown, ??? (see revisions)
 |  Documentation:
 |  Related Files:
 */

turtle();

define putfirst(list);
    ;;; move list to top of database
    remove(list);add(list)
enddefine;

define 9 ===> x;
    ;;; print x on a new line with two asterisks, and no brackets or "'".
    pr('\n** ');
    ppr(x)
enddefine;

define drawline(p1,p2);
    jumpto(dl(p1));
    drawto (dl(p2))
enddefine;

define paintof(orient,label)->paint;
    ;;; get turtle paint for the given orientation and label
    vars database up;
    "^" -> up;
    [[hrz left ^up] [hrz right V] [lft left <] [lft right >]
     [rht left <] [rht right >] [vrt left < ] [vrt right >]]
        -> database;
    lookup([^orient ^label ?paint])
enddefine;

define showlabels();
;;; takes the LINELABELLING produced by LABELPIC and
;;; redraws the PICTURE using the chevron convention
    vars label p1 p2 orient label;
    foreach [line ?orient ?label ?p1 ?p2] do
        paintof(orient,label)-> paint;
        drawline(p1,p2)
    endforeach;
    display();
enddefine;

define labelof(line);
    if length(line) = 5 then line(3) else undef endif
enddefine;

define getans()->line;
    vars num;
    'here are the lines\n' ===>;
    1->num;
    foreach [line ==] do
        pr(num); sp(2); pr(it); nl(1); num+1->num;
    endforeach;
    readline()->line;
enddefine;

vars n2 compatible database;

define getconstraints();
    ;;; used by READLABELS to get user-defined labels
    vars ans num linerec newrec label;
    pr('Type in line number and label (left,right,undef):\
    e.g. "3 left".');
    'When you have finished type no.'===>;
    until (getans() ->>ans) = [no] do
        if match([?num:isinteger ?label],ans)
                and (label=undef or label="left" or label="right")
        then    ;;; Move record to top, and delete old label if any.
            remove(database(num));
            it -> linerec; length(linerec) -> num;
            if num == 4 then linerec -> newrec
            else [% linerec(1),linerec(2),linerec(num-1),linerec(num) %]
                    -> newrec
            endif;
            add(newrec);

            unless label == undef then
                if compatible(newrec, label)
                then    remove(newrec);
                    add([% it(1),it(2),label,it(3),it(4)%])
                else 'directly incompatible with other labels.\n'===>;
                    if num == 5 then remove(newrec); add(linerec) endif
                endif
            endunless
        else ['Bad arguments given:' ^ans] ===>
        endif;
        'Next line and label please. Type "no" if finished'===>
    enduntil;
    'constraints noted are\n'===>;
    foreach [line = = = =] do it=> endforeach;
enddefine;

define readlabels();
    'do you wish to label some lines? yes or no ' ===>;
    if readline() = [no]
    then pr('O.K. no constraints\n')
    else getconstraints();
    endif
enddefine;

define findneighbour (p, line);
    ;;; Finds the unique neighbour to LINE at its endpoint P, if it has one.
    vars n rec;  0->n;
    foreach [line == ^p ==] do
        if it /= line then it -> rec; n + 1 -> n endif
    endforeach;
    if n == 1 and length(rec) == 4
    then putfirst(rec)
    endif
enddefine;

define findneighbours();
    ;;; Finds unlabelled neighbours at ells of all labelled lines.
    vars p1 p2 line;
    foreach [line = = ?p1 ?p2] do
        it -> line; findneighbour(p1, line); findneighbour(p2, line)
    endforeach
enddefine;

define moveline (p1,p2);
    if present([line = ^p1 ^p2]) or present([line = ^p2 ^p1])
    then putfirst(it)
    endif
enddefine;

define reorder;
    ;;; This function finds all lines which are part of cross-bars of TEEs,
    ;;; and moves them to the top of the database.
    vars p1 p2 p3;
    foreach [junc tee ?p1 ?p2 = ?p3] do
        moveline(p1, p2); moveline(p1, p)
    endforeach
enddefine;

define teecompat(tee, line, label, pointnum);
;;; checks attempt to MARK a line belonging to a TEE junnction
    vars teejunc lefttop stembottom righttop;
    tee(3) -> teejunc, tee(4) -> lefttop; tee(5) -> stembottom;
    tee(6) -> righttop;
;;; identify category of source line
    match ([== ^teejunc ^stembottom],line)
    or match([== ^stembottom ^teejunc], line)
    ;;; so its a stem line no constraints

;;; check to see if its a left cap
    or  (match ([== ^lefttop ^teejunc], line)
        and label = "left")

    or  (match ([== ^teejunc ^lefttop], line)
        and label = "right")

;;; check to see if its a right cap
    or  (match([== ^teejunc ^righttop],line)
        and label = "left")

    or  (match([== ^righttop ^teejunc], line)
        and label = "right")
enddefine;

define getother(line,point)->line2;
    foreach [line == ^point ==]
    do if it /= line then it -> line2; return endif
    endforeach;
    false -> line2;
enddefine;

define ellcompat (junc, line, label, pointnum) -> result;
    vars line2 point bool;
    junc(3)->point;
    getother(line,point) ->line2;
    if labelof(line2)=undef
    then   true->result;
        ;;; The next line only affects the speed of the search, not its result.
        ;;; It causes the unlabelled adjacent line to be labelled next.
        unless n2=0 or database(1)=line2 then putfirst(line2)
        endunless;
    else    ;;; There are 3 independent tests each of which reverse
        ;;; the overall compatibility: equality of labels of LINE
        ;;; and LINE2, the sense of LINE (coded by pointnum), and the sense of LINE2.
        ;;; BOOL may be 1,2,3, or 4: all that matters is whether it is even or odd.
        pointnum->bool;
        if labelof(line2)=label then bool+1->bool endif;
        if line2(5)=point then bool+1->bool endif;
        (bool=1 or bool=3)->result
    endif
enddefine;

define iscompat(point,line,label,position)->result;
    vars type;
    if present([junc ?type ^point ==])
    then
        if type="tee" then teecompat(it,line,label,position)
        elseif type="ell" then ellcompat(it, line, label,position)
        else ['illegal junc at' ^point]===>; false
        endif
    else ['No junction at' ^point] ===>; false
    endif -> result
enddefine;

define compatible(line,label)->result;
        ;;; Establishes the contexts of each end of the LINE and
        ;;; calls appropriate compatability tests

    vars firstp secp;line(3)->firstp;line(4)->secp;
    ;;;check context of both endpoints
    iscompat(firstp, line, label, 1)
    and
    iscompat(secp, line, label, 2); ->result
enddefine;

vars completelabel;

define mark(line,label);
    vars newrec;
    if compatible(line, label)
        ;;; Continue down this branch or fail back at once?
    then
        [%line(1), line(2), label, line(3), line(4)%] -> newrec;
        remove(line); add(newrec);
        completelabel();
        remove(newrec); add(line)
    endif
enddefine;

define completelabel();
    ;;; The recursive component of the search,
    ;;; uses absence of unlabelled LINES to terminate.
    ;;; Exit means failure, success leads to a recursive call in MARK.
    vars line;
    n2+1->n2;
    if present([line = = =])
    then  it->line;             ;;; Continue search.
        mark(line,"right");     ;;; Try one branch of tree.
        mark (line, "left")     ;;; Try alternative branch.
        ;;; Now fail by exiting - return up the call tree.
    else        ;;; Labelling complete.
        showlabels(); n+1->n;
         pr(n2); pr(' Attempts to label a line.\n');
       'do you want to see the LINES? yes or no '===>;
        unless readline()=[no]
        then   foreach [line ==] do it=> endforeach
        endunless;
        'Continue? yes or no'===>;
        if readline()=[no] then setpop() endif  ;;; Back to user level.
        ;;; Now continue search by returning to higher levels and earlier choicepoints.
    endif
enddefine;

vars n n2;  ;;; counters for labellings and recursive calls of completelabel.
define relabelpic();
    vars database it; 0->n; 0->n2;
     rev(database)->database;   ;;; This makes a copy so changes will
        ;;; not affect global version created by SEEPICTURE.
        ;;; And puts LINES at the top for GETCONSTRAINTS.
     readlabels();    ;;;get constraints on labelling
    findneighbours(); reorder();   ;;; These are not essential -
        ;;; they just speed up the search by re-ordering the database.
        ;;; Now attempt complete labelling
    completelabel();
    n2.pr; pr(' Attempts to label a line.\n\t');
    [^n 'labellings found altogether.']===>
enddefine;

define labelpic();
    seepicture();
    relabelpic()
enddefine;
