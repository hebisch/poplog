/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/lib/geometry.p
 >  Purpose:        Geometry procedures.
 >  Author:         Aaron Sloman and David Hogg
 >  Documentation:  comments in this file
 >  Related Files:  LIB * GEOM3D
 */

#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +oldvar;

vars realtol disttol angtol;

0.0001 -> realtol;  0.1 -> disttol;  sin(5) -> angtol;

;;; A collection of geometry routines. This file contains functions for
;;; points, vecs and angles. Each of these is represented as a three element
;;; strip, defined below. angles are represented essentially as two
;;; direction-cosines (COS(ANG) and SIN(ANG)), and all operations on angles
;;; assume this. Facilities are provided to enable the user to type things in
;;; as if in degrees. So "90 DEGREES" means "THE ANGLE CORRESPONDING TO 90
;;; DEGREES".

;;;The file also defines additional types of objects, namely orientations,
;;;lines, and segments.
;;;In this file we also define, besides points and angles, vecs and directions.
;;;vecs are thought of as the "difference" between two points.
;;;directions are thought of as analogous to vecs, but having no
;;;magnitude. they are represented as if they were unitvecs, i.e. by the two direction-cosines.

;;;users may easily define the relevant basic functions as they like:
;;;for instance CONSPAIR could be CONSPOINT, FRONT and BACK could be XOF and YOF,
;;;and DESTPAIR could be DESTPOINT (but see below). The same functions could be used as
;;;values for CONSANGLE COSOF SINOF and DESTANGLE.






vars twopi degtorads;
pi*2        ->twopi;
pi/180 -> degtorads;

recordclass point xof yof;

recordclass angle cosof sinof;

recordclass vec xofvec yofvec;

recordclass direction cosofdir sinofdir;

define destpoint pt;
    if pt.ispoint then
        xof(pt); yof(pt);
    else
        pt;  ;;;Assume other coordinate is on the stack
    endif
enddefine;


define 2 degrees n;
    ;;;N represents an angle in degrees. convert N to internal representation.
    ;;;normally required only for user interface.
    consangle(cos(n),sin(n))
enddefine;

define macro degs;
    dl([% .numberread  degrees %])
enddefine;


;;;so inside functions you can write DEGS 90, DEGS 45, etc. An angle will be created
;;;at compile time.




define 9 indegrees ang;
    ;;;this is for converting angles, represented as an angle data structure
    ;;;to degrees. normally relevant only for user interface.
    ang.arc;
enddefine;


;;;Note: the define DESTPOINT redefined below is defined to accept either a
;;;   point data-structure, or two numbers. In either case it returns the
;;;   co-ordinates. User-defined versions of DESTPOINT should have similar
;;;   capabilities, for use with the turtle package, etc.



;;;functions for manipulating points.

define roundpt pt;
    conspoint(pt.xof.round,pt.yof.round);
enddefine;

define distance;
    ;;;Arguments on stack.
    ;;;Accept either two points or four numbers: two pairs of coordinates.
    ;;;This is handled by DESTPOINT.
    vars x1 y1 x2 y2;
    .destpoint ->y2->x2;
    .destpoint ->y1 ->x1;

    (x2-x1) ->x1;
    (y2-y1) ->y1;
    sqrt(x1*x1 + y1*y1)
enddefine;


define eqpoint;
    .distance < disttol;
enddefine;

define midpoint pta ptb;
    conspoint((pta.xof+ptb.xof)/2,(pta.yof+ptb.yof)/2);
enddefine;

define addcoords x1 y1 x2 y2;
    x1+x2; y1+y2;
enddefine;

define subcoords x2 y2 x1 y1;
    x2-x1, y2-y1
enddefine;

define addpointvec pt vec;
    ;;;given a point and a vec return new point got by adding them.
    conspoint(
        addcoords(
            destpoint(pt), destvec(vec)))
enddefine;

define ppvec pta ptb;
    ;;;return the vec from ptb to pta;
    consvec(
        subcoords(
            destpoint(ptb),destpoint(pta)))
enddefine;


define magnitude vec;
    sqrt( xofvec(vec)*xofvec(vec) + yofvec(vec)*yofvec(vec));
enddefine;

define dirofvec vec;
    ;;;divide the vec by its magnitude to get a unit vec.
    vars l; vec.magnitude ->l;
    if abs(l) <= disttol then mishap(vec, 652) endif;
    consdirection(xofvec(vec)/l, yofvec(vec)/l)
enddefine;

define multvec lngth vec;
    consvec(
        lngth*(vec.xofvec),lngth*(vec.yofvec));
enddefine;

vars vecofdir;
multvec ->vecofdir;      ;;;given magnitude and direction, return vec.



define findnewpt pt lngth dir;
    ;;;dir is a direction.
    ;;;find the point reached by moving from pt in direction dir a distance lngth.
    addpointvec(pt, vecofdir(lngth,dir));
enddefine;




;;;functions concerned with angles. angles include a pair of
;;;direction-cosines, accessed via SINEOF and COSOF.

define arc ang =>degs;
    ;;;converts pair of direction cosines to degrees in range 0 - 360
    vars c s;
    ang.destangle ->s ->c;
    if abs(c) > abs(s) then
        arctan(s/c)->degs;
        if c < 0.0 then 180+degs->degs; endif;
        if degs < 0.0 then 360+degs->degs; endif
    else
        90 - arctan(c/s) ->degs;
        if s < 0.0 then 180+degs->degs; endif
    endif;
enddefine;

define angleof dx dy;
    ;;;Find angle in degrees for direction of vec (DX DY)
    consangle(dx,dy) indegrees
enddefine;


define acute ang;
    ang.cosof >= -angtol and ang.sinof >= 0
enddefine;

define obtuse ang;
    ang.cosof <= angtol and ang.sinof >= 0
enddefine;

define isbiggerang  ang1  ang2;
    ;;;going counter clockwise from horizontal you'll meet ANG2 before ANG1;
    if sign(ang1.sinof) = sign(ang2.sinof)
    then
        if ang1.sinof >= 0.0 then ang1.cosof <= ang2.cosof else ang1.cosof >=ang2.cosof endif;
    else
        ang1.sinof < 0.0 or
        not(ang2.sinof< 0.0) and ang1.cosof <= ang2.cosof
    endif;
enddefine;

define ppdirection pta ptb;
    ;;;direction from PTA to PTB, represented as a unit vec.
    vars dx dy mag;
    ptb.xof - pta.xof ->dx;
    ptb.yof - pta.yof ->dy;
    sqrt(dx*dx + dy*dy) ->mag;
    consdirection(dx/mag, dy/mag)
enddefine;

define oppdir dir;
    consdirection(-cosofdir(dir) , -sinofdir(dir))
enddefine;

define angdir degs;
    ;;;rads in radians. produces an angle.
    consangle(degs.cos,degs.sin);
enddefine;

define addcosines cosa sina cosb sinb;
    cosa*cosb - sina*sinb, sina*cosb + cosa*sinb
enddefine;

define subcosines cosa sina cosb sinb;
    cosa*cosb + sina*sinb, sina*cosb - cosa*sinb
enddefine;

define destbothangs a b;
    a.destangle; b.destangle;
enddefine;

define angplus; .destbothangs.addcosines.consangle enddefine;
;;;a and b are angles represented by sine and cosine. find the result of adding them.

define angminus; .destbothangs.subcosines.consangle enddefine;
;;;%subtract angle b from angle a.

define eqang a b;
    ;;;replace a and b by cosines of the angle got by subtracting them.
    subcosines(destbothangs(a,b)) ->b ->a;
    a > 0 and abs(b) <= angtol
enddefine;

vars eqdir; eqang ->eqdir;


vars ppplus ppminus;        ;;;temporary.
addpointvec ->ppplus;
ppvec  ->ppminus;


vars dirplusang; angplus ->dirplusang;
vars dirminusang;  angminus ->dirminusang;
vars dirminusdir; angminus ->dirminusdir;


define parallel a b;
    ;;;equivalent to eqang(a,b) or eqang(oppdir(a),b).
    abs(a.sinofdir*b.cosofdir - a.cosofdir*b.sinofdir) <= angtol
enddefine;

define cornangle pta ptb ptc;
    ;;;positive angle from ba to bc
    dirminusdir(
        ppdirection(ptb,ptc),
        ppdirection(ptb,pta))
enddefine;

define rotate vec ang;
    ;;;can also be use to transform a point to new axes.
    vars x y c s;
    ang.destangle ->s ->c;
    vec.destvec ->y ->x;
    consvec(
        x*c - y*s,
        y*c + x*s)
enddefine;







;;;functions for lines, or lines and points, etc.
;;;assume every line has two fields called orientof and perpof.
;;;perpof is a number, representing the perpendicular distance
;;;from the origin to the line. for details see function pdperp.
;;;orientof is a orientation (represented by its sine and cosine).
;;;the angle is that between the line and the x axis, facing along the line from
;;;left to right, or, in the case of vertical lines, from bottom to top.
;;;thus, if the angle is a, in degrees, then -90 < a <= 90.
;;;so the cosine should always be positive. the function orientofdir ensures this.

recordclass line orientof perpof;


recordclass orient orientcos orientsin;


define lldistance l1 l2;
    ;;;Distance between two parallel lines;
    abs((l1.perpof) - (l2.perpof))
enddefine;

define orientofdir dir;
    ;;;Return orientation corresponding to an angle in range -89 to +91.

    vars c s;
    dir.destdirection ->s ->c;
    consorient(
        if abs(c) < 0.0174      ;;;Cosine of 89 degrees. See also ISMORERIGHT.
        then
            if s < 0.0 then -c, -s
            else c, s
            endif
        elseif c < 0.0
        then -c, -s
        else c, s
        endif)
enddefine;

define perporient o;
    ;;;return new orientation perpendicular to O.
    orientofdir(dirplusang(o,90 degrees))
enddefine;

define poperp pt o;
    ;;;Perpendicular distance from origin to line through point defined by O (an orientation).
    ;;;the distance is positive if the origin is on the "right" of the line.
    ;;;Instead of a point, accept two co-ordinates.
    vars x y;
    pt.destpoint ->y ->x;
    y*o.orientcos - x*o.orientsin
enddefine;

define poline pt o;
    ;;;line through point pt with orientation O.
    consline(o, poperp(pt,o))
enddefine;

define ppline pta ptb;
    
    ;;;line through two points.
    poline(pta, orientofdir(ppdirection(pta,ptb)))
enddefine;

define perpline pt l;
    ;;;return line perpendicular to l through pt;
    poline(pt, perporient(orientof(l)))
enddefine;

define distalong pt o;
    ;;;if l is the line through pt with orientation o, and p0 is the point where
    ;;;the perpendicular from origin to l meets l, then this function gives the
    ;;;distance along l from p0 to pt. it may be negative, if pt is to the "left" of
    ;;;p0.
    pt.xof*o.orientcos + pt.yof*o.orientsin
enddefine;

define ptalong ldist l;
    ;;;return the point on l whose distance from the perpendicular to l through
    ;;;origin is ldist.
    vars perp c s;
    l.perpof ->perp;
    l.orientof.destorient ->s ->c;
    conspoint(
        ldist*c - perp*s,
        perp*c + ldist*s)
enddefine;

define plproject pt l;
    ;;;projection of point pt onto the line l.
    ptalong(distalong(pt,l.orientof),l)
enddefine;

define footofperp l;
    ;;;return the point at the foot of the perpendicular from the origin to line l.
    ptalong(0,l);
enddefine;

define pldistance pt l;
    ;;;perpendicular distance from pt to line.
    ;;;positive if point is on "left" of line.
    poperp(pt,l.orientof) - l.perpof
enddefine;

define isponline pt l;
    ;;;is the point on the line: true or false.
    abs(pldistance(pt,l)) < disttol
enddefine;



;;;now some functions for testing whether things intersect. predicates begin with "is-".

define ispplcross pta ptb l;
    ;;;Does the segment joining PTA and PTB intersect line L?
    sign(pldistance(pta,l)) /= sign(pldistance(ptb,l))
enddefine;

define isppppcross pta ptb pt1 pt2;
    ;;;Does segment joining the first two points intersect the segment joining the second two?
    vars xa xb ya yb x1 x2 y1 y2 a12 a13 a14 a23 a24 a34;
    pta.destpoint ->ya ->xa;
    ptb.destpoint ->yb ->xb;
    pt1.destpoint ->y1 ->x1;
    pt2.destpoint ->y2 ->x2;
    (xa - xb)*(ya + yb) ->a12;
    (xa - x1)*(ya + y1) ->a13;
    (xa - x2)*(ya + y2) ->a14;
    (xb - x1)*(yb + y1) ->a23;
    (xb - x2)*(yb + y2) ->a24;
    (x1 - x2)*(y1 + y2) ->a34;
    sign(a12+a23-a13) /= sign(a12+a24-a14) and
    sign(a34-a14+a13) /= sign(a34-a24+a23)
enddefine;

define llintersect l1 l2;
    ;;;point of intersection of two lines.
    vars c1 s1 c2 s2 perp1 perp2 d;
    l1.perpof ->perp1;
    l1.orientof.destorient ->s1 ->c1;
    l2.perpof ->perp2;
    l2.orientof.destorient ->s2 ->c2;
    c1 * s2  -  s1 * c2  ->d;
    if abs(d) < angtol then
        mishap([%l1,l2%],651)
    else

        conspoint(
            (perp1*c2 - perp2*c1)/d,
            (perp1*s2 - perp2*s1)/d)
    endif;
enddefine;

define ppppintersect pta ptb pt1 pt2;
    ;;;return the point at which the lines through the first two and the
    ;;;second two intersect, or cause error (in llintersect) if lines are parallel.
    ;;;using llintersect is simple, but inefficient.
    llintersect(ppline(pta,ptb),ppline(pt1, pt2))
enddefine;





;;;a few functions for ordering points to correspond to the effects of
;;;the function orientofdir.

define ismoreright pta ptb;
    ;;;% If PTA is "moreright" than PTB, then the direction from PTB to PTA is canonical.
    ;;;See define ORIENTOFDIR. The constant 0.0174 corresponds roughly to
    ;;;the cosine of 89 degrees.
    if  abs(pta.xof - ptb.xof)/(abs(ptb.yof - pta.yof) + 0.0001) < 0.0174 then
        pta.yof >= ptb.yof
    else
        pta.xof > ptb.xof
    endif
enddefine;

define orderpoints pta ptb;
    ;;;returns the two points in the appropriate order.
    if ismoreright(ptb,pta) then pta, ptb else ptb, pta endif
enddefine;




;;;now segments.
;;;a segment has two points and a line defining it.
;;;the direction from end1of the segment to end2of the segment corresponds to
;;;the orientof the line.

recordclass seg end1of end2of lineof;


define seglength seg;
    distance(end1of(seg), end2of(seg))
enddefine;



define destsegpoints seg;
    end1of(seg); end2of(seg);
enddefine;


define orientofseg seg;
    orientof(lineof(seg));
enddefine;

define updaterof orientofseg val seg;
    val -> orientof(lineof(seg));
enddefine;


define perpofseg seg;
    perpof(lineof(seg));
enddefine;


define  updaterof perpofseg seg;
        -> perpofseg(lineof(seg));
enddefine;



define ppseg pta ptb;
    ;;;construct a segment, given two points.
    consseg(orderpoints(pta,ptb), ppline(pta,ptb))
enddefine;

define isponseg pt seg;
    ;;;Is the point on the segment?
    vars o d;
    if isponline(pt,seg.lineof) then
        seg.orientofseg ->o;
        distalong(pt,o) ->d;
        (disttol + d > distalong(seg.end1of,o)) and
        (distalong(seg.end2of,o) > d-disttol)
    else false
    endif;
enddefine;

define ispbetween pt pta ptb;
    ;;;is the first point between the other two?
    isponseg(pt, ppseg(pta,ptb))
enddefine;

define issegacrossline seg l;
    ;;;Does the segment SEG intersect the line L?
    if not(parallel(orientofseg(seg), orientof(l))) and ispplcross(seg.end1of,seg.end2of,l) then
        true else false
    endif
enddefine;


define issegacrossseg seg1 seg2;
    ;;;Do the two segments intersect?
    issegacrossline(seg1,lineof(seg2)) and
    issegacrossline(seg2, lineof(seg1))
enddefine;


define ssintersect seg1 seg2;
    ;;;Find the point where lines through the segments intersect.
    ;;;Or generate an error.
    llintersect(lineof(seg1),lineof(seg2));
enddefine;




define getorient x;
    ;;;Get the orientation of a line or segment.
    if x.isline then x.orientof
    elseif x.isseg then x.orientofseg
    else mishap(x, 'NOT LINE OR SEG', 2399)
    endif;
enddefine;

define getperp x;
    ;;;Get the perp of either a line or a segment.
    if x.isline then x.perpof
    elseif x.isseg then x.perpofseg
    else mishap(x, 'NOT LINE OR SEG', 2399)
    endif;
enddefine;

define lldistance l1 l2;
    ;;;Redefine this to take lines or segments as arguments.
    abs((l1.getperp) - (l2.getperp))
enddefine;

define isllparallel l1 l2;
    ;;;Check whether a pair of lines or segments is parallel, within ANGTOL.
    parallel(l1.getorient, l2.getorient)
enddefine;

vars colinetol;

define iscollinear l1 l2;
    ;;;Test whether two lines or segments are collinear.
    lldistance(l1,l2) < colinetol and isllparallel(l1,l2)
enddefine;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Oct 28 1985 - removed some POP-2isms
 */
