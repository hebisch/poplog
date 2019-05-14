/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/lib/geom3d.p
 >  Purpose:        3-D geometry package
 >  Author:         David Hogg (see revisions)
 >  Documentation:
 >  Related Files:  LIB *GEOMETRY
 */

#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +oldvar;

recordclass point3 xxof yyof zzof;

recordclass axis translation linear;

vars mtopars scale newvmatrix conspoint axispoints
        move fmmmult fmpmult fppadd;
        ;;; Forward declarations to fix BR isl-fr.4406

define praxis axis;
    vars pop_pr_places x y z pan tilt roll mirror;
    1 -> pop_pr_places;
    destpoint3(translation(axis)) -> z -> y -> x;
    mtopars(linear(axis)) -> mirror -> scale -> roll -> tilt -> pan;
    pr('x='); pr(x); pr('  y='); pr(y); pr('  z='); pr(z);
    pr('  pan='); pr(pan); pr('  tilt='); pr(tilt); pr('  roll='); pr(roll);
    pr('  scale='); pr(scale);
    if mirror = 1 then pr('  x-reflected'); endif;
    nl(1);
enddefine;

define mmmult aa bb => cc;
    vars i j;
    newvmatrix([1 3 1 3],0) -> cc;
    for j from 1 to 3 do
        for i from 1 to 3 do
            aa(1,j)*bb(i,1)+aa(2,j)*bb(i,2)+aa(3,j)*bb(i,3) -> cc(i,j);
        endfor;
    endfor;
enddefine;

define smmult scalar matrix => m;
    vars i j;
    newvmatrix([1 3 1 3],0) -> m;
    for j from 1 to 3 do
        for i from 1 to 3 do
            scalar*matrix(i,j) -> m(i,j);
        endfor;
    endfor;
enddefine;

define mpmult matrix point => result;
    vars j;
    {point3 0 0 0} -> result;
    for j from 1 to 3 do
        matrix(1,j)*xxof(point)+matrix(2,j)*yyof(point)+matrix(3,j)*zzof(point)
            -> result(j+1);
    endfor;
enddefine;

define pp3add p1 p2;
    {point3 %xxof(p1)+xxof(p2),yyof(p1)+yyof(p2),zzof(p1)+zzof(p2)%};
enddefine;

define mappoint point axis;
    pp3add(translation(axis),mpmult(linear(axis),point));
enddefine;

define mapoutaxis axis1 axis2;
    {axis %mappoint(translation(axis1),axis2),mmmult(linear(axis2),
             linear(axis1))%};
enddefine;

define rxmatrix a => m;
    vars cosa sina;
    newvmatrix([1 3 1 3],0) -> m;
    cos(a) -> cosa; sin(a) -> sina;
    cosa -> m(2,2); -sina -> m(2,3);
    sina -> m(3,2); cosa -> m(3,3);
    1 -> m(1,1);
enddefine;

define rymatrix a => m;
    vars cosa sina;
    newvmatrix([1 3 1 3],0) -> m;
    cos(a) -> cosa;  sin(a) -> sina;
    cosa -> m(1,1); sina -> m(1,3);
    -sina -> m(3,1); cosa -> m(3,3);
    1 -> m(2,2);
enddefine;

define rzmatrix a => m;
    vars cosa sina;
    cos(a) -> cosa; sin(a) -> sina;
    newvmatrix([1 3 1 3],0) -> m;
    cosa -> m(1,1); -sina -> m(1,2);
    sina -> m(2,1);  cosa -> m(2,2);
    1 -> m(3,3);
enddefine;

define makeaxis x y z pan tilt roll scale;
    ;;; consaxis(conspoint3(x,y,z),
    ;;; smmult(scale,mmmult(rzmatrix(roll),mmmult(rxmatrix(tilt),rymatrix(pan)))));
    vars cosa sina cosb sinb cosc sinc a b c m;
    cos(pan) -> cosa;  sin(pan) -> sina;
    cos(tilt) -> cosb;  sin(tilt) -> sinb;
    cos(roll) -> cosc;  sin(roll) -> sinc;
    newvmatrix([1 3 1 3],0) -> m;
    scale*(cosc*cosa+sinc*sinb*sina) -> m(1,1);
    scale*sinc*cosb -> m(2,1);
    scale*(sinc*sinb*cosa-cosc*sina) -> m(3,1);
    scale*(cosc*sinb*sina-sinc*cosa) -> m(1,2);
    scale*cosc*cosb -> m(2,2);
    scale*(sinc*sina+cosc*sinb*cosa) -> m(3,2);
    scale*cosb*sina -> m(1,3);
    -scale*sinb -> m(2,3);
    scale*cosb*cosa -> m(3,3);
    ;;;  scale*cosa -> m(1,1);  0 -> m(2,1);  -scale*sina -> m(3,1);
    ;;;  scale*sinb*sina -> m(1,2);  scale*cosb -> m(2,2);  scale*sinb*cosa -> m(3,2);
    ;;;  scale*cosb*sina -> m(1,3);  -scale*sinb -> m(2,3);  scale*cosb*cosa -> m(3,3);
    consaxis(conspoint3(x,y,z),m);
enddefine;

define project point;
    vars s;
    ;;; 1000/zzof(point) -> s;
    conspoint(round(xxof(point)+64),round(31-yyof(point)));
enddefine;

define mtopars m => pan tilt roll scale mirror;
    vars m11 m21 m31 m12 m22 m32 m13 m23 m33 cosb;
    m(1,1) -> m11; m(2,1) -> m21; m(3,1) -> m31;
    m(1,2) -> m12;  m(2,2) -> m22;  m(3,2) -> m32;
    m(1,3) -> m13;  m(2,3) -> m23;  m(3,3) -> m33;
    ;;; if determinant(m) < 0 then
    ;;;     -m11 -> m11;  -m12 -> m12;  -m13 -> m13;
    ;;;     1 -> mirror;
    ;;; else
    0 -> mirror;
    ;;; endif;
    sqrt(m11*m11+m12*m12+m13*m13) -> scale;
    sqrt(m33*m33+m13*m13) -> cosb;
    arctan2(cosb,-m23) -> tilt;
    if cosb /= 0 then
        arctan2(m33,m13) -> pan;
        arctan2(m22,m21) -> roll;
    else
        arctan2(m11,m31) -> roll;
        0-> pan;
    endif;
enddefine;

define determinant m;
    m(1,1)*(m(2,2)*m(3,3)-m(2,3*m(3,2))
        + m(2,1)*(m(2,1)*m(3,3)-m(1,3)*m(3,2))
        + m(3,1)*(1,2)*m(2,3)-m(1,3)*m(2,2));
enddefine;

define axisscale axis;
    vars m;
    linear(axis) -> m;
    sqrt(m(1,1)*m(1,1)+m(1,2)*m(1,2)+m(1,3)*m(1,3));
enddefine;

syscancel("arctan2");
define arctan2 c s => degs;
    ;;; Produces an angle between -179 and 180 degrees from direction cosines.
    if abs(c) > abs(s) then
        arctan(s/c) -> degs;
        if c < 0.0 then 180+degs -> degs; endif;
        if degs < 0.0 then 360+degs -> degs; endif;
    else
        90 - arctan(c/s) -> degs;
        if s < 0.0 then 180+degs -> degs; endif;
    endif;
    round(degs) -> degs;
    if degs > 180 then degs - 360 -> degs endif;
enddefine;

define drawaxis axis;
    ;;; uses LIB AED
    vars mx my mz x y z lx rx ly ry lz rz;
    applist(axispoints,
            procedure point;
                    project(mappoint(point,axis));
            endprocedure)
        -> rz -> lz -> z -> mz -> ry -> ly -> y -> my -> rx -> lx -> x -> mx;
    move(mx); draw(x); move(lx); draw(x); draw(rx);
    move(my); draw(y); move(ly); draw(y); draw(ry);
    move(mz); draw(z); move(lz); draw(z); draw(rz);
enddefine;


vars axispoints;

[%
     conspoint3(-128,0,0) , conspoint3(128,0,0) , conspoint3(112,-8,0) , conspoint3(112,8,0),
     conspoint3(0,-128,0) , conspoint3(0,128,0) , conspoint3(-8,112,0) , conspoint3(8,112,0),
     conspoint3(0,0,-128) , conspoint3(0,0,128) , conspoint3(0,-8,112) , conspoint3(0,8,112) %]
    -> axispoints;

define fmappoint x y z axis;
    vars m t;
    destaxis(axis) -> m -> t;
    arrayvector(m) -> m;
    m(1)*x + m(2)*y + m(3)*z + xxof(t);
    m(4)*x + m(5)*y + m(6)*z + yyof(t);
    m(7)*x + m(8)*y + m(9)*z + zzof(t);
enddefine;

define fmapoutaxis axis1 axis2 result;
    fmappoint(translation(axis1),axis2,translation(result));
    fmmmult(linear(axis2),linear(axis1),linear(result));
enddefine;

define fmappoint point axis result;
    fmpmult(linear(axis),point,result);
    fppadd(translation(axis),result,result);
enddefine;

define fmpmult m p r;
    vars x y z;
    destpoint3(p) -> z -> y -> x;
    m(1,1)*x+m(2,1)*y+m(3,1)*z -> xxof(r);
    m(1,2)*x+m(2,2)*y+m(3,2)*z -> yyof(r);
    m(1,3)*x+m(2,3)*y+m(3,3)*z -> zzof(r);
enddefine;

define fppadd p1 p2  result;
    xxof(p1)+xxof(p2) -> xxof(result);
    yyof(p1)+yyof(p2) -> yyof(result);
    zzof(p1)+zzof(p2) -> zzof(result);
enddefine;

define fmmmult m1 m2 r;
    vars row col;
    for row from 1 to 3 do
        for col from 1 to 3 do
            m1(1,row)*m2(col,1)+m1(2,row)*m2(col,2)+m1(3,row)*m2(col,3) -> r(col,row);
        endfor;
    endfor;
enddefine;

/*  --- Revision History ---------------------------------------------------
--- John Williams, Jan 28 1992
        Fixed BR isl-fr.4406
--- Mark Rubinstein, Oct 28 1985
        Removed some POP-2isms
--- John Williams, Jul 30 1985
        replaced VECTOROF with ARRAYVECTOR
        added 'syscancel("arctan2")' because of conflict with system ARCTAN2
 */
