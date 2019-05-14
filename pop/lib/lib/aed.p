/*  --- Copyright University of Sussex 1995.  All rights reserved. ---------
 >  File:           C.all/lib/lib/aed.p
 >  Purpose:        driver for AED display.
 >  Author:         Dave Hogg, Feb 1982 (see revisions)
 >  Documentation:
 >  Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +oldvar;

;;; Procedure names correspond to the AED manual

256 -> buffersize;
inits(buffersize) -> aedbuffer;  0 -> bufpt;
inits(1) -> aedinbuffer;
sysopen(device_open_name(popdeverr),2,true) -> aedserial;

define aedout byte;
    1 + bufpt -> bufpt;
    byte -> subscrs(bufpt,aedbuffer);
    if bufpt = buffersize then
        .flushaedbuffer;
    endif;
enddefine;

define flushaedbuffer;
    syswrite(aedserial,aedbuffer,bufpt);
    0 -> bufpt;
enddefine;

define aedin;
  erase(sysread(aedserial,aedinbuffer,1));
  aedinbuffer(1);
enddefine;

define setgraphic;
    aedout(27);
enddefine;

define setalpha;
    aedout(1);
    .flushaedbuffer;
enddefine;

define sen ftype otype rtype ctype ptype;
  aedout(71);
  aedout(ftype); aedout(otype); aedout(rtype);
  aedout(ctype); aedout(ptype);
enddefine;

define ffd;
  aedout(12);
enddefine;

define sec colour;
    aedout(67); spuout(colour);
enddefine;

define sbc colour;
    aedout(91); spuout(colour);
enddefine;

define  sap size font hspace vspace link;
    aedout(94); spuout(size); spuout(font);
    spuout(hspace); spuout(vspace); spuout(link);
enddefine;

define sct addr n;
    aedout(75);
    spuout(addr); spuout(n);
enddefine;

define sctr rl gl bl;
    spuout(rl); spuout(gl); spuout(bl);
enddefine;

define swm n;
  aedout(76); aedout(n);
enddefine;

define srm n1 n2 n3 n4;
  aedout(77); aedout(n1); aedout(n2); aedout(n3); aedout(n4);
enddefine;

define mov x y;
    aedout(81);  xyout(x,y);
enddefine;

define mvr dx dy;
    aedout(105); spsout(dx); spsout(dy);
enddefine;

define rcp;
  sen(46,46,46,77,46);
  aedout(106);
  xyin();
  sen(46,46,46,56,46);
enddefine;

define wpx colour;
    aedout(84); spuout(colour);
enddefine;

define wmp; aedout(107); enddefine;

define wmpr dx dy;
    spsout(dx); spsout(dy);
enddefine;

define rpx;
    aedout(89); spuin();
enddefine;

define dva x y;
    aedout(65); xyout(x,y);
enddefine;

define dvr dx dy;
    aedout(108); spsout(dx); spsout(dy);
enddefine;

define dmv; aedout(109); enddefine;

define dmvr dx dy;
    spsout(dx); spsout(dy);
enddefine;

define dcl radius;
    aedout(79); spuout(radius);
enddefine;

define dfc radius;
    aedout(110); spuout(radius);
enddefine;

define dfr x y;
    aedout(111); xyout(x,y);
enddefine;

define ifl; aedout(73); enddefine;

define bfl colour;
    aedout(66); spuout(colour);
enddefine;

define ofl; aedout(86); enddefine;

define scc colour1 colour2 blinktime;
    aedout(99); spuout(colour1); spuout(colour2); spuout(blinktime);
enddefine;

define ejc; aedout(85); enddefine;

define djc; aedout(100); enddefine;

define dca x y;
    aedout(112); xyout(x,y);
enddefine;

define ecu; aedout(53); enddefine;

define rjp;
    aedout(113); xyin();
enddefine;

define dai x y;
    aedout(114);  xyout(x,y);
enddefine;

define whs; aedout(88); enddefine;

define rhs; aedout(116); enddefine;

define whr; aedout(92); enddefine;

define rhr; aedout(97); enddefine;

define sho x;
    aedout(102); dpuout(x);
enddefine;

define hsr n;
    aedout(119); spsout(n);
enddefine;

define svo y;
    aedout(101); dpuout(y);
enddefine;

define vsr n;
    aedout(120); spsout(n);
enddefine;

define bso x y;
    aedout(103); dpuout(x); dpuout(y);
enddefine;

define rho;
    aedout(121);  dpuin();
enddefine;

define rvo;
    aedout(122); dpuin();
enddefine;

define epa; aedout(104); enddefine;

define szr x y;
    aedout(69); spuout(x); spuout(y);
enddefine;

define hom; aedout(95); enddefine;

define xyout x y;
    vars xlo xhi ylo yhi;
    x//256 -> xhi -> xlo;
    y//256 -> yhi -> ylo;
    aedout(16*xhi+yhi); aedout(xlo); aedout(ylo);
enddefine;

define spuout x;
    aedout(x);
enddefine;

define dpuout x;
    x//256->xhi->xlo;
    aedout(xhi);  aedout(xlo);
enddefine;

define spuin;
    vars buffer;
    initb(1) -> buffer;
    aed_parin(buffer,1);
    subscr(1,buffer)
enddefine;

define xyineight;
  vars xlo xhi ylo yhi;
  .flushaedbuffer;
  .aedin//16 -> xhi -> yhi;
  .aedin+256*xhi;
  .aedin+256*yhi;
enddefine;

define xyin;
  vars xhi yhi;
  .flushaedbuffer;
  (.aedin-48)//4 -> xhi -> yhi;
  256*xhi + 16*(.aedin-48) + (.aedin-48);
  256*yhi + 16*(.aedin-48) + (.aedin-48);
enddefine;

define wda buffer bytecount;
    .flushaedbuffer;
    aed_dmaout(buffer,bytecount,46);
enddefine;

define rda buffer bytecount;
    .flushaedbuffer;
    aed_dmain(buffer,bytecount,47);
enddefine;

vars xscale yscale xorigin yorigin;
0->xorigin; 0->yorigin; 1->xscale; 1->yscale;

define setframe frame1 frame2;
    dl(frame1)->ayhi->aylo->axhi->axlo;
    dl(frame2)->yhi->ylo->xhi->xlo;
    (axhi-axlo)/(xhi-xlo) -> xscale;
    axlo-xscale*xlo -> xorigin;
    (ayhi-aylo)/(yhi-ylo) -> yscale;
    aylo-yscale*ylo -> yorigin;
enddefine;

define scalex x;
    intof(x*xscale+xorigin);
enddefine;

define scaley y;
    intof(y*yscale+yorigin);
enddefine;

define unscalex x;
    intof((x-xorigin)/xscale);
enddefine;

define unscaley y;
    intof((y-yorigin)/yscale);
enddefine;

define move;
  vars x y;
  .destpoint -> y -> x;
    mov(scalex(x),scaley(y));
enddefine;

define draw;
  vars x y;
  .destpoint -> y -> x;
    dva(scalex(x),scaley(y));
enddefine;

define palette;
    aedout(12);
    .setgraphic;
    1 -> colour;
    0 -> red;
    while red <= 255 do
        0 -> green;
        while green <= 255-red do
            255-(red+green) -> blue;
            sct(colour,1);
            sctr(red,green,blue);
            sec(colour);
            colour+1 -> colour;
            mov(intof(1.8*red)+32,intof(1.8*green)+32);
            drcl(27);  .ifl;
            green+32 -> green;
        endwhile;
        red+32 -> red;
    endwhile;
    .setalpha;
enddefine;

define dalpha string x y;
    move(x,y);
    .setalpha;
    pr(string);
    .setgraphic;
enddefine;

define block xlo xhi ylo yhi;
    move(xlo,ylo);
    dfr(scalex(xhi),scaley(yhi));
enddefine;

define resetaed;
    .setgraphic;
    aedout(48);
enddefine;

define setcolortable red green blue;
  .setgraphic;
  sct(0,64);
  for i from 1 to 64 do
    sctr(red(i),green(i),blue(i));
  endfor;
enddefine;

define macro zap;
  .setgraphic; .ffd; .setalpha;
enddefine;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  3 1995
        Now sets compile_mode +oldvar.
 */
