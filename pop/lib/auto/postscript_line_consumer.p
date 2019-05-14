/*--- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/postscript_line_consumer.p
 > Purpose:         PostScript (d)string line consumer
 > Author:          Jonathan Meyer, Apr 1 1993 (see revisions)
 > Documentation:   REF * POSTSCRIPT_LINE_CONSUMER
 > Related Files:
 */
section;
compile_mode :pop11 +strict;

/*
postscript_line_consumer(file) -> line_cons
postscript_line_consumer(file, arg_list) -> line_cons
postscript_line_consumer(file, arg_list, font_name, font_height,
                        font_pixel_height, line_pixel_height,
                        colour_vec, X_width_p) -> line_cons
        Creates a consumer which takes (d)strings and generates postscript
        code for them.
*/

include vedscreendefs;

;;; Fonts
define lconstant font_names = newmapping([
    ['bookman'      {'Bookman-Light' 'Bookman-Demi' 'Bookman-LightItalic' 'Bookman-DemiItalic'}]
    ['courier'      {'Courier' 'Courier-Bold' 'Courier-Oblique' 'Courier-BoldOblique'}]
    ['avantgarde'   {'AvantGarde-Book' 'AvantGarde-Demi' 'AvantGarde-BookOblique' 'AvantGarde-DemiOblique'}]
    ['helvetica'    {'Helvetica' 'Helvetica-Bold' 'Helvetica-Oblique' 'Helvetica-BoldOblique'}]
    ['helvetica narrow'
                    {'Helvetica-Narrow' 'Helvetica-Narrow-Bold' 'Helvetica-Narrow-Oblique' 'Helvetica-Narrow-BoldOblique'}]
    ['palatino'     {'Palatino-Roman' 'Palatino-Bold' 'Palatino-Italic' 'Palatino-BoldItalic'}]
    ['new century schoolbook'
                    {'NewCenturySchlbk-Roman' 'NewCenturySchlbk-Bold' 'NewCenturySchlbk-Italic' 'NewCenturySchlbk-BoldItalic'}]
    ['times'        {'Times-Roman' 'Times-Bold' 'Times-Italic' 'Times-BoldItalic'}]
    ['charter'      {'Charter-Roman' 'Charter-Bold' 'Charter-Italic' 'Charter-BoldItalic'}]
    ['utopia'       {'Utopia-Regular' 'Utopia-Bold' 'Utopia-Italic' 'Utopia-BoldItalic'}]
    ['hln'  'helvetica narrow']
    ['ncs'  'new century schoolbook']
], 8, false, false)
enddefine;

define lconstant paper_names = newassoc([
    [a3     {'A3'       842 1191}]
    [a4     {'A4'       595 842}]
    [a5     {'A5'       420 595}]
    [a6     {'A6'       297 420}]
    [letter {'Letter'   612 792}]
])
enddefine;


define lconstant concat_lines(v);
    lvars s;
    nonwriteable consstring(#| for s in_vector v do explode(s), `\n` endfor |#)
enddefine;

;;; The following PostScript header is printed at the start of each document.
;;; It defines DSC comments that structure the document to conform with
;;; DSC 3.0. It also defines the PostScript code used to setup the fonts.

;;; (We use vectors of strings so we can insert comments into them.
;;; However, to save space, these are coerced into single strings.)

lconstant

preamble1 = concat_lines({
    '%%!PS-Adobe-3.0'
    '%%%%Creator: Poplog (postscript_line_consumer)'
/* PARAM 1 = date */
    '%%%%CreationDate: %S'
}),


preamble2 = concat_lines({
/* PARAM 1 = title */
    '%%%%Title: %S'
}),


preamble3 = concat_lines({

/* PARAM 1 = username */
    '%%%%For: %S'

/* PARAMS 2 - 5 = font names */
    '%%%%DocumentNeededResources: font %S %S'
    '%%%%+ font %S %S'

/* PARAMS 6 - 9 = ISOLatin1 font names */
    '%%%%DocumentSuppliedResources: font %S %S'
    '%%%%+ font %S %S'
    '%%%%+ font VT100Graphics VT100Graphics-Bold'

    '%%%%Orientation: Portrait'
/* PARAMS 10 - 12 = paper size: name, width, height */
    '%%%%DocumentMedia: %S %P %P'
/* PARAMS 13 - 16 = bounding box x1, y1, x2, y2 */
    '%%%%BoundingBox: %P %P %P %P'
    '%%%%PageOrder: Ascend'
    '%%%%Pages: (atend)'
    '%%%%EndComments'

    '%%%%BeginProlog'

    '/docsave save def'

    ;;; declare various shorthand procedures
    '/Bd {bind def} bind def'
    '/Mt {moveto} Bd'
    '/Lt {lineto} Bd'
    '/RLt {rlineto} Bd'
    '/Sc {setrgbcolor} Bd'
    '/Slw {setlinewidth} Bd'
}),

preamble4 = concat_lines({
    '/Sh {show} Bd'
    '/Stringwidth {stringwidth pop} Bd'
}),

preamble4a = concat_lines({

    '/vWorkString ( ) def'

    ;;; <string> Sh  (show for X adjust)
    '/Sh {'
    '  { currentpoint exch vWorkString 0 4 index put vWorkString show'
    '    vCurrXCharPixWidths 3 index get vMeanWidth mul vXMeanPixWidth div'
    '    add exch Mt pop'
    '  } forall'
    '} Bd'

    ;;; <string> Stringwidth <width>  (stringwidth for X adjust)
    '/Stringwidth {'
    '  0 exch'
    '  { vCurrXCharPixWidths exch get vMeanWidth mul vXMeanPixWidth div add'
    '  } forall'
    '} Bd'
}),

preamble5 = concat_lines({

    ;;; define SelectFont function to use selectfont if it is available
    '/languagelevel where'
    '{pop languagelevel} {1} ifelse'
    '2 lt {'
    '/SelectFont { exch findfont exch dup type /arraytype eq {makefont} {scalefont} ifelse'
    '   setfont } Bd'
    '} {'
    '/SelectFont /selectfont load def'
    '} ifelse'

    ;;; <fontname> <font height> <pixwidth array> SF
    '/SF { /vCurrXCharPixWidths exch def SelectFont } Bd'

    ;;; function used to draw characters of VT100 graphics font

    '/HVLine {'
    '   /vl exch def /hl exch def'
    '   vGLx100 0 0 vGLyb vGLx100 vGLyt setcachedevice'
    '   newpath'
    '   hl 0 ne {'
    '       hl 2 eq {vGLx50} {0} ifelse vGLym Mt'
    '       hl 1 eq {vGLx50} {vGLx100} ifelse vGLym Lt'
    '   } if'
    '   vl 0 ne {'
    '       vGLx50 vl 2 eq {vGLym} {vGLyt} ifelse Mt'
    '       vGLx50 vl 1 eq {vGLym} {vGLyb} ifelse Lt'
    '   } if'
    '   stroke'
    '} Bd'

    ;;; Creates a font for drawing VED vt100 graphics characters
    ;;; Usage: <fontname> <linewidth> DefVT100GraphicsFont

    '/DefVT100GraphicsFont {'
    '   10 dict begin'
    '       /FontType 3 def'
    '       /FontMatrix [.01 0 0  .01 0 0] def'
    '       /FontBBox [0 0 100 100] def'
    '       /Encoding 256 array def'
    '       /LineWidth exch def'
    '       0 1 255 {Encoding exch /.notdef put} for'
    '       Encoding'
    '       dup 65  /leftend put'
    '       dup 66  /rightend put'
    '       dup 67  /hline put'
    '       dup 68 /bottomend put'
    '       dup 69 /bottomleft put'
    '       dup 70 /bottomright put'
    '       dup 71 /bottomtee put'
    '       dup 72 /topend put'
    '       dup 73 /topleft put'
    '       dup 74 /topright put'
    '       dup 75 /toptee put'
    '       dup 76 /vline put'
    '       dup 77 /lefttee put'
    '       dup 78 /righttee put'
    '       dup 79 /cross put'
    '       dup 81 /diamondsign put'
    '       dup 90 /hairspace put'
    '       pop'
    '       /CharProcs 19 dict def'
    '       CharProcs begin'
    '           /.notdef {} def'
    '           /leftend     { 3 0 HVLine } Bd'
    '           /rightend    { 3 0 HVLine } Bd'
    '           /topend      { 0 3 HVLine } Bd'
    '           /bottomend   { 0 3 HVLine } Bd'
    '           /topleft     { 2 2 HVLine } Bd'
    '           /topright    { 1 2 HVLine } Bd'
    '           /bottomleft  { 2 1 HVLine } Bd'
    '           /bottomright { 1 1 HVLine } Bd'
    '           /lefttee     { 2 3 HVLine } Bd'
    '           /righttee    { 1 3 HVLine } Bd'
    '           /toptee      { 3 2 HVLine } Bd'
    '           /bottomtee   { 3 1 HVLine } Bd'
    '           /hline       { 3 0 HVLine } Bd'
    '           /vline       { 0 3 HVLine } Bd'
    '           /cross       { 3 3 HVLine } Bd'
    '           /diamondsign {'
    '               vGDx100 0 vGDx10 vGDyb vGDx90 vGDyt setcachedevice'
    '               vGDx50 vGDyb Mt vGDx90 vGDym Lt vGDx50 vGDyt Lt vGDx10 vGDym Lt closepath fill'
    '           } Bd'
    '           /hairspace   { vGHSx100 0 0 0 0 0 setcachedevice } Bd'
    '       end'

    '       /BuildGlyph {'
    '           exch dup /LineWidth get Slw /CharProcs get exch'
    '           2 copy known not {pop /.notdef} if'
    '           get exec'
    '       } Bd'

    '       /BuildChar {'
    '           1 index /Encoding get exch get'
    '           1 index /BuildGlyph get exec'
    '       } Bd'

    '       currentdict'
    '   end'
    '   definefont pop'
    '} Bd'

    ;;; finds an Adobe font and converts it to use ISOLatin1Encoding
    ;;; usage: <isofontname> <fontname> DefISOLatin1Font
    '/DefISOLatin1Font {'
    '   findfont dup length dict begin'
    '       { 1 index /FID ne {def} {pop pop} ifelse} forall'
    '       /Encoding ISOLatin1Encoding def'
    '       currentdict'
    '   end'
    '   definefont pop'
    '} Bd'

    ;;; define shorthand for setting the various fonts

/* PARAMS 1 - 8 = rep 4 times ISOLatin1 fontname, pixwidth array name */
% lvars s;
  for s in ['/Sf' '/SfB' '/SfI' '/SfBI'] do
    s <> ' { /%S vFontHeight %S SF } Bd'
  endfor
%
    '/SfG  { /VT100Graphics vFontHeight vGXCharPixWidths SF } Bd'
    '/SfGB { /VT100Graphics-Bold vFontHeight vGXCharPixWidths SF } Bd'

    ;;; procedure for underlining a string
    ;;; usage: <string> Ul <string>
    '/Ul {'
    '   gsave dup Stringwidth'
        ;;; move down 1/2 font descent and draw a line under the string.
    '   currentpoint vFontDescent 2 div add newpath Mt 0 RLt stroke'
    '   grestore'
    '} Bd'

    ;;; procedure for drawing coloured outline around string
    ;;; usage: <string> <red> <green> <blue> Ol <string>
    '/Ol {'
    '   gsave Sc dup Stringwidth /swidth exch def'
    '   currentpoint vFontDescent add newpath Mt'
/* PARAM 9 = Line height */
    '   swidth 0 RLt 0 %P RLt'
    '   swidth -1 mul 0 RLt closepath fill grestore'
    '} Bd'

    '%%%%EndProlog'

    '%%%%BeginSetup'

    ;;;; define ISOLatin1Encoding fonts (standard/bold/italic/bolditalic)

/* PARAMS 10 - 21 = rep 4 times dup(ISOLatin1 fontname), fontname endrep */
% repeat 4 times
    '%%%%BeginResource font %S',
    '/%S /%S DefISOLatin1Font',
    '%%%%EndResource'
  endrepeat
%

    ;;; set up font metrics

/* PARAM 22 = font height */
    '/vFontHeight %P def'
    'Sf'
;;; 'gsave newpath 0 0 moveto (_jpg) false charpath pathbbox pop pop'
;;; '    /vFontDescent exch def pop grestore'
/* PARAM 23 = line height */
    'gsave newpath 0 0 moveto (|^[{_jpg) false charpath pathbbox'
    '    /vFontInkAscent exch def pop'
    '    vFontHeight %P sub 2 div add /vFontDescent exch def pop grestore'

    ;;; define VT100 graphics fonts (standard/bold)

    '%%%%BeginResource font VT100Graphics'
    '/VT100Graphics 4 DefVT100GraphicsFont'
    '%%%%EndResource'

    '%%%%BeginResource font VT100Graphics-Bold'
    '/VT100Graphics-Bold 7 DefVT100GraphicsFont'
    '%%%%EndResource'

}),

preamble6 = concat_lines({
    '/vGLinesWidth (X) Stringwidth def'
    '/vGDiamondWidth (X) Stringwidth def'
    '/vGHairSpaceWidth 1 def'
}),

preamble6a = concat_lines({
    '(       eeeeetttsssiiiaaaooorrnnlldchupfmgby)'
    '/vXMeanPixWidth 0 2 index {vXCharPixWidths exch get add} forall def'
    '/vMeanWidth exch stringwidth pop def'
    'SfG'
    '/vGLinesWidth (A) Stringwidth def'
    '/vGDiamondWidth (Q) Stringwidth def'
    '/vGHairSpaceWidth (Z) Stringwidth def'
}),

preamble7 = concat_lines({
    '/vGLx50  vGLinesWidth 50  mul vFontHeight div def'
    '/vGLx100 vGLinesWidth 100 mul vFontHeight div def'
    '/vGLyb   vFontDescent 100 mul vFontHeight div def'
/* PARAMS 1 - 2 = (50, 100) * line_height/font_height */
    '/vGLym  %P vGLyb add def'
    '/vGLyt %P vGLyb add def'

    '/vGDx10  vGDiamondWidth 10  mul vFontHeight div def'
    '/vGDx50  vGDiamondWidth 50  mul vFontHeight div def'
    '/vGDx90  vGDiamondWidth 90  mul vFontHeight div def'
    '/vGDx100 vGDiamondWidth 100 mul vFontHeight div def'
    '/vGDyt   vFontInkAscent 700 mul vFontHeight div 8 div def'
    '/vGDym   vGDyt 4 mul 7 div def'
    '/vGDyb   vGDyt 7 div def'

    '/vGHSx100 vGHairSpaceWidth 100 mul vFontHeight div def'

    '%%%%EndSetup'

;;; output text starts here...
});


lconstant
    black = nonwriteable {0 0 0},
    white = nonwriteable {16:FFFF 16:FFFF 16:FFFF},
    grey80 = nonwriteable {16:CCCC 16:CCCC 16:CCCC},
    grey70 = nonwriteable {16:B333 16:B333 16:B333},
    grey50 = nonwriteable {16:8000 16:8000 16:8000},
    grey30 = nonwriteable {16:4CCD 16:4CCD 16:4CCD},

    default_colormap = nonwriteable {%
        black,white,  white,black,  grey50,white,  black,grey80,
        grey70,white, white,grey30, grey30,white,  grey30,grey80,
        black,white,  white,black,  grey50,white,  black,grey80,
        grey70,white, white,grey30, grey30,white,  grey30,grey80,
    %}
;


define postscript_line_consumer(device);
    lvars
        arglist = [], consume, arg, s, X_width_p = false,

        ;;; document state
        done_preamble = false, done_postamble = false,
        line_count = 0, page_count = 0,
        control_char,

        ;;; graphics state
        last_dattr, last_bold, last_ital, last_ul,
        last_foreground, last_background, last_graphics,

        ;;; fonts
        font = false, fontB, fontI, fontBI, ILfont, ILfontB, ILfontI, ILfontBI,
        font_height = false, line_height = false,
        font_pixel_height = false, line_pixel_height = false,
        colourmap = default_colormap, c0foreground, c0background,

        paper_name, paper_width, paper_height, hmargin, vmargin,
        actual_vmargin, lines_per_page,

      ;

    lconstant
        GRAPH_CHAR_DECR = `\Gle` - `A`,
        SPACE_CHARS = '\t\s\Ss\Sf\St\Sp\Sn\(16:9A)',
        XCPW    = 'vXCharPixWidths',
        BXCPW   = 'vBXCharPixWidths',
        IXCPW   = 'vIXCharPixWidths',
        BIXCPW  = 'vBIXCharPixWidths',
        GXCPW   = 'vGXCharPixWidths'
    ;

    ;;; forward decl;
    lconstant procedure Eject_page;

    ;;; starting a new document
    define Output_preamble();

        define out_widths(vname, attr);
            lvars c, n;
            lconstant s = writeable initdstring(1);
            printf(vname, '/%P [');
            if X_width_p then
                if attr then
                    for c from 0 to 16:FF do
                        c || attr -> fast_subscrdstring(1, s);
                        spr(X_width_p(s));
                        if c && 16:F == 16:F and c /== 16:FF then
                            cucharout(`\n`)
                        endif
                    endfor
                else
                    for c from 0 to `Z` do
                        if (`A` <= c and c <= `Q`) or c == `Z` then
                            c + GRAPH_CHAR_DECR -> fast_subscrdstring(1, s);
                            X_width_p(s)
                        else
                            0
                        endif;
                        spr();
                        if c && 16:F == 16:F then cucharout(`\n`) endif
                    endfor
                endif
            endif;
            npr('] def')
        enddefine;

        printf(preamble1, [% sys_convert_date(sys_real_time(),false) %]);
        if device_open_name(device) then
            printf(preamble2, [% device_open_name(device) %])
        endif;

        printf(preamble3, [%
            ;;; PARAM 1 = username
            sysgetusername(popusername),
            ;;; PARAMS 2 - 5 = font names
            font, fontB, fontI, fontBI,
            ;;; PARAMS 6 - 9 = ISOLatin1 font names
            ILfont, ILfontB, ILfontI, ILfontBI,
            ;;; PARAMS 10 - 12 = paper size: name, width, height
            paper_name, paper_width, paper_height,
            ;;; PARAMS 13 - 16 = bounding box x1, y1, x2, y2 */
            hmargin, vmargin, paper_width-hmargin, paper_height-vmargin,
        %]);

        out_widths(XCPW, 0);
        out_widths(BXCPW, `\[b]`);
        out_widths(IXCPW, `\[i]`);
        out_widths(BIXCPW, `\[bi]`);
        out_widths(GXCPW, false);

        printf(if X_width_p then preamble4a else preamble4 endif);

        printf(preamble5, [%
            ;;; PARAMS 1 - 8 = rep 4 times ISOLatin1 fontname, pixwidth array name
            ILfont, XCPW, ILfontB, BXCPW, ILfontI, IXCPW, ILfontBI, BIXCPW,
            ;;; PARAM 9 = line height
            line_height,
            ;;; PARAMS 10 - 21 = rep 4 times dup(ISOLatin1 fontname), fontname
            dup(ILfont), font, dup(ILfontB), fontB,
                dup(ILfontI), fontI, dup(ILfontBI), fontBI,
            ;;; PARAM 22 = font height
            font_height,
            ;;; PARAM 23 = line height
            line_height
        %]);
        printf(if X_width_p then preamble6a else preamble6 endif);
        lvars f = (50 * line_height) / font_height;
        printf(preamble7, [%
            ;;; PARAMS 1 - 2 = (50, 100) * line_height/font_height
            f, f+f
        %]);
        cucharout(`\n`)
    enddefine;

    ;;; reached the end of the document
    define Output_postamble();
        nprintf('%%%%Trailer');
        nprintf('\ndocsave restore\n');
        nprintf(page_count, '\n%%%%Pages: %P');
        nprintf('%%%%EOF');
    enddefine;

    ;;; reached the end of the page
    define Output_end_page();
        ;;; restore the saved context
        npr('\npgsave restore\n');
        ;;; output this page
        npr('\nshowpage\n');
    enddefine;

    define Pr_colour_comm(colour, comm);
        dlvars colour;
        define cval(i); fast_subscrv(i, colour) / 16:FFFF enddefine;
        printf(comm, cval(3), cval(2), cval(1), '%P %P %P %S ')
    enddefine;

    ;;; starting a new page
    define Output_new_page();
        page_count + 1 -> page_count;
        printf(page_count, page_count, '\n%%%%Page: %P %P\n\n');
        ;;; reset graphics state between each page to preserve page
        ;;; independence.
        npr('%%BeginPageSetup');
        npr('/pgsave save def');
        npr('% set graphics state');
        npr('.5 Slw');

        ;;; clip bounding box
        lvars   x1 = hmargin, y1 = vmargin, x2 = paper_width-hmargin,
                y2 = paper_height-vmargin,
                args = [% x1, y1, x1, y2, x2, y2, x2, y1 %];
        nprintf('%P %P Mt %P %P Lt %P %P Lt %P %P Lt closepath clip', args);
        sys_grbg_list(args);
        if c0background /= white then
            Pr_colour_comm(c0background, 'Sc fill')
        endif;
        Pr_colour_comm(c0foreground, 'Sc');
        ;;; correct vertical position of text lines
        nprintf(line_height, '0 %P vFontDescent add neg translate');

        npr('%%EndPageSetup');
        false ->> last_dattr ->> last_graphics -> last_ul;
        c0foreground -> last_foreground;
        c0background -> last_background;
        undef ->> last_bold ->> last_ital ->;
    enddefine;

    ;;; write characters in chars buffer
    lvars chars = [];
    define Flush_chars();
        returnif(chars == []);

        repeat #| `\s`,`)`, explode(chars), `(` |# times cucharout() endrepeat;
        if last_background /= white then
            Pr_colour_comm(last_background, 'Ol')
        endif;
        if last_ul then pr('Ul '); endif;
        pr('Sh\n');
        sys_grbg_list(chars);
        [] -> chars;
    enddefine;

    ;;; write a single character to chars buffer
    define Write_char(c);
        lvars c;
        if last_graphics then
            c - GRAPH_CHAR_DECR -> c;
        else
            if c == `\^L` then
                Eject_page();
                true -> control_char;
                return;
            endif;
            if strmember(c, SPACE_CHARS) then
                ;;; map tabs and other space characters to \s
                `\s` -> c;
            elseif strmember(c, '()\\') then
                ;;; escape this character with a backslash
                conspair(`\\`, chars) -> chars;
            elseif c &&/=_0 128 then
                ;;; it's an eight-bit character - write it as an escaped octal
                conspair(`\\`, chars) -> chars;
                conspair((c // 64) + `0`, chars) -> chars -> c;
                conspair((c // 8) + `0`, chars) -> chars -> c;
                c + `0` -> c;
            endif;
        endif;
        conspair(c, chars) -> chars;
    enddefine;

    ;;; set the current graphics state from a dchar and return it with
    ;;; attributes stripped
    define Set_dchar_attributes(dchar) -> dchar;
        lvars dchar;
        lvars dattr, bold, ital, ul, index, foreground, background, graphics;
        dchar && 16:FF0000 -> dattr;
        dchar && 16:FFFF -> dchar;

        ;;; see if its a special graphics character (see REF * ITEMISE)
        if dchar >= 16:81 and dchar <= 16:9A ->> graphics then
            ;;; a couple of these characters can be handled in ISOLatin1
            if dchar == `\Go` then
                ;;; degree
                16:B0 -> dchar;
                false -> graphics;
            elseif dchar == `\G.` then
                ;;; periodcentered
                16:B7 -> dchar;
                false -> graphics;
            endif;
        endif;

        ;;; if the character is a whitespace character, we ignore
        ;;; attributes which aren't significant to whitespace (i.e.
        ;;; bold and italic).
        if strmember(dchar, SPACE_CHARS) then
            dattr &&~~ VEDCMODE_SP_INSIG_BITS -> dattr
        endif;

        dattr &&/=_0 `\[u]` -> ul;
        dattr &&/=_0 `\[b]` -> bold;
        dattr &&/=_0 `\[i]` -> ital;

        (dattr && `\[7]`) >> #_<VEDCMODE_COLOUR_SHIFT-1>_# -> index;
        if dattr &&/=_0 `\[A]` then index + 16 -> index endif;
        subscrv(index+1, colourmap) -> foreground;
        subscrv(index+2, colourmap) -> background;

        if bold /== last_bold or ital /== last_ital
                or graphics /== last_graphics then
            Flush_chars();
            ;;; select the font.
            ;;; note that there is not an italic version of the graphics font
            pr('Sf');
            if graphics then pr('G') endif;
            if bold then pr('B'); endif;
            if ital and not(graphics) then pr('I') endif;
            pr(' ');
        endif;

        if ul /== last_ul then
            Flush_chars();
        endif;

        if foreground /= last_foreground or background /= last_background
        then
            Flush_chars();
            if foreground /= last_foreground then
                Pr_colour_comm(foreground, 'Sc')
            endif
        endif;

        ;;; record the graphics state
        foreground  -> last_foreground;
        background  -> last_background;
        ul          -> last_ul;
        bold        -> last_bold;
        ital        -> last_ital;
        graphics    -> last_graphics;
        dattr       -> last_dattr;
    enddefine;

    ;;; output a single dchar
    define Output_dchar(dchar);
        lvars dchar;
        returnif(dchar && 16:FFFF == `\Nt`);    ;;; ignore trailing newline
        if control_char then
            Output_new_page();
            false -> control_char;
        endif;
        Write_char(Set_dchar_attributes(dchar));
    enddefine;

    ;;; write a newline
    define Output_newline();
        Flush_chars();
        line_count + 1 -> line_count;
        if line_count == lines_per_page then
            Output_end_page();
            0 -> line_count;
        endif;
    enddefine;

    ;;; do a form feed
    define Eject_page();
        ;;; move to last line on page and then output a newline
        lines_per_page - 1 -> line_count;
        Output_newline();
    enddefine;

    ;;; write a single line
    define Output_line(dstring);
        lvars dstring;

        if line_count == 0 then ;;; starting a new page
            Output_new_page();
        endif;

        ;;; skip empty lines
        false -> control_char;

        if dstring /= nullstring
        and (skipchar(`\s`, 1, dstring) or isdstring(dstring)) then
            ;;; move to the next line
            printf((lines_per_page-line_count)*line_height + actual_vmargin,
                    hmargin,
                    '\n%P %P Mt\n');
            appdata(dstring, Output_dchar); ;;; write the string
        endif;
        unless control_char then
            Output_newline();
        endunless;
    enddefine;

    define get_font_spec(name) -> spec;
        while isstring(font_names(name) ->> spec) do
            spec -> name
        endwhile
    enddefine;


    if isprocedure(device) then
        ;;; Extra args to enable text to be laid out identically to the
        ;;; XVed (var width mode) window.

        ;;; Procedure is for computing pixel widths of chars
        (), device -> (device, font, font_height, font_pixel_height,
                        line_pixel_height, colourmap, X_width_p)
    endif;
    if islist(device) then
        ;;; optional arg list
        (), device -> (device, arglist)
    endif;

    discout(device) -> consume;
    discout_device(consume) -> device;

    ;;; defaults
    unless font then 'courier' -> font endunless;
    unless font_height then 11 -> font_height endunless;
    "a4" -> paper_name;
    25, 25 -> (hmargin, vmargin);

    for arg in arglist do

        define do_arg(arg) -> ok;
            lvars ok = false, kwd, val, n;
            returnunless(isstring(arg) or isword(arg));
            [% sys_parse_string(arg, `=`, uppertolower) %] -> arg;
            returnunless(listlength(arg) == 2);
            dl(arg) -> (kwd, val);
            true -> ok;
            if kwd = 'font' then
                [% sys_parse_string(val, `:`) %] -> val;
                returnif(val == []);
                unless (hd(val) ->> arg) = nullstring then
                    ;;; font name
                    if get_font_spec(arg) then
                        arg -> font
                    else
                        mishap(arg, 1, 'postscript_line_consumer: INVALID FONT NAME')
                    endif
                endunless;
                tl(val) -> val;
                returnif(val == []);
                unless (hd(val) ->> arg) = nullstring then
                    if strnumber(arg) ->> n then
                        n -> font_height
                    else
                        mishap(arg, 1, 'postscript_line_consumer: INVALID FONT HEIGHT')
                    endif
                endunless;
                tl(val) -> val;
                returnif(val == []);
                unless (hd(val) ->> arg) = nullstring then
                    if strnumber(arg) ->> n then
                        n -> line_height
                    else
                        mishap(arg, 1, 'postscript_line_consumer: INVALID LINE HEIGHT')
                    endif
                endunless
            elseif kwd = 'paper' then
                if paper_names(consword(val) ->> arg) then
                    arg -> paper_name
                else
                    mishap(val, 1, 'postscript_line_consumer: INVALID PAPER SIZE')
                endif
            elseif kwd = 'margin' or kwd = 'hmargin' or kwd = 'vmargin' then
                if strnumber(val) ->> arg then
                    if kwd = 'margin' then
                        arg ->> hmargin -> vmargin
                    elseif kwd = 'hmargin' then
                        arg -> hmargin
                    else
                        arg -> vmargin
                    endif
                else
                    mishap(val, 1, 'postscript_line_consumer: INVALID MARGIN SIZE')
                endif
            else
                false -> ok
            endif
        enddefine;

        unless do_arg(arg) then
            mishap(arg, 1, 'postscript_line_consumer: INVALID ARGUMENT')
        endunless
    endfor;

    if isword(font) then word_string(font) -> font endif;
    get_font_spec(uppertolower(font)) -> font;
    for s in_vector font do 'ISOLatin1-' <> s endfor
                    -> (ILfont, ILfontB, ILfontI, ILfontBI);
    explode(font) -> (font, fontB, fontI, fontBI);

    explode(paper_names(paper_name))
                    -> (paper_name, paper_width, paper_height);

    unless line_height then
        font_height * if font_pixel_height and line_pixel_height then
                        line_pixel_height / font_pixel_height
                      else
                        11_/10
                      endif -> line_height
    endunless;
    (paper_height - vmargin*2) // line_height -> (s, lines_per_page);
    vmargin + s/2 -> actual_vmargin;
    subscrv(1, colourmap) -> c0foreground;
    subscrv(2, colourmap) -> c0background;


    ;;; THE LINE CONSUMER (a lexical closure)
    procedure(line, device);
        dlocal  cucharout       = consume,
                pr              = sys_syspr,
                pop_pr_quotes   = false,
                poplinewidth    = false,
                pop_pr_ratios   = false;
        unless done_preamble then
            Output_preamble();
            true -> done_preamble;
        endunless;
        if line == termin then
            unless done_postamble then
                unless line_count == 0 then
                    Eject_page();
                endunless;
                Output_postamble();
                true -> done_postamble;
                cucharout(termin);
            endunless;
        elseunless done_postamble then
            Output_line(line);
        endif;
    endprocedure(%device%);
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 20 1999
        Now handles full colour for XVed var width mode
--- John Gibson, May 24 1999
        Further improvements
--- John Gibson, Apr 23 1999
        Numerous improvements and bug fixes.
--- Robert John Duncan, May 18 1995
        Changed procedure diamondsign for printing `\G#` to make it look
        like it does on screen. Removed procedures for printing `\Go` and
        `\G.` and mapped them to their ISOLatin1 equivalents instead.
 */
