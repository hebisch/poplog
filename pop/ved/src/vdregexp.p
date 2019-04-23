/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/ved/src/vdregexp.p
 > Purpose:         Search/Substitute in VED using regular expressions
 > Author:          Jonathan Meyer, Sept 9 1992 (see revisions)
 > Documentation:   REF *VEDSEARCH *REGEXP
 > Related Files:   SRC *REGEXP_COMPILE.P *REGEXP_SEARCH.P
 */

;;; ---- REGULAR EXPRESSION SEARCH AND REPLACE -----------------------------

#_INCLUDE 'vddeclare.ph'
#_INCLUDE '../../lib/include/vedsearchdefs.ph'

global constant
    procedure (
        isregexp,
        regexp_anchored,
        regexp_break_count,
        regexp_compile,
        regexp_delimeter,
        regexp_subexp,
        regexp_subexp_count,

        vedatitemstart, vedatitemend, vedissubitem, vedcurrentchar,
        vedinascii, vedrepeater, vedvedrepeater, vedinsertstring, vedwiggle,
    ),
  ;

global vars procedure ved_copy;

global vars vedhardtabs;

constant procedure $-Sys$-Checkr_regexp;

;;; ----------------------------------------------------------------------

section $-Sys$-Ved =>
                    vedlinestart
                    ved_try_search ved_check_search
                    ved_search_state vedsearchdata
                    vedsafesubstitute vedsearchdoesselect
                    vedwildcards vedwrapped
                    ved_set_search ved_query_last_search
                    ved_search_or_substitute
                    ved_search ved_backsearch ved_re_search ved_re_backsearch
                    ved_s ved_gs ved_gsr

                    ;;; used by XVed
                    ved_regexp_compile ved_regexp_search ved_regexp_substitute
                    ved_regexp_jumpto
                    ;

constant procedure Delete_char;

vars
    vedwrapped              = 0,     ;;; count on number of wrap-arounds.
    vedsafesubstitute       = true,  ;;; copy before global substitutes ?
    vedwildcards            = true,  ;;; true, false or "ed"
    vedsearchdoesselect     = false, ;;; if true, search does XVed select
  ;

constant
    No_search_string    = '\{b}no search string',
    Cannot_find         = '\{b}not found',
    ;

/* The following variables are accessed using -valof- :
uses-by_name (
        vedstartwordleft, vedendwordright, vedendrange, vedmarkfind,
        ved_mcp, vedcharnext, vedprevpara, vednextparaend,
        vedprevsent, vednextsentend, vedselectioncoords,
        vedselection_adjust, vedselection_set_primary, vedselection_clear,
        vvedclipboard, vvedfoundstring,
);
*/

lconstant
    Buffer_empty_str        = '\{b}buffer empty',

    macro (
        ;;; 0-9 are reserved for @( @) subexpressions (@1, @2, @3, etc.)
        REPEAT_ALL          = 10,   ;;; @&
        PASTE               = 11,   ;;; @p
        POP11_EXPR          = 12,   ;;; @:
      ),
;

;;; this doesn't really belong here...
define vedlinestart(string);
    ;;; Does the current line start with the string?
    lvars string, line=vedthisline();
    isstartstring(string, line) and vedissubitem(string,1,line) == 1
enddefine;


;;; returns true if line, col is inside the region line1, col1, line2, col2
define lconstant Is_inside(line, col, line1, col1, line2, col2);
    lvars line, col, line1, col1, line2, col2, startok, endok;
    line == line1 and col >= col1 -> startok;
    line == line2 and col <= col2 -> endok;

    line > line1 and line < line2
    or (startok and endok)
    or (line1 /== line2 and (startok or endok))
enddefine;

;;; returns true if line, col is before line1, col1, false otherwise.
;;; 1 is returned if line, col equals line1, col1.
define lconstant Is_before(line, col, line1, col1);
    lvars line, col, line1, col1;
    if line == line1 then
        if col == col1 then
            1
        else
            col fi_< col1
        endif
    else
        line fi_< line1
    endif;
enddefine;

;;; lifted from vdsearch.p
define lconstant Buffer_empty();
    if vvedbuffersize == 0 then true
    elseif vvedbuffersize /== 1 then false
    else not(skipchar(` `,1,Buffer_line(1)))
    endif;
enddefine;

;;; returns a buffer line and its size. Sets -vedline-
define lconstant Get_line(line) -> (str, size);
    lvars line, str, size;
    line -> vedline;
    vedthisline() -> str;
    vedusedsize(str) -> size;
enddefine;

;;; ---- CONSTRAIN CLASSES (much like XVed selection classes) -------------

define lconstant proc = valof <> apply; enddefine;

define lconstant startword();
    unless vedcolumn == 1 or vedcolumn fi_> vvedlinesize
    or vedatitemstart(vedcolumn,  vedthisline(), vvedlinesize) then
        ;;; not at a word boundary
        proc("vedstartwordleft");
    endunless;
enddefine;

define lconstant endword();
    returnif(vedcolumn fi_> vvedlinesize);
    if vedatitemend(vedcolumn, vedthisline(), vvedlinesize+1) then
        unless vedchartype(vedcurrentchar()) == vedchartype(`\s`) then
            vedcharright()
        endunless
    else
        ;;; not at a word boundary
        proc("vedendwordright");
    endif;
enddefine;

define lconstant startwindow();
    vedjumpto(vedlineoffset+1, 1);
enddefine;

define lconstant endwindow();
    vedscreendown(); vedscreenright();
    vedcolumn+1 -> vedcolumn;
enddefine;

define lconstant endrange();
    proc("vedendrange");
    vedtextright();
    vedcolumn + 1 -> vedcolumn;
enddefine;

define lconstant screenleft;
    dlocal vedleftmargin = 0;
    vedscreenleft();
enddefine;

lconstant
    constrainclasses = [
        word        {%startword, endword%}
        line        {%screenleft, vedtextright%}
        range       {%proc(%"vedmarkfind"%), endrange%}
        procedure   {%proc(%"ved_mcp"%) <> proc(%"vedmarkfind"%), proc(%"vedendrange"%) <> vedtextright%}
        file        {%vedtopfile, vedendfile <> vedcharleft%}
        selection   {%identfn, identfn%} ;;; dummy entry
        sentence    {%proc(%"vedcharnext"%) <> proc(%"vedprevsent"%), proc(%"vednextsentend"%) <> vedcharright%}
        paragraph   {%proc(%"vedcharnext"%) <> proc(%"vedprevpara"%), proc(%"vednextparaend"%) <> vedcharright%}
        window      {%startwindow, endwindow%}
        toendfile   {%identfn, vedendfile <> vedcharleft%}
        tostartfile {%vedtopfile, identfn%}
    ];

;;; takes a string, returns constrain class word, or -false- if nothing matches
define lconstant Is_constrain_class(str);
    lvars str, classes = constrainclasses, class;
    while classes /== [] do
        dest(classes) -> classes -> class;
        dest(classes) -> classes -> /* constrain spec */;
        returnif(isstartstring(str, class))(class);
    endwhile;
    false;
enddefine;

;;; takes a contrain class, returns coordinates in the VED buffer.
define lconstant Constrain_coords(type) -> (line1, col1, line2, col2);
    lvars type, line1, col1, line2, col2;

    define lconstant Decode_type(type);
        lvars type, list;
        dlocal vedline, vedcolumn;
        if type.isvector and datalength(type) == 4 then
            explode(type);
        elseif type == "selection" and vedusewindows == "x" then
            valof("vedselectioncoords").explode
        else
            unless fast_lmember(type, constrainclasses) ->> list then
                mishap(type, 1, 'INVALID CONSTRAIN TYPE');
            endunless;
            list.tl.hd -> list;
            vedmarkpush(), false -> vvedmarkprops;
            vedpositionpush();
            fast_apply(list(1)), vedline, vedcolumn;
            vedpositionpop(), vedpositionpush();
            fast_apply(list(2)), vedline, vedcolumn;
            vedpositionpop();
            vedmarkpop()
        endif;
    enddefine;

    Decode_type(type) -> (line1, col1, line2, col2);
enddefine; /* Constrain_coords */

;;; ----- SEARCHING FOR A REGULAR EXPRESSION --------------------------------

;;; Takes coordinates in a ved buffer, determines where to start searching.
define lconstant Start_coords(startline, startcol, endline, endcol,
                        backw, fromhere, wrap)
            /* -> line, col, skip, wrap */;
    lvars startline, startcol, endline, endcol, backw, fromhere, skip, wrap;
    unless fromhere then -1 ->> vedline -> vedcolumn endunless;
    unless Is_inside(vedline, vedcolumn, startline, startcol, endline, endcol)
    then
        ;;; the cursor is not inside the region - skip to the first or last
        ;;; character of the region
        0 -> skip; ;;; don't skip over the start location endif;
        unless wrap then
            ;;; we need to test if wrapping is off, and the cursor is beyond
            ;;; the end of the region (forward search) or before the start of
            ;;; the region (backward search) since these searches should fail.
            if (Is_before(vedline, vedcolumn, startline, startcol) and backw)
            or (Is_before(endline, endcol, vedline, vedcolumn) and not(backw))
            then
                ;;; cannot wrap around - set -skip- to false.
                false -> skip;
            endif;
        endunless;
        vedwrapped + 1 -> vedwrapped;
        false -> wrap;
        if backw then
            ;;; the last character in the region
            endline, endcol
        else
            ;;; the first character in the region
            startline, startcol
        endif;
    else
        fromhere == 0 and 0 or 1 -> skip;
        vedline, vedcolumn,
    endunless;
    skip, wrap,
enddefine;

/*
The following  low  level procedures  are  used to  perform  search  and
substitute within VED. These procedures  do not set vedsearchdata  (i.e.
calling one of these  procedures does not change  what happens when  the
user hits the RE_SEARCH key):

ved_regexp_compile(string_1, string_2,                       [procedure]
                    ignore_case, embed) -> search_p
ved_regexp_try_compile(string_1, string_2,                   [procedure]
                        ignore_case, embed) -> search_p
        Constructs a search procedure  for use with  ved_regexp_search ,
        or ved_regexp_substitute  . string_1  specifies what  to  search
        for. string_2 is a (d)string specifying what to replace it with,
        or false if there isn't  a substitution string (ie. if  search_p
        is only going to be used with ved_regexp_search ).

        The strings  are  itemised  first (see  REF * ITEMISE)  and  may
        therefore contain  backslash  sequences  as  recognised  by  the
        Poplog itemiser (eg. \n, \s, \t, etc.).

        string_1 may  contain  wildcard  search  patterns  (see  'Search
        Pattern Elements' above), and string_2 may contain  substitution
        patterns (See 'Substitution Pattern Elements' above).

        If ignore_case is  false ,  then the search  is case  sensitive,
        otherwise the search  is case-insensitive. If  embed is  false ,
        the search will only match non-embedded items, otherwise it will
        match embedded or non-embedded items.

        If ved_regexp_compile  encounters  an error  while  parsing  the
        strings it calls vederror with a brief description of the error;
        on the  other hand  ved_regexp_try_compile returns  false if  it
        cannot parse the  strings (this is  the only difference  between
        the two procedures).


ved_regexp_search(search_p, constrain, back, wrap, start, n) [procedure]
                -> (first_line, first_column, last_line, last_column)
        Searches forwards or  backwards within the  region specified  by
        constrain (and  possibly  wrapping over  the  end of  a  region)
        looking for  a  match  using  search_p  (see  ved_regexp_compile
        above).

        constrain dictates  the region  in the  vedbuffer to  which  the
        search is constrained. constrain can be any one of the following
        words:

            word
            line
            selection (XVed only)
            sentence
            procedure
            range
            paragraph
            window
            file
            toendfile
            tostartfile

        constrain can also be a vector of vedline/vedcolumn positions of
        the form:

            {% START_LINE, START_COLUMN, END_LINE, END_COLUMN %}

        If back is false , searching  goes towards the end of the  file,
        otherwise it  goes  towards  the beginning  of  the  file  (i.e.
        backwards).

        start is either false , true , or 0, as follows:

            false
                searching starts from the start (forward search) or end
                (backward search) of the region.

            true
                searching starts from one character ahead (forward
                search) or behind (backward search) the current cursor
                position.

            0   searching starts on the cursor location (ie. the cursor
                location may be included in the match).

        In all cases, if  the cursor position is  outside of the  search
        region, searching starts from the start (forward search) or  end
        (backward search) of the region.

        If wrap is true ,  and the end of  the search region is  reached
        before finding a match, the search wraps around over the end  of
        the  region  and  continues  from  the  start  of  the   region.
        ved_regexp_search increments vedwrapped by 1 if it wraps  around
        the region in this manner.

        If n is an integer, the n'th match is searched for.

        If no match  is found, first_line,  first_column, last_line  and
        last_column  will   be  false   .   If  the   search   succeeds,
        ved_regexp_search   returns   four   VED   buffer    coordinates
        first_line, first_column, last_line, last_column where

            vedjumpto(first_line, first_column)

        will place the cursor on the start of the match, and

            vedjumpto(last_line, last_column + 1)

        will place the cursor one character after then end of the match.

        Note that last_column will be one less than first_column if  the
        pattern matches zero characters eg.  for patterns such as  '@$'.
        Thus the result:

                (4, 18, 4, 17)

        indicates that  a zero-length  match  occurs at  the  eighteenth
        column of line four.

        Similarly, last_column will  be zero  if all  of the  characters
        including the line break on previous line to last_line are  part
        of the match (ie. if the pattern ends in @z@a). Thus the result:

                (12,1,13,0)

        indicates that the whole of line 12 including the line break  at
        the end of line 12 are part of the match.


ved_regexp_jumpto(line_1, column_1, line_2, column_2, bool)  [procedure]
        Takes the  result of  a ved_regexp_search  . If  the search  has
        failed, vederror is used to tell  the user that the pattern  was
        not found.  Otherwise vedjumpto  is used  to jump  to the  first
        character of the match.

        In addition, if XVed is running and bool is true , the  matching
        text will be selected and placed on the clipboard.


ved_regexp_substitute(search_p, constrain, back, wrap, start, [procedure]
                      n, once_per_line, ask_first, silent)
        The first five arguments are the same as for ved_regexp_search
        (see above).

        If  vedsafesubstitute     is  true     (see  below)   then   the
        substitution region is copied to vveddump before doing a  global
        substitution.

        n is an integer indicating  the maximum number of  substitutions
        to perform. If it is false  , as many substitutions as  possible
        are done.

        If once_per_line  is  false,  every  match  with  search_p  is
        replaced, otherwise only  the first match  (forwards search)  or
        last match (backwards  search) on  each line  is replaced.  Note
        that if  once_per_line is  true  , the  search starts  from  the
        start (forward search) or end  (backward search) of the  initial
        line.

        If ask_first  is  false  ,  all possible  substitutions  in  the
        region  are  done.  If  it  is  true  ,  the  user  is  prompted
        interactively before each substitution.

        If silent is false  , the user  is kept informed as to how  many
        (global)  substitutions  have  been  performed.  Otherwise  this
        information is  not displayed  and the  substitution takes  less
        time.
*/


;;; ved_regexp_search
;;;     takes something to search for, and details of where to search.
;;;     returns line1, col1, line2, col2 of match. These will be false if the
;;;     search fails.
define ved_regexp_search(search_p, constrain, backward, wrap,
                                fromhere, count);
    lvars
        procedure search_p,     ;;; a *regexp search procedure
        constrain,              ;;; a constrain region
        backward,               ;;; true if search goes backward
        wrap,                   ;;; true if should wrap over region boundary
        count,                  ;;; number to skip
        fromhere,               ;;; distance from vedline/col to start search
        startline, startcol,    ;;; start of region of buffer to search
        endline, endcol,        ;;; end of region of buffer to search
        firstline, firstcol,    ;;; where the search starts from
        wrapline,               ;;; remebered first line (used for wrapping)
        nlines,                 ;;; total number of lines in regular exp. -1
        eline,                  ;;; used to cache endline - nlines
        skip,                   ;;; 1 if should skip the start position
        line,
      ;

    dlocal vedline, vedcolumn;

    define lconstant Perform_match(line, ahead);
        lvars
            line,           ;;; a vedbuffer line number
            ahead,          ;;; true if searching ahead of start position
            str, strlen,    ;;; (computed) string we are searching
            col, nchars,    ;;; (computed) location in string to search
            i,
            lastline,       ;;; last line number of multi-line expression
            laststr,        ;;; last line string of multi-line expression
            laststrlen,     ;;; length of last line string
          ;
        Get_line(line) -> (str, strlen);
        ;;; this could be more compact, but it would need more tests
        ;;; and would therefore be slower.
        if nlines == 0 then
            if line == startline or line == endline or line == firstline then
                ;;; if its the first line, check the column valid
                returnif (line == firstline and firstcol == 0)(false);
                if ahead then
                    ;;; ahead of the start position
                    (line == firstline and firstcol fi_+ skip or 1) -> col;
                else
                    line == startline and startcol or 1 -> col;
                endif;
                (line == endline and fi_min(endcol, strlen) or strlen)
                            fi_- col fi_+ 1 -> nchars;
                returnunless((strlen == 0 and col == 1)
                            or col fi_<= strlen)(false);
            else
                1 -> col; strlen -> nchars;
            endif;
            ;;; START_POS, STRING, NCHARS
            col, str, nchars
        else
            line fi_+ nlines -> lastline;
            Get_line(lastline) -> (laststr, laststrlen);
            if line == startline or lastline == endline or line == firstline
            then
                ;;; if its the first line, check the column valid
                returnif (line == firstline and firstcol == 0)(false);
                if ahead then
                    ;;; ahead of the start position
                    (line == firstline and firstcol fi_+ skip or 1) -> col;
                else
                    line == startline and startcol or 1 -> col;
                endif;
                (lastline == endline and fi_min(endcol, laststrlen)
                        or laststrlen) -> nchars;
                returnunless((strlen == 0 and col == 1)
                            or col fi_<= strlen)(false);
            else
                1 -> col; laststrlen -> nchars;
            endif;
            lastline fi_-1 -> lastline;
            ;;; START_POS, STRING_1, STRING_2, ..., STRING_M, NCHARS
            col, str,
            fast_for vedline from line fi_+1 to lastline do
                vedthisline()
            endfast_for,
            laststr, nchars
        endif;
        ;;; apply the search procedure
        fast_apply(backward, search_p) -> (col, nchars);
        if col then
            if isinteger(count) and count fi_> 1 then
                ;;; find another match
                count fi_- 1 -> count;
                procedure;
                    ;;; we need to recurse because the same string may appear
                    ;;; more than once on a line
                    dlocal firstline = line, firstcol = col, skip, backward;
                    if backward then
                        col fi_-1 -> backward;
                        0
                    else
                        1
                    endif -> skip;
                    Perform_match(line, ahead);
                endprocedure();
                return;
            endif;
            ;;; found a match
            if nlines == 0 then col fi_+ nchars fi_-1 -> nchars; endif;
            line, col, line fi_+ nlines, nchars, true
        else
            ;;; try again
            false
        endif;
    enddefine; /* Perform_match */

    $-Sys$-Checkr_regexp(search_p)->;

    ;;; cannot match string in empty buffer!
    returnif(Buffer_empty())(false, false, false, false);
    regexp_break_count(search_p) -> nlines; ;;; number of additional lines
    ;;; calculate where the region in the buffer is.
    Constrain_coords(constrain) -> (startline, startcol, endline, endcol);
    ;;; calculate where to start in the region
    if Is_before(endline, endcol, startline, startcol) then
        return(false,false,false,false);
    endif;
    ;;; if the pattern contains @z@a line breaks, then it may end in a line
    ;;; break. In this case we need to check if the endpoint is past the
    ;;; last character of a line, and if so advance it to the next line.
    if nlines /== 0 and (endline fi_> vvedbuffersize
            or endcol fi_> vedusedsize(Buffer_line(endline))) then
        endline fi_+ 1 -> endline;
        0 -> endcol;
    endif;
    returnif(startline fi_> endline - nlines)(false,false,false,false);
    endline fi_- nlines -> eline;
    Start_coords(startline, startcol, endline, endcol,
            backward, fromhere, wrap) -> (firstline, firstcol, skip, wrap);
    ;;; Start_coords sets skip to -false- if it cannot find a start coordinate
    returnunless(skip)(false,false,false,false);
    ;;; remember where the start is (for wrapping)
    firstline -> wrapline;
;;; to trace calls to the underlying search procedure, uncomment these lines:
;;; dlocal poptraceindent = poptraceindent + 1;
;;; systrace_proc(%search_p, "regexp_search", false%) -> search_p;
    unless backward then
        ;;; GOING FORWARD
        fast_for line from wrapline to eline do
            returnif(Perform_match(line, true));
        endfast_for;
        ;;; wrap
        vedwrapped + 1 -> vedwrapped;
        if wrap then
            -1 -> firstline; ;;; ignore firstline from now on
            fast_for line from startline to wrapline do
                returnif(Perform_match(line, false));
            endfast_for;
        endif;
    else
        ;;; GOING BACKWARD
        ;;; the first line is a special case - we set -backward- to be the
        ;;; the starting column for the search (see REF *REGEXP) 'cos
        ;;; we are interested in a match which -starts- before the cursor
        ;;; and not just matches which -end- before the cursor.
        firstcol fi_- skip -> backward; ;;; set backward to starting col
        returnif(Perform_match(wrapline, false));
        true -> backward;               ;;; set backward to -true-
        ;;; do rest of lines to start of file
        fast_for line from wrapline fi_-1 by -1 to startline do
            returnif(Perform_match(line, false));
        endfast_for;
        ;;; wrap
        vedwrapped + 1 -> vedwrapped;
        if wrap then
            -1 -> firstline; ;;; ignore firstline from now on
            fast_for line from eline by -1 to wrapline do
                returnif(Perform_match(line, true));
            endfast_for;
        endif;
    endunless;
    ;;; NOT FOUND
    false, false, false, false;
enddefine; /* ved_regexp_search */

;;; takes result of ved_regexp_search and jumps to the matching text.
define ved_regexp_jumpto(line1, col1, line2, col2, select);
    lvars line1, col1, line2, col2, select;
    if line1 then
        vedjumpto(line1, col1);
        if select and vedsearchdoesselect and vedusewindows == "x" then
            ;;; mark the selection
            valof("vedselection_adjust")(line1, col1, line2, col2+1, false);
            valof("vedselection_set_primary")(true);
        endif;
    else
        if Buffer_empty() then
            vederror(Buffer_empty_str);
        else
            vederror(Cannot_find);
        endif;
    endif;
enddefine;

;;; ---- SEARCH AND SUBSTITUTE ----------------------------------------------


define ved_regexp_substitute(search_p, constrain, backward, wrap, fromhere,
                maxsubs, onceperline, repmode, silent);
    lvars
        search_p,               ;;; as returned by ved_parse_search
        constrain,              ;;; a constrain region
        backward,               ;;; true if search goes backward
        wrap,                   ;;; true if we should wrap over region boundary
        fromhere,               ;;; distance from vedline/col to start search
        onceperline,            ;;; one do one sub. per line?
        repmode,                ;;; replacement mode
        silent,                 ;;; print out messages during substitutions?
        maxsubs,                ;;; do at most maxsubs substitutions
        repstring,              ;;; replace string
        startline, startcol,    ;;; where the search starts from
        endline, endcol,        ;;; where it ends
        firstline, firstcol,    ;;; position of first match
        saveline = false, savecol, ;;; position of last substitution
        line1, col1,            ;;; start of a match
        line2, col2,            ;;; end of a match
        count = 0,              ;;; counter of number of substitutions
        msg = false,            ;;; ved message
        emptymatch,             ;;; true if the match contains no chars.
        clipboard = false,      ;;; cache of x clipboard
        colon_expressions,      ;;; @: expressions
        savechanged = vedchanged, ;;; current value of vedchanged.
      ;

    dlocal
        veddelspaces    = false, ;;; don't remove whitespace after linebreak
        vedautowrite    = false, ;;; don't save changes to disc
        vedbreak        = false, ;;; insertions/deletions don't have linebreaks
        vedhardtabs     = true,  ;;; make sure hard tabs are deleted.
        vedwrapped      = 0,     ;;; counter for wrap-around
        vvedpromptchar  = false,
      ;

    ;;; inserts a replacement string, deleting old text.
    define lconstant Do_substitution();
        lvars c, d, i, n, str = repstring, slen = datalength(str),
            dattr, dattrmode, dattrand, dattror, or_op, fast_or_op;
        dlocal pop_enable_interrupts, colon_expressions;

        define lconstant Copy_to_vveddump();
            dlocal vvedmarklo = startline, vvedmarkhi = endline;
            ved_copy();
        enddefine;

        define lconstant fast_filter_vchar(vc) /* -> (vc, dc) */;
            lvars vc;
            if iscompound(vc) then
                vc;
                fast_apply(fast_front(vc) fi_&& dattrand, dattror, fast_or_op)
                        ->> fast_front(vc)
            else
                dup(fast_apply(vc fi_&& dattrand, dattror, fast_or_op))
            endif
        enddefine;

        ;;; places sub-string of vedstring on the stack, decoding Ved tabs.
        define lconstant Dest_subdstring(i, nchars, string, decode_tabs);
            lvars i, dc, echar, nchars, string, decode_tabs;

            ;;; from vdtabs.p
            define lconstant Tab_size_at(col);
                lvars col;
                (vedindentstep fi_- ((col fi_- 1) fi_rem vedindentstep))
            enddefine;

            i fi_+ nchars fi_-1 -> echar;
            until i fi_> echar do
                fast_filter_vchar(fast_subscrvedstring(i, string)) -> dc;
                if decode_tabs and dc fi_&& 16:FFFF == `\t` then
                    Tab_size_at(i)
                else
                    1
                endif fi_+ i -> i
            enduntil
        enddefine;

        ;;; places region of vedbuffer on the stack
        define lconstant Dest_region();
            lvars vc;
            dlocal vedline, vedcolumn;
            vedjumpto(line1, col1);
            until (vedline == line2 and vedcolumn fi_> col2)
                    or vedline fi_> line2
            do
                quitif((vedvedrepeater() ->> vc) == termin);
                fast_filter_vchar(vc) ->
            enduntil
        enddefine;

        ;;; places contents of X clip buffer onto the stack
        define lconstant Dest_clipboard();
            unless vedusewindows == "x" then
                vederror('\{b}XVed not running - cannot paste');
            endunless;
            unless clipboard then
               /*
                 When the user types '%p' to paste the clipboard, we want to
                 pick up the text on the CLIPBOARD first, and on PRIMARY if
                 there is nothing on the clipboard. Because vvedclipboard
                 always searches PRIMARY then CLIPBOARD, the only way to get
                 at CLIPBOARD is to clear PRIMARY.
                */
                false -> valof("vvedclipboard");

                unless isstring(valof("vvedclipboard") ->> clipboard) then
                    vederror('\{b}nothing to paste');
                endunless;
            endunless;
            Dest_subdstring(1, datalength(clipboard), clipboard, false);
        enddefine;

        define lconstant Dest_expr(p);
            lvars procedure p;

            define dlocal cucharout(/* c */);
                fast_apply(() && dattrand, dattror, or_op);
            enddefine;

            define lconstant Pr(item);
                lvars item;
                if item.isinteger then
                    ;;; integers are treated as character codes
                    if item &&/=_0 16:FF00FF then
                        consstring(item, 1)->; ;;; this should produce an error
                    endif;
                    cucharout(item);
                else
                    ;;; all other objects are printed using pr
                    pr(item);
                endif
            enddefine;

            applist(conslist(apply(p)), Pr);
        enddefine;

        count + 1 -> count; ;;; increment counter saying how many subs are done

        if not(repmode) then ;;; Global substitution.
            ;;; if this is the first substitution, and vedsafesubstitute is on
            ;;; then we copy all of the search region to vveddump so that
            ;;; the user can undo the substitution.
            if count == 1 and vedsafesubstitute then
                Copy_to_vveddump();
            endif;
            if not(silent) and (count fi_< 10 or erase(count fi_// 10) == 0)
            then
                ;;; show the first 10 substitutions, and every 10th after that
                vedputmessage(count sys_>< nullstring)
            endif;
        elseif vedsearchdoesselect and vedusewindows == "x" then
            ;;; Interactive substitution in XVed.
            valof("vedselection_clear")(false);
        endif;

        vedjumpto(line1, col1);

        ;;; Make a string of characters found to match search string. We
        ;;; only do this if vvedfoundstring is defined, since there is
        ;;; no place where we actually use the string. (But @: expressions
        ;;; might use it).
        if isdefined("vvedfoundstring") then
            0 -> dattror; nonop || -> or_op; nonop fi_|| -> fast_or_op;
            16:FFFFFF -> dattrand;
            consvedstring(#| Dest_region() |#) -> valof("vvedfoundstring");
        endif;

        unless str.isstring then
            ;;; str contains substitution patterns so
            ;;; build replacement string on the stack.
            (#|
                fast_for i from 1 to slen do
                    fast_subscrintvec(i, repstring) -> d;
                    if (d fi_>> 24 ->> c) /== 0 then
                        d && 16:FFFFFF -> dattr;
                        if (dattr &&/=_0 16:FF) then ;;; ~
                            nonop ||/& -> or_op; nonop fi_||/& -> fast_or_op;
                            dattr  << 16 -> dattror;
                            16:FFFFFF -> dattrand;
                        else ;;; +, -, or =
                            nonop || -> or_op; nonop fi_|| -> fast_or_op;
                            dattr && 16:FF0000 -> dattror;
                            ((dattr && 16:FF00) << 8) || 16:FFFF -> dattrand;
                        endif;
                        if c == REPEAT_ALL then
                            Dest_region();
                        elseif c == PASTE then
                            Dest_clipboard();
                        elseif c == POP11_EXPR then
                            Dest_expr(
                                dest(colon_expressions) -> colon_expressions);
                        elseif c fi_< 10 then ;;; @n (n is 1-9)
                            Dest_subdstring(regexp_subexp(c, search_p), true);
                        else
                            mishap(c, 1, 'BAD SUBSTITUTE CODE');
                            ;;; what ?
                        endif;
                    else
                        d
                    endif,
                endfor,
            |#) -> str;
        endunless;

        ;;; do the substitution
        if emptymatch then
            vedjumpto(line1, col1);
        else
            ;;; start at end of match
            vedjumpto(line2, col2 + 1);
            ;;; delete characters until we hit line1/col1
            until vedline == line1 and vedcolumn == col1 do
                Delete_char();
            enduntil;
        endif;
        vedinsertstring(str);

        ;;; adjust anchor points if they've been affected by the change
        unless vedstatic then
            define lconstant Adjust(line, col, backward) -> (line, col);
                lvars line, col, backward;
                if line2 fi_< line then
                    line fi_+ (vedline fi_- line2) -> line;
                elseif line2 == line and col2 fi_< col then
                    line fi_+ (vedline fi_- line2) -> line;
                    col fi_+ (vedcolumn fi_- col2 fi_- 1) -> col;
                elseif line1 fi_< line or line1 == line and col1 fi_<= col then
                    ;;; change has deleted the anchor: reposition it at
                    ;;; one or other end of the affected region
                    ;;; depending on the direction of travel
                    if backward then
                        (vedline, vedcolumn)
                    else
                        (line1, col1)
                    endif -> (line, col);
                endif;
            enddefine;
            Adjust(startline, startcol, false) -> (startline, startcol);
            Adjust(endline, endcol, true) -> (endline, endcol);
            Adjust(firstline, firstcol, backward) -> (firstline, firstcol);
            if saveline then
                Adjust(saveline, savecol, false) -> (saveline, savecol);
            endif;
        endunless;

        ;;; if this is an interactive replace, remember the end of the
        ;;; last substitution so that cursor can be positioned there.
        if repmode then (vedline, vedcolumn) -> (saveline, savecol) endif;

        ;;; update the constrain vector since inserting/deleting text
        ;;; may have moved an anchor.
        startline, startcol, endline, endcol -> explode(constrain);

        ;;; Specify where next search starts from (may need to move cursor).
        if backward then
            ;;; move the cursor back to the start of the match
            vedjumpto(line1, onceperline and 1 or col1);
        elseif onceperline then
            ;;; start the next search at the end of this line
            vedtextright(); vedrepeater()->; 0 -> fromhere;
        elseunless emptymatch then
            ;;; we're already one character beyond the end of the
            ;;; replacement text, so start the next search from here
            0 -> fromhere;
        endif;
    enddefine; /* Do_substitution */

    ;;; STARTS HERE
    pdprops(search_p) -> repstring;
    unless repstring.isvector
    and (repstring(2).isstring or repstring(2).isintvec)
    then
        mishap(repstring, 1, 'NO REPLACEMENT STRING');
    endunless;
    repstring(3) -> colon_expressions;
    repstring(2) -> repstring;

    if repmode.isboolean then
        ;;; true = ask before each sub, false = do all subs
    elseif repmode == 0 or repmode == 1 then
        ;;; 0 = change this one, 1 = change this one and search for next.
        repmode + 1 -> maxsubs;
        0 -> fromhere;
    else
        mishap(repmode,1, 'INVALID SUBSTITUTION MODE');
    endif;
    if fromhere == true then
        if onceperline then
            ;;; if we're starting in the middle of a line, it's too
            ;;; difficult to arrange that we match on it only once, so
            ;;; start at the appropriate end
            if backward then
                screenleft();
            elseunless vedcolumn == 1 then
                vedtextright();
            endif;
        endif;
        ;;; we really do want to start here, not one step beyond ...
        0 -> fromhere;
    endif;
    if regexp_anchored(search_p) then
        ;;; if the pattern starts with ^ or @a, or ends with $ or @z, then
        ;;; we should only consider each line once, regardless of what the
        ;;; user says.
        true -> onceperline;
    endif;

    ;;; calculate where the region in the buffer is.
    Constrain_coords(constrain) -> (startline, startcol, endline, endcol);
    consvector(startline, startcol, endline, endcol, 4) -> constrain;
    -1 ->> firstline -> firstcol;

    ;;; SEARCH LOOP

    vedpositionpush();
    unless maxsubs.isinteger then 2:1e28 -> maxsubs endunless;

    while count fi_< maxsubs do
        ;;; perform the search
        ved_regexp_search(search_p, constrain, backward, wrap, fromhere, false)
                -> (line1, col1, line2, col2);

        unless line1 then
            ;;; there is no match.
            if firstline == -1 then
                Cannot_find -> msg
            elseif repmode then
                '\{b}that\'s all' -> msg;
            endif;
            quitloop; ;;; quit the search
        endunless;

        ;;; next time round we go one position from the cursor location
        true -> fromhere;

        ;;; set emptymatch to true if the pattern matches an empty string
        line1 == line2 and col2 fi_< col1 -> emptymatch;

        if firstline == -1 then
            ;;; this is the first match - remember where it was
            if backward then
                (line2, emptymatch and col1 or col2)
            else
                (line1, col1)
            endif -> (firstline, firstcol);
            0 -> vedwrapped;
        elseif vedwrapped /== 0 then
            ;;; we have gone round once: quit if this match overlaps
            ;;; with the first one
            if backward then
                ;;; quit if start of this match overlaps end of first match
                quitif(Is_before(line1, col1, firstline, firstcol));
            else
                ;;; quit if end of this match overlaps start of first match
                quitif(Is_before(firstline, firstcol, line2, emptymatch and
                                                             col1 or col2));
            endif;
            false -> wrap; ;;; don't wrap again
        endif;

        ;;; for interactive substitution, remember where the match
        ;;; started so we can return here at the end
        if repmode then (line1, col1) -> (saveline, savecol) endif;

        ;;; replacement modes 0 and 1
        if repmode == 1 and count == 1 then
            if vedsearchdoesselect and vedusewindows == "x" then
                ;;; mark the selection
                valof("vedselection_adjust")(line1, col1, line2, col2+1, false);
            endif;
            quitloop;

        ;;; interactive replacement
        elseif repmode == true then
            vedjumpto(line1, col1);
            if vedsearchdoesselect and vedusewindows == "x" then
                ;;; select the match
                valof("vedselection_adjust")(line1, col1, line2, col2+1, false);
            endif;
            vedputmessage('\{b}OK to replace?');
            lvars _char;
            repeat  ;;; until proper answer read in
                vedwiggle(vedline,vedcolumn);
                uppertolower(vedinascii()) -> _char;    ;;; make uppercase work
                if _char == `\^?` or _char == `\^H` then
                    ;;; DEL don't do this substitution, but continue
                    unless backward or emptymatch then
                        vedjumpto(line2, col2);
                    endunless;
                    nextloop(2)
                elseif _char == `g` then
                    ;;; do it globally
                    false -> repmode;
                    if vedsearchdoesselect and vedusewindows == "x" then
                        ;;; clear the selection made above
                        valof("vedselection_clear")(false);
                    endif;
                    quitloop;
                elseif _char == `n` then
                    ;;; No don't do any more
                    quitloop(2)
                elseif strmember(_char,'y\r') then
                    ;;; User typed either <RETURN> (do change and continue)
                    ;;; or `y` (do change and stop).
                    Do_substitution();
                    if _char == `\r` then nextloop(2)
                    else quitloop(2)    ;;; user typed `y`
                    endif
                else
                    ;;; Wrong character typed in, prompt for another
                    vedputmessage('\{b}OK? [y, n, <RETURN>, <DEL> or g]')
                    ;;; Then restarts inner loop
                endif;
            endrepeat;
        endif;
        ;;; actually perform the substitution.
        Do_substitution();

    endwhile;

    vedpositionpop();

    if repmode == true and vedsearchdoesselect and vedusewindows == "x"
                and firstline /== -1 then
        ;;; found something - need to clear it.
        valof("vedselection_clear")(false);
    endif;

    if saveline then
        ;;; we need to jump to this line/col
        vedjumpto(saveline, savecol);
    endif;

    ;;; set the default message - and display the message.
    vedputmessage(msg or ((count sys_>< nullstring) <> ' \{b}substitutions'));

    ;;; increment vedchanged by count.
    count + (savechanged.isinteger and savechanged or 0) -> vedchanged;
enddefine; /* ved_regexp_substitute */


;;; ---- PARSING SEARCH AND SUBSTITUTION PATTERNS --------------------------

;;; pushes string through itemiser to handle backslash chars - not very elegant
define lconstant Itemise_string(string, should_err) -> string;
    lvars string, i, c, irep, should_err, strlen;
    if string.isword then fast_word_string(string) -> string endif;
    if string = '\\' then '\\\\' -> string endif;
    datalength(string) -> strlen;
    incharitem(stringin(consstring(#|
        `'`,
        fast_for i from 1 to strlen do
            fast_subscrdstring(i, string) -> c;
            if c && 16:FF == `'` then `\\` endif, c;
        endfor,
        `'`,
    |#))) -> irep;
    ;;; now push it though the itemiser - this will produce an ugly
    ;;; mishap if the string has badly formed \ sequences. We trap this:
    procedure;
        dlocal pop_longstrings = true;

        define dlocal pop_exception_final(count, mess, idstring, sev);
            lvars count, mess, idstring, sev;
            returnunless(isstartstring('incharitem-', idstring)
                         and isendstring(':syntax', idstring)) (false);
            if should_err then
                vederror('\{b}error parsing `\\` in \'' <> string <> '\{b}\'')
            else
                erasenum(count);
                exitfrom(false, Itemise_string);
            endif
        enddefine;

        irep()
    endprocedure() -> string;
enddefine;      /* Itemise_string */

;;; Takes a string+index, compiles one expression from the string starting
;;; just after the index, and returns a procedure and the new index. The
;;; procedure, when called, returns a count of the number of things that
;;; the expression left on the stack.
define lconstant Compile_@:_expression(i, string) -> (i, pdr);
    dlvars i, string, pdr;

    define lconstant count;
        i + 1 -> i;
    enddefine;

    lvars irep = incharitem(stringin(allbutfirst(i, string)) <> count);

    procedure; ;;; needed to get dlocal right
        dlocal proglist_state = proglist_new_state(pdtolist(irep));
        sysPROCEDURE(false, 0);
            lvars len_var = sysNEW_LVAR();
            sysCALL("stacklength"), sysPOP(len_var);
            pop11_comp_expr();
            sysCALL(sysCALL("stacklength"), sysPUSH(len_var), "fi_-");
        sysENDPROCEDURE() -> pdr;
    endprocedure();
enddefine;

define lconstant Prepare_search(regexp, repstring,
                insensitive, anywhere, should_err) -> regexp;
    lvars regexp, repstring, anywhere, insensitive,
        err, n, flags, c, d, dattr, i, slen, should_err,
        colon_expressions = [];

    ;;; CHECK ARGUMENTS
    unless regexp.isstring or regexp.isword then
        mishap(regexp, 1, 'STRING or WORD NEEDED');
    endunless;
    unless not(repstring) or repstring.isstring or repstring.isword then
        mishap(repstring, 1, 'STRING or WORD NEEDED');
    endunless;
    ;;; PARSE REGEXP
    2:11100 -> flags;
    if insensitive then flags || 2:1 -> flags endif;
    unless anywhere then flags || 2:10 -> flags endunless;

    ;;; use vedwildcards to determine the escape character and mode
    lvars esc_char;
    if vedwildcards == "ed" then
        `\\` -> esc_char;
        flags || 2:1e5 -> flags;
    elseif vedwildcards then
        `@` -> esc_char;
    else
        false -> esc_char;
    endif;

    unless esc_char == `\\` then
        ;;; itemise the regexp
        returnunless(Itemise_string(regexp, should_err) ->> regexp);
    endunless;
    ;;; compile the regexp
    if (regexp_compile(regexp, flags, false, esc_char) -> regexp ->> err) then
        if should_err then
            vederror(err);
        else
            return(false -> regexp);
        endif;
    endif;
    ;;; PARSE REPSTRING
    ;;; parses the replacement string looking for @1, @2, @&, @% etc.
    if repstring then
        ;;; itemise the string first
        if esc_char == `\\` and strmember(`\\`, repstring) then
            ;;; translate from \ patterns to @ patterns
            datalength(repstring) -> slen;
            consstring(#|
                for i from 1 to slen do
                    if subscrs(i, repstring) == `\\` and i < slen and
                            ((subscrs(i + 1, repstring) ->> c).isnumbercode
                            or strmember(c, '&:+-=~p')) then
                        ;;; \1 -> @1 etc
                        `@`
                    elseif (subscrs(i, repstring) ->> c) == `@` then
                        ;;; @ -> @@
                        `@`, `@`
                    else
                        c
                    endif;
                endfor;
            |#) -> repstring;
        endif;
        Itemise_string(repstring, should_err) -> repstring;
        returnunless(repstring)(false -> regexp);
        if strmember(`@`, repstring) then
            datalength(repstring) -> slen;
            consintvec(#|for i from 1 to slen do
                subscrdstring(i, repstring) -> d;
                d && 16:FF -> c;
                d && 16:FF0000 -> dattr;
                if c == `@` then
                    unless i < slen then
                        ;;; @ at the end of the repstring is substituted as @
                        d
                    else
                        i + 1 -> i;
                        subscrdstring(i, repstring) -> d;
                        d && 16:FF -> c;
                        lvars dattrmode = `+`;
                        if i /== slen and strmember(c, '+-=~') then
                            c -> dattrmode;
                            i + 1 -> i;
                            subscrs(i, repstring) -> c;
                        endif;
                        if dattrmode == `=` then
                            ;;; do nothing
                        elseif dattrmode == `+` then
                            dattr || 16:FF00 -> dattr;
                        elseif dattrmode == `~` then
                            (dattr >> 16) -> dattr;
                        else ;;; dattrmode == `-`
                            (dattr ||/& 16:FF0000) >> 8 -> dattr;
                        endif;
                        if isnumbercode(c) then
                            ;;; refers back to a parenthesis bracket
                            c - `0` -> d;
                            if d == 0 or d > regexp_subexp_count(regexp) then
                                returnunless(should_err)(false -> regexp);
                                vederror('\{b}@ digit out of range');
                            endif;
                            (d << 24) || dattr
                        elseif c == `&` then
                            (REPEAT_ALL << 24) || dattr;
                        elseif uppertolower(c) == `p` then
                            (PASTE << 24) || dattr
                        elseif c == `:` then
                            lvars p;
                            Compile_@:_expression(i, repstring) -> (i, p);
                            p :: colon_expressions -> colon_expressions;
                            (POP11_EXPR << 24) || dattr
                        elseif c == `n` then
                            `\n`
                        else
                            ;;; unknown @ pattern - use next character
                            d && 16:FF -> c;
                            if c == `+` or c == `-` then i - 1 -> i endif;
                            d
                        endif;
                    endunless;
                else
                    d
                endif;
            endfor |#) -> repstring;
        endif;
        ;;; set pdprops to a pair containing search and substitution string.
        {% pdprops(regexp), repstring, rev(colon_expressions) %}
            -> pdprops(regexp);
    endif;
enddefine; /* Prepare_search */

define ved_regexp_compile = Prepare_search(%true%) enddefine;
define ved_regexp_try_compile = Prepare_search(%false%) enddefine;

;;; ---- SEARCH STATE ------------------------------------------------------

lconstant
    ;;; a search vector with all of the defaults set up.
    default_search_data = {%
            false,  ;;; search procedure
            "file", ;;; search region
            true,   ;;; wrap around end of region
            true,   ;;; start from current cursor position
            false,  ;;; number of times to search
            false,  ;;; search string
            false,  ;;; substitute string
            false,  ;;; search result for last search
            false,  ;;; vvedanywhere
        %},
;

global constant vedsearchdata = writeable copy(default_search_data);

;;; returns true if we can redo a search
define lconstant Can_redo;
    subscrv(VEDSRCH_SEARCH_STRING, vedsearchdata).isstring;
enddefine;

;;; performs the search that is set up in vedsearchdata.
define lconstant Do_search(backward, select, should_err);
    lvars backward, isreplace, srch, search_p, result, should_err, select;
    ;;; backward specifies search direction.
    ;;; if should_err is -true- then we call vederror if something goes wrong,
    ;;; otherwise we return true or false to indicate status.
    ;;; if select is -true- the text is selected after it is found.

    vedsearchdata -> srch;
    ;;; COMPILE SEARCH PATTERN
    unless subscrv(VEDSRCH_SEARCH_P, srch) then
        ved_regexp_compile(
            subscrv(VEDSRCH_SEARCH_STRING, srch),
            subscrv(VEDSRCH_SUBSTITUTE_STRING, srch),
            false,
            subscrv(VEDSRCH_ANYWHERE, srch)
          ) -> subscrv(VEDSRCH_SEARCH_P, srch);
    endunless;

    ;;; do a search
    consvector(ved_regexp_search(
            subscrv(VEDSRCH_SEARCH_P, srch),
            subscrv(VEDSRCH_CONSTRAIN, srch),
            backward,
            subscrv(VEDSRCH_WRAP, srch),
            subscrv(VEDSRCH_FROM_HERE, srch),
            subscrv(VEDSRCH_COUNT, srch),
        ), 4) ->> result -> subscrv(VEDSRCH_RESULT, srch);

    /* SET VEDFOUNDLINE/COL FOR COMPATIBILITY */

    if should_err or result(1) then
        ;;; jump to the resulting text
        ved_regexp_jumpto(explode(result), select);
    endif;

    unless should_err then
        ;;; return a status flag
        return(result(1).isinteger);
    endunless;
enddefine; /* Do_search */

;;; sets what to look for next time round.
define lconstant Setup_search(item, repstr, options, should_err);
    lvars item, repstr, options, should_err,
            constrain = "file", insensitive = false,
            embed = true, srch, wrap = true;

    lvars opt;
    for opt in options do
        if opt == "nocase" then
            true -> insensitive;
        elseif opt == "nowrap" then
            false -> wrap;
        elseif opt == "noembed" then
            false -> embed;
        elseif opt == "back" then
            ;;; ignore this
        else
            opt -> constrain;
        endif;
    endfor;

    unless item.isstring then
        false -> embed;
        item sys_>< nullstring -> item;
    endunless;

    unless (constrain.isvector and datalength(constrain) == 4) or
                (Is_constrain_class(constrain) ->> constrain) then
        mishap(constrain, 1, 'INVALID CONSTRAIN REGION');
    endunless;

    ;;; start from the default search

    lvars old_repstr = subscrv(VEDSRCH_SUBSTITUTE_STRING, vedsearchdata);

    explode(default_search_data) -> explode(vedsearchdata);
    vedsearchdata -> srch;

    if repstr then
        repstr sys_>< nullstring
    else
        ;;; resuse the last replace string
        old_repstr
    endif ->> repstr -> subscrv(VEDSRCH_SUBSTITUTE_STRING, srch);

    item -> subscrv(VEDSRCH_SEARCH_STRING, srch);
    constrain -> subscrv(VEDSRCH_CONSTRAIN, srch);
    wrap -> subscrv(VEDSRCH_WRAP, srch);

    ;;; set old variables to maintain compatibility
    embed and `/` -> subscrv(VEDSRCH_ANYWHERE, srch);

    ;;; compile the search
    if should_err then
        ved_regexp_compile(item, repstr, insensitive, embed)
            -> subscrv(VEDSRCH_SEARCH_P, srch);
    else
        ved_regexp_try_compile(item, repstr, insensitive, embed) -> item;
        returnunless(item)(false);
        item -> subscrv(VEDSRCH_SEARCH_P, srch);
    endif;
    true;
enddefine; /* Setup_search */

define ved_set_search(item, repstr, options);
    lvars item, repstr, options;
    Setup_search(item, repstr, options, true) ->;
enddefine;

;;; returns what was last looked for and where it was found
define ved_query_last_search() with_nargs 5;

    subscrv(VEDSRCH_SEARCH_STRING, vedsearchdata),

    lvars result = subscrv(VEDSRCH_RESULT, vedsearchdata);
    if result.isvector and result(1).isinteger then
        explode(result)
    else
        false, false, false, false
    endif;
enddefine;

define active:9 ved_search_state;
    explode(vedsearchdata);
enddefine;

define updaterof active ved_search_state with_nargs 9;
    () -> explode(vedsearchdata);
enddefine;


;;; ---- HIGH LEVEL SEARCH PROCEDURES --------------------------------------

define lconstant Find(string, options, should_err);
    lvars string, options, should_err;
    ;;; we do a simple check to see if we are performing a repeated search
    ;;; for the same item.
    unless options == []
            and string == subscrv(VEDSRCH_SEARCH_STRING, vedsearchdata) then
        unless Setup_search(string, false, options, should_err) then
            ;;; could not parse the pattern
            return(false);
        endunless;
    endunless;
    Do_search(fast_lmember("back", options), false, should_err);
enddefine;

define ved_try_search = Find(%false%); enddefine;
define ved_check_search = Find(%true%); enddefine;

;;; ---- COMMAND LINE INTERFACE ----------------------------------------------

;;; splits vedargument into search string, replacement string & options string
define lconstant Split_search_string(str, isreplace)
                        -> (delim, pattern, repstring, options);
    lvars str, isreplace, delim, strlen, i, pattern, repstring, options,
            regex, c;
    false ->> delim ->> repstring -> pattern; nullstring -> options;
    returnunless(str.isstring and datalength(str) >= 1);
    str(1) -> delim; allbutfirst(1, str) -> str;
    returnif(str == nullstring);

    ;;; EXTRACT THE SEARCH PATTERN
    if delim == `\\` then
        ;;; the \ delimeter is a real pain because \ is also used by the
        ;;; itemiser. We handle this one specially:
        lconstant Backslash_chars = 'nbtsre^(GS[{\\';
        datalength(str) -> strlen;
        returnif(strlen == 1 and str(1) == `\\`); ;;; empty search string
        for i from 1 to strlen do
            if subscrs(i, str) == `\\` then
                if i == strlen then
                    ;;; \ at the end of the string
                    allbutlast(1, str) -> pattern;
                    return;
                else
                    i + 1 -> i;
                    unless strmember(subscrs(i, str), Backslash_chars) then
                        substring(1, i-2, str) -> pattern;
                        quitloop;
                    endunless;
                endif;
            endif;
        endfor;
        returnif(i > strlen)(str -> pattern);
    else
        ;;; the easiest way to find the end of a regular expression is to call
        ;;; the compiler on it. This is wasteful, but doesn't take very long.
        if (regexp_compile(str, 0, delim) -> regex ->> i) then
            vederror(i)
        endif;
        pdprops(regex) -> pattern;
        regexp_delimeter(regex) -> i;
    endif;

    if i then
        ;;; found a delimeter in the expression. May need to look further
        if i == 1 then
            ;;; the first character of the expression is the delimeter
            nullstring -> pattern; ;;; empty search string
        elseif i == datalength(str) then
            ;;; the delimeter is the last character of the expression
            return;
        endif;
        allbutfirst(i, str) -> str;
        if isreplace then
            ;;; EXTRACT THE SUBSTITUTION PATTERN
            str -> repstring;
            datalength(str) -> strlen;
            fast_for i from 1 to strlen do
                subscrs(i, str) -> c;
                if c == `@` and i /== strlen then
                    ;;; skip the next character
                    i + 1 -> i;
                elseif c == delim then
                    ;;; found the closing delimeter
                    subdstring(1, i - 1, str) -> repstring;
                    substring(i + 1, strlen - i, str) -> options;
                    quitloop;
                endif;
            endfast_for;
        else
            str -> options;
        endif;
    endif;
enddefine; /* Split_search_string */

;;; the following two procedures are used to redraw the command line:

;;; explodes the command name for an enter command
define lconstant Explode_command_name;
    lvars i;
    if vedargument
    and (datalength(vedcommand) - datalength(vedargument) ->> i) /== 0 then
        explode(substring(1, i, vedcommand));
        ;;; remove any whitespace
        while vedchartype(dup()) == `\s` do erase() endwhile,
    endif;
enddefine;

;;; explodes a search or replace pattern, escaping delimeters if neccessary
define lconstant Explode_pattern(str, delim_char);
    lvars str, delim_char, i, c, d, strlen = datalength(str);
    fast_for i from 1 to strlen do
        subscrdstring(i, str) -> d;
        d && 16:FF -> c;
        if c == `@` and i /== strlen then
            i + 1 -> i;
            d, subscrdstring(i, str);
        elseif c == delim_char then
            `@`, d
        else
            d
        endif
    endfast_for;
    if delim_char then delim_char endif;
enddefine;

;;; Used by all <ENTER> commands.
define ved_search_or_substitute(defoptions, mode);
    lvars
        defoptions, mode, srch,
        isreplace, delim, pattern, repstring, options,
        i, option, modkind = "+", optionrep, isredo = false,
        insensitive = false, embed = undef, backward = undef, count = false,
        once = false, ask = true, silent = false,
      ;

    unless vedargument.isstring then
        ;;; e.g. redoing last search via ved_re_search or ved_re_backsearch
        unless Can_redo() then
            vederror(No_search_string);
        endunless;
        Do_search(defoptions == true, true, true);
        return;
    endunless;

    nullstring ->> pattern -> options; false -> repstring;

    mode == true -> isreplace;

    if mode == undef then
        false -> delim; ;;; no delimeters
        vedargument -> pattern;
    else
        ;;; divide the vedargument string into its components and delimeter
        Split_search_string(vedargument, isreplace)
            -> (delim, pattern, repstring, options);
    endif;

    mode == true or isstring(mode) -> isreplace;

    unless pattern.isstring and pattern /= nullstring then
        ;;; the pattern is still empty - see if there is an existing pattern.
        unless
        (subscrv(VEDSRCH_SEARCH_STRING, vedsearchdata) ->> pattern)
        and pattern.isstring and pattern /= nullstring
        then
            ;;; no previous search data to redo search on
            vederror(No_search_string);
        endunless;
        if isreplace and vedargument = nullstring then
            ;;; replacement with empty vedargument - redo last replace:
            mode.isstring and mode or
            subscrv(VEDSRCH_SUBSTITUTE_STRING, vedsearchdata) -> repstring;
            if subscrv(VEDSRCH_ANYWHERE, vedsearchdata) then `/` else `"` endif
                    -> delim;
        endif;
        true -> isredo;
    endunless;

    if isreplace and not(repstring) then
        ;;; this is a replace and the replacement string is empty
        if mode.isstring then
            mode -> repstring;
        else
            vederror('\{b}no substitute string');
        endif;
    endif;

    ;;; create a new search data structure
    explode(default_search_data) -> explode(vedsearchdata);
    vedsearchdata -> srch;

    ;;; fill in the search string and replace string
    pattern -> subscrv(VEDSRCH_SEARCH_STRING, srch);
    repstring -> subscrv(VEDSRCH_SUBSTITUTE_STRING, srch);

    ;;; SET COMMAND LINE

    if isredo then
        ;;; need to refresh the status line
        if not(delim) and mode.isboolean then
            `/` -> delim;
        endif;
        vedputcommand(consdstring(#|
                ;;; the command
                Explode_command_name(),
                ;;; the pattern
                delim or `\s`,
                Explode_pattern(pattern, delim),
                ;;; the replace string
                if isreplace then Explode_pattern(repstring, delim) endif,
                ;;; any options
                if options /= nullstring then `\s`, options.explode endif,
            |#));
    endif;

    if not(repstring) then
        ;;; this is not a replace - must remember the last replace string
        subscrv(VEDSRCH_SUBSTITUTE_STRING, vedsearchdata) -> repstring;
    endif;

    incharitem(stringin(defoptions.isstring and defoptions or options))
            -> optionrep;

    ;;; PARSE OPTIONS

    ;;; test for a constrain class as the first option:
    optionrep() -> option;
    if option.isword and Is_constrain_class(option) ->> i then
        i -> subscrv(VEDSRCH_CONSTRAIN, srch);
        optionrep() -> option;
    endif;

    ;;; parse all the other options
    until option == termin do
        ;;; detects a constrain modifier
        if option == "+" or option == "-" then
            option == "+" -> modkind;
        elseif isinteger(option) then
            option -> count;
        elseunless option.isstring or option.isword then
            vederror('\{b}illegal option: ' <> (option sys_>< nullstring));
        elseif isstartstring(option, "back") then
            modkind -> backward;
        elseif isstartstring(option, "case") then
            not(modkind) -> insensitive;
        elseif isstartstring(option, "embed") then
            modkind -> embed;
        elseif isstartstring(option, "here") then
            modkind -> subscrv(VEDSRCH_FROM_HERE, srch);
        elseif isstartstring(option, "wrap") then
            modkind -> subscrv(VEDSRCH_WRAP, srch);
        elseif isreplace and isstartstring(option, "every") then
            not(modkind) -> once;
        elseif isreplace and isstartstring(option, "ask") then
            modkind -> ask;
        elseif isreplace and isstartstring(option, "verbose") then
            not(modkind) -> silent;
        else
            vederror('\{b}illegal option: ' <> (option sys_>< nullstring));
        endif;
        optionrep() -> option;
    enduntil;

    ;;; SET DEFAULTS

    if backward == undef then
        ;;; use delimeter to determine direction
        (delim == `\\` or delim == ```) or defoptions == true -> backward;
    endif;
    if embed == undef then
        ;;; use delimeter to determine embed mode
        delim /== ``` and delim /== `"` -> embed;
    endif;
    if count and count < 0 then
        ;;; reverse the search direction
        abs(count) -> count;
        not(backward) -> backward;
    endif;

    ;;; set this for compatibility
    embed and `/` -> subscrv(VEDSRCH_ANYWHERE, srch);

    ved_regexp_compile(pattern, repstring, insensitive, embed)
            -> subscrv(VEDSRCH_SEARCH_P, srch);

    if isreplace then
        ved_regexp_substitute(
            subscrv(VEDSRCH_SEARCH_P, srch),
            subscrv(VEDSRCH_CONSTRAIN, srch),
            backward,
            subscrv(VEDSRCH_WRAP, srch),
            subscrv(VEDSRCH_FROM_HERE, srch),
            count, once, ask, silent);
    else
        count -> subscrv(VEDSRCH_COUNT, srch);
        vedputmessage('\{b}searching ...');
        Do_search(backward, true, true);
        vedputmessage('\{b}found');
    endif;
enddefine; /* ved_search_or_substitute */

;;; ---- ENTER COMMANDS FOR SEARCH/SUBSTITUTE ------------------------------

define vars ved_search =
    ved_search_or_substitute(%false, false%)
enddefine;

define vars ved_backsearch =
    ved_search_or_substitute(%true, false%)
enddefine;

define vars ved_re_search();
    dlocal vedargument = false;
    unless Can_redo() then
        vederror(No_search_string)
    endunless;
    ved_search();
enddefine;

define ved_re_backsearch;
    dlocal vedargument = false;
    unless Can_redo() then
        vederror(No_search_string)
    endunless;
    ved_backsearch();
enddefine;

define vars ved_s =
    ved_search_or_substitute(%false, true%)
enddefine;

define vars ved_gs =
    ved_search_or_substitute(%'-ask -here', true%)
enddefine;

define vars ved_gsr =
    ved_search_or_substitute(%'range -ask -here', true%)
enddefine;

define Safe_search(p);
    lvars procedure p;
    dlocal vedwildcards = true, ved_search_state;
    p();
enddefine;

endsection;     /* $-Sys$-Ved */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  3 1996
        Made local pop_exception_final in Itemise_string test fro appropriate
        id-string.
--- John Gibson, Mar 11 1996
        Made ved_regexp_substitute locally set vvedpromptchar false.
--- John Gibson, Feb  6 1996
        Itemise_string uses local pop_exception_final instead of
        pr*mishap
--- John Gibson, Nov 18 1995
        Changed substitution routines to conserve embedded data in vedstrings
--- Robert John Duncan, Jul 27 1995
        Fixed ved_search_or_substitute to leave a space between the command
        name and argument when mode == "undef", i.e. for commands such as
        ved_ss, etc. which don't have a delimiter.
--- Robert John Duncan, Jun  7 1995
        Further changes to ved_regexp_substitute: it now starts searching
        from the cursor position (fromhere = 0) and finishes with the cursor
        on the last match or substitution, rather than always on the last
        substitution (i.e. saveline/savecol are set for each successful match
        as well as for each substitution)
--- Robert John Duncan, Jun  6 1995
        Fixed the interactive mode of ved_regexp_substitute so that it no
        longer ignores some possible substitutions or, conversely, offers
        for a second time matches which have already been rejected. The bug
        was caused by Do_substitution resetting firstline and firstcol to
        be the first substitution done rather than the first match found.
        Simply removing the assignments exposed errors in checking for
        termination of the search, and particularly in adjustment of the
        anchor points of the search after a substitution, so those things
        have had to be changed as well.
--- John Gibson, Feb 18 1994
        Made Decode_type assign false to vvedmarkprops so any temporary
        marked range created doesn't show in the window.
--- John Gibson, Jan 17 1994
        Fixed ved_search_or_substitute to set delim correctly when redoing
        a substitution (at least, correctly as regards embedding or not).
 */
