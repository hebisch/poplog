/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/lib/reformat.p
 > Purpose:         Automatic hard-copy manual producer
 > Author:          Diarmuid M. McIntyre, Jul 18 1993 (see revisions)
 > Documentation:   HELP * REFORMAT REF * REFORMAT
 > Related Files:   C.all/lib/lib/reformat/rf.start.tex
                    C.all/lib/lib/reformat/rf.end.tex
                    C.all/lib/lib/reformat/manualmaster.tex
                    C.all/lib/lib/reformat/makemanual
 */


#_TERMIN_IF DEF POPC_COMPILING


/*
                          ----------------------
                           THE REFORMAT PROGRAM
                          ----------------------



         CONTENTS - (Use <ENTER> g to access required sections)

 -- STUFF DONE BEFORE CALLING THE FILE UP AND BEFORE FORMATTING
 -- Variables, Libraries and Settings
 -- Short Procedures (used in all sections)
 -- Mishap procedures
 -- Preprocessing before formatting
 -- BEGINNING OF ACTIVE FORMATTING
 -- Control Procedures
 -- Section (sub)Headings
 -- Paragraphs
 -- Identifier headings
 -- Verbatim text (program code)
 -- Lists
 -- ... Bullet Lists
 -- ... Enumerated Lists
 -- ... Descriptive Lists
 -- Pictures (a provisional attempt)
 -- Tables
 -- Revision History
 -- Cross References
 -- ... to old style identifiers
 -- ... to documentation and other chapters
 -- Dealing with Dstrings
 -- General Substitutions
 -- Previewing the file as a potential chapter
 -- Creating Manual from REF files


The REFORMAT program (SEE HELP * REFORMAT) processes individual master
versions of REF files to be chapters in a automatically produced LATEX
Poplog manual. It also brings these chapters together to form the manual.
Files can be previewed individually as potential chapters in the manual
complete with a mini Contents and Index.

The REFORMAT program can be  divided up into 4 distinct stages.

   o The Preparation stage.

     Where globals are declared and initialised, libraries which are
     needed are loaded, and various important VED variables (i.e. vedbreak)
     given new values.
     In addition the specified file is taken and preprocessed to strip it
     of certain on-line features no longer necessary and the occasional
     substitution made.


   o The Formatting stage.

     This essentially goes  from the  top downwards, placing  the text  in
     appropriate environments.  The headings  in  the contents  show  what
     types  of  environments   there  are.   Environments  are   generally
     one-dimensional. However due to the embedded nature of the REF  files
     allowance has been made for recursive embedding of environments. When
     an embedded  environment  is  entered  into,  the  global  identation
     variables are saved and restored when the embedding ends.


   o The Post formatting stage

     This deals with cross references, attributed dstrings, and general
     substitutions.


   o The manual creation/previewing stage

     This takes either an individual ora master file and causes the
     above stages to be run on it. It then provides the result as input
     to the LATEX, Index making, and XDVI programs.


   */


/*
-- STUFF DONE BEFORE CALLING THE FILE UP AND BEFORE FORMATTING --------
*/


/*
-- Variables, Libraries and Settings ----------------------------------
*/

global vars rf_dir = syssearchpath(popuseslist, 'reformat');

unless rf_dir.isstring and rf_dir.sysisdirectory then
   mishap(0, 'COULD NOT LOCATE REFORMAT DIRECTORY');
endunless;

global vars rf_start_tex = rf_dir dir_>< 'rf.start.tex';
global vars rf_end_tex = rf_dir dir_>< 'rf.end.tex';


vars procedure (get_history, get_title, process_contents_and_overview,
subs_before_formatting, whole_file_action, delete_ending, preprocess,
format_actions, post_formatting_actions, is_heading,
heading_action, is_identifier_heading, ident_heading_action, substructure,
place_revision_history, component_parts, is_code, code_action,
is_bullet_list, bullet_action, is_enumerated_list, enumerated_action,
is_descriptive_list, descriptive_list_action, is_picture, picture_action, is_table,
table_action, is_paragraph, paragraph_action, old_identifier,
ref_reference_action, replace_dstrings, general_substitutions,
replace_double_quotes, graphics_char_substitutions, check_for_ref, datasearch, find_dstring,
appropriate_dstring_action);

global vars global_indent, included_reffiles, indent_size, in_a_list_environment, subs_list,
heading_type, title, authorlist, non_existent_identifiers, words_subs_list, graphics_subs_list,
chars_subs_list,islispfile, all_reffiles_included;

/* variable initialisations */

/* The following is the list of substitutions to be made after the text
has been formatted. Most of these are done to prevent problems with LATEX
special characters. Some of the strings to be substituted have already been
replaced by placement holders*/

    [ ['\\Gle' '-'] ['\\Gre' '-'] ['\\Gte' '|'] ['\\Gbe' '|']
        ['\\Gtl' '-'] ['\\Gtr' '-'] ['\\Gbl' '-'] ['\\Gbr' '-']
        ['\\Glt' '|'] ['\\Grt' '|'] ['\\Gtt' '-'] ['\\Gbt' '-']
        ['\\G-' '-'] ['\\G|' '|'] ['\\G+' '+'] ['\\Go' 'o']
        ['\\G#' '#'] ['\\G.' '.'] ] -> graphics_subs_list;


        [ ['-->' '\\rightarrow']   ;;; this might be used for id headings.
         ['---' ' --- ']
         ['!z!' '\\&']         ;;; '!z!' is The placement holder for &
         ['%' '\\%']
         ['#' '\\#']
         [log2 'log_[2]']
         ['H_Y_W' '\\~{}\\~{}']
         ['|_bl_' '$|$']
         ['_' '\\_']
         ['$HOME' '\\verb+$HOME+']
         ['^^' '{\\tt\\symbol{94}\\symbol{94}}']
         ['^' '{\\tt\\symbol{94}}']
         ['>' '\\verb+>+' ]
         ['<' '\\verb+<+']
         ['bachslashh' '$\\backslash$']
         ['lollol' '\\{']
         ['lcllcl' '\\}']
         [' label{' '\\label{']] -> chars_subs_list;


        [ ['This file' 'This chapter']
         ['this file' 'this chapter']
         ['This REF file' 'This chapter']
         ['This REF  file' 'This chapter']
         ['This  REF  file' 'This chapter']
         ['this REF file' 'this chapter']
         ['this  REF file' 'this chapter']
         ['this  REF  file' 'this chapter']
         ['This ref file' 'This chapter']
         ['This ref  file' 'This chapter']
         ['this ref file' 'this chapter']] -> words_subs_list;

     [ ^^chars_subs_list ^^words_subs_list ]->subs_list;


/* The following  starting values are updated by the program */

false -> all_reffiles_included;
false -> islispfile;
0 -> in_a_list_environment;
1 -> global_indent;
6.15 -> indent_size;
[] -> non_existent_identifiers;
[] -> included_reffiles;



/*
-- Short Procedures (used in all sections) ----------------------------
*/

/* this piece of code removes certain characters from a word. This is
done so LATEX can create a usable internal label. There is a slight
difference between this and the procedure index_label in that because this
an internal label, the underscores do not have to be replaced with @ for
substitution later. However other troublesome characters do have to be
removed*/

define prepare_internal_label(label) -> internal_label;
    lvars hold, len,item;
    if isword(label) then
        consstring(destword(label))-> label;
    endif;
    if islispfile then
        'lisp_' sys_><label -> label;
    elseif issubstring('vms_',vedcurrent) then
        'vms_'  sys_><label -> label;
    endif;
    consword(label)->label;
    conslist(destword(label)) -> hold;
    delete(95,hold)->hold;  ;;; _
    delete(61,hold)->hold;  ;;; =
    delete(94,hold)->hold;  ;;; ^
    delete(62,hold)->hold;  ;;; >
    delete(37,hold)->hold;  ;;; %
    delete(60,hold)->hold;  ;;; <
    delete(92,hold)->hold;  ;;; \
    delete(36,hold)->hold;  ;;; $
    delete(35,hold)->hold;  ;;; #
    length(hold)->len;
    for item in hold do
        item;
    endfor;
    consword(len)->internal_label;
enddefine;



/* this piece of code removes all the underscores (replacing them with @)
from a word. This is done so that LATEX can create an index properly. The
@'s will be resubstituted later when the text is being latexed */

define prepare_index_label(label)->indexlabel;
    lvars len, hold, newhold, item, x;
    if isword(label) then
        consstring(destword(label))-> label;
    endif;
    consword(label) -> label;
    conslist(destword(label)) -> hold;
    maplist(hold, procedure(x);
          if x = 95 then 64; else x; endif; endprocedure) ->newhold;
    length(newhold)->len;
    for item in newhold do
        item;
    endfor;
    consword(len)->indexlabel;
enddefine;



/* this procedure returns the starting location of text on the next line.
If the next line is empty, it returns false */

define thislinestart() -> indentation;
   unless (skipchar(` `,1,vedthisline()) ->>indentation) then
      0-> indentation;
   endunless;
enddefine;


/* this procedure returns the starting location of text on the next line.
If the next line is empty, it returns false */

define nextlinestart() -> indentation;
   vednextline();
   thislinestart() -> indentation;
   vedprevline();
enddefine;


/* this procedure returns the contents of the next line. If it is empty
the result will be an empty string */

define nextlinecontents()->contents;
   vedchardown();
   vedthisline()->contents;
   vedcharup();
enddefine;


/*taken from Chris Thornton's Autoformat program */

define /* lconstant */ vedemptyline;
       vedusedsize(vedthisline()) == 0 ;
enddefine;



/* this procedure checks to see is the cursor is currently on
a line of (to be) verbatim code */

define is_prepared_code()->result;
  if issubstring('\\verb?',vedthisline()) then
     true ->result;
  else
     false -> result;
  endif;
enddefine;

/*
-- Mishap procedures --------------------------------------------------
*/

/* Most of the other Mishap messages are the result of a simple one line test
and are thereofre embedded in code */

define check_for_inappropriate_titleform();
   vedpositionpush();
   unless vedtryfind('<<<<<<<<<<<<<<<<<<<<<                             >>>>>>>>>>>>>>>>>>>>>>') do
       mishap('>>>>>>>>',1, 'NON STANDARD TITLE BAR ');
   endunless;
   vedpositionpop();
enddefine;



/*
-- Preprocessing before formatting ------------------------------------
*/


/*
;;; leaves cursor just after the overview or the contents.
*/

/* The function of the following procedure is to get rid of the header
and the contents, keeping only the actual title and the author history
of the file. It does however keep the real title i.e. "VEDPROCS" and
adds this on to the title in a manner that it can later be cross
referenced. */

define actions_on_top();
    vars num,label,internal_label;
    vedwordrightdelete();        ;;; remove the file name
    vednextitem()->label;
    prepare_internal_label(label) -> internal_label;
    vedwordrightdelete();
    get_history()->authorlist;   ;;; store and delete the files history
    repeat 6 times               ;;; delete copyright notice
        vedlinedelete();
    endrepeat;
    get_title()->title;
    title sys_><'} label{ch:'sys_><internal_label sys_><'}'->title;  ;;; add on cross reference label to title
    process_contents_and_overview();
enddefine;



/* The following gets (and deletes) the information at the top of the file
detailing the author and the revision history. This information is later
placed at the end of the chapter */

define get_history()->authorlist;
    lvars item;
    '' -> authorlist;
    until (vednextitem() ->> item) = "COPYRIGHT" do
        if item = "&" then
            "and" -> item;
        endif;
        while vedcurrentchar() = 32 do    ;;; i.e. not a blank space
            vedwordrightdelete();         ;;; this deals with multi line
        endwhile;                         ;;; revision histories
        vedwordrightdelete();
        if item = "," or
            item = "." then
            authorlist sys_>< item -> authorlist;
        else
            authorlist sys_>< ' ' sys_>< item -> authorlist;
        endif;
    enduntil;
enddefine;

/* This procedure deletes the REF file header saving the title text for use
later as the chapter title. */

define get_title()->title;
    lvars item, title = '';
    check_for_inappropriate_titleform();
    vedscreenleft();
    until vedthisline() = '<<<<<<<<<<<<<<<<<<<<<                             >>>>>>>>>>>>>>>>>>>>>>' do
        vedwordrightdelete();
        until (vedmoveitem() ->> item) = ">>>>>>>>>>>>>>>>>>>>>>" do
            if vedcurrentchar() = 45 do    ;;; a hyphenated word
                item sys_><'-' ->item;
                veddotdelete();
                item sys_><vedmoveitem()->item;
            endif;
            if item = "&" then
                "and" -> item;
            endif;
            title sys_><' 'sys_>< item ->title;
        enduntil;
        vedscreenleft();
        vedlinedelete();
    enduntil;
    vedlinedelete();
    vedlinedelete();
    until not(vedemptyline()) do
       vedlinedelete()
    enduntil;
enddefine;


/* The following 2 procedures deal with the overview and the Contents.
The contents are deleted regardless of whether there is an overview.
The important thing is to ensure that it is the contents which are
deleted. This is done by testing for location of the word "CONTENTS"*/

/* The first  procedure deletes each line in the CONTENTS. It stops
deleting when it reaches two consecutive blank lines */


define deal_with_contents();
    lvars sizeprevline = 1;
    vedlinedelete();
    until (vednextline(); length(vedthisline())) = 0 and sizeprevline = 0 do
        length(vedthisline()) -> sizeprevline;
        vedlinedelete();
        vedprevline();
    enduntil;
enddefine;

define process_contents_and_overview();
    lvars sizeprevline = 1;
    if vedmoveitem() = "CONTENTS" then    ;;; i.e not overview
        deal_with_contents();
    else
        /* move cursor to the next blank line i.e after the overview*/
        until vedemptyline() do
            vednextline();
        enduntil;
        if vedmoveitem() = "CONTENTS" then
            deal_with_contents();
        else
            vedprevline();
        endif;
    endif;
    vedwordright();   ;;; Go to the beginning of the first heading and
    vedprevline();    ;;; go back a bit
    vedprevline();    ;;; This is also the position for beginning formatting
enddefine;


/* Just about all characters are substituted after the file has been
formatted in order to preserve the logical structure. However the
following must be substituted first as they are used either in latex
commands or by REFORMAT procedures. */

/*The reason for the char right and left business is to prevent
vedfind missing and occurence of an item directly under the cursor
i.e. the second & in &&. */

define subs_before_formatting();
    lvars substitutions = [['{' 'lollol']
        ['}' 'lcllcl']['&' '!z!']  ['|' '|_bl_']
        ['\\' 'bachslashh'] ['$' '\\$'] ['~~' 'H_Y_W']['~' '\\~{}']], item;
    for item in substitutions do
        vedendfile();
        vedinsertstring(item(1));   ;;; place a  dummy so we know when at end
        vedtopfile();
        vedcharright();
        until vednextitem() = termin do
            vedcharleft();
            vedfind(item(1));
            if is_code() then  ;;; i.e skip lines which will
                vedcharright();
                nextloop;                   ;;; be verbatim
            endif;
            repeat length(item(1)) times    ;;; delete the occurence of item.
                veddotdelete();
            endrepeat;
            vedinsertstring(item(2));       ;;; replace it
        enduntil;
        repeat length(item(2)) times     ;;; delete the dummy item
            vedchardelete();
        endrepeat;
    endfor;
enddefine;

/* This very simple procedure simply goes to the end of the file and
deletes the lines containing the copyright notice and the file
location. */

define delete_ending();
   vedendfile();
   vedbackfind(' C.');
   veddo('deof');        ;;; "delete to the end of the file"
   vedlinedelete();
   /* the reaching of termin will be the EOF indicator for other
        procedures*/
enddefine;


/* The following procedure is only called once in order to see what
convention we are following as regards headings. This is done after
the preprocessing bit but before anything else. This works on the
assumption (as specified in REF REFFORM), that only one type of heading
can be used in any one REF file.*/

define what_file_style();
    if  not(skipchar(`\G-,1,vedthisline())) then
         'new' ->heading_type;
    else
         'old'->heading_type;
    endif;
    if  issubstring('lisp_',vedcurrent) then
        true -> islispfile;
    else
        false -> islispfile;
    endif;
enddefine;



/*
-- BEGINNING OF ACTIVE FORMATTING -------------------------------------
*/

/*
-- Control Procedures  ------------------------------------------------
*/

/* As can be inferrred by the name, this procedure brings all the disparate
parts together. The reason for the dlocal variables is to ensure that their
behaviour is switched off. vvedpromptchar has recently been changed so that
the behaviour we are trying to prevent has been switched off, but I believe
it is better to protect against further changes and thus make the program
as robust as possible. This policy will be repeated many times */

define whole_file_action();
    dlocal vedbreak = false;
    preprocess();
    format_actions();
    post_formatting_actions();
enddefine;


/* This is the driving procedure for the preprocessing of the file. The
reason behind the pushing and popping is to leave the cursor in such a
position that format_actions() can process the first sectionlike any other
section. */

define preprocess();
     true ->vedediting;
    vedputmessage('PREPARING FILE FOR PROCESSING');
     dlocal vedediting = false;
    vedtopfile();
    actions_on_top();           ;;; perform necessary actions
    vedpositionpush();
    subs_before_formatting();
    delete_ending();
    vedpositionpop();
    vedpositionpush();
    until not(vedemptyline()) do ;;; Get to the beginning of the first heading
        vednextline();
    enduntil;
    what_file_style();   ;;;discover what file type is being used
    vedpositionpop();
enddefine;

/* This procedure is at the heart of the program. It contains the alogrithim
which keeps identifying text environments until the the end of the file. It
goes like this:

     If it is an empty line then

         we are not in a identifier entry or a section any more.
         refresh the global variables.

         if the next line is empty then we are about to enter a section
                 call section action

         else its an identifier entry
                 call identifier action

     else call substructure to find out what appropriate action to execute
*/


define format_actions();
    true ->vedediting;
   vedputmessage('PLACING TEXT IN LATEX ENVIRONMENTS - NOW LINE ' sys_><vedline);
    dlocal vedediting = false;

    /*Place chapter heading at start of file */
   vedpositionpush();
   vedtopfile();
   vedinsertstring('\\chapter{' sys_><title sys_><'\n');
   vedpositionpop();

     until vednextitem() = termin  do
        if vedemptyline() then       ;;; definitely out of identifier/section
              6.15 -> indent_size;     ;;; refreshes values
              1-> global_indent;
              vednextline();
              if vedemptyline() then
                  until not(vedemptyline()) do
                     vednextline();
                  enduntil;
                  if is_heading() then
                     true ->vedediting;
                     vedputmessage('PLACING TEXT IN LATEX ENVIRONMENTS - NOW LINE ' sys_><vedline);
                     dlocal vedediting = false;
                     heading_action();
                     if is_heading() then
                          heading_action();
                          if is_identifier_heading() then
                              true ->vedediting;
                              vedputmessage('PLACING TEXT IN LATEX ENVIRONMENTS - NOW LINE ' sys_><vedline);
                              dlocal vedediting = false;
                              ident_heading_action();
                          endif;
                     elseif  is_identifier_heading() then
                          true ->vedediting;
                          vedputmessage('PLACING TEXT IN LATEX ENVIRONMENTS - NOW LINE ' sys_><vedline);
                          dlocal vedediting = false;
                          ident_heading_action();
                     endif;
                  endif;
              elseif  is_identifier_heading() then
                   true ->vedediting;
                   vedputmessage('PLACING TEXT IN LATEX ENVIRONMENTS - NOW LINE ' sys_><vedline);
                   dlocal vedediting = false;
                   ident_heading_action();
              else
                 unless is_paragraph() then
                     mishap(format_actions,1, 'EXPECTING IDENTIFIER ENTRY HEADING ');
                 endunless;
              endif;
         else
           substructure();
         endif;
     enduntil;
enddefine;



/* the following proceedure puts a shell around the call to substructure
saving the global variables before the call and restoring them after */
/*
NOTE: that substructure will be called after the section heading or the
identifier heading has been called. It will also be called if we are dealing
with an embedded text group. i.e. a list within a list or program code.

Each component group leaves the cursor one line after the end of the text
group. This will either be a blank line if we are out of a identifier entry
(or a section), or the first line of another text group.
*/

define substructure();
    lvars holdglobal_indent, holdindent_size;
    global_indent -> holdglobal_indent;
    indent_size -> holdindent_size;
    component_parts();
    holdglobal_indent -> global_indent;
    holdindent_size-> indent_size;
enddefine;



/* This procedure is simply a giant if statement which tries to identify what
type of text environment  the current text  group is. Each  of the text
group types are defined  according to REF  REFFORM. Not  that if all  fail
then  it returns eventually to format_actions() to check if  we are at the
end of  the file */

define component_parts();
    vars no_of_columns, type_of_list;
    if is_code() then
        code_action();
    elseif (is_table()->>no_of_columns) then
        table_action(no_of_columns);
    elseif (is_bullet_list()->>type_of_list) then
        bullet_action(type_of_list);
    elseif is_enumerated_list() then
        enumerated_action();
    elseif (is_descriptive_list()->>type_of_list) then
        descriptive_list_action(type_of_list);
/*    elseif is_picture() then
        picture_action(); */
    elseif is_paragraph() then
        paragraph_action();
    elseif is_heading() then      ;;; i.e. subheading immediately after
        heading_action();         ;;; main heading
    elseif is_identifier_heading() then
       mishap(vednextitem(),1, 'IDENTIFIER ENTRY HEADING MUST BE PRECEEDED BY 2 BLANK LINES');
    endif;
enddefine;



/* This procedure packages together the procedures which work on the file
after the text has been placed into LATEX environments. */

define post_formatting_actions();
       true ->vedediting;
       vedputmessage('DEALING WITH IDENTIFIER CROSS-REFERENCES');
       dlocal vedediting = false;
     old_identifier();              ;;; i.e. -vedbreak-
       true ->vedediting;
       vedputmessage('DEALING WITH DOCUMENTATION CROSS-REFERENCES');
       dlocal vedediting = false;
     ref_reference_action();     ;;; cross references to documentation
       true ->vedediting;
      vedputmessage('REPLACING VED-ATTRIBUTED STRINGS');
       dlocal vedediting = false;
     replace_dstrings();            ;;; stripping attributes off characters
       true ->vedediting;
       vedputmessage('MAKING APPROPRIATE SUBSTITUTIONS');
       dlocal vedediting = false;
     general_substitutions();
       true ->vedediting;
       vedputmessage('REPLACING WORD QUOTES');
       dlocal vedediting = false;
     replace_double_quotes();      ;;; a special substitution case
       true ->vedediting;
       vedputmessage('REPLACING ANY VED GRAPHICS CHARACTERS');
       dlocal vedediting = false;
     graphics_char_substitutions();
     place_revision_history();     ;;; placing it at the end
enddefine;



/*
-- Section (sub)Headings ----------------------------------------------
*/

/*
If called with do nothing unless heading. No false value returned.
*/

/* recognition  and text insertion were decoupled. However in this case, any
lines surrounding the heading are deleted thus leaving only the heading text
itself for heading action to deal with */

define is_heading()-> result;
    vars size;
    vvedlinesize -> size;
    if size > 0 then
        /* oldstyle */
        if heading_type = 'old' then
            vednextline();
            if vvedlinesize = size and not(skipchar(`-,1,vedthisline())) then
                vedlinedelete();
                true ->result;
            else
                false->result;
            endif;
            vedprevline();

            /* newstyle */
        elseif heading_type = 'new' then
            if not(skipchar(`\G-,1,vedthisline())) and
                (vedchardown();  vvedlinesize = size; vedcharup()) then
                vedlinedelete();
                vedchardown();
                vedlinedelete();
                vedcharup();
                true ->result;
            elseif
                (vedchardown(); vvedlinesize = size and
                    not(skipchar(`\G-,1,vedthisline())); vedcharup()) then
                vedchardown();
                vedlinedelete();
                true ->result;
                vedprevline();
            else
                false ->result;
            endif;
        endif;
    else
        vedprevline();    ;;; only if the line is empty;
        false ->result;
    endif;
enddefine;



/* This is only a wee bit unwieldy because different conventions have
been adopted with regards to show sections and subsections */

define heading_action();
    lvars holdtext,afternum,newholdtext,len;
    vedthisline() -> holdtext;
    if  heading_type = 'old' then
        if skipchar(`.,1,holdtext) = 4 then
            repeat 4 times
               veddotdelete();
            endrepeat;
            vedinsertstring('\\subsection{ 'sys_><holdtext sys_><' } ');
        else
            vedinsertstring('\\section{ 'sys_><holdtext sys_><' } ');
        endif;

    elseif heading_type = 'new' then

        /* remove numbers */
        issubstring('  ',holdtext)-> afternum;
        length(holdtext) -> len;
        substring(afternum+2,len-afternum-1,holdtext) -> newholdtext;

        /* find out what type of section and act appropriately */
        if skipchar(`.,1,holdtext) = 4 then
            vedinsertstring('\\subsubsection{ 'sys_><newholdtext sys_><' } ');
        elseif subscrs(2,holdtext) = 46 or
            subscrs(3,holdtext) = 46 then  ;;; i.e. "."
            vedinsertstring('\\subsection{ 'sys_><newholdtext sys_><' } ');
        else
            vedinsertstring('\\section{ 'sys_><newholdtext sys_><' } ');
        endif;

    endif;
    vedcleartail();  ;;; remove original text;
    vednextline();

    until  vvedlinesize > 0 do    ;;; to allow for mistakes by writers
        vednextline();
    enduntil;
enddefine;



/*
-- Paragraphs ---------------------------------------------------------
*/

/* This leaves the cursor placed at the beginning of one line below the
paragraphs end. i.e. on the next block of text or on the second blank line.*/

/*This procedure is called by the identifier heading but can also be called
independantly. It merely identifies a paragraph as being such.
Three types of paragraphs are recognised.

    * regular paragraphs which are right justified, with both the current
      line and the next line starting at the same global indentation,

    * one line paragraphs where the next line is empty or indented (i.e. the
      beginning of a new paragraph) and the current line is shorter than 72.

    * regular paragraph with an indentation of 4 on the first line.
*/

define is_paragraph()->result;
   lvars  local_indent = thislinestart(),
          nextline_indent = nextlinestart();
   if (local_indent = global_indent and length(vedthisline()) >= 72 and
     nextline_indent = global_indent) or

      (local_indent =< global_indent and
     (nextline_indent = 0 or local_indent + 4 = nextline_indent)) or

      (local_indent = global_indent+4 and length(vedthisline()) >= 72 and
      nextline_indent = global_indent)
   then
      true->result;
   else
      false->result;
   endif;
enddefine;


/* This simply places a paragraphs text in a LaTeX parbox the size of which
is determined by the current state of the global variable indent_size.
   The in_environment variable is for the benefit of procedures dealing with
verbatim text. There is no real need for it here but it is best to make such
things obvious as confusion might occur when it appears apparently at random
later.*/

define paragraph_action();
   vedinsertstring('\\begin{flushright}\\parbox{' sys_><indent_size sys_><'in}{\n');
   until vedemptyline() do
     vedchardown();
   enduntil;
   vedinsertstring('} \n \\end{flushright}');   ;;; close parbox
   vedlinebelow();
   vednextline();
enddefine;



/*
-- Identifier headings ------------------------------------------------
*/

/* This leaves the cursor placed at the beginning of one line below the
first paragraph-block end. i.e. on the next block of text or on the
second blank line. i.e. on the \ of the next comment*/

/*identifier recognition procedure - 93 is ascii for "]" */

define is_identifier_heading()->result;
    if not(vedemptyline())
    and last(vedthisline()) = 93 then
        if length(vedthisline()) >= 72 then
            true -> result;
        else
            mishap(vednextitem(),1, 'IDENTIFIER ENTRY HEADING MUST BE RIGHT-JUSTIFIED');
        endif;
    else
        false -> result;
    endif;
enddefine;



/*This procedure lowercases two instances of words in identifier headings
- those within brackets and those after an assignment arrow
    This is done because since the words are bolded, we no longer need them
uppercase. However we cannot simply do ved_lcl because an identifier can have
uppercase in it which we want preserved

NOTE: This procedure is only called if the file is in the old style format*/

define my_lcl();
    /*within brackets*/
    if issubstring('(',3,vedthisline()) then  ;;; 3 to prevent Lisp changes
        3-> vedcolumn;
        vedfind('(');
        vedcharright();
        until vedcurrentchar() = 41 do          ;;; closing bracket
            ved_lcw();
        enduntil;
        vedbackfind('(');
    endif;
    vedtextleft();
    /*before or after identifier arrows*/
    if issubstring('->',1,vedthisline()) then
        if issubstring('hfill',1,vedthisline()) then
            vedfind('->');
            vedcharright();
            vedcharright();
            until vedcurrentchar() = 93 do      ;;;end of line
                ved_lcw();
            enduntil;
        else
            until vedcurrentchar() = 45 do      ;;;beginning of assignment
                ved_lcw();
                vedwordright();
            enduntil;
        endif;
    endif;
    vednextline();
enddefine;

/* This next procedure simply returns the identifier name
taking care not to include the bracket if it is a LISP entry.
This method is preffered over vedmoveitem because it allows
us to get hyphenated words as well of those made up of
type mixtures */

define getlabel()->thelabel;
    ''-> thelabel;
    if vedcurrentchar() = 40 then   ;;; A LISP entry
       vedcharright();
    endif;
    until vedcurrentchar() = 40 or
        vedcurrentchar() = 32 do
        thelabel sys_><vedmoveitem() ->thelabel;
        vedcharleft();
        unless vedcurrentchar() = 32 then
            vedcharright();
        endunless;
    enduntil;
    consword(thelabel) -> thelabel;
enddefine;


/*Every program needs one of these. A test which got more
and more complicated - at least I put it in a procedure!*/

define isa_kludge(label)->result;
    if label = "lollol" or
       label = "lcllcl" or
       label = "#|_bl_" or
       label = "fi_H_Y_W" or
       label = "|_bl_#" or
       label = """  then
        true -> result;
    else
        false -> result;
    endif;
enddefine;


define ident_heading_action();
    lvars thelabel,text,internal_label,index_label;
    getlabel()->thelabel;
    unless isa_kludge(thelabel) then
        prepare_internal_label(thelabel) -> internal_label;
        prepare_index_label(thelabel)->index_label;
        /*allow for cross referencing */
        vedinsertstring(' label{' sys_><internal_label sys_><'}~\\index{' sys_>< index_label
sys_><'|bold}\\hspace{-0.1cm}');
    endunless;
    vedscreenleft();
    vedinsertstring('\\section*{}\n\\vspace{-0.5cm}\\noindent \n');
    vedinsertstring(vedthisline());
    vedinsertstring('\\\\');
    vedcleartail();
    vedbackfind("[");
    vedinsertstring('\\hfill ');
    if heading_type = 'old' then
        my_lcl();
        else
        vednextline();
    endif;
    until thislinestart() = 9  do      ;;;deal with multiple identifier headings
        if last(vedthisline()) = 93 and length(vedthisline()) >= 72 then
            vedtextright();
            vedbackfind("[");
            vedinsertstring('\\hfill ');
            vedtextleft();
            getlabel()->thelabel;

            unless isa_kludge(thelabel) then
                prepare_internal_label(thelabel) -> internal_label;
                prepare_index_label(thelabel)->index_label;
                vedinsertstring(' label{' sys_><internal_label sys_><'}~\\index{' sys_><index_label sys_><'|bold}\\hspace{-0.1cm}' );
            endunless;

        endif;
        if thislinestart() > 1 then
            vedtextleft();                ;;; for assignments which
            vedinsertstring('\\indent ');  ;;; stretch over several lines
        endif;
        vedtextright();
        vedinsertstring('\\\\');       ;;;by adding a newline at the end of each
        vedcleartail();                      ;;;until we reach an indented paragraph
        if heading_type = 'old' then
            my_lcl();
            else
            vednextline();
        endif;
    enduntil;
    vedlineabove();
    vedinsertstring('\\vspace{-0.475cm}'); ;;;close text-gap
    vednextline();
    5.8 -> indent_size;
    9->global_indent;
    if is_paragraph() then
        vedlineabove();
        vedinsertstring('\\nopagebreak[4]');
        vednextline();
        paragraph_action();
        else
        mishap(vednextitem(),1, 'IDENTIFIER ENTRY SHOULD BEGIN WITH A PARAGRAPH');
    endif;
enddefine;



/*
-- Verbatim text (program code) ---------------------------------------
*/

/* This starts at the beginning of the fixed width text and after
placing it in a verbatim environment. It recognises the two types
of fixed width text i.e. those lines starting with either 'Sp' or 'Sf'.*/


define is_code()->result;
    lvars hold;
    if vedemptyline() then
       false -> result;
    elseif vedthisline()(1) = 159 or       ;;; Sp
        vedthisline()(1) = 156 then        ;;; Sf
        true->result;
    else
        false->result;
    endif;
enddefine;



/* The following procedure places recognised verbatim code within a verbatim
environment. Rather than doing it as a group, the procedure treat each line
individually. This is because of a LATEX problem nesting Verbatim
environments.  */

define code_action();
     /* this comment is placed so the user can see what has happened */
    vedprevline();
    vedinsertstring('\\vspace{0.15cm}\n' );
    vedinsertstring(' \n % \\\\ THIS IS VERBATIM (\\verb?) CODE% \n \\noindent');
    vednextline();
    while not(vedemptyline()) do
        if length(vedthisline()) = 1 then   ;;;  deal with empty lines
            veddotdelete();                 ;;;  in program text
            vedinsertstring('\\\\');
            else
                veddotdelete();
                vedinsertstring('\\verb?');
                vedtextright();
                vedinsertstring('  ?\\\\*');  ;;;close one line verbatim environment
        endif;
        vednextline();
    endwhile;
    vedinsertstring(' \\vspace{0.15cm}\n');   ;;; reduce spacing in text
    vednextline();
enddefine;

/*
-- Lists --------------------------------------------------------------
*/

/* The program recognises three types of lists: bullet, enumerated
and descriptive. */

/*
-- ... Bullet Lists ---------------------------------------------------
*/


/* There are two charcters allowed to mark bullet lists. These are checked
for and if present then the text group is recognised as such. */

define is_bullet_list() -> bullet_type;
      if member(vednextitem(), [%"'\G#'"% o ]) then
         vednextitem() ->bullet_type;
      else
           false ->bullet_type;
      endif;
enddefine;


/* This procedure defines what action must be taken in the case of a bullet
list being identified. The global 'in_environment' is made true and the text
once again placed in a flushedright parbox.

   An interesting feature of lists is the way they can contain other features
such as program code or indeed other lists.
Therefore if a substructure is encountered then the process of text-group division
begins all over again. This recursive ability can continue indefinitely*/


define bullet_action(type);
    lvars indent, normalbit;
    1 + in_a_list_environment -> in_a_list_environment;
    vedlineabove();
    vedprevline();
    vedinsertstring('\\begin{flushright} \n');
    if thislinestart() > 8  and in_a_list_environment < 1 then
    vedinsertstring('\\begin{listinident}');
    1 -> normalbit;
    elseif thislinestart() < 8  then
     vedinsertstring('\\begin{listnotinident}');
      0-> normalbit;
    else 2 ->  normalbit;
    endif;
    vedinsertstring('\\begin{itemize}');
    vednextline();
    vednextline();

    /* adjust indentation globals accordingly */

    thislinestart() -> indent;
    if indent > global_indent then
        indent_size - 0.4->indent_size;
        indent -> global_indent;
    endif;

    until (not(vedemptyline()) and thislinestart() < indent) or
        (not(vedemptyline()) and not(vednextitem()) = type) do

        locchar(` `, thislinestart(), vedthisline()) -> vedcolumn;
        vedclearhead();
        vedinsertstring('\\item ');
        /* deal with descriptive part of list item*/
        while not(vedemptyline()) and not(vednextitem()  = type)
        and  not(member(vednextitem(),[%"'\G#'"% o ])) do
            vednextline();
            if   nextlinestart() > global_indent+5 or
                (vednextline(); is_code(); vedprevline()) then
                vednextline();
                substructure();
                if  thislinestart() < global_indent then
                    quitloop(2);
                endif;
                /*allow for blank line in description */
            elseif vedemptyline() and nextlinestart() > indent+1  and
                not(member(vednextitem(),[%"'\G#'"% o ])) then
                vednextline();
            elseif vedemptyline()  and nextlinestart() = 0 then
                vednextline();
                quitloop(2);
            elseif vedemptyline() and nextlinestart() < indent then
                vednextline();
                quitloop();
            elseif vedemptyline() then
                  vednextline();
            endif;

        endwhile;

    enduntil;

    vedprevline();
    vedlineabove();
    vedinsertstring('\n \\end{itemize}');
    if  normalbit = 1 then
       vedinsertstring('\\end{listinident}');
    elseif normalbit = 0 then
       vedinsertstring('\\end{listnotinident}');
    endif;
    vedinsertstring('\n   \\end{flushright}');
    vednextline();
    vednextline();
    in_a_list_environment -1 ->in_a_list_environment;
enddefine;


       /*
-- ... Enumerated Lists -----------------------------------------------
*/

/* this automatically deals with the problem of whether the bracket or
the enumerator is the indented thing */

define is_enumerated_list()->result;
     lvars indent;
     thislinestart() ->indent;
     if locchar(`)`,1, vedthisline()) = 1+indent or
           (locchar(`(`,1, vedthisline()) = indent and
                vedthisline()(indent+2) = `)`) or
           (locchar(`.`,1, vedthisline()) = 1+indent and
           not( locchar(`e`,1, vedthisline()) = 2+indent) and
           not( locchar(`g`,1, vedthisline()) = 2+indent) and
           not( locchar(`B`,1, vedthisline()) = 2+indent)) then
           true -> result;

     else
        false->result;
     endif;
enddefine;


define enumerated_action();
    lvars num, normalbit,indent;
    in_a_list_environment + 1 -> in_a_list_environment;
    1 -> num;
    vedprevline();

    vedinsertstring('\\begin{flushright} \n');
    if thislinestart() > 8  and in_a_list_environment < 1 then
    vedinsertstring('\\begin{listinident}');
    1 -> normalbit;
    elseif thislinestart() < 8  then
     vedinsertstring('\\begin{listnotinident}');
      0-> normalbit;
     else 2 ->  normalbit;
    endif;
    vedinsertstring('\\begin{enumerate}');
    vednextline();
    thislinestart() ->indent;
         if indent > global_indent then
             indent -> global_indent;
             indent_size - 0.4 ->indent_size;
         endif;
    until
        (not(vedemptyline()) and thislinestart() < global_indent) or
        (not(vedemptyline()) and
            not(isnumber(vednextitem())) and
            not(vednextitem() = "(")) do
        locchar(` `, global_indent, vedthisline()) -> vedcolumn;
        vedclearhead();
        vedinsertstring('\\item ');

        while not(vedemptyline()) do
            vednextline();
            if not(vedemptyline()) and (isnumber(vednextitem()) or
                    vednextitem() = "(") then
                nextloop(2);

                /*go through description */

            elseif vedemptyline() then

                 /*end of section/identifier entry*/
                if nextlinestart() = 0 then
                    vedlinebelow();
                    quitloop(2);

                elseif global_indent+1 < nextlinestart() and
                    nextlinestart() < global_indent+5 then
                    vedinsertstring('\\\\');
                    vednextline();

                    /*look out for further indentation*/
                elseif   nextlinestart() > global_indent+5 or
                    (vednextline(); is_code(); vedprevline()) then
                    vednextline();
                    substructure();
                    if isnumber(vednextitem()) or
                       vednextitem() = "(" or
                       thislinestart() < global_indent then
                       nextloop(2);
                    endif;
                endif;
            endif;
        endwhile;
        vednextline();
    enduntil;

    if vedemptyline() then
        until not(vedemptyline()) do
           vedprevline();
        enduntil;
        vednextline();
    else vedprevline();
         vedlineabove();
    endif;

    vedinsertstring('\n \\end{enumerate}');
    if  normalbit = 1 then
       vedinsertstring('\\end{listinident}');
    elseif normalbit = 0 then
       vedinsertstring('\\end{listnotinident}');
    endif;
    vedinsertstring('\n \\end{flushright}  ');
    vednextline();
    vednextline();
    in_a_list_environment - 1 -> in_a_list_environment;
enddefine;


   /*
-- ... Descriptive Lists ----------------------------------------------
*/

/* This procedure recognises two types of descriptive lists - the first is
like

      item      blaha balah blah

and the other

      item
          blahh bajllksklaksalkslaskl

*/

define is_descriptive_list()->list_type;
     lvars indent;
     thislinestart() ->indent;
     unless length(vedthisline()) > 71 then
         if   issubstring('  ',indent,vedthisline()) then
           'type1' ->list_type;
         elseif nextlinestart() = indent+4  then
           'type2' ->list_type;
         else
           false-> list_type;
           return;
         endif;
    else
       false-> list_type;
    endunless;
enddefine;



define descriptive_list_action(type);
    lvars holdglobal_indent, holdindent_size, normalbit;
    in_a_list_environment + 1 -> in_a_list_environment;
    vedprevline();
    vedinsertstring('\\begin{flushright} \n');

    if thislinestart() > 8 and in_a_list_environment < 1 then
    vedinsertstring('\\begin{listinident}');
    1 -> normalbit;
    elseif thislinestart() < 8  then
     vedinsertstring('\\begin{listnotinident}');
      0-> normalbit;
     else 2 ->  normalbit;
    endif;
    vedinsertstring('\\begin{description}');
    vednextline();

    if thislinestart() > global_indent then
        thislinestart(); -> global_indent;
        indent_size - 0.4 ->indent_size;
    endif;

    if type = 'type1' then
        until not(vedemptyline()) and thislinestart() < global_indent do
            vedtextleft();
            vedinsertstring('\\item[');
            issubstring('  ',thislinestart(),vedthisline()) -> vedcolumn;
            vedinsertstring('  ]');
            vednextline();
            if vedemptyline() then
                vednextline();
                if thislinestart() > global_indent or is_code() then
                    substructure();
                elseif vedemptyline() then
                     /*end of Section/identifier*/
                    quitloop;
                endif;
            endif;
        enduntil;

    elseif type = 'type2' then
        until not(vedemptyline()) and thislinestart() < global_indent or
            not(vedemptyline()) and thislinestart() = global_indent and
            nextlinestart() = global_indent or
             vedemptyline() do
            vedtextleft();
            vedinsertstring('\\item[');
            vedtextright();
            vedinsertstring('  ]');
            vednextline();

            /*deal with multiline descriptions allowing for blank lines twixt paragraphs*/
            while not(vedemptyline()) do
                vednextline();
                if  vedemptyline() then
                    vednextline();

              /* This if statements main purpose is to catch embedding. The
                first is to catch a descriptive list where the item is the
                    same as the previous escriptions indentation */

                    if thislinestart() = global_indent+4  and
                        not(nextlinestart() = global_indent+8) then
                        vedprevline();
                        vedinsertstring('\\\\');
                        vednextline();
                    elseif length(vedthisline()) < 72 and
                        thislinestart()= global_indent+4 and
                        nextlinestart() = global_indent + 8 then

                        global_indent-> holdglobal_indent;
                        indent_size -> holdindent_size;
                        descriptive_list_action('type2');
                        holdglobal_indent -> global_indent;
                        holdindent_size-> indent_size;
                        if  thislinestart() = global_indent then
                            vedprevline();
                            quitloop;
                        elseif thislinestart() = global_indent+4 then
                            nextloop;
                        endif;

                    elseif vedemptyline() then
                        ;;;   [end of Section/identifier]
                        quitloop(2);
                    elseif  thislinestart() > global_indent+5 or
                      is_code() then
                        substructure();

                        /*deal with sublist starting immediately after
                         subenvironment i.e. sublist after verbatim in sysio*/

                        if length(vedthisline()) < 72 and
                            thislinestart()= global_indent+4 and
                            nextlinestart() = global_indent + 8 then

                            global_indent-> holdglobal_indent;
                            indent_size -> holdindent_size;
                            descriptive_list_action('type2');
                            holdglobal_indent -> global_indent;
                            holdindent_size-> indent_size;


                            if  thislinestart() = global_indent then
                                vedprevline();
                                quitloop;

                            elseif thislinestart() = global_indent+4 then
                                nextloop;
                            endif;

                            /*normal description continues*/


                        elseif  thislinestart() = global_indent then
                            vedprevline();
                            quitloop;
                        elseif thislinestart() < global_indent then
                             vedprevline();
                             vedprevline();
                             vedprevline();
                             vedprevline();
                             quitloop;
                        elseif thislinestart() = global_indent+4 then
                            nextloop;
                        endif;

                    elseif thislinestart() =< global_indent then
                        vedprevline();
                    endif;
                endif;
            endwhile;
            vednextline();
            if nextlinestart() = 0 then quitloop; endif;
        enduntil;
    endif;
    vedprevline();
    if vedemptyline() then
        until not(vedemptyline()) do
           vedprevline();
        enduntil;
    endif;
    vednextline();
    vedinsertstring('\\end{description}');
    if  normalbit = 1 then
       vedinsertstring('\\end{listinident}');
    elseif normalbit = 0 then
       vedinsertstring('\\end{listnotinident}');
    endif;
    vedinsertstring('\n \\end{flushright}');
    vedlinebelow();
    vednextline();
    in_a_list_environment - 1 ->in_a_list_environment;
enddefine;


   /*

-- Pictures (a provisional attempt) -----------------------------------


/* It would be nicer if it were possibly to represent the picture using
latex picture drawing. This is actually possible I believe - but is quite
time consuming. Therefore this bit could well be returned to at a
later date.*/

/*
In the following procedure - the onus is on the picture to identify itself
- asceptical sort of program. Note that confuusion could arise between this
and the recognition of the new style heading. This is dealt with by
checking to see if it is a heading first.*/



define is_picture() -> result;
  lvars num=3;
  false ->result;
  repeat num times
    if (issubstring('---',1,  vedthisline()) or
       issubstring('---- ',1,  vedthisline()) or
       issubstring('___',1,  vedthisline()) or
       issubstring('  /  ',1,  vedthisline())) and
       thislinestart() > 4 then
          true -> result;
          quitloop;
    endif;
    num-1 -> num;
    vednextline();
  endrepeat;
  repeat 3-num times
     vedprevline();
  endrepeat;
enddefine;

/* The \begin{boxedverbatim} is an extra style option that is given to
\documentstyle. If this program is exported then this procedure
will either have to rewritten or the boxed verbatim bit removed. All
\begin{boxedverbatim} does is to represent the verbatimed bit within
a frame box. Note that the procedure picks up a caption if it is present.
An alternative way of doing this would be to do a global replace later*/

define picture_action();
    lvars boxsize = indent_size - 0.4, capt = '', hold;
    vedprevline();
    vedinsertstring('\\begin{figure}\n');
    vedinsertstring('\\begin{center}\n');
    vedinsertstring('\\begin{boxedverbatim}{' sys_><boxsize sys_><'in}\n');
    until vedemptyline() and (nextlinestart() = global_indent
       or nextlinestart() = global_indent-4) do
       if issubstring('fig',1,vedthisline()) ->> hold then
           locchar(` , hold , vedthisline()) -> vedcolumn;
           vedclearhead();
           vedtrimline();
           veddotdelete();
           vedthisline()->capt;
           vedlinedelete();
           vedprevline();
       endif;
       vednextline();
     enduntil;
     vedinsertstring('\n\\end{boxedverbatim}\n');
     vedinsertstring('\\caption{' sys_><capt sys_><'}\n');
     vedinsertstring('\\end{center}\n');
     vedinsertstring('\\end{figure}\n');
     vednextline();
enddefine;

/* works with cursor on first line of picture and ends on second line below
picture i.e.        ------------

HERE
*/

-- Tables -------------------------------------------------------------
*/

/* The following table idenification procedure either returns -false- or
the number of columns in the table. This number is worked out by the
number of headings. */

define is_table()->no_of_columns;
    vars current;
    0 -> no_of_columns;
    thislinestart()->current;

    /* test to see if it is a table */
     if  (issubstring(' ---', current, nextlinecontents())) or
        (issubstring(' \G-\G-\G-', current, nextlinecontents())) and
        thislinestart() = nextlinestart() and
        length(vedthisline()) < 72 then

     /*Getting the number of columns */
        while issubstring('   ', current,vedthisline()) do
          vedendwordright();
          while ((vedchardown(); vedcurrentchar(); vedcharup()) = 45) or
                ((vedchardown(); vedcurrentchar(); vedcharup()) = 131) do
            vedendwordright();
          endwhile;
        no_of_columns + 1 -> no_of_columns;
        vedcolumn -> current;
        endwhile;
    else
        false -> no_of_columns;
    endif;
enddefine;



/* This one works on the very simple idea of copying down the tab positions from
from the line above. */

define table_action(no_of_columns);
    lvars emptyline, colenv, num1 =1, num2=1, headings_end = false;
    '\\\\'->emptyline;
    if no_of_columns < 3 then          ;;; This is based purely on a cosmetic heuristic
       '|l|'->colenv;
    else
       '|c|'->colenv;
    endif;
    repeat no_of_columns - 1 times
        '|c' sys_>< colenv -> colenv;
        '& ' sys_>< emptyline -> emptyline;
    endrepeat;

    vedprevline();
    vedinsertstring('\\begin{center}\n');
    vedinsertstring('\\begin{tabular}{' sys_><colenv sys_><'}\\hline\n');
    vedinsertstring(emptyline);
    vednextline();

    vedwordrightdelete();     ;;; delete gap before first heading and cursor

    /*prepare headings*/
    while not(headings_end) do
        vedinsertstring('\\multicolumn{1}{|c|}{\\large ');
        issubstring('  ', vedcolumn, vedthisline()) -> vedcolumn;
        unless num1 = no_of_columns then
            vedinsertstring('} & \n');
            num1 + 1 -> num1;
            else
            vedinsertstring('} \\\\ \n');
            vednextline();
            true -> headings_end;
            vedlinedelete();
        endunless;
    endwhile;

    vedprevline();      ;;; the one directly below the headings text;

    vedinsertstring(emptyline sys_><' \\hline \n');
    vedinsertstring(emptyline);
    vednextline();
    if vedemptyline() then
        vedlinedelete();
    endif;

    /* place tab characters at beginning of each column in first line of text */
    thislinestart() ->global_indent;
    until num2 = no_of_columns do
        vedendwordright();      ;;;
        issubstring('  ', vedcolumn, vedthisline()) -> vedcolumn;
        vedwordright();
        vedinsertstring('& ');
        num2 + 1 -> num2;
    enduntil;
    vedtextright();
    vedinsertstring('  \\\\');

    /* copy tab character positions from the previous line */
    vednextline();
    until vedemptyline() and nextlinestart() < global_indent
        /* (which could be zero for an empty line) */do
        vedprevline();
        vedtextleft();
        while (locchar(`& ,vedcolumn,vedthisline()) ->> vedcolumn) do
            vedchardown();
            vedinsertstring('& ');
            vedcharup();
        endwhile;
        vedchardown();
        vedtextright();
        vedinsertstring('  \\\\');
        vednextline();
    enduntil;

    vedinsertstring(emptyline sys_><' \\hline \n');
    vedinsertstring('\\end{tabular}\n');
    vedinsertstring('\\end{center}');
    vednextline();
enddefine;


/*
-- Revision History ---------------------------------------------------
*/
/* The written history - this is simply the insertion of the author list
which was procured duriing the preprocessing stage */


define place_revision_history();
    vedinsertstring('\n \\subsubsection*{Revision History}\n \n');
    vedinsertstring('The material presented in this Chapter was originally \
written by ' sys_><authorlist sys_><'.\n ');
enddefine;



/*
-- Cross References ---------------------------------------------------
*/
/*
-- ... to old style identifiers ---------------------------------------
*/

/*
Identifiers in the main body of the text can be represented in one of two ways

    if old_style then
         one word with a hyphen on each end

    newstyle
      bold printed string - one word which is not on a line which
        ends in ]

This procedure deal with the old style of identifier. It recognises identifier
names and remove hyphens, places them in a bold environment and checks to see
if they can be cross referenced.

The new style of identifier is dealt with by the procedure which dealswith
dstrings (below)*/


define old_identifier();
    lvars label, moretocome, internal_label,index_label;
    vedendfile();
    if heading_type = 'new' then
         return;
    endif;
    vedinsertstring('  -atest-');
    vedtopfile();

    while moretocome do
        vedfind('-@.@*-');

        /*prevent matching with hyphenated words but cathcing those in
          brackets*/
        unless vedcolumn = 1 or
            ((vedcharleft(); (vedcurrentchar() = 32
                            or vedcurrentchar() = 40); vedcharright()) and
            /*(not end-of-word)*/
            (vedcharright(); not(vedcurrentchar() = 32); vedcharleft())) then
            vedfind('-');
            nextloop;
        endunless;

        /* check to see if it is -atest- */
        if  (vedchardown(); vednextitem() = termin; vedcharup()) then
            vedlinedelete();
            false -> moretocome;
            nextloop;
        endif;

        /*prevent action being taken on identifier heading lines and/or lines of code*/
        if is_prepared_code() or
            issubstring('hfill',1,vedthisline()) then
            vednextline();
            nextloop;
        else
            /*remove hyphens and wrap in bold font environment*/
            veddotdelete();
            vedinsertstring(' {\\bf ');
            vednextitem() ->label;
            until vedcurrentchar() = 45 do
                vedcharright();
            enduntil;
            veddotdelete();
            vedinsertstring('}');

                if  check_for_ref(label) then
                    /*prepare and place cross reference labels*/
                    prepare_internal_label(label) ->internal_label;
                    prepare_index_label(label) -> index_label;
                    vedinsertstring('~[\\pageref{' sys_><internal_label sys_><'}]~\\index{' sys_><index_label sys_><'} ');
                endif;
        endif;
    endwhile;
enddefine;



/* This procedure takes an identifier label and checks to see if
the REF FILE it is detailed in is included in the Manual.
   If called by replace_dstrings, it will return -false- if the
string is not actually an identifier. It is actually the next procedure
which does the checking*/


define check_for_ref(label) -> result;
    lvars filename;
    datasearch(label)->filename;   ;;; returns false or the string
    if filename then
        if islispfile then
            'lisp_' sys_><filename -> filename;
        elseif issubstring('vms_',vedcurrent) then
            'vms_'  sys_><filename -> filename;
        endif;
        consword(filename)->filename;
        if member(uppertolower(filename),included_reffiles) then
            true ->result;
        else
            false->result;
        endif;
    else
        false->result;
    endif;
enddefine;

/*
This procedure takes an identifier label and checks to see if
there is a unique reference to it and if so returns the name of the
file it is located in.

 The reason things look so fiendishly complicated is that there are three subsystems to
be check and the procedure sys_search_doc_index can give either 1 or 2 results
*/

define datasearch(label)->filename;
    lvars occurences, filedata, fullpathname;
    if (sys_search_doc_index(label, '$usepop/pop/ref',true) = 0) and

        (sys_search_doc_index(label, '$usepop/pop/x/pop/ref' ,true) = 0) and

        (sys_search_doc_index(uppertolower(label), '$usepop/pop/lisp/ref',true) = 0)  then

        /* for REF file writers reference*/
        [^label ^^non_existent_identifiers] -> non_existent_identifiers;
        false -> filename;
    else
        if  (not(sys_search_doc_index(label, '$usepop/pop/ref',true) = 0) and
            (sys_search_doc_index(label, '$usepop/pop/ref',true)-> occurences; ->filedata)) or


            (not(sys_search_doc_index(label, '$usepop/pop/x/pop/ref' ,true) = 0) and
            (sys_search_doc_index(label, '$usepop/pop/x/pop/ref' ,true) -> occurences; ->filedata)) or


            (not(sys_search_doc_index(uppertolower(label), '$usepop/pop/lisp/ref',true) = 0) and
            (sys_search_doc_index(uppertolower(label), '$usepop/pop/lisp/ref',true) -> occurences; ->filedata)) then
            unless occurences > 1 then
                subscrv(2,filedata)->fullpathname;         ;;; get the fullpathname
                sys_fname_nam(fullpathname)-> filename;    ;;; strip the pathname
                uppertolower(filename)->filename;          ;;; for checking included_reffiles
            else
                false->filename;
            endunless;
        endif;
    endif;
    clearstack();
enddefine;


/*
-- ... to documentation and other chapters ----------------------------
*/


/* This procedure finds each non_embedded '*' in the file. It then checks
to see whether the Documentation file it references is in the manual and
if so replaces it with an automatcially generated cross reference to what
chapter it is. If not it is left alone. In both cases the asterisk is
removed and the reference (to a chapter or documentation file is italicised).

Note that this procedure also deals with all references to Documentation files
which are dstrings

REF FILES which are referenced in the text can be replaced by a
coressponding chapter name if present. Simply do a check in
included_reffiles and have a label placed after each REF FILE there.
If not present then we can leave alone. */

define ref_reference_action();
    lvars ref_name, internal_ref_name, moretocome, doc_type;
    vedendfile();
    vedinsertstring('  *atest');
    vedtopfile();

    while moretocome do
        vedfind('*');

        /* Check the asterisk is not on a verbatim line*/
        if is_prepared_code() then
            vednextline();
            nextloop;
        endif;

        /* Take no action if the asterisk has attributes */
        if  subscrdstring(vedcolumn,vedthisline()) > 1000 then
            vednextline();
            nextloop;
        endif;

        unless vedcolumn = 1 or
            (vedcharleft(); (vedcurrentchar() = 32
                or vedcurrentchar() = 44
                or vedcurrentchar() = 157); vedcharright()) then
            nextloop;
        endunless;

        /* check to see if it is *atest */
        if  (vedchardown(); vednextitem() = termin; vedcharup()) then
            vedlinedelete();
            false -> moretocome;
            nextloop;
        endif;

        veddotdelete();                            ;;; Delete asterisk
        if vedcurrentchar() = 157 then               ;;; remove special space
            veddotdelete();                       ;;; character if present;
            vedinsertstring(' ');
            vedcharleft();
        endif;
        if vedcolumn =< thislinestart() then       ;;; the doc type is on
            vedstartwordleft();                    ;;; another line
        else
            vedcharleft();
            if vedcurrentchar() = 157 then
                veddotdelete();
                vedinsertstring(' ');
            endif;
        endif;
        vedstartwordleft();
        vedmoveitem()->doc_type;
        if length(vedthisline()) < vedcolumn then     ;;; if the doc type was
            vednextline();                            ;;; on another line
        endif;
        if isinteger(doc_type) or (length(doc_type) = 1 and
                not(member(doc_type, [ , . ]))) then
            vedinsertstring(' *');
            nextloop;
        endif;
        vednextitem()->ref_name;
        vedendwordright();
        if doc_type = "REF" and
            member(uppertolower(ref_name),included_reffiles) or
            member('lisp_' sys_>< uppertolower(ref_name),included_reffiles) or
            member('vms_' sys_>< uppertolower(ref_name),included_reffiles) then
            vedwordleftdelete();
            vedwordleftdelete();
            prepare_internal_label(ref_name) -> internal_ref_name;
            vedinsertstring(' Chapter~\\ref{ch:' sys_><internal_ref_name sys_><'}');
        elseif doc_type = "REF" and all_reffiles_included then
            if vedcurrentchar() = 47 then
                until vedcurrentchar() = 32 do vedcharright();
                enduntil;
            endif;
            vedinsertstring(' (included in another volume) ');
        elseif
            member(doc_type,["REF" "INCLUDE" "SHOWLIB" "HELP" "DOC" "TEACH" "LIB"]) then
            vedwordleftdelete();

            /* deal with where the doc_type was on the previous line*/
            if vedcolumn = 1 then
                vedwordleftdelete();
                vedwordleftdelete();
            endif;
            vedwordleftdelete();
            vedinsertstring(' {\\em ' sys_><doc_type sys_><' 'sys_><ref_name sys_><'}');
        else
            vedwordleftdelete();                        ;;; no doc name
            vedinsertstring(' {\\em ' sys_><ref_name sys_><'}');
        endif;
    endwhile;
enddefine;


/*
-- Dealing with Dstrings ----------------------------------------------
*/

/* This obvious named procedure replaces all parts of a dstring in a file
which have non-zero attibutes -. It works from top to bottom ending at the
bottom.

It treats each bolded string as a possible identifier name. Action:
strip it of bold characteristics, bold and cross reference it*/

define replace_dstrings();
    vars dstring_value;
    vedtopfile();
    while find_dstring()->>dstring_value do
        appropriate_dstring_action(dstring_value);
    endwhile;
enddefine;


/* This procedure places the cursor at the next character in the text which
has non-zero attributes. It does this by locating a line which is a dstring
and then testing the attributes of each character fronm the first column. */

define find_dstring()->dstring_value;
    lvars line;
    for line from vedline to vvedbuffersize do
        if isdstring(subscrv(line, vedbuffer)) then
            ;;; found a line with nonzero attributes somewhere in it
            vedjumpto(line, 1);
            until (subscrdstring(vedcolumn,vedthisline()) ->> dstring_value) > 255
              do
              vedcolumn+1->vedcolumn;
              if vedcolumn > length(vedthisline()) then
                 vednextline(); quitloop; endif;
            enduntil;
            return;
        endif;
    endfor;
    false -> dstring_value;  ;;; if it has got here then none has been found;
enddefine;


define in_ref_entry(label) -> result;
    lvars hold;
    vedpositionpush();
    if vedtrybackfind(label sys_><'|bold') then
        vedline -> hold;
        vedpositionpop();
        vedpositionpush();
        vedbackfind('|bold');
        if vedline = hold then
            true -> result;
        else
            false -> result;
        endif;
        vedpositionpop();
    else
        vedpositionpop();
        false -> result;
    endif;
enddefine;


/* This procedure carries out the appropriate action on a found dstring
The numbers refer to the value of its attributes. For those dstrings which
are bolded it presumes they are identifiers and checks to see if there is
cross reference.

Note that by the time this is called all dstrings not in the main text should
have been dealt with. So too will have references to other REF and HELP
files (by ref_reference_action*/

/* although this procedure presumes that the non-zero attributes are all
contained n just one item, the manner of global replacement ensures that
multi-item strings are dealt with. Seeing as most text embedded dstrings
with non zero attributes are just one item long - this method is more
efficeient that actually searching for the end of the string.*/

define appropriate_dstring_action(dstring_value);
    lvars label,internal_label,index_label;
    if dstring_value < 300 then return; endif;
    unless  is_prepared_code() then
        if dstring_value > 3140000 then
            vedinsertstring('{\\sf{');
        elseif dstring_value > 2090000 then
            vedinsertstring('{\\em{');
        else
            vedinsertstring('{\\bf{');
        endif;
    endunless;
    vednextitem()->label;
    vedinsertstring('(');       ;;;a dummy to prevent space deletion
    if islispfile then
          if dstring_value = 1048618 then
             veddotdelete();
             label sys_>< vednextitem() -> label;
          endif;
    endif;
    vedendwordright();
    while vedcurrentchar() = 36 do
       veddotdelete();
    endwhile;
    vedwordleftdelete();
    while vedcurrentchar() = 45 do    ;;; a hyphenated word
       label sys_><'-'->label;
       veddotdelete();
       if vedcurrentchar() = 32 then
          quitloop;
       endif;
       label sys_><vednextitem()->label;
       vedendwordright();
       vedwordleftdelete();
    endwhile;
    if islispfile then
       if vedcurrentchar() = 42 then
          veddotdelete();
          label sys_>< '*' -> label;
       endif;
    endif;
    vedcharleft();      ;;; remove dummy
    veddotdelete();

    /* if a line of code then simply strip the label of its attributes*/
    if is_prepared_code() then
            label sys_><'' ->label;            ;;; turn possible integer into a string;
            vedinsertstring(label);
    else
        unless isinteger(label) then
            if  dstring_value < 1050000  and
                not(issubstring('|bold',1,vedthisline())) and
                not(issubstring('->',1,vedthisline())) and
                not(length(label) = 1) and
                not(issubstring('hfill',1,vedthisline())) and
                not(in_ref_entry(label)) and
                check_for_ref(label) then
                prepare_internal_label(label) -> internal_label;
                prepare_index_label(label) -> index_label;
                vedinsertstring(label sys_><'}}~[\\pageref{'sys_><internal_label sys_><'}]\\index{' sys_><index_label sys_><'}');
            else
                vedinsertstring(label sys_><'}}');
            endif;
        else
            label sys_><'' ->label;            ;;; turn integer into a string;
            vedinsertstring(label sys_><'}}');
        endunless;
    endif;
    vedprevline();
enddefine;



/* the cursor goes to the top of the file and ends up one line above
the place of the last substitution.*/



/*
-- General Substitutions -----------------------------------------------
*/

/* This bit deals with the general substitution of strings. It's major
area of responsibility is to prevent normal characters in a file being
interpreted as special Latex characters */

/* This procedure replaces or backslashes all the text items which LATEX
has trouble representing. The repeat loop is due to not knowing how large the
substituted string is going to be. As in the substituting before the
formatting, a dummy item is placed at the end of the file, to prevent
wrapping. */

define general_substitutions();
    lvars item;
    for item in subs_list do
        vedendfile();
        vedinsertstring(item(1));
        vedtopfile();
        vedcharright();
        until vednextitem() = termin do    ;;; i.e. just after the dummy item
            vedcharleft();
            vedfind(item(1));
            unless is_prepared_code() then
                repeat length(item(1)) times
                    veddotdelete();
                endrepeat;
                vedinsertstring(item(2));
            else
                vedchardown();
            endunless;
            if vedcurrentchar() = 39 then    ;;;prevents the possibility
               vedcharright();               ;;;of the next item being
            endif;                           ;;;an end-of-string causing UTS
        enduntil;
        vedlinedelete();
    endfor;
enddefine;



/* This procedure replaces all the ved graphics characters with normal characters.
As in the substituting  of general characters, a dummy item is placed at the end
of the file, to prevent wrapping. */

define graphics_char_substitutions();
    lvars item, hold;
    for item in graphics_subs_list do
        vedendfile();
        substring(2,length(item(1))-1,item(1)) -> hold;    ;;; remove the `\` from the string
        veddo('ic ' sys_>< hold);                          ;;; insert dummy character
        vedtopfile();
        vedcharright();
        until vednextitem() = termin do    ;;; i.e. just after the dummy item
            vedcharleft();
            vedfind(item(1));
            veddotdelete();
            vedinsertstring(item(2));
            if vedcurrentchar() = 39 then    ;;;prevents the possibility
               vedcharright();               ;;;of the next item being
            endif;                           ;;;an end-of-string causing UTS
        enduntil;
        vedlinedelete();
    endfor;
enddefine;


/* Latex represents doubles quotes marks (") by double string and double
inverted string quotes. i.e. instead of "item" we must change the text to
read ``item''. This procedure achieves that.
    It does not however replace single occurences of " i.e. where there is no
matching pair. neither does it replace those which are contained within lines
of codes. These latter lines are marked by containing "\verb?".
    The procedure works by finding an instance of a " and then checking to
to see if it has an acceptable partner. The problem of there being no
occurences for the search to match with, and the wrapping problem, is dealt
with by having a dummy " placed at the end of the file which is deleted when
reached and the search  terminated.*/

define replace_double_quotes();
    lvars moretocome;
    vedendfile();
    vedinsertstring('"');
    vedtopfile();
    while moretocome do
        vedfind('"');
        if  (vedchardown(); vednextitem() = termin; vedcharup()) then
            vedlinedelete();
            false -> moretocome;
            nextloop;
        endif;
        unless is_prepared_code() then
          vedpositionpush();
          vedfind('"');
          if (vedcharright(); vedcurrentchar() < 65) then  ;;; i.e. a space or punctunation
               vedpositionpop();
               veddotdelete();
               vedinsertstring('``');
               vedfind('"');
               veddotdelete();
               vedinsertstring('\'\'');
           else
               vedpositionpop();
               vedcharright();
           endif;
       endunless;
    endwhile;
enddefine;


/*
TODO
make sure all references to verbatim in the cross references are okay.
A                 */

/*
-- Previewing the file as a potential chapter -------------------------
*/

/* the following bits of code deals with enabling the REF file writer
to preview their REF file as a potnetial chapter in the book. */

/* this line sets up the user to be able to use the ENTER LATEX CLEAR to
remove all created files. The list of files to be removed is also expanded.
*/

uses ved_latex;
['.mm', '.ind', '.idx', '.tex*', '.log',
'.dvi', '.aux', '.toc', '.ilg',] -> ved_latex_clear;

/* This procedure wraps the end product of the REFORMAT program in a shell
of LATEX preamble and post amble so that it can be viewed as a potential
chapter in the manual. It does this by reading in the contents of the
two files containing the `ambles'*/

define  place_appropriate_text();
    vedtopfile();
    vedinsertstring('\n\n\\begin{document}\n\\tableofcontents \n');
    vedtopfile();
    veddo('r ' sys_>< rf_start_tex);
    vedendfile();
    veddo('r ' sys_>< rf_end_tex);
enddefine;

/* This procedure allows the shell script makemanual to be run as
a pop-11 procedure - this is done simply by changing the syntax
around*/

define manualmake(filename);
    lvars filename;
    if sys_fname_extn(filename) = '.tex' then
        sys_fname_nam(filename) -> filename;
    endif;
    sysobey(
            'xterm -e ' sys_>< rf_dir dir_>< 'makemanual ' sys_>< filename sys_>< ' &',
            `%`
    );
enddefine;

/* As the name suggests, this procedure latexes and displays the current
file by running the makemanual procedure with it as argument */

define latex_and_display(filename);
    lvars filename;
    true ->vedediting;
    vedputmessage('RUNNING MAKEMANUAL');
    false -> vedediting;
    manualmake(filename);
    ved_w1();
enddefine;

/* This procedure is the previewing program. It takes a copy of the current
file, gets its name, and creates a copy of it. It then REFORMATs the copy
and presents it as a potential chapter. The copy is written and quit and
a message appears on the status line telling of its location. Note that the
original is unchanged*/

define ved_filepreview();
    dlocal vedediting = false;
    dlocal vedbreak = false;
    vars
        tex_filename = vedcurrent sys_>< '.tex',
        root_filename = vedcurrent;
    ved_w1();                                    ;;; write original
    if vedpresent(tex_filename) then             ;;; an old version in the bufferlist
       veddo('ved ' sys_>< tex_filename);
       ved_rrq();
    endif;
    veddo('name ' sys_>< tex_filename);         ;;; make a copy
    whole_file_action();                        ;;; REFORMAT the copy
    place_appropriate_text();                   ;;; place in book shell
    ved_w1();                                   ;;; write result
    latex_and_display(root_filename);           ;;; process and display
    veddo('qved ' sys_>< root_filename);        ;;; quit copy, get original
    true -> vedediting;
    vedputmessage('TEX FILE IN 'sys_>< tex_filename sys_>< ' -- REMEMBER latex clear');
enddefine;


/*

-- Creating Manual from REF files -------------------------------------
*/


define get_reffiles(filename)->included_refs;
    lvars filename, included_refs = [];

    ;;; RETURNS -true- IF THE SPECIFIED POSITION IS AFTER A LATEX COMMENT
    define lconstant after_comment(line, col) -> commented;
        lvars line, col, commented = false;
        substring(1, col-1, vedbuffer(line)) -> line;
        until not(issubstring('\\%', line) ->> col) do;
            substring(1, col-1, line)
                sys_>< substring(col+2, datalength(line)-col-1, line) -> line;
        enduntil;
        issubstring('%', line) -> commented;
    enddefine;

    define :inline lconstant ri_mishap();
        (mishap(0, 'MALFORMED \\refinclude LINE ' sys_>< vedline))
    enddefine;

    define lconstant nextrefinclude() -> file;
        lvars file;
        if vedtryfind('\\\\refinclude') then
            if not(after_comment(vedline, vedcolumn)) then
                vedcolumn+11 -> vedcolumn;
                unless vedcurrentchar() == `{` then ri_mishap() endunless;
                lvars string = vedbuffer(vedline), close_pos;
                unless locchar(`}`, vedcolumn, string) ->> close_pos then
                endunless;
                sysparse_string(
                    substring(vedcolumn+1, close_pos-vedcolumn-1, string),
                    false
                ) -> file;
                if length(file) == 1 then
                    hd(file).consword -> file;
                    lvars char;
                    if locchar(`\``, 1, file) or locchar(`'`, 1, file)
                        or locchar(`"`, 1, file)
                    then
                        ri_mishap();
                    endif;
                else
                    ri_mishap();
                endif;
            endif;
        else
            false -> file;
        endif;
    enddefine;

    lvars filename_present = vedpresent(filename);
    veddo('ved ' sys_>< filename);
    vedtopfile();
    lvars old_wrapped = vedwrapped, file;
    until (nextrefinclude() ->> file) == false
        or vedwrapped > old_wrapped
    do
        if isword(file) then
            file :: included_refs -> included_refs;
        endif;
    enduntil;

    /* quit the file unless the user previously had it in their bufferlist */
    unless filename_present do;
        ved_q();
    endunless;

enddefine;


define make_manual(master);
    lvars master, hold;
    dlocal vedediting = false;
    dlocal vedbreak = false;
    if vedversions = false then
       pop_file_versions -> hold;
    else
       vedversions -> hold;
    endif;
    1 -> vedversions;
    unless master.sys_fname_extn = '.tex' do
        master sys_>< '.tex' -> master;
    endunless;

    unless readable(master) do
        mishap(master, 1, 'COULD NOT OPEN FILE');
    endunless;

    lvars ref_files = get_reffiles(master);
    lvars ref_file;
    for ref_file in ref_files do
        uppertolower(ref_file) :: included_reffiles -> included_reffiles;
    endfor;
    ;;; CHECK ALL REF FILES EXIST

    for ref_file in ref_files do;
        unless issubstring('lisp_', ref_file) or issubstring('vms_', ref_file) then
            unless syssearchpath(vedreflist, ref_file) do;
                mishap(ref_file, 1, 'CANNOT LOCATE REF FILE');
            endunless;
        endunless;
    endfor;
    for ref_file in ref_files do;
        /* The VMS line is only applicable if the manual is being produced at sussex */
        if issubstring('lisp_', ref_file) then
           veddo('lisp ref ' sys_>< substring(6,length(ref_file)-5,ref_file));
        elseif issubstring('vms_', ref_file) then         ;;; i.e. 'vms_sysutil'
           /* get the vms directory version of things */
           veddo('ved 'sys_>< '$popmaster/C.vms/ref/' sys_>< substring(5,length(ref_file)-4,ref_file));
        else
           veddo('ref ' sys_>< ref_file);
        endif;
           veddo('name ' sys_>< ref_file sys_>< '.tex');
           whole_file_action();
           ved_wq();
    endfor;

    lvars real_master = sys_fname_nam(master) sys_>< '_rf.tex';
    true ->vedediting;

    vedputmessage('/* CREATING MANUAL (IN SEPARATE XTERM) FROM ' sys_>< real_master sys_>< ' */');
    lvars i;
    for i from 1 to 500000 do  ;;; just to leave the message on the screen
    endfor;
    false -> vedediting;
    veddo('ved ' sys_>< real_master);
    ved_clear();
    vedinsertstring('\\include{' sys_>< sys_fname(master,1,4) sys_>< '}\n');
    vedtopfile();
    vedinsertstring('\n\n\\begin{document}\n \\pagenumbering{roman}');
    vedtopfile();
    veddo('r ' sys_>< rf_start_tex);
    vedendfile();
    veddo('r ' sys_>< rf_end_tex);
    ved_wq();
    if vedversions = false then
       hold -> pop_file_versions;
    else
       hold -> vedversions;
    endif;

    manualmake(real_master);

enddefine;


/* This is simply the VED ENTER version of the above procedure */

define ved_makemanual();
    dlocal vedbreak = false;
    dlocal vedediting = false;
    if vedargument = nullstring then
        rf_dir dir_>< 'manualmaster.tex' -> vedargument;
        true ->vedediting;
        vedputmessage('USING ' sys_>< vedargument);
        dlocal vedediting = false;
    endif;
    make_manual(vedargument);
enddefine;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jun  8 1995
        Regular-expression matching is now built in
--- Dermot M. McIntyre, Jul 26 1993
        Allowed for cross-volume referencing, restricting back up copies
        of created files, and caused Latex processing to be done in a
        separate xterm.
--- Dermot M. McIntyre, Jul 21 1993
        Removed final bugs, made more efficient
--- Dermot M. McIntyre, Jul 20 1993
        Added capability for sub-system cross referencing and including
        cross system files in the same master file.
--- Dermot M. McIntyre, Jul 18 1993
        Removed several major bugs - and made more user friendly.
 */
