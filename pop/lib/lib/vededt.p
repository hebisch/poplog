/* --- Copyright: see following header.                 ----------
 > --- Distributed with the permission of British Coal -----------
 > File:            C.all/lib/lib/vededt.p
 > Purpose:         Set up VED to emulate EDT
 > Author:          B. Armstrong 19th May 1987
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +oldvar;


/************************************************************************/
/*                                                                      */
/*      Module name: Vededt                                             */
/*                                                                      */
/*      Copyright (C)           British Coal          1987              */
/*                                                                      */
/*      Description:                                                    */
/*        Setup VED to behave a bit like EDT                            */
/*                                                                      */
/*      Author:     B Armstrong       Date:           19-May-87         */
/*                                                                      */
/*      Reader:     ?                 Date:           ?                 */
/*                                                                      */
/*                      Modification Status                             */
/*                      -------------------                             */
/*                                                                      */
/*    Date:           Prog        Description                           */
/*                                                                      */
/*    19-May-87       BA          Written                               */
/*                                                                      */
/************************************************************************/

  vars
    forward       gold        select
    prev_pos      direction   command
    sf            rv          bv_vps
    cur_pos       start_range
    ;
    true->forward;
    false->rv;
    false->gold;
    false->select;
    false->command;
    [0 0]->start_range;
    [0 0]->prev_pos;
    [0 0]->cur_pos;

  /****************************************************************/
  /*                                                              */
  /*    See if position p1 is after position p2, where p1 and p1  */
  /*    are lists of the form;- [line column]                     */
  /*                                                              */
  /****************************************************************/
  define bv_after(p1,p2)->after;
    p1(1)>p2(1)                 /* Later line ?                   */
    or p1(1)==p2(1) and p1(2)>p2(2 ) /* Same line,later column ?  */
      ->after;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    See if position p is inside the currently selected range  */
  /*    start_range to cur_pos. All three positions are lists of  */
  /*    the form;- [line column]                                  */
  /*                                                              */
  /****************************************************************/
  define bv_in_range(p)->in_range;
    vars s,e;
    if bv_after(cur_pos,start_range) /* Current pos after start of range ? */
    then
      cur_pos,start_range->s->e; /* Copy                          */
    else
      start_range,cur_pos->s->e; /* Swap                          */
    endif;
    not (bv_after(s,p))         /* Start not after this position ? */
    and bv_after(e,p)->in_range; /* Before end ?                  */
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Read the character currently under the cursor and rewrite */
  /*    to screen in reverse video if in select range.            */
  /*                                                              */
  /****************************************************************/
  define bv_draw_ch();
  vars ch;
    vedcurrentchar()->ch;       /* Get current character          */
    vedsetcursor();             /* Set the cursor                 */
    if bv_in_range([^vedline ^vedcolumn] ) /* In select range ?   */
    then
      if not(rv)
      then
        vedscreenescape('[7m'); /* Reverse video                  */
      endif;
      true->rv;                 /* Flag reverse                   */
    else
      if rv
      then
        vedscreenescape('[0m'); /* Normal video                   */
      endif;
      false->rv;                /* Flag normal                    */
    endif;
    vedscreenoutput(ch);        /* Write tha character            */
    vedcharnext();              /* Move on                        */
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Redraw select range.                                      */
  /*                                                              */
  /****************************************************************/
  define bv_draw_select();
    vars s,e;
    if prev_pos(1)<=vedlineoffset /* Previous position off top ?  */
    then
      [^(vedlineoffset+1) 1]->s; /* Start from top                */
      cur_pos->e;
    elseif prev_pos(1)>=vedlineoffset+vedwindowlength /* Off bottom ? */
    then
      cur_pos->s;               /* Start from current pos         */
      vedjumpto((vedlineoffset+vedwindowlength-1),1);
      [^vedline ^vvedlinesize]->e ; /* End at end of bottomn line */
    else
      if bv_after(cur_pos,prev_pos ) /* Current pos after previous ? */
      then
        cur_pos,prev_pos->s->e; /* Prev_pos to cur_pos            */
      else
        prev_pos,cur_pos->s->e; /* Cur_pos to prev_pos            */
      endif;
    endif;
    vedjumpto(s(1),s(2));       /* Jump to start                  */
    false->rv;                  /* Start in normal                */
    until bv_after([^vedline ^vedcolumn],e) do
      bv_draw_ch();             /* Draw a charcter                */
    enduntil;
    vedjumpto(cur_pos(1),cur_pos(2)) ; /* Back to current position */
    if rv
    then
      vedscreenescape('[0m');   /* Normal video                   */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Keep the cursor in the middle third of the screen.        */
  /*                                                              */
  /****************************************************************/
  define bv_midwindow();
    vars p,cp;
    vedline-vedlineoffset->p;   /* Current position relative to windo w */
    [^vedline ^vedcolumn]->cp;  /* Save current position          */
    while p>(2 * vedwindowlength div 3) do
      if select                 /* Select range active ?          */
      and bv_in_range([^(vedlineoffset+vedwindowlength) 1])
      then
        vedscreenescape('[7m'); /* Reverse video                  */
        vedscrollup();          /* Scroll up                      */
        vedscreenescape('[0m'); /* Normal video                   */
      else
        vedscrollup();          /* Scroll up                      */
      endif;
      p-1->p;                   /* Bump p                         */
    endwhile;
    while p<(vedwindowlength div 3) do
      if select                 /* Select range active ?          */
      and bv_in_range([^vedlineoffset 1])
      then
        vedscreenescape('[7m'); /* Reverse video                  */
        vedscrolldown();        /* Scroll down                    */
        vedscreenescape('[0m'); /* Normal video                   */
      else
        vedscrolldown();        /* Scroll down                    */
      endif;
      p+1->p;                   /* Bump p                         */
    endwhile;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Perform procedure proc n times, taking account of select  */
  /*    range.                                                    */
  /*                                                              */
  /****************************************************************/
  define bv_move(n, proc);
    vars i;
    if select                   /* Select range active ?          */
    then
      [^vedline ^vedcolumn]->prev_pos ; /* Save the previous position */
      for i from 1 to n do
        proc();                 /* Do the move                    */
      endfor;
      vedcheck();               /* Ensure current pos in window   */
      vedsetcursor();           /* Correctly position the cursor  */
      [^vedline ^vedcolumn]->cur_pos ; /* Save the current position */
      bv_draw_select();         /* Re-draw range                  */
    else
      for i from 1 to n do
        proc();                 /* Do the move                    */
      endfor;
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Gold key definition, set gold flag true.                  */
  /*                                                              */
  /****************************************************************/
  define bv_gld();
    true->gold;                 /* Gold key pressed               */
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Enter, opens command.                                     */
  /*                                                              */
  /****************************************************************/
  define bv_enter();
    select->sf;                 /* Save select flag               */
    false->select;              /* Ensure select off              */
    vedpositionstack->bv_vps;   /* Save the position stack        */
    vedenter();                 /* Open command line              */
    true->command;              /* In the command buffer          */
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Locate vvedsrchstring forwards or backwards.              */
  /*                                                              */
  /****************************************************************/
  define bv_locate();
    if forward                  /* Forward locate ?               */
    then
      vedlocate(vvedsrchstring); /* Search forwards               */
    else
      vedbacklocate(vvedsrchstring) ; /* Search backwards         */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    PF3 key definition, get vvedsrchstring or search for      */
  /*    previous string.                                          */
  /*                                                              */
  /****************************************************************/
  define bv_find();
    if gold                     /* Gold pressed ?                 */
    then
      false->gold;              /* Not gold any more              */
      false->vvedsrchstring;    /* Clear search string            */
    endif;
    if not(vvedsrchstring)      /* Empty search string ?          */
    then
      bv_enter();               /* Enter                          */
      vedputcommand('/                        ');
      vedcharnext();            /* Position cursor                */
    else
      bv_move(1, bv_locate);    /* Locate the string              */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    PF4 key definition, delete or undelete line.              */
  /*                                                              */
  /****************************************************************/
  define bv_delline();
    if gold                     /* Gold key pressed ?             */
    then
      false->gold;              /* Not gold any more              */
      ved_yankl();              /* Un-delete line                 */
      bv_move(1,vedscreenleft); /* Start of line                  */
    else
      vedlinedelete();          /* Delete line                    */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Keypad 7 key definition, enter command line.              */
  /*                                                              */
  /****************************************************************/
  define bv_cmd();
    if gold                     /* Gold pressed ?                 */
    then
      false->gold;              /* Not gold any more              */
      bv_enter();               /* Enter command                  */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Keypad 8 key definition, page up or down.                 */
  /*                                                              */
  /****************************************************************/
  define bv_page();
    if forward                  /* What direction are we going ?  */
    then
      bv_move(1,vedscreendown); /* Go forwards one screen         */
    else
      bv_move(1,vedscreenup);   /* Go backwards one screen        */
    endif;
    bv_midwindow();             /* Stay within centre portion of windo w */
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Keypad - key definition, delete or undelete word.         */
  /*                                                              */
  /****************************************************************/
  define bv_delword();
    vars p;
    if gold                     /* Gold key pressed ?             */
    then
      false->gold;              /* Not gold any more              */
      [^vedline ^vedcolumn]->p; /* Remember position              */
      ved_yankw();              /* Un delete word                 */
      vedjumpto(p(1),p(2));     /* Move back to previous pos      */
    else
      vedwordrightdelete();     /* Delete a word                  */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Keypad 4 key definition, forward or bottom of file.       */
  /*                                                              */
  /****************************************************************/
  define bv_fw();
    if gold                     /* Gold key pressed ?             */
    then
      false->gold;              /* Not gold any more              */
      bv_move(1,vedendfile);    /* Go to end of file              */
    else
      true->forward             /* Forwards                       */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Keypad 5 key definition, backward or top of file.         */
  /*                                                              */
  /****************************************************************/
  define bv_bw();
    if gold                     /* Gold key pressed ?             */
    then
      false->gold;              /* Not gold any more              */
      bv_move(1,vedtopfile);    /* Go to top of file              */
    else
      false->forward            /* Backwards                      */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Terminate select range.                                   */
  /*                                                              */
  /****************************************************************/
  define bv_terminate_range();
    vedpositionpush();          /* Push end of range              */
    false->select;              /* Not in select range any more   */
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Splice vvedcut_dump back into file.                       */
  /*                                                              */
  /****************************************************************/
  define bv_paste();
    false->gold;                /* Not gold any more              */
    ved_splice();               /* Paste the most recently cut rang e */
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Keypad 6 key definition, cut current select range.        */
  /*                                                              */
  /****************************************************************/
  define bv_cut();
    if select                   /* Select range in action ?       */
    then
      if gold                   /* Gold pressed ?                 */
      then
        bv_move(1,bv_paste);    /* Paste what's in vvedcut_dump   */
      else
        bv_terminate_range();   /* Terminate the select range     */
        ved_cut();              /* Cut the marked range           */
      endif;
    elseif gold                 /* Gold pressed ?                 */
    then
      bv_move(1,bv_paste);      /* Paste what's in vvedcut_dump   */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Keypad , key definition, delete character.                */
  /*                                                              */
  /****************************************************************/
  define bv_delchar();
    if gold                     /* Gold key pressed ?             */
    then
      false->gold;              /* Not gold any more              */
    else
      veddotdelete();           /* Delete character under cursor  */
    endif;
  enddefine;


  /****************************************************************/
  /*                                                              */
  /*    Keypad 1 key definition, move left or right one word or,  */
  /*    change case of select range or current character.         */
  /*                                                              */
  /****************************************************************/
  define bv_movw();
    vars p1,p2,line col;

    if gold                     /* Gold key pressed ?             */
    then
      false->gold;              /* Not gold any more              */
      if select                 /* Range selected ?               */
      then
        bv_terminate_range();   /* Terminate the select range     */
        fast_destpair(fast_destpair(vedpositionstack))
        ->vedpositionstack->p2->p1 ; /* Get positions             */
        if bv_after(p1,p2)      /* 'p1' after 'p2' ?              */
        then
          p1,p2->p1->p2;        /* Swap them using the stack      */
        endif;
        vedjumpto(p1(1),p1(2)); /* Move to start of range         */
        p2(1)->line; p2(2)->col; /* Final positions               */
        until vedline=line and vedcolumn=col do
          vedchangecase();      /* Change case of current character.. . */
        enduntil;               /* ...throughout select range     */
      else
        bv_move(1,vedchangecase); /* Change case of current characte r */
      endif;
    elseif forward              /* Which direction ?              */
    then
      bv_move(1,vedwordright);  /* Move right one word            */
    else
      bv_move(1,vedwordleft);   /* Move left one word             */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Move to end of line, or end of next line if already at    */
  /*    end of line.                                              */
  /*                                                              */
  /****************************************************************/
  define bv_end_line();
    if forward                  /* Which direction ?              */
    then
      if vedcolumn>=vvedlinesize /* End of line already ?         */
      then
        vedchardown();          /* Move down a line               */
      endif;
      vedtextright();           /* Move to end of this line       */
    else
      vedcharup();              /* Move up one line               */
      vedtextright();           /* Move to end of this line       */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Keypad 2 key definition, end of line, or end of next line */
  /*    if already at end of line, or delete to end of current    */
  /*    line.                                                     */
  /*                                                              */
  /****************************************************************/
  define bv_eoln();
    if gold                     /* Gold key pressed ?             */
    then
      false->gold;              /* Not gold any more              */
      vedcleartail();           /* Delete to end of line          */
    else
      bv_move(1,bv_end_line);
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Keypad 3 key definition, move one character left or       */
  /*    right.                                                    */
  /*                                                              */
  /****************************************************************/
  define bv_movc();
    if gold                     /* Gold key pressed ?             */
    then
      false->gold;              /* Not gold any more              */
    elseif forward              /* Which direction ?              */
    then
      bv_move(1,vedcharnext);   /* Move to next character         */
    else
      bv_move(1,vedcharleft);   /* Move to next character         */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Return key definition, this is part of an abortive        */
  /*    attempt to get round the termination of select ranges     */
  /*    when command line entered.                                */
  /*                                                              */
  /****************************************************************/
  define bv_cr();
    vedinsertvedchar();         /* Obey the command               */
    false->command;             /* End of command                 */
    sf->select;                 /* Restore select flag            */
    bv_vps->vedpositionstack;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Start of line, or if already at start of line start of    */
  /*    next line.                                                */
  /*                                                              */
  /****************************************************************/
  define bv_start_line();
    if forward                  /* Which direction ?              */
    then
      vednextline();            /* Start of next line             */
    else
      if vedcolumn=1            /* Already at start of line ?     */
      then
        vedcharup();            /* Move up one line               */
      endif;
      vedscreenleft();          /* Move to start of this line     */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Keypad 0 key definition, start of line, or if already     */
  /*    at start of line start of next line, or open new line.    */
  /*                                                              */
  /****************************************************************/
  define bv_newln();
    if gold                     /* Gold key pressed ?             */
    then
      false->gold;              /* Not gold any more              */
      vedcharinsert(13);        /* Open new line                  */
      vedcharup();              /* Go up a line                   */
      vedscreenleft();          /* Start of line                  */
    else
      bv_move(1,bv_start_line);
      bv_midwindow();           /* Stay within centre portion of windo w */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Keypad . key definition, start select range or cancell    */
  /*    current select range.                                     */
  /*                                                              */
  /****************************************************************/
  define bv_range();
    if gold and select          /* Gold and select range active ? */
    then
      false->select;            /* Not select any more            */
      false->gold;              /* Cancell gold                   */
      vedrefresh();             /* Refresh screen                 */
    elseif not(select)          /* Don't do anything if select alread y */
    then
      true->select;             /* Range is open                  */
      vedpositionpush();        /* Push the start of range        */
      [^vedline ^vedcolumn]->start_range ; /* Save start of range */
    else
      vederror('Select range already active');
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Keypad up-arrow key definition, move up to previous line. */
  /*                                                              */
  /****************************************************************/
  define bv_up();
    false->gold;                /* Cancell gold                   */
    bv_move(1,vedcharup);       /* Up to next line                */
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Keypad down-arrow key definition, move down to next line. */
  /*                                                              */
  /****************************************************************/
  define bv_down();
    false->gold;                /* Cancell gold                   */
    bv_move(1,vedchardown);     /* Down to next line              */
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Keypad left-arrow key definition, move to previous        */
  /*    character.                                                */
  /*                                                              */
  /****************************************************************/
  define bv_left();
    false->gold;                /* Cancell gold                   */
    bv_move(1,vedcharleft);     /* Left to next char              */
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Keypad right-arrow key definition, move to next character.*/
  /*                                                              */
  /****************************************************************/
  define bv_right();
    false->gold;                /* Cancell gold                   */
    bv_move(1,vedcharnext);     /* Right to next char             */
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Insert last character from input stream.                  */
  /*                                                              */
  /****************************************************************/
  define bv_outch();
    false->gold;                /* Cancell gold                   */
    bv_move(1,vedinsertvedchar); /* Output the character          */
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    QWERTY c or C key definition, find start of comment.      */
  /*                                                              */
  /****************************************************************/
  define bv_c();
    if gold                     /* Gold key pressed ?             */
    then
      false->gold;              /* Not gold any more              */
      '/*'->vvedsrchstring;     /* Search for comment             */
      bv_move(1,bv_locate);     /* Find it                        */
    else
      bv_outch();               /* Insert a c                     */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    QWERTY d key definition, insert date into file.           */
  /*                                                              */
  /****************************************************************/
  define bv_d();
    if gold                     /* Gold key pressed ?             */
    then
      false->gold;              /* Not gold any more              */
      veddo('date c');          /* Include date in file           */
      vedpositionpush();        /* Push the current position      */
      bv_move(9,vedcharleft);   /* Move left 9 times              */
      vedpositionpush();        /* Push the current position      */
      ved_cut();                /* Cut the time                   */
    else
      bv_outch();               /* Insert a d                     */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    QWERTY D key definition, insert date and time into file.  */
  /*                                                              */
  /****************************************************************/
  define bv_cap_d();
    if gold                     /* Gold key pressed ?             */
    then
      false->gold;              /* Not gold any more              */
      veddo('date c');          /* Include date in file           */
    else
      bv_outch();               /* Insert a d                     */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    QWERTY e or E key definition, exit current file.          */
  /*                                                              */
  /****************************************************************/
  define bv_e();
    if gold                     /* Gold key pressed ?             */
    then
      false->gold;              /* Not gold any more              */
      ved_wq();                 /* Write this file and quit       */
    else
      bv_outch();               /* Insert an e                    */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Insert date in header.                                    */
  /*                                                              */
  /****************************************************************/
  define bv_date();
    bv_move(3,bv_locate);       /* Find third question mark       */
    vedpositionpush();          /* Start of cut                   */
    bv_move(11,vedcharnext);    /* Move 11 characters to right    */
    vedpositionpush();          /* End of cut                     */
    ved_cut();                  /* Cut the spaces out             */
    true->gold;                 /* Gold                           */
    bv_d();                     /* Insert date                    */
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    QWERTY h or H key definition, insert header into current  */
  /*    file (See comment at top of file).                        */
  /*                                                              */
  /****************************************************************/
  define bv_h();
    if gold                     /* Gold key pressed ?             */
    then
      veddo('ved poplib:header.p'); /* Edit the header file       */
      vedtopfile();             /* Go to top of file              */
      vedpositionpush();        /* Push start of range            */
      vedendfile();             /* End if file                    */
      vedpositionpush();        /* Push end of range              */
      ved_cut();                /* Cut the whole of the file      */
      ved_rrq();                /* Quit                           */
      ved_splice();             /* Splice it into other file      */
      vedtopfile();             /* Go to top of file              */
      true->forward;            /* Forward search                 */
      '?'->vvedsrchstring;      /* Search for '?'                 */
      bv_date();                /* Put the date in                */
      bv_date();                /* Put the date in                */
      vedtopfile();             /* Go to top of file              */
      false->gold;              /* Not gold any more              */
    else
      bv_outch();               /* Insert an h                    */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    QWERTY l or L key definition, write the current file and  */
  /*    load it.                                                  */
  /*                                                              */
  /****************************************************************/
  define bv_l();
    if gold                     /* Gold key pressed ?             */
    then
      false->gold;              /* Not gold any more              */
      ved_w1();                 /* Write current file             */
      ved_load();               /* Load it                        */
    else
      bv_outch();               /* Insert an l                    */
    endif;
  enddefine;


  /****************************************************************/
  /*                                                              */
  /*    QWERTY q or Q key definition, quit current file.          */
  /*                                                              */
  /****************************************************************/
  define bv_q();
    if gold                     /* Gold key pressed ?             */
    then
      false->gold;              /* Not gold any more              */
      ved_q();                  /* Get out                        */
    else
      bv_outch();               /* Insert a q                     */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    QWERTY s or S key definition, global substitute.          */
  /*                                                              */
  /****************************************************************/
  define bv_s();
    if gold                     /* Gold key pressed ?             */
    then
      false->gold;              /* Not gold any more              */
      bv_enter();               /* Enter command mode             */
      vedputcommand('gs/                        ');
      bv_move(3,vedcharnext);   /* Position cursor                */
    else
      bv_outch();               /* Insert an s                    */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    QWERTY t or T key definition, transpose characters to     */
  /*    left of cursor.                                           */
  /*                                                              */
  /****************************************************************/
  define bv_t();
    if gold                     /* Gold key pressed ?             */
    then
      false->gold;              /* Not gold any more              */
      ved_sw();                 /* Swap the two characters to left */
    else
      bv_outch();               /* Insert a t                     */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    QWERTY 0 key definition, delete a few extraneous spaces.  */
  /*                                                              */
  /****************************************************************/
  define bv_0();
    if gold                     /* Gold key pressed ?             */
    then
      false->gold;              /* Not gold any more              */
      veddo('gs/< /</');        /* Get rid of spaces after<       */
      veddo('gs/ </</');        /* Get rid of spaces before<      */
      veddo('gs/> />/');        /* Get rid of spaces after>       */
      veddo('gs/ >/>/');        /* Get rid of spaces before>      */
      veddo('gs/+ /+/');        /* Get rid of spaces after+       */
      veddo('gs/ +/+/');        /* Get rid of spaces before+      */
      veddo('gs/- /-/');        /* Get rid of spaces after-       */
      veddo('gs/ -/-/');        /* Get rid of spaces before-      */
      veddo('gs/[ /[/');        /* Get rid of spaces after [      */
      veddo('gs/ ]/]/');        /* Get rid of spaces before]      */
      veddo('gs/ (/(/');        /* Get rid of spaces before(      */
      veddo('gs/( /(/');        /* Get rid of spaces after(       */
      veddo('gs/ )/)/');        /* Get rid of spaces before)      */
    else
      bv_outch();               /* Insert an 0                    */
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Move cursor to position n by inserting or deleting spaces.*/
  /*                                                              */
  /****************************************************************/
  define bv_pos(n);
    while vedcolumn<n do
      vedcharinsert(32);        /* Insert a space                 */
    endwhile;
    while vedcolumn>n do
      vedcharleft();            /* Move left                      */
      if vedcurrentchar()/=32   /* Not a space ?                  */
      then
        vedcharnext();          /* Move back right                */
        quitloop;               /* Get out                        */
      endif;
      veddotdelete();           /* Delete the space               */
    endwhile;
    if vedcolumn>=n             /* Correct column or after ?      */
    then
      vedcharleft();            /* Move left                      */
      if vedcurrentchar()/=32   /* Not a space ?                  */
      then
        vedcharnext();          /* Move back right                */
        vedcharinsert(32);      /* Insert a space                 */
      else
        vedcharnext();          /* Move back right                */
      endif;
    endif;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    See if we are at the start of a comment.                  */
  /*                                                              */
  /****************************************************************/
  define bv_start_comment()->s;
  vars p;
    vedcolumn->p;               /* Mark the position              */
    vedwordright();             /* Move right a word              */
    vedcurrentchar()=47->s;     /* Slash ?                        */
    vedjumpto(vedline, p);      /* Jump back                      */
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Move to end of comment.                                   */
  /*                                                              */
  /****************************************************************/
  define bv_end_comment();
    vedtextright();             /* Move to end of line            */
    bv_move(2,vedcharleft);     /* Get to end of comment          */
    bv_pos(67);                 /* Position the end of comment    */
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Split a long comment.                                     */
  /*                                                              */
  /****************************************************************/
  define bv_split_comment();
    vars s;
    vedinsertstring('*/');      /* Put in the end of comment      */
    bv_move(2,vedcharleft);     /* Get to end of comment          */
    bv_pos(67);                 /* Move to column 67              */
    bv_move(2,vedcharnext);     /* Move right two                 */
    vedcharinsert(13);          /* Put in a return                */
    '/*'->s;
    vedinsertstring(s);         /* Put in the start of comment    */
    vedjumpto(vedline,1);       /* Reset cursor                   */
    vedinsertstring('                                ');
                                /* Put in 32 spaces               */
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    Process a comment.                                        */
  /*                                                              */
  /****************************************************************/
  define bv_proc_comment();
    vars s;
    if vedcolumn>67             /* Comment starts too far to right ? */
    then
      vedcharinsert(13);        /* Put in a return                */
    endif;
    bv_pos(33);                 /* Position the start of comment  */
    bv_end_comment();           /* Do the end of comment          */
    while vedcolumn>79 do       /* Comment ends off the screen ?  */
      while vedcolumn>67 do     /* Move back onto screen          */
        vedwordleft();          /* Move left one word             */
        bv_start_comment()->s;  /* Start of comment ?             */
        quitif(s);              /* Quit if start of comment       */
      endwhile;
      if s                      /* Start of comment ?             */
      then
        vedcharinsert(13);      /* Put in a return                */
        vedinsertstring('                                ');
                                /* Put in 32 spaces               */
      else
        bv_split_comment();     /* Split the comment              */
      endif;
      bv_end_comment();         /* Do the end of comment          */
    endwhile;
  enddefine;

  /****************************************************************/
  /*                                                              */
  /*    QWERTY 9 key definition, Lay out comments.                */
  /*                                                              */
  /****************************************************************/
  define bv_9();
    vars s, p, l;
    if gold                     /* Gold key pressed ?             */
    then
      [^vedline ^vedcolumn]->p; /* Mark the position              */
      false->gold;              /* Not gold any more              */
      vvedsrchstring->s;        /* Save search string             */
      bv_move(1,vedtopfile);    /* Move to top of file            */
      true->forward;            /* Go forwards                    */
      '/*'->vvedsrchstring;     /* Search for start of comment    */
      bv_move(1,bv_locate);     /* Find it                        */
      vedline->l;               /* Start from current position    */
      while vedline>=l do       /* Terminate when we wrap         */
        if vedcolumn>8          /* Leave comments in columns 1 to 8 */
        then
          bv_proc_comment();    /* Process this comment           */
        endif;
        vedline->l;             /* Remember this line             */
        bv_move(1,bv_locate);   /* Find next                      */
      endwhile;
      vedjumpto(p(1), p(2));    /* Jump back                      */
      s->vvedsrchstring;        /* Restore search string          */
    else
      bv_outch();               /* Insert an e                    */
    endif;
  enddefine;


/******************************************************************/
/*                                                                */
/*    Key redefinitions.                                          */
/*                                                                */
/*    Keypad and others.                                          */
/*                                                                */
/******************************************************************/

vedsetkey('\^[OP',bv_gld);      /* PF1(GOLD)                      */
vedsetkey('\^[OR',bv_find);     /* PF3(find)                      */
vedsetkey('\^[OS',bv_delline);  /* PF4(delete line)               */
vedsetkey('\^[Ow',bv_cmd);      /* 7(command)                     */
vedsetkey('\^[Ox',bv_page);     /* 8(page)                        */
vedsetkey('\^[Oy','');          /* 9(nothing)                     */
vedsetkey('\^[Om',bv_delword);  /*-(delete word)                  */
vedsetkey('\^[Ot',bv_fw);       /* 4(forwards)                    */
vedsetkey('\^[Ou',bv_bw);       /* 5(backwards)                   */
vedsetkey('\^[Ov',bv_cut);      /* 6(cut/paste)                   */
vedsetkey('\^[Ol',bv_delchar);  /* ,(delete character)            */
vedsetkey('\^[Oq',bv_movw);     /* 1(move word)                   */
vedsetkey('\^[Or',bv_eoln);     /* 2(move to end of line)         */
vedsetkey('\^[Os',bv_movc);     /* 3(move one character)          */
vedsetkey('\^[OM',bv_cr);       /* Carriage return                */
vedsetkey('\^[OM','\^M');       /* ENTER(carriage return)         */
vedsetkey('\^[Op',bv_newln);    /* 0(next/new line)               */
vedsetkey('\^[On',bv_range);    /* .(range select)                */
vedsetkey('\^[[A',bv_up);       /* UP ARROW                       */
vedsetkey('\^[[B',bv_down);     /* DOWN ARROW                     */
vedsetkey('\^[[C',bv_right);    /* RIGHT ARROW                    */
vedsetkey('\^[[D',bv_left);     /* LEFT ARROW                     */

/******************************************************************/
/*                                                                */
/*    QWERTY keys.                                                */
/*                                                                */
/******************************************************************/

vedsetkey('a',bv_outch);        /* Insert an 'a'                  */
vedsetkey('b',bv_outch);        /* Insert a 'b'                   */
vedsetkey('c',bv_c);            /* Gold-c = find comment          */
vedsetkey('d',bv_d);            /* Gold-d = get date(not time)    */
vedsetkey('e',bv_e);            /* Gold-e = exit                  */
vedsetkey('f',bv_outch);        /* Insert an 'f'                  */
vedsetkey('g',bv_outch);        /* Insert a 'g'                   */
vedsetkey('h',bv_h);            /* Gold-h include header file     */
vedsetkey('i',bv_outch);        /* Insert an 'i'                  */
vedsetkey('j',bv_outch);        /* Insert a 'j'                   */
vedsetkey('k',bv_outch);        /* Insert a 'k'                   */
vedsetkey('l',bv_l);            /* Gold-l = write and load current fil e */
vedsetkey('m',bv_outch);        /* Insert an 'm'                  */
vedsetkey('n',bv_outch);        /* Insert an 'n'                  */
vedsetkey('o',bv_outch);        /* Insert an 'o'                  */
vedsetkey('p',bv_outch);        /* Insert a 'p'                   */
vedsetkey('q',bv_q);            /* Gold-q = quit                  */
vedsetkey('r',bv_outch);        /* Insert an 'r'                  */
vedsetkey('s',bv_s);            /* Gold-s = global substitute     */
vedsetkey('t',bv_t);            /* Gold-t = transpose             */
vedsetkey('u',bv_outch);        /* Insert a 'u'                   */
vedsetkey('v',bv_outch);        /* Insert a 'v'                   */
vedsetkey('w',bv_outch);        /* Insert a 'w'                   */
vedsetkey('x',bv_outch);        /* Insert an 'x'                  */
vedsetkey('y',bv_outch);        /* Insert a 'y'                   */
vedsetkey('z',bv_outch);        /* Insert a 'z'                   */
vedsetkey('A',bv_outch);        /* Insert an 'A'                  */
vedsetkey('B',bv_outch);        /* Insert a 'B'                   */
vedsetkey('C',bv_c);            /* Gold-c = find comment          */
vedsetkey('D',bv_cap_d);        /* Gold-d = get date and time     */
vedsetkey('E',bv_e);            /* Gold-e = exit                  */
vedsetkey('F',bv_outch);        /* Insert an 'F'                  */
vedsetkey('G',bv_outch);        /* Insert a 'G'                   */
vedsetkey('H',bv_h);            /* Gold-h include header file     */
vedsetkey('I',bv_outch);        /* Insert an 'i'                  */
vedsetkey('J',bv_outch);        /* Insert a 'J'                   */
vedsetkey('K',bv_outch);        /* Insert a 'K'                   */
vedsetkey('L',bv_l);            /* Gold-l = write and load current fil e */
vedsetkey('M',bv_outch);        /* Insert an 'M'                  */
vedsetkey('N',bv_outch);        /* Insert an 'N'                  */
vedsetkey('O',bv_outch);        /* Insert an 'O'                  */
vedsetkey('P',bv_outch);        /* Insert a 'P'                   */
vedsetkey('Q',bv_q);            /* Gold-q = quit                  */
vedsetkey('R',bv_outch);        /* Insert an 'R'                  */
vedsetkey('S',bv_s);            /* Gold-s = global substitute     */
vedsetkey('T',bv_t);            /* Gold-t = transpose             */
vedsetkey('U',bv_outch);        /* Insert a 'U'                   */
vedsetkey('V',bv_outch);        /* Insert a 'V'                   */
vedsetkey('W',bv_outch);        /* Insert a 'W'                   */
vedsetkey('X',bv_outch);        /* Insert an 'X'                  */
vedsetkey('Y',bv_outch);        /* Insert a 'Y'                   */
vedsetkey('Z',bv_outch);        /* Insert a 'Z'                   */
vedsetkey('0',bv_0);            /* Gold-0 gets rid of loads of space s */
vedsetkey('1',bv_outch);        /* Insert a '1'                   */
vedsetkey('2',bv_outch);        /* Insert a '2'                   */
vedsetkey('3',bv_outch);        /* Insert a '3'                   */
vedsetkey('4',bv_outch);        /* Insert a '4'                   */
vedsetkey('5',bv_outch);        /* Insert a '5'                   */
vedsetkey('6',bv_outch);        /* Insert a '6'                   */
vedsetkey('7',bv_outch);        /* Insert a '7'                   */
vedsetkey('8',bv_outch);        /* Insert a '8'                   */
vedsetkey('9',bv_9);            /* Gold-9 = Line up comments      */
vedsetkey(';',bv_outch);        /* Insert a ';'                   */
vedsetkey(':',bv_outch);        /* Insert a ':'                   */
vedsetkey('\'',bv_outch);       /* Insert a '''                   */
vedsetkey('"',bv_outch);        /* Insert a '"'                   */
vedsetkey('[',bv_outch);        /* Insert a '['                   */
vedsetkey(']',bv_outch);        /* Insert a ']'                   */
vedsetkey('{',bv_outch);        /* Insert a '{'                   */
vedsetkey('}',bv_outch);        /* Insert a '}'                   */
vedsetkey('!',bv_outch);        /* Insert a '!'                   */
vedsetkey('@',bv_outch);        /* Insert a '@'                   */
vedsetkey('#',bv_outch);        /* Insert a '#'                   */
vedsetkey('$',bv_outch);        /* Insert a '$'                   */
vedsetkey('%',bv_outch);        /* Insert a '%'                   */
vedsetkey('^',bv_outch);        /* Insert a '^'                   */
vedsetkey('&',bv_outch);        /* Insert a '&'                   */
vedsetkey('*',bv_outch);        /* Insert a '*'                   */
vedsetkey('(',bv_outch);        /* Insert a '('                   */
vedsetkey(')',bv_outch);        /* Insert a ')'                   */
vedsetkey('_',bv_outch);        /* Insert a '_'                   */
vedsetkey('-',bv_outch);        /* Insert a '-'                   */
vedsetkey('+',bv_outch);        /* Insert a '+'                   */
vedsetkey('=',bv_outch);        /* Insert a '='                   */
vedsetkey('~',bv_outch);        /* Insert a '~'                   */
vedsetkey('`',bv_outch);        /* Insert a '`'                   */
vedsetkey('\\',bv_outch);       /* Insert a '\'                   */
vedsetkey('|',bv_outch);        /* Insert a '|'                   */
vedsetkey(',',bv_outch);        /* Insert a ','                   */
vedsetkey('.',bv_outch);        /* Insert a '.'                   */
vedsetkey('<',bv_outch);        /* Insert a '<'                   */
vedsetkey('>',bv_outch);        /* Insert a '>'                   */
vedsetkey('/',bv_outch);        /* Insert a '/'                   */
vedsetkey('?',bv_outch);        /* Insert a '?'                   */
vedsetkey(' ',bv_outch);        /* Insert a ' '                   */

/******************************************************************/
/*                                                                */
/*    Setup a few vedvars.                                        */
/*                                                                */
/******************************************************************/

2->vedindentstep;               /* Indent by two spaces           */
false->veddelspaces;            /* Not quite sure about this      */
false->vedbreak;                /* Stop ved from breaking lines   */
