/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.x/x/pop/lib/XpmDemo.p
 > Purpose:         Load and run the Xpm demo
 > Author:          Julian Clinton, Feb 14 1996
 > Documentation:   REF *Xpm
 > Related Files:   LIB *Xpm
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

uses popxdemo;

exload_batch;

uses popxlib;
uses Xm;

include xpt_coretypes.ph;
include xt_constants.ph;
include XmConstants.ph;

uses
    xt_init,
    xt_widget,
    xt_widgetclass,
    xt_widgetinfo,
    xt_composite,
    xt_popup,
    xt_event,
    xt_callback,
    xt_resource,

    xtTopLevelShellWidget,
    xmRowColumnWidget,
    xmPushButtonWidget,
;

uses Xpm;

endexload_batch;


;;; A yellow gem
;;;
vars diamond1_icon = {%
'48 48 6 1',
'. c #000',
'X c #fafaf8f8cccc',
'o c #c8c8b5b56060',
'O c #ffffe8e87c7c',
'+ c #ffffffffffff',
'@ c #818173733939',
'................................................',
'................................................',
'.......................Xo.......................',
'......................XOOo......................',
'....................XXOOOOoo....................',
'...................XXXOOOOooo...................',
'..................XXXOOOOOOooo..................',
'................+XXXXOOOOOOooooo................',
'...............X+XXXXOOOOOOoooooo...............',
'..............XX+XXXOOOOOOOOoooooo..............',
'.............XXX+XXXOOOOOOOOoooooooo............',
'............XXXX+XXOOOOOOOOOOoooooooo...........',
'..........XXXXXX+XXOOOOOOOOOOooooooooo..........',
'.........XXXXXXX+XXOOOOOOOOOOooooooooooo........',
'........XXXXXXXX+XOOOOOOOOOOOOooooooooooo.......',
'.......XX+XXXXXX+XOOOOO+OOOOOOoooooooooooo......',
'.....XXXXX+XXXXX+OOOOO+OOOOOOOOooooooooooooo....',
'....XXXXXXX+XXXX+OOOO+OOOOOOOOOoooooooooooooo...',
'..XXXXXXXXXX+XXX+OOO+OOOOOOOOOOooooooooooooo@@..',
'...OOXXXXXXXX+X+++O+OOOOOOOOOOOOooooooooo@@@@...',
'....OOOOOXXXXX+++++OOOOOOOOOOOOOoooooo@@@@@@....',
'....OOOOOOOOO+++++++OOOOOOOOOOOOOoo@@@@@@@@@....',
'....++++++++++++++++++++++++ooooo@@@@@@@@@@.....',
'......OOOOOOO+++++++ooooooooooooo@@@@@@@@@......',
'.......OOOOOOO+++++ooooooooooooo@@@@@@@@@.......',
'.......OOOOOO+O+++o+oooooooooooo@@@@@@@@@.......',
'........OOOO+OOO+ooo+oooooooooo@@@@@@@@@........',
'.........OO+OOOO+Oooo+ooooooooo@@@@@@@@.........',
'..........+OOOOO+Ooooo+oooooooo@@@@@@@..........',
'.........+.OOOOO+Oooooo+oooooo@@@@@@@...........',
'...........OOOOO+OOooooooooooo@@@@@@@...........',
'............OOOO+OOoooooooooo@@@@@@@............',
'.............OOO+OOoooooooooo@@@@@@.............',
'..............OO+OOOooooooooo@@@@@..............',
'..............OO+OOOoooooooo@@@@@@..............',
'...............O+OOOOooooooo@@@@@...............',
'................OOOOOooooooo@@@@................',
'.................OOOOoooooo@@@@.................',
'..................OOOOooooo@@@..................',
'..................OOOOoooo@@@@..................',
'...................OOOoooo@@@...................',
'....................OOOooo@@....................',
'.....................OOoo@@.....................',
'.....................OOoo@@.....................',
'......................OO@@......................',
'.......................O@.......................',
'................................................',
'................................................'
%};


;;; A blue gem
;;;
vars diamond2_icon = {%
'48 48 10 1',
'. c #000',
'X c #fafaf8f8cccc',
'o c #c8c8b5b56060',
'O c #ffffe8e87c7c',
'+ c #ffffffffffff',
'@ c #818173733939',
'# c #3b3bb9b9ffff',
'$ c #30307373a5a5',
'% c #15159797dfdf',
'& c #21214f4f7272',
'................................................',
'................................................',
'.......................#$.......................',
'......................#%%$......................',
'....................##%%%%$$....................',
'...................###%%%%$$$...................',
'..................###%%%%%%$$$..................',
'................+####%%%%%%$$$$$................',
'...............#+####%%%%%%$$$$$$...............',
'..............##+###%%%%%%%%$$$$$$..............',
'.............###+###%%%%%%%%$$$$$$$$............',
'............####+##%%%%%%%%%%$$$$$$$$...........',
'..........######+##%%%%%%%%%%$$$$$$$$$..........',
'.........#######+##%%%%%%%%%%$$$$$$$$$$$........',
'........########+#%%%%%%%%%%%%$$$$$$$$$$$.......',
'.......##+######+#%%%%%+%%%%%%$$$$$$$$$$$$......',
'.....#####+#####+%%%%%+%%%%%%%%$$$$$$$$$$$$$....',
'....#######+####+%%%%+%%%%%%%%%$$$$$$$$$$$$$$...',
'..##########+###+%%%+%%%%%%%%%%$$$$$$$$$$$$$&&..',
'...%%########+#+++%+%%%%%%%%%%%%$$$$$$$$$&&&&...',
'....%%%%%#####+++++%%%%%%%%%%%%%$$$$$$&&&&&&....',
'....%%%%%%%%%+++++++%%%%%%%%%%%%%$$&&&&&&&&&....',
'....++++++++++++++++++++++++$$$$$&&&&&&&&&&.....',
'......%%%%%%%+++++++$$$$$$$$$$$$$&&&&&&&&&......',
'.......%%%%%%%+++++$$$$$$$$$$$$$&&&&&&&&&.......',
'.......%%%%%%+%+++$+$$$$$$$$$$$$&&&&&&&&&.......',
'........%%%%+%%%+$$$+$$$$$$$$$$&&&&&&&&&........',
'.........%%+%%%%+%$$$+$$$$$$$$$&&&&&&&&.........',
'..........+%%%%%+%$$$$+$$$$$$$$&&&&&&&..........',
'.........+.%%%%%+%$$$$$+$$$$$$&&&&&&&...........',
'...........%%%%%+%%$$$$$$$$$$$&&&&&&&...........',
'............%%%%+%%$$$$$$$$$$&&&&&&&............',
'.............%%%+%%$$$$$$$$$$&&&&&&.............',
'..............%%+%%%$$$$$$$$$&&&&&..............',
'..............%%+%%%$$$$$$$$&&&&&&..............',
'...............%+%%%%$$$$$$$&&&&&...............',
'................%%%%%$$$$$$$&&&&................',
'.................%%%%$$$$$$&&&&.................',
'..................%%%%$$$$$&&&..................',
'..................%%%%$$$$&&&&..................',
'...................%%%$$$$&&&...................',
'....................%%%$$$&&....................',
'.....................%%$$&&.....................',
'.....................%%$$&&.....................',
'......................%%&&......................',
'.......................%&.......................',
'................................................',
'................................................'
%};

define load_xpm(f, win, i, s);
    XpmReadFileToPixmap(XptDefaultDisplay, win, f, i, s, null_external_ptr);
enddefine;

define make_xpm(p, win, i, s);
    XpmCreatePixmapFromData(XptDefaultDisplay, win, p, i, s, null_external_ptr);
enddefine;

define XpmConsArgList(len);
    lvars len = len fi_>> 1;
    nc_consXptArgList((), len), len
enddefine;

vars XpmDemo_shell = false;

define XpmDemo();
lvars XpmDemo_shell, rowcol, btn1;

    XtVaCreatePopupShell('xpmDemo', xtTopLevelShellWidget,
        pop_ui_control_panel, (#|
            XmN title,              'X PixMap Demo',
            XmN iconName,           'X PixMap Demo',
            XmN deleteResponse,     XmUNMAP,
            XmN allowShellResize,   true,
        |#)) -> XpmDemo_shell;

    XtVaCreateManagedWidget('', xmRowColumnWidget, XpmDemo_shell, (#|
            ;;; XmN packing, XmPACK_TIGHT,
            ;;; XmN orientation, XmVERTICAL,
        |#)) -> rowcol;

    XtRealizeWidget(XpmDemo_shell);

    lvars win = XtWindow(rowcol);

    lconstant
        pm1 = writeable initintvec(1),  ;;; default diamond
        pm2 = writeable initintvec(1),  ;;; "arm" diamond
        pmmask = writeable initintvec(1),   ;;; mask pixmap
    ;

    ;;; create the diamond icons from literal data
    ;;;
    make_xpm(diamond1_icon, win, pm1, pmmask);
    make_xpm(diamond2_icon, win, pm2, pmmask);

    XtVaCreateManagedWidget('', xmPushButtonWidget, rowcol, (#|
        XmN labelType, XmPIXMAP,
        XmN labelPixmap, pm1(1),
        XmN armPixmap, pm2(1),
        |#)) -> btn1;

    XtRealizeWidget(XpmDemo_shell);
    XtPopup(XpmDemo_shell, XtGrabNone);
enddefine;

XpmDemo();

endsection;
