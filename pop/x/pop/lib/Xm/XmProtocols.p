/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/XmProtocols.p
 > Purpose:         Handling WM protocol messages
 > Author:          Jonathan Meyer, Feb  9 1991 (see revisions)
 > Documentation:   HELP *MOTIF
 */
compile_mode :pop11 +strict;

section;
exload_batch;

uses XmAtomMgr;

/* MORE SUPPORT NEEDED for this to work well */

XptPopLoadProcedures XmProtocols [^^XM_EXLIBS]
lconstant
    XmAddProtocols(w,x,y,z) :void,
    XmRemoveProtocols(w,x,y,z) :void,
    XmAddProtocolCallback(v,w,x,y,z) :void,
    XmRemoveProtocolCallback(v,w,x,y,z) :void,
    XmActivateProtocol(x,y,z) :void,
    XmDeactivateProtocol(x,y,z) :void,
    XmSetProtocolHooks(t,u,v,w,x,y,z) :void,
    _XmInstallProtocols(x) :void,
;


define :inline lconstant XM_WM_PROTOCOL_ATOM(shell);
    XmInternAtom(XtDisplay(shell),'WM_PROTOCOLS',false)
enddefine;

define XmAddWMProtocols(shell, protocols, num_protocols);
    lvars shell, protocols, num_protocols;
    XmAddProtocols(shell, XM_WM_PROTOCOL_ATOM(shell),
             protocols, num_protocols)
enddefine;

define XmRemoveWMProtocols(shell, protocols, num_protocols);
    lvars shell, protocols, num_protocols;
    XmRemoveProtocols(shell, XM_WM_PROTOCOL_ATOM(shell),
            protocols, num_protocols)
enddefine;

define XmAddWMProtocolCallback(shell, protocol, callback, closure);
    lvars shell, protocol, callback, closure;
    XmAddProtocolCallback(shell, XM_WM_PROTOCOL_ATOM(shell),
                protocol, callback, closure)
enddefine;

define XmRemoveWMProtocolCallback(shell, protocol, callback, closure);
    lvars shell, protocol, callback, closure;
    XmRemoveProtocolCallback(shell, XM_WM_PROTOCOL_ATOM(shell),
                protocol, callback, closure)
enddefine;

define XmActivateWMProtocol(shell, protocol);
    lvars shell, protocol;
    XmActivateProtocol(shell, XM_WM_PROTOCOL_ATOM(shell), protocol);
enddefine;

define XmDeactivateWMProtocol(shell, protocol);
    lvars shell, protocol;
    XmDeactivateProtocol(shell, XM_WM_PROTOCOL_ATOM(shell), protocol);
enddefine;

define XmSetWMProtocolHooks(shell, protocol, pre_h, pre_c, post_h, post_c);
    lvars shell, protocol, pre_h, pre_c, post_h, post_c;
    XmSetProtocolHooks(shell, XM_WM_PROTOCOL_ATOM(shell),
             protocol, pre_h, pre_c, post_h, post_c)
enddefine;

constant XmProtocols = true;

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 16 1993
        Fixed up, moved XmCR_WM_PROTOCOLS to XmConstants.ph
--- Andreas Schoter, Jul 15 1991
    Added global constant XmProtocols for compatibility with uses
 */
