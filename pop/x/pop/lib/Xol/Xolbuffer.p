/* --- Copyright University of Sussex(x)991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xol/Xolbuffer.p
 > Purpose:         Buffer & text buffer utilities
 > Author:          Jonathan Meyer, Sep (x,y)(x)990 (see revisions)
 > Documentation:   HELP *OpenLook
 */
compile_mode :pop11 +strict;

include Xolbuffer.ph;

section;

define BufferFilled(Buffer);
    lvars Buffer;
    exacc Buffer.size == exacc Buffer.used;
enddefine;

define BufferLeft(Buffer);
    lvars Buffer;
    (exacc Buffer.size - exacc Buffer.used) > 0;
enddefine;

define BufferEmpty(Buffer);
    lvars Buffer;
    exacc Buffer.used == 0;
enddefine;


XptPopLoadProcedures Xolbuffer [^^XOL_EXLIBS]
    AllocateBuffer(x,y) :exptr,
    GrowBuffer(x,y) :void,
    CopyBuffer(x) :exptr,
    FreeBuffer(x) :void,
    InsertIntoBuffer(x,y,z) :int,
    ReadFileIntoBuffer(x,y) :int,
    ReadStringIntoBuffer(x,y) :int,

    ;;; These are not acceptable as Pop identifier names, so they're
    ;;; all prefixed with Ol_ (the rest of the procedures in this file
    ;;; should be too!). JG Apr 93.
    Ol_stropen(x) :exptr                        <- stropen,
    Ol_strgetc(x) :int                          <- strgetc,
    Ol_strclose(x) :void                        <- strclose,
    Ol_streexp() :exptr.exacc_ntstring          <- streexp,
    Ol_strexp(x,y,z) :exptr.exacc_ntstring      <- strexp,
    Ol_strrexp(x,y,z) :exptr.exacc_ntstring     <- strrexp,

    ;;; text buffer
    AllocateTextBuffer(x,y,z) :exptr,
    FreeTextBuffer(x,y,z) :void,
    ReadFileIntoTextBuffer(x,y,z) :exptr,
    ReadStringIntoTextBuffer(x,y,z) :exptr,

    GetTextBufferLocation(x,y,z) :exptr.exacc_ntstring,

    ForwardScanTextBuffer(x,y,z) :int,
    BackwardScanTextBuffer(x,y,z) :int,

    ReplaceBlockInTextBuffer(u,v,w,x,y,z) :int,
    ReplaceCharInTextBuffer(v,w,x,y,z) :int,

    IncrementTextBufferLocation(w,x,y,z) :exptr,
    LocationOfPosition(x,y) :exptr,
    LineOfPosition(x,y) :int,
    PositionOfLocation(x,y) :int,
    PositionOfLine(x,y) :int,
    LastTextBufferPosition(x) :int,
    LastTextBufferLocation(x) :exptr,
    StartCurrentTextBufferWord(x,y) :exptr,
    EndCurrentTextBufferWord(x,y) :exptr,
    PreviousTextBufferWord(x,y) :exptr,
    NextTextBufferWord(x,y) :exptr,
    NextLocation(x,y) :exptr,
    PreviousLocation(x,y) :exptr,

    GetTextBufferLine(x,y) :exptr.exacc_ntstring,
    GetTextBufferChar(x,y) :int,
    CopyTextBufferBlock(w,x,y,z) :int,
    GetTextBufferBlock(x,y,z) :exptr.exacc_ntstring,
    GetTextBufferBuffer(x,y) :exptr,
    SaveTextBuffer(x,y) :int,

    RegisterTextBufferUpdate(x,y,z) :void,
    UnregisterTextBufferUpdate(x,y,z) :int,

    RegisterTextBufferScanFunctions(x,y) :void,
    RegisterTextBufferWordDefinition(x) :void,
;

constant Xolbuffer = true;

endsection;
