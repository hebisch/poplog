/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/include/Xolbuffer.ph
 > Purpose:         Buffer & text buffer defs
 > Author:          Jonathan Meyer, Sep  2 1990 (see revisions)
 > Documentation:   HELP *OpenLook
 */

#_TERMIN_IF DEF XOLBUFFER_INCLUDED

include XolConstants.ph;

section;

;;; typedef char BufferElement;

i_typespec
    Buffer {
        size: int,
        used: int,
        esize: int,
        p: exptr,
    };

/*#define Bufferof(type) \
   struct \
      { \
      int    size; \
      int    used; \
      int    esize; \
      type * p; \
*/

iconstant macro (
        LNMIN   = 8,
        LNINCRE = 24,
);

iconstant macro (
        FTINCRE        = 10,
        LTINCRE        = 50,
        PTINCRE        = 10,
        BTINCRE        =  5,
        MAXLPP         = 50,
        BLOCKSIZE      = 4096,
        PQLIMIT        = 10,

        NOTOPEN = 1, READWRITE = 2, READONLY = 3 , NEWFILE = 4,
        ;;; TextFileStatus;

        EDIT_FAILURE = 1, EDIT_SUCCESS = 2,
        ;;; EditResult;

        SCAN_NOTFOUND = 1, SCAN_WRAPPED = 2, SCAN_FOUND = 3, SCAN_INVALID = 4,
        ;;; ScanResult;

        SAVE_FAILURE = 1, SAVE_SUCCESS = 2,
        ;;; SaveResult;

        TEXT_BUFFER_NOP                = 0,
        TEXT_BUFFER_DELETE_START_LINE  = 2:1e0,
        TEXT_BUFFER_DELETE_START_CHARS = 2:1e1,
        TEXT_BUFFER_DELETE_END_LINE    = 2:1e2,
        TEXT_BUFFER_DELETE_END_CHARS   = 2:1e3,
        TEXT_BUFFER_DELETE_JOIN_LINE   = 2:1e4,
        TEXT_BUFFER_DELETE_SIMPLE      = 2:1e5,
        TEXT_BUFFER_INSERT_SPLIT_LINE  = 2:1e6,
        TEXT_BUFFER_INSERT_LINE        = 2:1e7,
        TEXT_BUFFER_INSERT_CHARS       = 2:1e8,

);

i_typespec
    TextBlock: int,
    TextPage: int,
    PageQueue {
        pageindex: int,
        timestamp: ulong,
    },

    BlockTable {
        size: int,
        used: int,
        esize: int,
        p: exptr.:TextBlock,
    },

   Page {
       bytes: int,
       lines: int,
       qpos: int,
       dpos: exptr.:BlockTable[], ;;;BlockTable *  dpos;
   },

   Line {
        pageindex: int,
        buffer: exptr,
        userData: ulong,
    },

    PageTable {
        size: int,
        used: int,
        esize: int,
        p: exptr.:Page[]
    },

    LineTable {
        size: int,
        used: int,
        esize: int,
        p: exptr.:Line[],
    },

    TextLocation {
        line: int,
        offset: int,
        buffer: exptr, ;;; char *
    },

;;;typedef int TextUndoHint;

    TextUndoItem {
        string: ntstring_ptr,
        start: exptr.:TextLocation,
        end: exptr.:TextLocation,
        hint: int, ;;; TextUndoHint
    },
    TextUpdateCallback {
       f: exptr.XptImportProcedure,
       d: exptr,
    },

    TextBuffer {
        filename: ntstring_ptr,
        tempfile: exptr,
        blockcnt: TextBlock,
        blocksize: TextBlock,
        lines: LineTable,
        pages: PageTable,
        free_list: exptr.:BlockTable[],
        pqueue: exptr.:PageQueue[],
        pagecount: TextPage,
        pageref: TextPage,
        curpageno: TextPage,
        buffer: exptr,
        dirty: byte,
        status: int, ;;; TextFileStatus
        refcount: int,
        update: exptr.:TextUpdateCallback,
        delete: TextUndoItem,
        insert: TextUndoItem,
   },
;

/* `executable' macros - not implemented yet

#define TextBufferUserData(text,line)  text-> lines.p[line].userData
#define TextBufferName(text)           (text-> filename)
#define TextBufferModified(text)       (text-> dirty)
#define TextBufferEmpty(text)          (text-> lines.used == 1 && \
                                        text-> lines.p[0].buffer-> used == 1)
#define TextBufferNamed(text)          (text-> filename != NULL)
#define LinesInTextBuffer(text)        (text-> lines.used)
#define LastTextBufferLine(text)       (text-> lines.used - 1)
#define LastCharacterInTextBufferLine(text, line)              \
   (text-> lines.p[line].buffer-> used - 1)
#define LengthOfTextBufferLine(text, line) (text-> lines.p[line].buffer-> used)

#define SameTextLocation(x,y)         (x.line == y.line && x.offset == y.offset)
*/

iconstant XOLBUFFER_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- Andreas Schoter, Jul 15 1991
    Sectionized and added global constant XolArrow for compatibility with uses
--- Jonathan Meyer, Nov 30 1990
    Commented out #defines.
 */
