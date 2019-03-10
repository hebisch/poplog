/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/getstore_fixed.p
 > Purpose:
 > Author:          John Gibson, Jan 29 1990 (see revisions)
 > Documentation:   REF *EXTERNAL_DATA
 */

;;; ----------------- FIXED-ADDRESS STRUCTURES ---------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'memseg.ph'
#_INCLUDE 'gctypes.ph'

constant
        procedure (sys_encode_string, is_fixed)
    ;

section $-Sys;

constant
        procedure (Check_rorvkey, Check_vkey, Open_seg_expand_for,
        Try_new_fixed_seg, Extern$-Get_encoding_funcs)
    ;

vars
        max_mem_lim
    ;

endsection;

weak constant
        procedure (Sys$-Copy_exptrmem), exptr_mem_key
    ;

weak vars
        Sys$-exptr_mem_fixed_hold_list
    ;


;;; ----------------------------------------------------------------------

section $-Sys => cons_fixed, init_fixed, copy_fixed, sys_encode_string_fixed,
                 free_fixed_hold;

vars
    ;;; Total amount of memory (words) tied up in free block table
    free_block_total_mem    = 0,

    ;;; Set by Sysgarbage
    _Lgc_allowed_fxd_alloc  = _0,   ;;; words
    ;


deftype
    fb      = (struct FREE_BLOCK_HEADER),
    node    = (struct FREE_BLOCK_HEADER);   ;;; actually, (struct NODE)


    /*  Node structure in a 2-3 tree
    */
struct NODE
  { (word)  FREEBLK_SIZE_NEXT;  ;;; }
    full    KEY;                ;;; } as FREE_BLOCK_HEADER (memseg.ph)
>-> int     FREEBLK_SIZE;       ;;; }
    int     NODE_OTHER_SIZE;    ;;; size of second block in node (0 if none)
    (word)  FREEBLK_SIZE_LAST;  ;;; last block in this size's chain
    fb      NODE_OTHER_BLK;     ;;; pointer to other block if has one
    node    NODE_LEFT,          ;;; left child
            NODE_CENTRE,        ;;; centre child (if has other block)
            NODE_RIGHT,         ;;; right child
            NODE_SUPER;         ;;; parent (simple ptr into index if root)
  };

    /*  Keys for free blocks in fixed-address structure segments.
        First is for 2-word blocks, second for any other size
    */
define lconstant Fb_hash(_fb);
    lvars _fb;
    _pint(fast_apply(_fb, _fb!KEY!K_GET_SIZE))
enddefine;

lconstant
    fb_K_APPLY  = consref(Exec_nonpd),
    fb_K_EQUALS = consref(nonop ==),
    fb_K_PRINT  = consref(Minimal_print),
    fb_K_HASH   = consref(Fb_hash),

    free_block2_key = struct KEY =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL _biset _:M_K_NO_FULL_FROM_PTR,
                                ;;; K_FLAGS
        _:GCTYPE_FREE_BLOCK,    ;;; K_GC_TYPE
        Rec1_getsize,           ;;; K_GET_SIZE

        "FREE_BLOCK",           ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        false,                  ;;; K_RECOGNISER
        fb_K_APPLY,             ;;; K_APPLY
        nonop ==,               ;;; K_SYS_EQUALS
        fb_K_EQUALS,            ;;; K_EQUALS
        Minimal_print,          ;;; K_SYS_PRINT
        fb_K_PRINT,             ;;; K_PRINT
        fb_K_HASH,              ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE
        %}
    ;

constant
    free_block_key = struct KEY =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL _biset _:M_K_NO_FULL_FROM_PTR,
                                ;;; K_FLAGS
        _:GCTYPE_FREE_BLOCK,    ;;; K_GC_TYPE
        procedure(); ()!FREEBLK_SIZE endprocedure,
                                ;;; K_GET_SIZE

        "FREE_BLOCK",           ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        false,                  ;;; K_RECOGNISER
        fb_K_APPLY,             ;;; K_APPLY
        nonop ==,               ;;; K_SYS_EQUALS
        fb_K_EQUALS,            ;;; K_EQUALS
        Minimal_print,          ;;; K_SYS_PRINT
        fb_K_PRINT,             ;;; K_PRINT
        fb_K_HASH,              ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE
        %};



    /*  Tables of individual free block lists and search trees
    */
define lconstant newtable(_nbytes);
    lvars _nbytes;
    (writeable inits(_pint(_nbytes)))@V_WORDS
enddefine;

lconstant
    ;;; individual lists -- start of list and last for each
    _free_block_lists      = newtable(##(b)[_1|struct NODE]),
    _free_block_list_lasts = newtable(##(b)[_1|struct NODE]),

    ;;; trees
    macro _NTREES       = _6,
    ;;; this includes popint(0) entry at end
    _free_block_trees   = newtable(##(b)[_NTREES _add _1|node]),
    ;

define lconstant Tree_addr(_size);
    lvars _size;

    define :inline lconstant _TREEADDR(n);
        _free_block_trees@(node)[_int(n)]
    enddefine;

    if     _size _lt @@(w)[_2:1e5] then
        _TREEADDR(0)
    elseif _size _lt @@(w)[_2:1e7] then
        _TREEADDR(1)
    elseif _size _lt @@(w)[_2:1e10] then
        _TREEADDR(2)
    elseif _size _lt @@(w)[_2:1e14] then
        _TREEADDR(3)
    elseif _size _lt @@(w)[_2:1e19] then
        _TREEADDR(4)
    else
        _TREEADDR(5)
    endif
enddefine;

    /*  Clear all free block lists and trees
    */
define Clear_free_blocks();
    lvars _tab, _lim;
    0 -> free_block_total_mem;

    _free_block_lists -> _tab, _tab@(fb){@@(struct NODE)++} -> _lim;
    while _tab <@(fb) _lim do
        _NULL -> _tab!(fb)++ -> _tab
    endwhile;

    _free_block_trees -> _tab, _tab@(node)[_NTREES] -> _lim;
    while _tab <@(node) _lim do
        _NULL -> _tab!(node)++ -> _tab
    endwhile;
    ;;; dummy simple entry at end
    0 -> _tab!(node)
enddefine;


;;; --- ROUTINES FOR STORING AND ALLOCATING FREE BLOCKS ---------------------

    /*  Replace _oldnode with _newnode in the parent of _oldnode
    */
define lconstant Trans_super(_oldnode, _newnode);
    lvars _super, _newnode, _oldnode;
    returnif(_oldnode == _newnode);
    _oldnode!NODE_SUPER ->> _super -> _newnode!NODE_SUPER;
    if issimple(_super) then
        _newnode -> _mkcompound(_super)!(w)
    elseif _super!NODE_LEFT == _oldnode then
        _newnode -> _super!NODE_LEFT
    elseif _super!NODE_RIGHT == _oldnode then
        _newnode -> _super!NODE_RIGHT
    else
        _newnode -> _super!NODE_CENTRE
    endif
enddefine;

    /*  Make a 2-link (i.e. 1-block) node
    */
define lconstant Make_2link(_node, _left, _right);
    lvars _node, _left, _right;
    _left   -> _node!NODE_LEFT;
    _right  -> _node!NODE_RIGHT;
    unless _left == _NULL then
        _node ->> _left!NODE_SUPER -> _right!NODE_SUPER
    endunless;
    _0 -> _node!NODE_OTHER_SIZE
enddefine;

    /*  Make a 3-link (i.e. 2-block) node
    */
define lconstant Make_3link(_node, _left, _centre, _right, _other);
    lvars _node, _left, _centre, _right, _other;
    _left   -> _node!NODE_LEFT;
    _centre -> _node!NODE_CENTRE;
    _right  -> _node!NODE_RIGHT;
    unless _left == _NULL then
        _node ->> _left!NODE_SUPER ->> _centre!NODE_SUPER
                                        -> _right!NODE_SUPER
    endunless;
    _other              -> _node!NODE_OTHER_BLK;
    _other!FREEBLK_SIZE -> _node!NODE_OTHER_SIZE
enddefine;

    /*  Insert a block of memory of _size into the free blocks table
    */
define Make_free_block(_blk, _size);
    lvars   size1, size2, _node, _next, _size, _blk;

    free_block_total_mem fi_+ _pint(##(w){_size}) -> free_block_total_mem;

    _NULL -> _blk!FREEBLK_SIZE_NEXT;
    if _size _lt @@(struct NODE)++ then
        ;;; too small for tree, put in individual list
        if _size == @@(w)[_2] then
            free_block2_key -> _blk!KEY
        else
            free_block_key -> _blk!KEY;
            _size -> _blk!FREEBLK_SIZE
        endif;
        _free_block_list_lasts@(fb){_size} -> _next;
        if _free_block_lists!(fb){_size} == _NULL then
            _blk -> _free_block_lists!(fb){_size}
        else
            _blk -> _next!(fb)!FREEBLK_SIZE_NEXT
        endif;
        _blk -> _next!(fb);
        return
    endif;

    ;;; else insert in appropriate tree
    free_block_key -> _blk!KEY;
    _size -> _blk!FREEBLK_SIZE;
    Tree_addr(_size) -> _next;
    if (_next!(node) ->> _node) == _NULL then
        ;;; empty tree -- make root node
        _blk -> _blk!FREEBLK_SIZE_LAST;
        Make_2link(_blk, _NULL, _NULL);
        _blk -> _next!(w);
        _mksimple(_next) -> _blk!NODE_SUPER;
        return
    endif;

    ;;; else insert in tree with root _node
    repeat
        _node!FREEBLK_SIZE -> size1;
        if _size _lt size1 then
            _node!NODE_LEFT -> _next
        elseif _size == size1 then
            ;;; insert in this block's chain
            _blk ->> _node!FREEBLK_SIZE_LAST!FREEBLK_SIZE_NEXT
                                        -> _node!FREEBLK_SIZE_LAST;
            return
        elseif (_node!NODE_OTHER_SIZE ->> size2) _lt _size then
            _node!NODE_RIGHT -> _next
        elseif _size == size2 then
            ;;; insert in other block's chain
            _node!NODE_OTHER_BLK -> _node;
            _blk ->> _node!FREEBLK_SIZE_LAST!FREEBLK_SIZE_NEXT
                                        -> _node!FREEBLK_SIZE_LAST;
            return
        else
            _node!NODE_CENTRE -> _next
        endif;
        quitif(_next == _NULL);
        _next -> _node
    endrepeat;

    ;;; no block of _size already in tree -- insert new

    define lconstant New(_node, _blk);
        lvars   _node, _left, _centre, _right, _link = _node,
                _lnew = _NULL, _rnew = _NULL, _blk, _other;
    LOOP:
        _node!NODE_LEFT     -> _left;
        _node!NODE_CENTRE   -> _centre;
        _node!NODE_RIGHT    -> _right;
        if _nonzero(_node!NODE_OTHER_SIZE) then
            ;;; 2-block node -- split into 2 ones and pass centre up
            _node!NODE_OTHER_BLK -> _other;
            if _link == _left then
                Make_2link(_blk, _lnew, _rnew);
                Make_2link(_other, _centre, _right);
                _blk -> _lnew, _other -> _rnew, _node -> _blk
            elseif _link == _centre then
                Make_2link(_node, _left, _lnew);
                Make_2link(_other, _rnew, _right);
                _node -> _lnew, _other -> _rnew
            else
                Make_2link(_node, _left, _centre);
                Make_2link(_blk, _lnew, _rnew);
                _node -> _lnew, _blk -> _rnew, _other -> _blk
            endif;
            _node -> _link;
            if issimple(_node!NODE_SUPER ->> _node) then
                ;;; was root -- insert new root to heighten tree by 1
                Trans_super(_link, _blk);
                Make_2link(_blk, _lnew, _rnew)
            else
                ;;; continue up tree
                goto LOOP
            endif
        else
            ;;; 1-block node -- make 2-block
            if _link == _left then
                ;;; insert to left
                Trans_super(_node, _blk);
                Make_3link(_blk, _lnew, _rnew, _right, _node)
            else
                ;;; insert to right
                Make_3link(_node, _left, _lnew, _rnew, _blk)
            endif
        endif
    enddefine;

    ;;; dummy to establish where to insert in first node
    _blk -> _blk!FREEBLK_SIZE_LAST;
    _node -> if _size _lt size1 then
                _node!NODE_LEFT
             elseif size2 _lt _size then
                _node!NODE_RIGHT
             else
                _node!NODE_CENTRE
             endif;
    chain(_node, _blk, New)
enddefine;      /* Make_free_block */

    /*  Delete the terminal link _link in _node
    */
define lconstant Delete_terminal_link(_node, _link);
    lvars   _node, _left, _centre, _right, _other, _link,
            _newlink = _NULL;

LOOP:
    ;;; pull down node on link _link
    _node!NODE_LEFT     -> _left;
    _node!NODE_CENTRE   -> _centre;
    _node!NODE_RIGHT    -> _right;
    if _zero(_node!NODE_OTHER_SIZE) then
        ;;; 1-block node
        if _link == _left then
            ;;; left linked
            if _zero(_right!NODE_OTHER_SIZE) then
                ;;; 1-block node to right -- coalesce and continue up tree
                Make_3link(_node, _newlink, _right!NODE_LEFT, _right!NODE_RIGHT,
                                                                    _right);
                _node -> _newlink
            else
                ;;; 2-block node to right -- rotate anticlockwise
                _right!NODE_OTHER_BLK -> _other;
                Trans_super(_node, _right);
                Make_2link(_node, _newlink, _right!NODE_LEFT);
                Make_2link(_other, _right!NODE_CENTRE, _right!NODE_RIGHT);
                Make_2link(_right, _node, _other);
                return
            endif
        else
            ;;; right linked
            if _zero(_left!NODE_OTHER_SIZE) then
                ;;; 1-block node to left -- coalesce and continue up tree
                Make_3link(_left, _left!NODE_LEFT, _left!NODE_RIGHT, _newlink,
                                                                    _node);
                _left -> _newlink
            else
                ;;; 2-block node to left -- rotate clockwise
                _left!NODE_OTHER_BLK -> _other;
                Trans_super(_node, _other);
                Make_2link(_node, _left!NODE_RIGHT, _newlink);
                Make_2link(_left, _left!NODE_LEFT, _left!NODE_CENTRE);
                Make_2link(_other, _left, _node);
                return
            endif
        endif;
        _node -> _link;
        if issimple(_node!NODE_SUPER ->> _node) then
            ;;; reached root
            Trans_super(_link, _newlink)
        else
            goto LOOP   ;;; continue up tree
        endif
    else
        ;;; 2-block node
        _node!NODE_OTHER_BLK -> _other;
        if _link == _centre then
            ;;; centre linked
            if _zero(_left!NODE_OTHER_SIZE) then
                ;;; 1-block node to left -- swing down to left
                Trans_super(_node, _other);
                Make_3link(_left, _left!NODE_LEFT, _left!NODE_RIGHT, _newlink,
                                                                _node);
                Make_2link(_other, _left, _right)
            elseif _zero(_right!NODE_OTHER_SIZE) then
                ;;; 1-block node to right -- swing down to right
                Make_3link(_other, _newlink, _right!NODE_LEFT, _right!NODE_RIGHT,
                                                                _right);
                Make_2link(_node, _left, _other)
            else
                ;;; 2-block nodes on both sides
                ;;; --  rotate anticlockwise from right
                _other -> _centre;
                _right!NODE_OTHER_BLK -> _other;
                Make_2link(_centre, _newlink, _right!NODE_LEFT);
                Make_2link(_other, _right!NODE_CENTRE, _right!NODE_RIGHT);
                Make_3link(_node, _left, _centre, _other, _right)
            endif
        else
            ;;; left or right linked
            if _zero(_centre!NODE_OTHER_SIZE) then
                ;;; 1-block node in centre
                if _link == _left then
                    ;;; swing down to centre left
                    Trans_super(_node, _other);
                    Make_3link(_node, _newlink, _centre!NODE_LEFT,
                                                    _centre!NODE_RIGHT, _centre);
                    Make_2link(_other, _node, _right)
                else
                    ;;; swing down to centre right
                    Make_3link(_centre, _centre!NODE_LEFT, _centre!NODE_RIGHT,
                                                    _newlink, _other);
                    Make_2link(_node, _left, _centre)
                endif
            else
                ;;; 2-block node in centre
                if _link == _left then
                    ;;; rotate anticlockwise from centre to left
                    _centre!NODE_OTHER_BLK -> _left;
                    Trans_super(_node, _centre);
                    Make_2link(_node, _newlink, _centre!NODE_LEFT);
                    Make_2link(_left, _centre!NODE_CENTRE, _centre!NODE_RIGHT);
                    Make_3link(_centre, _node, _left, _right, _other)
                else
                    ;;; rotate clockwise from centre to right
                    _centre!NODE_OTHER_BLK -> _right;
                    Make_2link(_other, _centre!NODE_RIGHT, _newlink);
                    Make_2link(_centre, _centre!NODE_LEFT, _centre!NODE_CENTRE);
                    Make_3link(_node, _left, _centre, _other, _right)
                endif
            endif
        endif
    endif
enddefine;      /* Delete_terminal_link */

    /*  Replace _blk in _node with _newblk
    */
define lconstant Replace_block(_node, _blk, _newblk);
    lvars _node, _newblk, _blk;
    if _blk == _node then
        ;;; replace _node with _newblk
        if _zero(_node!NODE_OTHER_SIZE) then
            Make_2link(_newblk, _node!NODE_LEFT, _node!NODE_RIGHT)
        else
            Make_3link(_newblk, _node!NODE_LEFT, _node!NODE_CENTRE,
                                _node!NODE_RIGHT, _node!NODE_OTHER_BLK)
        endif;
        Trans_super(_node, _newblk)
    else
        ;;; replace _node other with _newblk
        _newblk                 -> _node!NODE_OTHER_BLK;
        _newblk!FREEBLK_SIZE    -> _node!NODE_OTHER_SIZE
    endif
enddefine;      /* Replace_block */

    /*  Delete _blk from _node
    */
define lconstant Delete_block(_node, _blk);
    lvars super, _n, _node, _blk, _other, _nother;
    if _node!NODE_LEFT == _NULL then
        ;;; terminal node
        if _zero(_node!NODE_OTHER_SIZE) then
            ;;; delete 1-block node (_blk == _node)
            if issimple(_node!NODE_SUPER ->> super) then
                ;;; deleting tree with only a root node
                _NULL -> _mkcompound(super)!(w)     ;;; _NULL table entry
            else
                Delete_terminal_link(super, _node)
            endif
        elseif _blk == _node then
            ;;; replace _node with other block
            _node!NODE_OTHER_BLK -> _other;
            Trans_super(_node, _other);
            Make_2link(_other, _NULL, _NULL)
        else
            ;;; just delete other block
            _0 -> _node!NODE_OTHER_SIZE
        endif
    else
        ;;; non-terminal node -- we switch this with its predecessor (the
        ;;; rightmost terminal block to its left), and then delete it.
        if _blk == _node then _node!NODE_LEFT else _node!NODE_CENTRE endif -> _n;
        until _n!NODE_RIGHT == _NULL do _n!NODE_RIGHT -> _n enduntil;
        if _zero(_n!NODE_OTHER_SIZE) then
            ;;; _n is 1-block
            if (_n!NODE_SUPER ->> super) == _blk then
                ;;; ! after Replace_block, _n will point to itself !
                _n -> super
            endif;
            Replace_block(_node, _blk, _n);
            Delete_terminal_link(super, _n)
        else
            ;;; _n is 2-block
            Replace_block(_node, _blk, _n!NODE_OTHER_BLK);
            _0 -> _n!NODE_OTHER_SIZE
        endif
    endif
enddefine;      /* Delete_block */


    /*  Search the current free blocks to fit request _size
    */
define lconstant Find_free_block(_size);
    lvars   size1, size2, _node, _next, _srchsize = _size, _last_gr, _blk,
            _tab, _size;

    free_block_total_mem fi_- _pint(##(w){_size}) -> free_block_total_mem;

    if _srchsize _lt @@(struct NODE)++ then
        ;;; too small for tree, use individual lists
        _free_block_lists -> _tab;
        if (_tab!(fb){_srchsize} ->> _blk) /== _NULL then
            _blk!FREEBLK_SIZE_NEXT -> _tab!(fb){_srchsize};
            return(_blk, true)
        endif;
        ;;; look for _size +2 words
        _srchsize _add @@(w)[_2] -> _srchsize;
        while _srchsize _lt @@(struct NODE)++ do
            if (_tab!(fb){_srchsize} ->> _blk) /== _NULL then
                _blk!FREEBLK_SIZE_NEXT -> _tab!(fb){_srchsize};
                Make_free_block(_blk@(w){_size}, _srchsize _sub _size);
                return(_blk, true)
            endif;
            @@(w){_srchsize}++ -> _srchsize
        endwhile;
        _free_block_trees@(node)[_0] -> _tab
    else
        Tree_addr(_srchsize) -> _tab
    endif;

    ;;; search in trees -- table has popint 0 entry at end

    repeat
        ;;; get next tree
        while (_tab!(w)++ -> _tab ->> _node) == _NULL do endwhile;
        if issimple(_node) then
            ;;; no block of _size or greater than _size+1
            free_block_total_mem fi_+ _pint(##(w){_size})
                                            -> free_block_total_mem;
            return(_size, false)
        endif;

        ;;; search tree or subtree starting from _node
        repeat
            _NULL -> _last_gr;
            repeat
                _node!FREEBLK_SIZE -> size1;
                if _srchsize _lt size1 then
                    _node -> _last_gr;
                    _node!NODE_LEFT -> _next
                elseif _srchsize == size1 then
                    _node -> _blk;
                    quitloop(3)
                elseif (_node!NODE_OTHER_SIZE ->> size2) _lt _srchsize then
                    _node!NODE_RIGHT -> _next
                elseif _srchsize == size2 then
                    _node!NODE_OTHER_BLK -> _blk;
                    quitloop(3)
                else
                    _node -> _last_gr;
                    _node!NODE_CENTRE -> _next
                endif;
                quitif(_next == _NULL);
                _next -> _node
            endrepeat;

            ;;; no block of _srchsize found
            ;;; if no blocks of greater size, loop for next tree
            nextif(_last_gr == _NULL) (2);

            _last_gr -> _node;
            if _srchsize _lt _node!FREEBLK_SIZE then
                _node
            else
                _node!NODE_OTHER_BLK
            endif -> _blk;

            ;;; finished if already looking for _size +2 words
            quitif(_srchsize /== _size) (2);

            ;;; look for _size +2 words
            _srchsize _add @@(w)[_2] -> _srchsize;
            ;;; restart search from last greater node, or if that is
            ;;; exactly _size+1 (too small), from next higher up tree
            if _blk!FREEBLK_SIZE _lt _srchsize
            and _node!NODE_OTHER_SIZE _lt _srchsize then
                while iscompound(_node!NODE_SUPER ->> _blk) do
                    if _blk!NODE_RIGHT == _node then
                        _blk -> _node
                    else
                        _blk -> _node;
                        quitloop
                    endif
                endwhile
            endif
        endrepeat
    endrepeat;

    ;;; found block big enough (_blk in _node)
    if (_blk!FREEBLK_SIZE_NEXT ->> _next) /== _NULL then
        _blk!FREEBLK_SIZE_LAST -> _next!FREEBLK_SIZE_LAST;
        Replace_block(_node, _blk, _next)
    else
        Delete_block(_node, _blk)       ;;; delete it from tree
    endif;
    if _nonzero(_blk!FREEBLK_SIZE _sub _size ->> size1) then
        Make_free_block(_blk@(w){_size}, size1)
    endif;
    _blk, true
enddefine;      /* Find_free_block */


;;; --- ALLOCATING MEMORY TO FIXED-ADDRESS SEGMENTS ------------------------

define lconstant Alloc_store_fixed(_size);
    lvars   _size, _allowed, _seg, _rsize, _gapsize,
            _minsize, _maxsize, _lim, _used, _ofs_round;

    define lconstant Find_reserve(_size);
        lvars _seg = _lowest_heap_seg--@(struct SEG), _ptr, _lim, _size;
        while (_seg@(struct SEG)++ ->> _seg)
                                <@(struct SEG) _seg_table_next_free do
            nextunless(_seg!SEG_FLAGS _bitst _M_SEG_FIXED_STRUCTS);
            _seg!SEG_FREE_PTR@(w){_size} -> _ptr;
            _seg!SEG_BASE_PTR@(w){_seg!SEG_SIZE} -> _lim;
            nextif(_ptr >@(w) _lim);
            ;;; space here
            _Lgc_allowed_fxd_alloc _sub ##(w){_size} -> _Lgc_allowed_fxd_alloc;
            _seg!SEG_FREE_PTR;      ;;; return this
            _ptr -> _seg!SEG_FREE_PTR;
            return(true)
        endwhile;
        false
    enddefine;

    @@(w)[_Lgc_allowed_fxd_alloc] -> _allowed;
    if _allowed _sgreq _size then
        ;;; not over allowed increment, use any reserved space
        ;;; available
        returnif(Find_reserve(_size))
    endif;

    ;;; else must garbage collect -- compute size of reserve to create
    ;;; by shifting the open seg when the gc is done.
    @@(w)[ _negate(Open_seg_expand_for(_0)) _add _int(max_mem_lim) ]
            _sub @@(w){_userhi@~POPBASE, _open_seg_base_ptr} -> _maxsize;
    _size -> _minsize;
    _0 ->> _used -> _ofs_round;
    _seg_table_next_free -> _lim;
    _seg_table--@(struct SEG) -> _seg;
    while (_seg@(struct SEG)++ ->> _seg) <@(struct SEG) _lim do
        nextif(_seg!SEG_FLAGS _bitst _M_SEG_NON_POP
                            or _seg!SEG_BASE_PTR <@(w) _system_end);
        _maxsize _sub _seg!SEG_SIZE -> _maxsize;
        nextunless(_seg >=@(struct SEG) _lowest_heap_seg
                            and _seg!SEG_FLAGS _bitst _M_SEG_FIXED_STRUCTS);
        (@@(w){_seg!SEG_FREE_PTR, _seg!SEG_BASE_PTR} ->> _rsize)    ;;; used
                                                _add _used -> _used;
        if _seg@(struct SEG)++ == _lim then
            ;;; the 'open' fixed seg (under the open seg) -- any
            ;;; reserved pages will be absorbed into the shift
            _maxsize _add _seg!SEG_SIZE _sub _rsize -> _maxsize;
            @@(vpage){_rsize|w.r} _sub _rsize -> _ofs_round;    ;;; used below
            quitloop
        endif;
        _seg!SEG_SIZE _sub _rsize -> _rsize;    ;;; reserve
        _allowed _sub _rsize -> _allowed;
        if _rsize _sgreq _size then _0 -> _minsize endif
    endwhile;

    lconstant macro _MINSIZE = @@(w)[_2:1e11];
    if _used _slt _MINSIZE and _minsize _slt _MINSIZE then
        _MINSIZE -> _minsize
    endif;
    if _minsize _sgr _allowed then _minsize -> _allowed endif;
    if _maxsize _slt _allowed then _maxsize -> _allowed endif;
    if _neg(_allowed _sub _ofs_round ->> _gapsize) then _0 -> _gapsize endif;

    Sysgarbage(true, 'fixd', _pint(@@(vpage){_gapsize|w.r}));
    if _zero(_used) then ##(w){_minsize} -> _Lgc_allowed_fxd_alloc endif;

    ;;; re-search trees (returns block and true if sucessful,
    ;;; _size and false if not)
    returnif(Find_free_block(_size)) -> ;   ;;; erase _size if fails
    ;;; re-search reserve
    returnif(Find_reserve(_size));      ;;; returns block if successful

    ;;; can't fit size
    if _allowed _slt _size then
        ;;; _allowed wasn't enough (because of popmemlim)
        mishap(0, 'rom: MEMORY LIMIT (popmemlim) EXCEEDED (using fixed space)',
                                        'heap-fixed:mem-limit')
    else
        ;;; was enough, but couldn't make gap (reasons, in order of
        ;;; seriousness: (1) insufficient memory; (2) insufficient memory
        ;;; to use the copying garbage collector to shift up; (3) the open
        ;;; seg is blocked by external memory).
        if _size _sgr _allowed then _size -> _allowed endif;
        Try_new_fixed_seg(@@(vpage){_allowed|w.r}, @@(vpage){_size|w.r});
        unless Find_reserve(_size) then     ;;; returns block if successful
            mishap(0, 'rom: NO MORE MEMORY AVAILABLE (needing fixed space)',
                                        'heap-fixed:mem-nomore')
        endunless
    endif
enddefine;      /* Alloc_store_fixed */


    /*  Get a fixed-address block of size _size
    */
define Get_store_fixed(/* _size */) with_nargs 1;
    ;;; immediately restore _curr_heap_seg values set to fixed ones
    ;;; by fixed constructor procedures below.
    Set_curr_heap_seg(_saved_curr_heap_seg);

    ;;; returns block and true if succeeds, _size and false if not
    unless Find_free_block() then
        chain((), Alloc_store_fixed)
    endunless
enddefine;      /* Get_store_fixed */


;;; --- USER INTERFACE ---------------------------------------------------

    /*  List for holding on to fixed-address structures
    */
lvars
    fixed_hold_list     = [],
;

lconstant
    dummy_seg = struct  {   word    RAW_SIZE;
                            full    KEY;
                        >-> struct SEG
                                    SEGENTRY;
                        }
                    =>> {%  @@(struct POPREC1)++ _add @@(struct SEG)++,
                            rawstruct_key,
                            =>> {% _NULL, _NULL, _0,
                                _M_SEG_SPECIAL _biset _M_SEG_FIXED_STRUCTS %}
                        %},

    macro SET_FIXED_ALLOC = [
        ;;; assignments to cause Get_store to hand over to Get_store_fixed
        ;;; (which then immediately restores the saved seg -- mishap will
        ;;; also clear these assignments).
        _curr_heap_seg -> _saved_curr_heap_seg;
        dummy_seg@SEGENTRY -> _curr_heap_seg;
        _NULL -> _curr_seg_free_ptr;
        _NULL -> _curr_seg_free_lim;
        ],

    macro ADD_HOLD = [
        if _hold then
            conspair(dup(), fixed_hold_list) -> fixed_hold_list
        endif
        ],
    ;

define cons_fixed(/* cons args, */ _key);
    lvars _key, _hold = false;

    define lconstant Conspair() -> _pair with_props conspair;
        lvars _pair;
        Get_store_fixed(@@(struct PAIR)++) -> _pair;
        pair_key -> _pair!KEY;
        -> _pair!P_BACK -> _pair!P_FRONT
    enddefine;

    if isboolean(_key) then _key -> _hold -> _key endif;
    Check_rorvkey(_key);
    SET_FIXED_ALLOC;
    if _key!K_FLAGS _bitst _:M_K_RECORD then
        if _key == pair_key then
            ;;; can't call conspair because of the free pair list, so
            ;;; just use a special procedure
            Conspair(())
        else
            fast_apply((), _key!K_CONS_R)
        endif
    else
        fast_apply((), _key!K_CONS_V)
    endif;
    ADD_HOLD
enddefine;

define init_fixed(/* init arg, */ _key) with_nargs 2;
    lvars _key, _hold = false;
    if isboolean(_key) then _key -> _hold -> _key endif;
    Check_vkey(_key);
    SET_FIXED_ALLOC;
    fast_apply((), _key!K_INIT_V);
    ADD_HOLD
enddefine;

define copy_fixed(item);
    lvars item, _hold = false;
    if isboolean(item) then ((), item) -> (item, _hold) endif;
    SET_FIXED_ALLOC;
    if iscompound(item) and item!KEY == weakref exptr_mem_key then
        weakref[exptr_mem_key] Copy_exptrmem(item, _hold)
    else
        copy(item);
        ADD_HOLD
    endif;
enddefine;

    /*  N.B. The hold arg for this must be true if supplied (since
        false is interpreted as no encoding)
    */
define sys_encode_string_fixed(string);
    lvars string, encoding_name, new, _hold = false;
    if string == true then (), string -> (string, _hold) endif;
    if isword(string) then
        ;;; optional encoding name
        (), string -> (string, encoding_name);
        ;;; must load this before SET_FIXED_ALLOC in case it allocs structures
        Extern$-Get_encoding_funcs(encoding_name) -> ;
        (string, encoding_name)
    elseunless string then
        ;;; false for encoding
        (() ->> string, false)
    else
        string
    endif;
    SET_FIXED_ALLOC;
    if (sys_encode_string() ->> new) == string then
        ;;; no alloc done
        if is_fixed(string) then
            Set_curr_heap_seg(_saved_curr_heap_seg);
            return(string)
        endif;
        copy(string) -> new
    endif;
    new;
    ADD_HOLD
enddefine;

define General_fixed_alloc(/* args, */ p);
    lvars procedure p, _hold = false;
    if isboolean(dup()) then () -> _hold endif;
    SET_FIXED_ALLOC;
    chain(/* args */, _hold, p)
enddefine;


    /*  Remove an item from fixed_hold_list or exptr_mem_fixed_hold_list
    */
define free_fixed_hold(_item);
    lvars _item, _pair, _last = false, _list_id;
    returnif(issimple(_item));
    if _item!KEY == weakref exptr_mem_key then
        ident weakref[exptr_mem_key] exptr_mem_fixed_hold_list
    else
        ident fixed_hold_list
    endif -> _list_id;
    fast_for _pair on fast_idval(_list_id) do
        if _pair <@(w) _system_end then     ;;; in case locked in
            [] -> if _last then fast_back(_last) else fast_idval(_list_id) endif;
            return
        endif;
        unless fast_front(_pair) == _item then
            _pair -> _last, nextloop
        endunless;
        fast_back(_pair) -> if _last then fast_back(_last)
                            else fast_idval(_list_id)
                            endif;
        _free_pairs -> fast_back(_pair);
        _pair -> _free_pairs;
        return
    endfor
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 11 1997
        Added sys_encode_string_fixed.
--- John Gibson, Jan 27 1996
        Made _saved_curr_heap_seg a perm var
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Oct 18 1994
        free*pairs -> _free_pairs
--- John Gibson, Sep 14 1992
        Made free_block(2)_key full keys (since sys_grbg_fixed means that
        free blocks can become visible), and other mods
--- John Gibson, Sep  3 1992
        Added General_fixed_alloc and modified copy_fixed to call
        Copy_exptrmem for exptr_mems
--- John Gibson, Sep 14 1991
        Changed dummy_seg to use rawstruct instead of string
--- John Gibson, Dec 11 1990
        p*opmemlim replaced by internal -max_mem_lim-
--- Simon Nichols, Nov  8 1990
        Changed mishap codes to lower case.
--- John Gibson, Sep 12 1990
        Special fix into -cons_fixed- for dealing with pairs
--- John Gibson, May  4 1990
        Added -free_fixed_hold-
--- John Gibson, May  3 1990
        Moved -is_fixed- to separate file.
--- John Gibson, Mar  3 1990
        Installed in masters.
 */
