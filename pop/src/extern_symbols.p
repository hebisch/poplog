/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/src/extern_symbols.p
 > Purpose:         External symbols and shared libraries
 > Author:          John Gibson, Feb 25 1997 (see revisions)
 */

#_INCLUDE 'declare.ph'
#_INCLUDE 'external.ph'
#_IF DEF WIN32
#_INCLUDE 'win32defs.ph'
#_ELSEIF DEF UNIX
#_INCLUDE 'unixdefs.ph'
#_ENDIF

;;; ----------------------------------------------------------------------

section $-Sys$-Extern;

#_IF DEF SHLIB_EXTN

vars _image_handle = _0;    ;;; for SunOS


    /*  dummy external load to force linking against the dynamic linker
        library (not needed for IRIX 5/OSF1 -- everything's in libc.so)
    */
#_IF DEF UNIX and not(DEFV IRIX >= 5.0 or DEF OSF1)
exload dl [^DL_LIB]
    lconstant exload_dummy;     ;;; anything will do
endexload;
#_ENDIF

lconstant msvec = writeable {0 0 1};        ;;; for sys_pr_message

    /* return a string describing the last dynamic linker error
    */
define lconstant shlib_error() -> msg;
    lvars msg = false;
    ;;; NB: Linux ELF dlerror() is unreliable, so we don't use it
#_IF DEFV SYSTEM_V >= 4.0 or DEF OSF1 or DEF AIX
    lvars _msg = _extern dlerror();
    _nonzero(_msg) and Consstring_bptr(_msg, _-1, CSB_FIXED) -> msg;
#_ENDIF
enddefine;

    /*  dlopen() flags values
    */
#_IF DEFV SUNOS >= 5.3 or DEF ALPHA_LINUX
lconstant macro DLOPEN_FLAGS = 16:101;  ;;; RTLD_LAZY|RTLD_GLOBAL
#_ELSEIF DEF AIX
lconstant macro DLOPEN_FLAGS = 16:10004;    ;;; RTLD_LAZY|RTLD_GLOBAL
#_ELSEIF DEF DGUX
lconstant macro DLOPEN_FLAGS = 16:005;  ;;; RTLD_LAZY|RTLD_GLOBAL
#_ELSEIF DEF NCR
lconstant macro DLOPEN_FLAGS = 16:003;  ;;; RTLD_LAZY_GLOBAL
#_ELSEIF DEF OSF1 or DEF LINUX_ELF
lconstant macro DLOPEN_FLAGS = 16:001;  ;;; RTLD_LAZY
#_ELSEIF DEFV IRIX >= 5.0
lconstant macro DLOPEN_FLAGS = 16:001;  ;;; RTLD_LAZY
#_ENDIF

    /* open a shared library, allowing for unresolved references
    */
define Shlib_open(name, add_extn) -> handle;
    lvars name, handle = false, msg;
    lstackmem stackbuff _nbuf;
    if add_extn then name <> SHLIB_EXTN -> name endif;
#_IF DEF WINDOWS
    lvars t_name = Tchars_in(name, wkstring1);
    lvars _handle = _extern pop_load_library(t_name@V_TCHARS);
    if _zero(_handle) then GET_LAST_ERROR endif;
#_ELSEIF DEF VMS
    lvars _handle = _0;     ;;; not used
    name -> handle;
#_ELSEIF DEF HPUX
  #_IF DEF HP9000_700
    lconstant macro BIND_DEFERRED = _1; ;;; from <dl.h>
  #_ELSE
    lconstant macro BIND_DEFERRED = _2; ;;; from <shl.h>
  #_ENDIF
    lvars _handle = _extern shl_load(Encode_sys(name,_nbuf), BIND_DEFERRED, _0);
#_ELSEIF DEF AIX
    lconstant macro (
        DLOPEN_MFLAGS = DLOPEN_FLAGS||16:40000, ;;; RTLD_MEMBER
    );
    if isendstring('.a', name) then
        ;;; archive -- use module name shr.o
        name <> '(shr.o)' -> name
    endif;
    ;;; if name ends with ')', assume it's libfoo.a(modname)
    lvars _handle = _extern dlopen(Encode_sys(name,_nbuf),
                                if isendstring(')', name) then _:DLOPEN_MFLAGS
                                else _:DLOPEN_FLAGS
                                endif);
#_ELSEIF DEFV IRIX >= 5.0
    ;;; use SG version of dlopen, to accumulate symbols
    lvars _handle = _extern sgidladd(Encode_sys(name,_nbuf), _:DLOPEN_FLAGS);
#_ELSEIF DEF DLOPEN_FLAGS
    lvars _handle = _extern dlopen(Encode_sys(name,_nbuf), _:DLOPEN_FLAGS);
#_ELSE_ERROR
#_ENDIF
    if _nonzero(_handle) then Cons_extern_ptr(_handle) -> handle endif;
    returnif(handle);

    if shlib_error() ->> msg then
        '%can\'t open %S %S (%S)'
    else
        '%can\'t open %S %S (%M)'
    endif -> fast_subscrv(1, msvec);
    'Warning:' -> fast_subscrv(2, msvec);
    sys_pr_message(SHLIB_NAME, name, if msg then msg, 3 else 2 endif,
                                                msvec, nullstring, `W`)
enddefine;

    /* close a shared library
    */
define Shlib_close(handle);
    lvars handle;
#_IF DEF WINDOWS
    _extern pop_free_library(handle!XP_PTR) -> ;
#_ELSEIF DEF VMS
#_ELSEIF DEF HPUX
    _extern shl_unload(handle!XP_PTR) -> ;
#_ELSE
    _extern dlclose(handle!XP_PTR) -> ;
#_ENDIF
enddefine;

    /* get the value of a symbol and assign it to the external pointer
       value
    */
define Shlib_findsym(handles, try_previous_p, symbol, value) -> success;
    lvars handles, try_previous_p, symbol, value, msg, success = false;
    lconstant hvec = writeable initv(1);
    unless isvector(handles) then
        handles -> fast_subscrv(1, hvec);
        hvec -> handles
    endunless;

#_IF DEF WINDOWS
    lvars i;
    fast_for i to datalength(handles) do
        lvars _handle = fast_subscrv(i, handles)!XP_PTR;
        lvars _addr = _extern pop_get_proc_address(_handle, symbol@V_BYTES);
        if _nonzero(_addr) then
            _addr -> value!XP_PTR;
            return(true -> success);
        endif;
    endfor;
#_ELSEIF DEF VMS
#_ELSEIF DEF HPUX
    ;;; search all the shared libraries
    dlvars _handle = _0;
    _zero(_extern shl_findsym(ident _handle, symbol, _0, value@XP_PTR))
        -> success;
#_ELSEIF DEFV SUNOS >= 5.3
    lconstant macro RTLD_NEXT = _-1; ;;; from <dlfcn.h>
    lvars _value = _0;
    ;;; search in the image and any initial shared libraries
    if _zero(_image_handle) then
        _extern dlopen(_NULL, _:DLOPEN_FLAGS) -> _image_handle;
    endif;
    if _nonzero(_image_handle) then
        _extern dlsym(_image_handle, symbol) -> _value;
    endif;
    if _zero(_value) then
        ;;; search in any subsequently-loaded libraries
        _extern dlsym(RTLD_NEXT, symbol) -> _value;
    endif;
    if _nonzero(_value) then
        _value -> value!XP_PTR;
        true -> success;
    endif;
#_ELSE
    define lconstant Try_handle(handle);
        lvars handle, _value;
    #_IF DEF DEBUG
        '%searching for %S in handle 16:%x' -> fast_subscrv(1, msvec);
        'Debug -' -> fast_subscrv(2, msvec);
        sys_pr_message(symbol, handle!XP_PTR, 2, msvec, nullstring, `I`);
    #_ENDIF
        if _nonzero( _extern  dlsym(handle!XP_PTR, symbol) ->> _value) then
            _value -> value!XP_PTR;
            exitfrom(true, Shlib_findsym);
        endif;
    enddefine;
    ;;; look in the image first
    lconstant image_handle = writeable consexternal_ptr();
    if _zero(_image_handle) then
        _extern dlopen(_NULL, _:DLOPEN_FLAGS) ->> _image_handle
            -> image_handle!XP_PTR;
    endif;
    Try_handle(image_handle);
    ;;; ... then try DSOs opened in previous loads
    if try_previous_p then try_previous_p(Try_handle) endif;
    ;;; ... and finally look in the current batch
    appdata(handles, Try_handle);
#_ENDIF

    returnif(success);

    if shlib_error() ->> msg then
        '%can\'t find value for symbol %S (%S)'
    else
        '%can\'t find value for symbol %S'
    endif -> fast_subscrv(1, msvec);
    'Error:' -> fast_subscrv(2, msvec);
    sys_pr_message(symbol, if msg then msg, 2 else 1 endif, msvec,
                                                        nullstring, `W`)
enddefine;

#_ENDIF     ;;; DEF SHLIB_EXTN


;;; --- SYMBOL NAME TRANSLATION -------------------------------------------

define Name_translate(name, lang, cvt_always) -> name;
    lvars name, lang, cvt_always;
    if isword(name) then
        copy(name!W_STRING) -> name;
        true -> cvt_always
    endif;
#_IF DEF WINDOWS
    ;;; currently, things seem to work best with no transformations
#_ELSEIF DEF VMS
    ;;; This damn procedure name is 32 chars long, 1 over the limit for VMS
    ;;; symbols, and the VMS version is called XtDisplayStringConvWarning
    ;;; anyway
    if name = 'XtDisplayStringConversionWarning' then
        'XtDisplayStringConvWarning' -> name
    elseif datalength(name) fi_> 31 then
        substring(1, 31, name) -> name
    endif;
#_ELSE
    returnunless(cvt_always);
  #_IF not(DEF SYSTEM_V or DEF COFF or DEF HP9000_700 or DEF LINUX_ELF)
    '_' <> name -> name;
  #_ENDIF
    ;;; the only one that needs something special is Unix FORTRAN
    if lang = 'FORTRAN' then
        uppertolower(name) <> '_' -> name
    endif;
#_ENDIF
enddefine;


;;; --- CHARACTER ENCODING FUNCTIONS ----------------------------------------

vars encoding_funcs_loaded = [];        ;;; N.B. not restored

define Get_encoding_funcs(encoding_name) -> funcptr;
    lvars encoding_name, funcptr, base_name, symbol, fname, handle;
    Check_word(encoding_name);
    returnif(list_assoc_val(encoding_name, encoding_funcs_loaded) ->> funcptr);

#_IF DEF SHLIB_EXTN
    ;;; try loading it
    Cons_extern_ptr(_0) -> funcptr;
    'encoding_' <> fast_word_string(encoding_name) -> base_name;
    Name_translate(base_name, false, true) -> symbol;
    sysfileok('$popexternlib/' <> base_name) -> fname;
    unless Shlib_open(fname, true) ->> handle then
        mishap(encoding_name, 1, 'UNKNOWN CHAR ENCODING NAME (see above)')
    elseunless Shlib_findsym(handle, false, symbol, funcptr) then
        mishap(encoding_name, 1, 'ERROR LOADING CHAR ENCODING FUNCTIONS (see above)')
    endunless;
    encoding_name -> funcptr!XP_PROPS;
    cons_assoc(encoding_name, funcptr, encoding_funcs_loaded)
                                            -> encoding_funcs_loaded;
#_ELSE
    mishap(encoding_name, 1, 'DYNAMIC LOADING OF CHAR ENCODING FUNCTIONS NOT SUPPORTED')
#_ENDIF
enddefine;

endsection;     /* $-Sys$-Extern */



/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Nov 27 1998
        Fixed Shlib_findsym to use the proper flags value when calling
        dlopen()
--- Robert Duncan, Jun 05 1998
        Added case for DG/UX in Shlib_open
--- John Gibson, May 13 1998
        More AIX changes
--- John Gibson, Mar 20 1998
        Added AIX case in Shlib_open.
--- Robert Duncan, Jul 31 1997
        Removed call to DEB*UG_PRINTF
--- John Gibson, Mar 13 1997
        Added Encode_sys calls for Unix
 */
