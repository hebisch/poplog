/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XKeyboard.p
 > Purpose:
 > Author:          Gareth Palmer,  4 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */




uses XConstants;
uses XUser;


global constant macro (

    /* modifier names.  Used to build a SetModifierMapping request or,
       to read a GetModifierMapping request.  These correspond to the,
       masks defined above. */

    ShiftMapIndex   = 0,
    LockMapIndex    = 1,
    ControlMapIndex = 2,
    Mod1MapIndex    = 3,
    Mod2MapIndex    = 4,
    Mod3MapIndex    = 5,
    Mod4MapIndex    = 6,
    Mod5MapIndex    = 7,
);


external declare XKeyboard in c;
    (external_import_procedure XptImportProcedure)



    /*
     * Compose sequence status structure, used in calling XLookupString.
     */
    typedef struct _XComposeStatus {
        char *compose_ptr;      ;;; state table pointer
        int chars_matched;      ;;; match state
    } XComposeStatus;



    KeySym XKeycodeToKeysym(display, keycode, index)
    Display *display;
    KeyCode keycode;
    int index;
    {}

    KeyCode XKeysymToKeycode(display, keysym_kcode)
    Display *display;
    KeySym keysym_kcode;
    {}

    char *XKeysymToString(keysym_str)
    KeySym keysym_str;
    {}

    KeySym XStringToKeysym(string)
    char *string;
    {}

    KeySym XLookupKeysym(event, index)
    XKeyEvent *event;
    int index;
    {}

    void XRebindKeysym(display, keysym, mod_list, mod_count, string, num_bytes)
    Display *display;
    KeySym keysym;
    KeySym *mod_list;
    int mod_count;
    unsigned char *string;
    int num_bytes;
    {}

    int XLookupString(event, buffer, num_bytes, keysym, status)
    XKeyEvent *event;
    char *buffer;                    ;;; RETURN
    int num_bytes;
    KeySym *keysym;                  ;;; RETURN
    XComposeStatus *status;           ;;; Not implemented
    {}

    void XQueryKeymap(display, keys)
    Display *display;
    char keys[32];                   ;;; RETURN
    {}

    KeySym *XGetKeyboardMapping(display, first_keycode, keycode_count,
                                keysyms_per_keycode)
    Display *display;
    KeyCode first_keycode;
    int keycode_count;
    int *keysyms_per_keycode;         ;;; RETURN
    {}

    void XChangeKeyboardMapping(display, first_code, keysyms_per_code, keysyms,
                                  num_codes)
    Display *display;
    int first_code;
    int keysyms_per_code;
    KeySym *keysyms;
    int num_codes;
    {}

    void XRefreshKeyboardMapping(event)
    XMappingEvent *event;
    {}

    void XSetModifierMapping(display, mod_map)
    Display *display;
    XModifierKeymap *mod_map;
    {}

    XModifierKeymap *XGetModifierMapping(display)
    Display *display;
    {}

    XModifierKeymap *XDeleteModifiermapEntry(modmap, keysym_entry, modifier)
    XModifierKeymap *modmap;
    KeyCode keysym_entry;
    int modifier;
    {}

    XModifierKeymap *XInsertModifiermapEntry(modmap, keysym_entry, modifier)
    XModifierKeymap *modmap;
    KeyCode keysym_entry;
    int modifier;
    {}

    XModifierKeymap *XNewModifiermap(max_keys_per_mod)
    int max_keys_per_mod;
    {}

    void XFreeModifiermap(modmap)
    XModifierKeymap *modmap;
    {}

endexternal;


xlib_external_require XKeyboard;


global vars XKeyboard = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
