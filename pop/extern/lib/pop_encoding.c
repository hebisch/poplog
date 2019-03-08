/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 * File:            C.all/extern/lib/pop_encoding.c
 * Purpose:         Unicode -> multibyte encoding
 * Author:          Robert Duncan, May  1 1997
 */

#include <stddef.h>
#include "c_core.h"
#include "../unicode/encoding.h"

/* Assigned by the updater of pop_sys_encoding */
globaldef CODING_PAIR * _pop_sys_encoding_funcs;

size_t pop_uc_to_mb(ucptr, inlen, bptr, outlen)
  CHAR16 *ucptr;
  size_t inlen;
  CHAR8 *bptr;
  size_t outlen;
  {
    if (_pop_sys_encoding_funcs)
      { unsigned ilen = inlen, olen = outlen, state = 0;
        (*_pop_sys_encoding_funcs->encode)(ucptr, &ilen, bptr, &olen,
                                                            &state, 0);
        return(ilen == 0 ? olen : (size_t)-1);
      }
    else
      { /* default 'encoding' preserves Latin-1 */
        size_t len = outlen;
        while (inlen-- != 0)
          { CHAR16 uc = *ucptr++;
            if (outlen-- == 0) return((size_t)-1);
            *bptr++ = (uc <= 0xff) ? uc : '?';
          }
        return(len-outlen);
      }
  }
