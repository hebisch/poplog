int
do_div(int a, int b) {
    return a/b;
}

void
do_bgi_mult(unsigned int * dst, int len, unsigned int * xp,
     unsigned int m) {
    unsigned int c = 0;
    int i = 0;
    while(i < len) {
        unsigned long long pp = (unsigned long long)(xp[i])*
                               (unsigned long long)m +
                               (unsigned long long)c;
        dst[i] = pp;
        c = (pp >> (32ULL));
        i++;
    }
    dst[i] = c;
}

void
do_bgi_mult_add(unsigned int * dst, int len, unsigned int * xp,
     unsigned int m) {
    unsigned int c = 0;
    int i = 0;
    while(i < len) { 
        unsigned long long pp = (unsigned long long)(xp[i])*
                               (unsigned long long)m +
                               (unsigned long long)(dst[i]) +
                               (unsigned long long)c;
        dst[i] = pp; 
        c = (pp >> (32ULL));
        i++;
    }
    dst[i] += c;
}

void
do_bgi_sub_mult(unsigned int * dst, int len, unsigned int * xp,
     unsigned int m) {
    unsigned int c = 0;
    int i = 0;
    while(i < len) {
        unsigned long long pp =
                               (unsigned long long)(dst[i]) -
                               (unsigned long long)c -
                               (unsigned long long)(xp[i])*
                               (unsigned long long)m;
        dst[i] = pp;
        c = -(pp >> (32ULL));
        i++;
    }
    dst[i] -= c;
}

void
do_bgi_add(unsigned int * dst, int xlen, unsigned int * xp,
     int ylen, unsigned int * yp) {
    unsigned int c = 0;
    if (ylen < xlen) {
        int tmp = xlen;
        xlen = ylen;
        ylen = tmp;
        unsigned int * tmpp = xp;
        xp = yp;
        yp = tmpp;
    }
    unsigned int xext = (unsigned int)(((int)(xp[xlen - 1])) >> 31);
    unsigned int yext = (unsigned int)(((int)(yp[ylen - 1])) >> 31);
    int i = 0;
    while(i < xlen) {
        unsigned long long pp = (unsigned long long)(xp[i])
                              + (unsigned long long)(yp[i])
                              + (unsigned long long)c;
        dst[i] = pp;
        c = (pp >> (32ULL));
        i++;
    }
    while(i < ylen) {
        unsigned long long pp = (unsigned long long)xext
                              + (unsigned long long)(yp[i])
                              + (unsigned long long)c;
        dst[i] = pp;
        c = (pp >> (32ULL));
        i++;
    }
    {
        unsigned long long pp = (unsigned long long)xext
                              + (unsigned long long)yext
                              + (unsigned long long)c;
        dst[i] = pp;
    }
}

void
do_bgi_sub(unsigned int * dst, int xlen, unsigned int * xp,
     int ylen, unsigned int * yp) {
    unsigned int c = 0;
    int mlen = (xlen < ylen)?xlen:ylen;
    int i = 0;
    unsigned int xext = (unsigned int)(((int)(xp[xlen - 1])) >> 31);
    unsigned int yext = (unsigned int)(((int)(yp[ylen - 1])) >> 31);
    while(i < mlen) {
        unsigned long long pp = (unsigned long long)(xp[i])
                              - (unsigned long long)(yp[i])
                              - (unsigned long long)c;
        dst[i] = pp;
        c = -(pp >> (32ULL));
        i++;
    }
    while(i < xlen) {
        unsigned long long pp = (unsigned long long)(xp[i])
                              - (unsigned long long)yext
                              - (unsigned long long)c;
        dst[i] = pp;
        c = -(pp >> (32ULL));
        i++;
    }
    while(i < ylen) {
        unsigned long long pp = (unsigned long long)xext
                              - (unsigned long long)(yp[i])
                              - (unsigned long long)c;
        dst[i] = pp;
        c = -(pp >> (32ULL));
        i++;
    }
    {
        unsigned long long pp = (unsigned long long)xext
                             - (unsigned long long)yext
                              - (unsigned long long)c;
        dst[i] = pp;
    }
}

void
do_bgi_negate(unsigned int * dst, int xlen, unsigned int * xp) {
    unsigned int c = 0;
    int i = 0;
    unsigned int xext = (unsigned int)(((int)(xp[xlen - 1])) >> 31);
    while(i < xlen) {
        unsigned long long pp =
                              - (unsigned long long)(xp[i])
                              - (unsigned long long)c;
        dst[i] = pp;
        c = -(pp >> (32ULL));
        i++;
    }
    {
        unsigned long long pp =
                              - (unsigned long long)xext
                              - (unsigned long long)c;
        dst[i] = pp;
    }
}

void
do_bgi_negate_no_ov(unsigned int * dst, int xlen, unsigned int * xp) {
    unsigned int c = 0;
    int i = 0;
    while(i < xlen) {
        unsigned long long pp =
                              - (unsigned long long)(xp[i])
                              - (unsigned long long)c;
        dst[i] = pp;
        c = -(pp >> (32ULL));
        i++;
    }
}

unsigned int
do_bgi_div(unsigned int * dst, int len, unsigned int * xp,
     unsigned int divisor) {
    unsigned int rem = 0;
    while(len > 0) {
        len--;
        unsigned long long pp =
            (((unsigned long long)rem)<<(32ULL)) +
            (unsigned long long)(xp[len]);
        unsigned long long q = 
            pp / (unsigned long long)divisor;
        unsigned long long r =
            pp % (unsigned long long)divisor;
        dst[len] = q;
        rem = r;
   }
   return rem;
}

unsigned int
do_bgi_lshift(unsigned int * dst, int off, int len, unsigned int * xp) {
    int i;
    unsigned int rem = 0;
    for(i = 0; i < len; i++) {
        unsigned long long pp =
            (((unsigned long long)(xp[i])) << off) | rem;
        dst[i] = pp;
        rem = (pp >> 32LL);
    }
    return rem;
}

void
do_bgi_rshiftl(unsigned int * dst, int off, int len, unsigned int * xp) {
    unsigned int rem = 0;
    while(len > 0) {
        len--;
        unsigned int pp = xp[len];
        dst[len] = (pp >> off) | rem;
        rem = pp << (32 - off);
    }
}

struct ldh_str {
    double div_inv;
    unsigned int div_hi;
};

int
do_quotient_estimate_init(struct ldh_str * ldh, int len,
                          unsigned int * div) {
    unsigned int divh = div[len - 1];
    int shift = 0;
    if ((divh & 0xffff0000) == 0) {
        shift += 16;
        divh <<= 16;
    }
    if ((divh & 0xff000000) == 0) {
        shift += 8;
        divh <<= 8;
    }
    if ((divh & 0xf0000000) == 0) {
        shift += 4;
        divh <<= 4;
    }
    if ((divh & 0xc0000000) == 0) {
        shift += 2;
        divh <<= 2;
    }
    if ((divh & 0x80000000) == 0) {
        shift += 1;
        divh <<= 1;
    }
    unsigned int divl = div[len - 2];
    if (shift > 0) {
        divh |= divl >> (32 - shift);
        divl <<= shift;
        if (len > 2) {
            unsigned int pp = div[len - 3];
            divl |= pp >> (32 - shift);
        }
    }
    ldh->div_hi = divh;
    double ddiv = (double)divh;
    double d2_to_32 = ((double)(1<<16))*((double)(1<<16));
    ddiv *= d2_to_32;
    ddiv += (double)divl;
    ldh->div_inv = (d2_to_32 + ((double)1)/((double)(1<<15)))/ddiv;
    return shift;
}

unsigned int
do_quotient_estimate(struct ldh_str * ldh, unsigned int lo,
                          unsigned int hi) {
    if (hi > 0) {
        double d2_to_32 = ((double)(1<<16))*((double)(1<<16));
        double dval = (double) hi;
        dval *= d2_to_32;
        dval += ((double) lo + (double)1);
        dval *= ldh->div_inv;
        if (dval >= 4294967295.0) {
            return 4294967295U;
        } else {
            return (unsigned int)dval;
        }
    } else {
        return (lo >= ldh->div_hi)?1:0;
    }        
}
