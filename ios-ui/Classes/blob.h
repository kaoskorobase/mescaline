//
//  blob.h
//  Mescaline
//
//  Created by z on 03.05.11.
//  Copyright 2011 Null2 GmbH. All rights reserved.
//
#include <machine/endian.h>
#include <stdint.h>
#include <vector>



uint32_t swap(uint32_t x)
{
    int32_t y;
#if BYTE_ORDER == BIG_ENDIAN
    y = x;
#else
    unsigned char* dst = (unsigned char*)&y;
    unsigned char* src = (unsigned char*)&x;
    dst[0] = src[3];
    dst[1] = src[2];
    dst[2] = src[1];
    dst[3] = src[0];
#endif
    return y;
}

double swap(double d)
{
    double a;
#if BYTE_ORDER == BIG_ENDIAN
    a = d;
#else
    unsigned char *dst = (unsigned char *)&a;
    unsigned char *src = (unsigned char *)&d;
    dst[0] = src[7];
    dst[1] = src[6];
    dst[2] = src[5];
    dst[3] = src[4];
    dst[4] = src[3];
    dst[5] = src[2];
    dst[6] = src[1];
    dst[7] = src[0];
#endif
    return a;
}

std::vector<double> blobToVector(const char* blob)
{
    uint32_t n = swap(*(uint32_t*)blob);
    std::vector<double> v;
    v.reserve(n);
    for (size_t i=0; i < n; i++) {
        v.push_back(swap(*(double*)(blob+sizeof(uint32_t)+i*sizeof(double))));
    }
    return v;
}
