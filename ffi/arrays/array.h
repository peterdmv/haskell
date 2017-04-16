#include <stdio.h>
#define SA_MAX_NAME_LENGTH 256

typedef unsigned char         SaUint8T;
typedef unsigned short        SaUint16T;

typedef struct {
    SaUint16T length;
    SaUint8T value[SA_MAX_NAME_LENGTH];
} SaNameT;

int foo(const SaNameT *objectName);
