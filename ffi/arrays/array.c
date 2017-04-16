#include "array.h"

int foo(const SaNameT *objectName)
{
    printf("Length: %d\n", objectName->length);

    for (int i = 0; i < objectName->length; i++) {
        printf("%x ", objectName->value[i]);
    }

    printf("<EOF\n");
    return 0;
}
