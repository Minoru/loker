#ifndef LOKER_ROUTINES_H
#define LOKER_ROUTINES_H

#include <stddef.h> /* NULL */

#define AND(CMD1, STATUS1, LABEL, CMD2, RETVAL) \
    STATUS1 = CMD1; \
    if(STATUS1 != 0) \
    { \
        RETVAL = STATUS1; \
        goto LABEL; \
    } \
    \
    RETVAL = CMD2; \
    LABEL : ;

#define OR(CMD1, STATUS1, LABEL, CMD2, RETVAL) \
    STATUS1 = CMD1; \
    if(STATUS1 == 0) \
    { \
        RETVAL = STATUS1; \
        goto LABEL; \
    } \
    \
    RETVAL = CMD2; \
    LABEL : ;

int exec_command(char **);
int pipeline(int, char***);

#endif

