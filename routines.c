#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

#include "routines.h"

int exec_command(char** cmd) {
    /*
     *  Executes specified command
     *
     *  Returns exit status of executed command, or, if command was killed by a
     *  signal, 128+signum, or 127 if command not found by execvp(), or 126 if
     *  fork() or execvp() failed
     */

    pid_t pid = fork();
    if(pid == -1) {
        perror("Error in fork()");
        return 126;
    }

    if(pid) {
        int retval;
        waitpid(pid, &retval, 0);
        if(WIFEXITED(retval)) {
            return WEXITSTATUS(retval);
        } else {
            /* killed by signal
             * return 128+number of signal that teminated our child */
            return 128 + WTERMSIG(retval);
        }
    } else {
        execvp(cmd[0], (char * const*)cmd);
        perror("Error in execvp()");
        switch (errno) {
            case EACCES:
                _exit(127);
                break;

            default:
                _exit(126);
        }
    }

    return 0;
}

