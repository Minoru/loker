#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "routines.h"

typedef int pipe_t[2];

int exec_command(const char* cmd[]) {
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

int pipeline(int ncmds, char*** cmds) {
    /*
     *  Creates chain of commands connected by pipelines
     *
     *  Returns exit status of executed command, or, if command was killed by a
     *  signal, 128+signum, or 127 if command not found by execvp(), or 126 if
     *  fork() or execvp() failed
     */
    pipe_t *pipes = NULL;
    pid_t *processes = NULL;
    int i, j, k, status;
    pid_t pid;
    
    pipes = malloc((ncmds-1) * sizeof(pipe_t));
    processes = malloc(ncmds * sizeof(pid_t));

    /* create ncmds-1 pipes */
    for(i = 0; i < ncmds-1; i++)
        if(pipe(pipes[i]) == -1) {
            for(j = 0; j < i; j++) {
                do {
                    k = close(pipes[i][0]);
                } while (k == -1 && (errno == EINTR || errno == EIO));
                do {
                    k = close(pipes[i][1]);
                } while (k == -1 && (errno == EINTR || errno == EIO));
            };
            free(pipes);
            free(processes);
            exit(126);
        }

    /* run first command and redirect it's output to pipe */
    pid = fork();
    if(pid == -1) {
        perror("Error in fork():");
        for(i = 0; i < ncmds-1; i++) {
            do {
                k = close(pipes[i][0]);
            } while (k == -1 && (errno == EINTR || errno == EIO));
            do {
                k = close(pipes[i][1]);
            } while (k == -1 && (errno == EINTR || errno == EIO));
        }
        free(pipes);
        free(processes);
        exit(126);
    } else if(pid) {
        processes[0] = pid;
    } else {
        dup2(pipes[0][1], 1);
        for(i = 0; i < ncmds-1; i++) {
            do {
                k = close(pipes[i][0]);
            } while (k == -1 && (errno == EINTR || errno == EIO));
            do {
                k = close(pipes[i][1]);
            } while (k == -1 && (errno == EINTR || errno == EIO));
        }
        execvp(cmds[0][0], cmds[0]);
        perror("Error in execvp():");
        switch(errno) {
            case EACCES:
                _exit(127);
                break;

            default:
                _exit(126);
        }
    }

    /* now run all commands from second to the one before last one */
    for(i = 1; i < ncmds-1; i++) {
        pid = fork();
        if(pid == -1) {
            perror("Error in fork():");
            for(j = 0; j < ncmds-1; j++) {
                do {
                    k = close(pipes[j][0]);
                } while (k == -1 && (errno == EINTR || errno == EIO));
                do {
                    k = close(pipes[j][1]);
                } while (k == -1 && (errno == EINTR || errno == EIO));
            }
            free(pipes);
            /* trying to interrupt processes gently */
            for(j = 0; j < i; j++)
                kill(processes[j], 15);
            /* killing everyone that left */
            for(j = 0; j < i; j++)
                kill(processes[j], 9);
            free(processes);
            exit(126);
        } else if(pid) {
            processes[i] = pid;
        } else {
            dup2(pipes[i-1][0], 0);
            dup2(pipes[i][1], 1);
            for(j = 0; j < ncmds-1; j++) {
                do {
                    k = close(pipes[j][0]);
                } while (k == -1 && (errno == EINTR || errno == EIO));
                do {
                    k = close(pipes[j][1]);
                } while (k == -1 && (errno == EINTR || errno == EIO));
            }
            execvp(cmds[i][0], cmds[i]);
            perror("Error in execvp():");
            switch(errno) {
                case EACCES:
                    _exit(127);
                    break;

                default:
                    _exit(126);
            }
        }
    }

    /* run last command */
    pid = fork();
    if(pid == -1) {
        perror("Error in fork():");
        for(i = 0; i < ncmds-1; i++) {
            do {
                k = close(pipes[i][0]);
            } while (k == -1 && (errno == EINTR || errno == EIO));
            do {
                k = close(pipes[i][1]);
            } while (k == -1 && (errno == EINTR || errno == EIO));
        }
        free(pipes);
        /* trying to interrupt processes gently */
        for(i = 0; i < ncmds-1; i++)
            kill(processes[i], 15);
        /* killing everyone that left */
        for(i = 0; i < ncmds-1; i++)
            kill(processes[i], 9);
        free(processes);
        exit(126);
    } else if(pid) {
        processes[ncmds-1] = pid;
    } else {
        dup2(pipes[ncmds-2][0], 0);
        for(i = 0; i < ncmds-1; i++) {
            do {
                k = close(pipes[i][0]);
            } while (k == -1 && (errno == EINTR || errno == EIO));
            do {
                k = close(pipes[i][1]);
            } while (k == -1 && (errno == EINTR || errno == EIO));
        }
        execvp(cmds[ncmds-1][0], cmds[ncmds-1]);
        perror("Error in execvp():");
        switch(errno) {
            case EACCES:
                _exit(127);
                break;

            default:
                _exit(126);
        }
    }

    for(i = 0; i < ncmds-1; i++) {
        do {
            k = close(pipes[i][0]);
        } while (k == -1 && (errno == EINTR || errno == EIO));
        do {
            k = close(pipes[i][1]);
        } while (k == -1 && (errno == EINTR || errno == EIO));
    }

    /* waiting for processes to end */ 
    for(i = 0; i < ncmds-1; i++)
        waitpid(processes[i], NULL, 0);

    /* waiting for last process, gather it's exit information */
    waitpid(processes[ncmds-1], &status, 0);

    free(pipes);
    free(processes);

    if(WIFEXITED(status))
        return WEXITSTATUS(status);
    else
        /* killed by a signal */
        return 128 + WTERMSIG(status);
}

