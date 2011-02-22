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

void close_pipelines(int, pipe_t*);
void kill_processes(int, pid_t*);

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
    int i, status;
    pid_t pid;
    
    pipes = malloc((ncmds-1) * sizeof(pipe_t));
    processes = malloc(ncmds * sizeof(pid_t));

    /* create ncmds-1 pipes */
    for(i = 0; i < ncmds-1; i++)
        if(pipe(pipes[i]) == -1) {
            close_pipelines(i, pipes);
            free(pipes);
            free(processes);
            exit(126);
        }

    /* run first command and redirect it's output to pipe */
    pid = fork();
    if(pid == -1) {
        perror("Error in fork():");
        close_pipelines(ncmds-1, pipes);
        free(pipes);
        free(processes);
        exit(126);
    } else if(pid) {
        processes[0] = pid;
    } else {
        dup2(pipes[0][1], 1);
        close_pipelines(ncmds-1, pipes);
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
            close_pipelines(ncmds-1, pipes);
            free(pipes);
            kill_processes(i, processes);
            free(processes);
            exit(126);
        } else if(pid) {
            processes[i] = pid;
        } else {
            dup2(pipes[i-1][0], 0);
            dup2(pipes[i][1], 1);
            close_pipelines(ncmds-1, pipes);
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
        close_pipelines(ncmds-1, pipes);
        free(pipes);
        kill_processes(ncmds-1, processes);
        free(processes);
        exit(126);
    } else if(pid) {
        processes[ncmds-1] = pid;
    } else {
        dup2(pipes[ncmds-2][0], 0);
        close_pipelines(ncmds-1, pipes);
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

    close_pipelines(ncmds-1, pipes);

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


/* Utility functions */
void close_pipelines(int n, pipe_t pipes[]) {
    /*
     *  Closes given amount of pipelines
     *
     * n is amount of pipes to close
     * pipes is array of them
     */

    int i, k, ret;

    for(i = 0; i < n; i++) {
        for(k = 0; k < 2; k++)
            do {
                ret = close(pipes[i][k]);
            } while (ret == -1 && (errno == EINTR || errno == EIO));
    }
}

void kill_processes(int n, pid_t processes[]) {
    /*
     *  Kills given number of processes
     *
     *  n is number of processes to kill
     *  processes is an array of PIDs
     */

    int i;

    /* trying to interrupt processes gently */
    for(i = 0; i < n; i++)
        kill(processes[i], 15);

    /* TODO: maybe we should use some delay here to let processes die gracefully :) */
    
    /* killing everyone that left */
    for(i = 0; i < n; i++)
        kill(processes[i], 9);
}

