#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

int main(int argc, char * const argv[]) {
  int child;
  printf("Running\n");
  if ((child=fork()) != 0) { printf("Forked %d\n",child); exit(0); }
  setsid();
  if ((child=fork()) != 0) { exit(0); }
  int sout = open("mystdout.log",O_CREAT | O_WRONLY | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
  int serr = open("mystderr.log",O_CREAT | O_WRONLY | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
  close(0); close(1); close(2);
  dup2(sout,1);
  dup2(serr,2);
  execv(argv[1], argv+1);
  printf("Exec failed\n");
  return 1;
}


