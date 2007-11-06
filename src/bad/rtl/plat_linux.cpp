#line 31 "./lpsrc/flx_linux.pak"
#define STAT "/proc/stat"
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "plat_linux.hpp"

// return number of cpus
int get_cpu_nr()
{
   FILE *fp;
   char line[16];
   int proc_nb, cpu_nr = -1;

   if ((fp = fopen(STAT, "r")) == NULL) {
      fprintf(stderr, ("Cannot open %s: %s\n"), STAT, strerror(errno));
      exit(1);
   }

   while (fgets(line, 16, fp) != NULL) {

      if (strncmp(line, "cpu ", 4) && !strncmp(line, "cpu", 3)) {
         char* endptr = NULL;
         proc_nb = strtol(line + 3, &endptr, 0);

         if (!(endptr && *endptr == '\0')) {
           fprintf(stderr, "unable to parse '%s' as an integer in %s\n", line + 3, STAT);
           exit(1);
         }

         if (proc_nb > cpu_nr)
            cpu_nr = proc_nb;
      }
   }

   fclose(fp);

   return (cpu_nr + 1);
}

