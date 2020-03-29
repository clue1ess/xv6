#include "types.h"
#include "stat.h"
#include "user.h"
#include "fcntl.h"
#define PGSIZE (4096)
#define PGROUNDUP(sz) ((((uint)sz)+PGSIZE-1) & ~(PGSIZE-1))
#define PGROUNDDOWN(a) (((uint)a) & ~(PGSIZE-1))

int var = 5;
int
main(void)
{
   char *addr;
   int pid;
   printf(1, "initial val : %d\n", var);
   var = 7;
   printf(1, "val after change before mprotect : %d\n", var);
   addr = (char *)PGROUNDDOWN(&var);
   mprotect((void *)addr, 1);
   pid = fork();
   if(pid == 0) {
	var = 11;
   	printf(1, "val after munprotect in child: %d\n", var);
	exit();
   }
   else
	wait();
   munprotect((void *)addr, 1);
   var = 9;
   printf(1, "val after munprotect : %d\n", var);
   exit();
}
