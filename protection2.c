#include "types.h"
#include "stat.h"
#include "user.h"
#include "fcntl.h"

char *argv[] = { "sh", 0 };

int
main(void)
{
   /*char *brk = sbrk(0);
   sbrk(PGROUNDUP(brk) - (uint)brk);
   char *start = sbrk(0);
   sbrk(PGSIZE * 1);
   mprotect(start, 1);
   

   munprotect(start, 1);*/
   exit();
}
