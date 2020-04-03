## Functions in xv6

1. main (main entry in xv6) [1217]

2. kinit1
    Physical page allocator meaning put pages starting from vstart to vend into kfree list so that they can be used for allocation.
    `void kinit1(void *vstart, void *vend)`

3. kvmalloc
    Allocate page table for scheduler in kernel address space.
    `void kvmalloc(void)`

4. setupkvm
    Set up kernel page table i.e create mapping of kmap in va.
    `pde_t* setupkvm(void)`

5. mappages
    Create PTEs for virtual addresses starting at va that refer to physical addresses starting at pa. 
    `static int mappages(pde_t *pgdir, void *va, uint size, uint pa, int perm)`

6. walkpgdir
    Return the address of the PTE in page table pgdir that corresponds to virtual address va.
    `static pte_t *walkpgdir(pde_t *pgdir, const void *va, int alloc)`

7. switchkvm
    Switch h/w page table register to the kernel−only page table, for when no process is running.
    `void switchkvm(void)`

8. userinit
    Set up first user process ie allocate PCB and set up trapframe.
    `void userinit(void)`

9. allocproc
    Find UNUSED proc, if found intialize it for the process if not return.
    `static struct proc allocproc(void)`

10. inituvm
    Copies binary of initcode.S into process's memory 
    `void inituvm(pde_t *pgdir, char *init, uint sz)`

11. mpmain
    Does some CPU setup and calls scheduler 
    `static void mpmain(void)`

12. scheduler
    Per−CPU process scheduler. Each CPU calls scheduler() after setting itself up. Scheduler never returns. It loops, doing:
    − choose a process to run
    − swtch to start running that process
    − eventually that process transfers control via swtch back to the scheduler.
    `void scheduler(void)`

13. switchuvm
    Switchuvm will tell hardware to set up target process's table and switch TSS.
    `void switchuvm(struct proc *p)`

14. init
    Init has to execute initcode.S whose binary has to be placed in init's address space 
    Init - the initial user program [7810]

15. shell [8001]

16. swtch
    Save current register context in old and then load register context from new.
    `void swtch(struct context **old, struct context *new)`

17. fork [2340]
    `int fork(void)`

18. copyuvm

19. exec [5910]
    `int exec(char *path, char **argv)`

20. allocuvm
    Allocate page tables and physical memory to grow process from oldsz to newsz.
    `int allocuvm(pde_t *pgdir, uint oldsz, uint newsz)`

21. loaduvm
    Load a program segment into pgdir. 
    `int loaduvm(pde_t *pgdir, char *addr, struct inode *ip, uint offset, uint sz)`

22. clearpteu
    Clear U flag in pte.
    `void clearpteu(pde_t *pgdir, char *uva)`

23. copyout
    Copy len bytes from p to user address va in page table pgdir.
    `int copyout(pde_t *pgdir, uint va, void *p, uint len)`

24. freevm
    Free a page table and all the physical memory pages in the user part.
    `void freevm(pde_t *pgdir)`

25. kfree
    Free the page of physical memory pointed at by v.
    `void kfree(char *v)`

26. kalloc
    Allocate one 4096−byte page of physical memory.
    `char* kalloc(void)`

27. deallocuvm
    Deallocate user pages to bring the process size from oldsz to newsz.
    `int deallocuvm(pde_t *pgdir, uint oldsz, uint newsz)`

28. tvinit
    Set up interrupt descriptors in kernel mode and system calls for user mode.
    `void tvinit(void)`

29. trap
