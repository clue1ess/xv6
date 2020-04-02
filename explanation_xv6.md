

PROCESS
A program in execution is called process.  ? need to write more

META NOTE: suppose two processes are running, above KERNBASE, two diff kstacks and two disjoint set of pages(given by respective pgdir).


SEGMENTATION

PAGING


                                                                    

----------------------------------------------------------------------------------------------------------------------------------------------
BIOS loads first sector(512)(known as bootloader) into a predefined location (0x7c00 for xv6) and jumps to it. It is responsibility of bootloader to find where is rest of kernel in the device and load it into memory and give control to it(jump to first instruction).
If the system has multiple processors, first processor boots up and initializes other processors.

Start at 8409 :
When system boots, it always starts in 16-bit real mode.
.code16
start:

1. Disable all interrupts
    When interrupt occurs i.e. knocking on INTR pin of processor, it will switch the execution to handler if interrupt bit in flag registor is set to 1. What we want is no disturbance until we set kernel and interrupt handlers, though BIOS has setup its own handlers, we want reinitialize the system completely. So, cli instruction will set interrupt bit to 0 in flag registor. 
2. Load all segments and set them to 0.
    Initially, we want flat address space. So, va = pa.
    Why?
        When it will enter into protected mode, it won't have paging. Segmentation is only there. Segmentation meaning, pa = va + base. For mapping virtual address exactly to physical address(i.e. identical mapping), base = 0. 
3. For entering into protected mode, load GDT. va = pa, initially.
    Why va = pa?
        higher addresses may not be available for some machines.
    load GDT:
        meaning initialize gdt descriptor which has base and limit. base = address of gdt and limit = size of (gdt) - 1 (adress of gdt desc - adress of gdt - 1).
    contents of gdt:
        1. null descriptor 
            SEG_NULLASM 
            #define SEG_NULLASM
                .word 0, 0;
                .byte 0, 0, 0, 0
        2. code descriptor
            SEG_ASM(STA_X|STA_R, 0x0, 0xffffffff)
            SEG_ASM(type, base, limit)
            has base = 0 and limit = 0xffffffff permissions = read(STA_R) and execute(STA_X)
        3. data descriptor
            SEG_ASM(STA_W, 0x0, 0xffffffff)
            has base = 0 and limit = 0xffffffff permissions = write(STA_W)

            How is descriptor stored exactly?
                bootloader puts data and code at 1MB initially, so code and data descriptors should be placed from 0 to 1MB-1 PA space
                Therfore, 

                

             Q.limit uses upper 20 bits, why?
             Q. why code and data has same mapping 0 to limit?


4. Enter protected mode.
    set protected enabled bit in control registor #0 (CR0)
5. Enter 32 bit mode using ljmp to start32 as cs caanot be modified directly.
    selector has 13 bit index, 1 bit whether it is LDT or GDT(leave it), 2 bits for priviledge level.
    ljmp cs:ip 
    cs ->
    pickup first descriptor i.e code segment descriptor, now index is upper 13 bits, so has to shift SEG_KCODE by 3 bits.
    pa = base * 16 + offset
    for identical mapping, base = 0, pa = offset.
    goto offset i.e start32.


.code32
start32 :

1. Set up protected mode data segments.
    for picking up data segment descriptor, sector should be loaded with SEG_KDATA << 3 same as above.
    fs and gs are never used registors in xv6, so set them 0. If anyone acess these registors, they will pickup null descriptor, and hence will cause trap(will see later).
2. set up stack pointer at 07c00 where start was there and it grows downwards i.e. towards 0.
    PA will look like this:(512 bytes)
    |           |... (bootmain)
    |           |start32
    |           |0x7c00(start) <-esp(for 32 bit mode) 
    |           |           
3. Call bootmain().
    Calling function means pushing return address at stack. So, return address of bootmain gets stored at 0x7c00 - 4 and esp decremented by 4.

bootmain:

loads rest of kernel into memory and jump to the first instruction.
Theory:
    Kernel has .c, .h files which are compiled by gcc, converted to .o files and then linked to get kernel executable file in elf format.
    Now, that file has all informations regarding where is code and data in the disk stored and where to load them into memory(at what address). Also, it has starting address, i.e. where to start execution of the kernel.
1. read first sector which is elf header and check whether it is valid or not using elf->magic_number
2. find program headers
3. loop over each program header and load it into memory
4. find entry point of kernel and give control to it.

Implementation:
1. Initialize some scratch space at address 0x10000(64k)
2. read first page of the disk(which is elf header) using readseg.
    void readseg(uchar* pa, uint count, uint offset)
    2.1 sector size = 512. Find no of sectors required
    2.2 set page boundary pa -= offset % SECTSIZE //??
    2.3 offset = (offset / SECTSIZE) + 1; //elf sectors start at 1, sector 0 is bootloader
    2.4 loop to required no, read that sector into memory given by pa using readsect(not required how it works.)
3. Check for magic number in elf header to verify it is not corrupted.
4. Elf has field called elf->phoff which specifies offset of program headers(list of program headers.These specifies where program should be loaded into memory and where it is present in disk and what is it's size). Elf also has field elf->phnum which specifies number of program headers available.
5. Iterate over each program header 
    5.1 pa = (uchar*)ph−>paddr; //get memory location where it should be loaded (Refer the note below)
    5.2 readseg(pa, ph−>filesz, ph−>off); //read disk at offset ph->off into pa, ph->filesz bytes.
    5.3 optimization, ph has two diff field memsz and filesz. So, if(memsz > filsz), set rest to 0. Meaning, disk has filesz of program available, so while loading into memory which is of memsz, set memsz-filesz = 0.
6. get starting point of execution of kernel using elf->entry and cast to function pointer and switch to it.

Note: Kernel generally loads at 0x100000 (pa) because first 1MB is reserved for memory mapped devices, like console, etc.
Q. What are memory mapped devices? 

entry:
    Kernel has to enable paging and setup high virtual adresses.
    elf has given entry point in virtual address space, but we haven't set up va yet. So, V2P_WO will subtract KERNBASE from it(will see later).    
1. Turn on paging
2. set up stack pointer at high address in kernel va.
3. jump to main.

1. Set up large pages for kernel.(there are large pages of size 4MB and small pages of size 4KB)
2. load cr3 with entrypgdir(convert va->pa using V2P_WO). cr3 anyways take pa.
    kernel executable has code compiled which has all address above KERNBASE, but we haven't set va yet.

    pde_t entrypgdir[NPDENTRIES] = {
    // Map VA’s [0, 4MB) to PA’s [0, 4MB)
    [0] = (0) | PTE_P | PTE_W | PTE_PS,
    // Map VA’s [KERNBASE, KERNBASE+4MB) to PA’s [0, 4MB)
    [KERNBASE>>PDXSHIFT] = (0) | PTE_P | PTE_W | PTE_PS,
    };
    va space:
    ...
    2GB + 4MB  --> second entry in entrypgdir which will map to 0-4MB in pa space
    2GB
    ...
    4MB        --> first entry in entrypgdir which will map to 0-4MB in pa space
    0

    KERNBASE = 0x80000000 and PDXSHIFT = 22 bits(largs page size is 4MB i.e 22 bits)
    Kernel will run above 2GB(KERNBASE) in va always and user will run from 0 to 2GB. As soon as, we setup kernel address space, kernel will shift to it and remove 0-4MB mapping in va space. This mapping was needed only to setup kernel at KERNBASE.

3. Turn on paging by loading somthing in cr0(not imp what it is for now).
4. Set up stack pointer in va space.
    movl $(stack + KSTACKSIZE), %esp 
    stack is variable declared by kernel which has address above KERNBASE. esp = KERNBASE + somthing + KSTACKSIZE and will grow downwards.
5. jump to main which has va, so to shift from different address spaces, pc relative jump don't work(why?). Hence indirect jump to main.

Now, esp and eip both will point above KERNBASE.

main:

Every process has two mappings- kernel side and user side. When process is formed, it first get memory in kernel space which will map to pa and that pa will be used to create user side mapping.

kinit1:
    We have KERNBASE-KERNBASE+4MB mapping using entrypgdir. Apart from kcode+kdata area, there is kheap above it. So, we need some data structure which can be used by kalloc and kfree to allocate pages and free pages. kinit1 does this.
    After setting up mapping for rest of the kernel, we will then use kinit2 just like we use kinit1. SO, kinit1 (end of kcode+kdata, 4MB) and kinit2(4MB, PHYSTOP). 
    Why two diff functions?
        AT time of kinit1, memory above 4MB is not mapped. It will mapped using kvmalloc().
 
Now we need to remove mapping of 0-4MB in va and also map rest of KERNBASE + 4MB address.
    va space after setting up kernel page table
    |   Memory mapped devices(l space.So, 0xFE000000 - 0xFFFFFFFF)
    |   Free space(kernel heap)(used by kalloc and kfree)
    |   Kernel(code + data) (Max)
    |   Memory mapped devices(1MB)
    KERNBASE
    ...

kvmalloc :
    area required for kernel pgdir is taken from kernel heap.
    1.1 allocate kernel pgdir using setupkvm. 
    1.2 switch to kernel pgdir using switchkvm.

Q. what happens to entrypgdir?
->it is static array allocated, so can't be deallocted while kpgdir is allocated through kernel heap and hence can be deallocated.

Note: os follows a covention that never map 0 address, meaning 0 address will not be a valid address.

setupkvm:
    set up kernel page table. Setupkvm returns pointer to kpgdir which is in va.
1. allocate a page using kalloc(still in kERNBASE-KERNBASE+4MB va space)
2. clear the page(no mappings)
3. kmap :
static struct kmap {
void *virt;
uint phys_start;
uint phys_end;
int perm;
} kmap[] = {
{ (void*)KERNBASE, 0, EXTMEM, PTE_W},           //I/O space
{ (void*)KERNLINK, V2P(KERNLINK), V2P(data), 0},//kern text+rodata
{ (void*)data, V2P(data), PHYSTOP, PTE_W},      //kern data+memory
{ (void*)DEVSPACE, DEVSPACE, 0, PTE_W},         //more devices
};

va space should for kernel should like this:
    Memory mapped devices ..
    Kernel r/w data + Kernel heap(data-KERNBASE+PHYSTOP --> data(v2p)-PHYSTOP)
    Kernel code + read-only data(KERNBASE+EXTMEM(KERNLINK)-data --> EXTMEM-data(v2p))
    Memory mapped devices(KERNBASE-KERNBASE+EXTMEM --> 0-EXTMEM)

#define PHYSTOP 0xE000000 -->224MB
loops over kmap elements and create PTEs for virtual adress given using mappages

mappages:
// Create PTEs for virtual addresses starting at va that refer to
// physical addresses starting at pa. va and size might not
// be page−aligned.
//lazy allocation as don't allocate everything at starting

1. determine no of pages meaning page table entries you want. 
    start = va & ~(pgsize-1)
    end = va & ~(pgsize-1)
    -> done by PGROUNDDOWN
2. loop from start to end
    (Refer 2-level Paging)
    2.1 walkpgdir to find pte or create page table if not allocated  and then find pte 
    2.2 set pte to physical address of particular page address.
    

walkpgdir:
// Return the address of the PTE in page table pgdir
// that corresponds to virtual address va. If alloc!=0,
// create any required page table pages.    

NOTE: pde and pte conatin physical adresses.

1. Get address of particular pde for given pgdir by obtainig top 10 bits of address given(using PDX which does PDXSHIFT i.i shifting by 22 bits)
2. Get what is there in pde.
3. If pde is empty, allocate pgtable using kalloc and clear the page and set pde to physical adress of pgtable. If pte is not empty, get pgtable.
4. Find pte in that pgtable(using PTX which does PTXSHIFT i.i shifting by 12 bits))
5. Return address of pte.

switchkvm :
    load cr3 with kernel pgdir(convert va->pa using v2p, remember setupkvm returns pointer in va space)


----------------------------------------------------------------------------------------------------------------------------------------------

// Per−process state
struct proc {
uint sz;                    //Size of process memory (bytes)
pde_t* pgdir;               //Page table
char *kstack;               //Bottom of kernel stack for this process (why char* , it stores va)
enum procstate state;       //Process state
volatile int pid;           //Process ID
struct proc *parent;        //Parent process
struct trapframe *tf;       //Trap frame for current syscall
struct context *context;    //swtch() here to run process
void *chan;                 //If non−zero, sleeping on chan ??
int killed;                 //If non−zero, have been killed
struct file *ofile[NOFILE]; //Open files
struct inode *cwd;          //Current directory
char name[16];              //Process name (debugging)
};

trapframe is not valid when process is running in user mode as contents of trapframe are going to change.


userinit:

1. allocproc -> allocates entry in process table and sets up kernel stack.

    1.1 acquire lock on process table
    1.2 searches for first unused entry
    1.3 if found, 
        1.3.1 release lock on process table
        1.3.2 mark that entry's state as EMBRYO and pid to nextpid(which is global int set to 1) and increment nextpid
        1.3.3 allocate kernel stack using kalloc (which allocates one page for kernel stack by checking in free list i.e. kmem and returns    physical adress of that page)
        1.3.4 if not enough space
            1.3.3.1 set entry's state as UNUSED and return
        1.3.5 if found
            1.3.5.1 set the sp pointer to virtual address of kernel stack which is p->kstack + KERNBASE
            1.3.5.2 leave space for trapframe i.e. sp = sp - sizeof(trapframe) and p->tf should point end of trapframe which is sp.
            1.3.5.3 leave space for trapret i.e. sp = sp - 4 and put address of trapret onto stack hence sp = sp + 4
            1.3.5.4 leave space for context i.e sp = sp - sizeof(context) and p->context = shou;d point to end of context which is sp.
            1.3.5.5 set memory locations by p->context to 0.
            1.3.5.6 set p->context->eip to forkret

                kernel stack
                 _______________
                |               |
                |               |
                |               |   <- trapframe
                |               |   
                |_______________|
                |_______________|   <- trapret 
                |               |    
                |               |    
                |               |   <- context
                |               |       (p->context->eip = forkret)
                |_______________|
                
    1.4 if not found, release lock on process table and return to main. No space for a new process to run.

2. now, we need physical address space for the process to run. So we need to set pgdir, pgtable and allocate pages. This is due to fact that xv6 uses 2 level paging.
    setupkvm -> setup page table for kernel address space at first (will see this later).

3. init has to execute initcode.S whose binary has to be placed in init's address spce which is done by inituvm
    inituvm -> copies binary of initcode.S into process's memory 

        3.1 allocate a page using kalloc
        3.2 clear the page 
        3.3 mappages create PTEs for virtual addresses starting at va that refer to physical addresses starting at pa. (will come to it later)
        3.4 move binary code to allocated page in 3.1 using memmove.

4. now we need to set user segment registors in trapframe in kstack(1.3.5.2)
    4.1 clear the page allocated to trapframe
    4.2 set cs to SEG_UCODE with user mode 
    4.3 set ds to SEG_UDATA with user mode
        SEG_UCODE and SEG_UDATA will be descriptors in GDT.
    4.4 set es and ss to ds
    4.5 set flag and pagesize.
    4.6 set eip to beginning of initcode.S. But we already know it is at 0. So p->tf->eip = 0
        normally, process start executing at return adress from forkret, but init being first process runs at 0. how?
            -> kstack
                trapframe
                trapret
                forkret?
                context?
            forkret returns to trapret, trapret pops all registers like ueax, ubax, etc. except ucs, ueip, ueflag, uss, usp which are poped by h/w. so ueip should point to beginning of process.
    4.7 set name of directory for debugging purpose ?
    4.8 safestrcpy ?
    4.9 set state of process = RUNNABLE (meaning ready to run, scheduler can schedule it)

now next thing is to call scheduler and start executing init.
mpmain calls scheduler.

mpmain:
1. calls idtinit which calls lidt to load ist registor
2. calls xchg?

// Per−CPU state
struct cpu {
    uchar id;                       //Local APIC ID; index into cpus[] below
    struct context *scheduler;      //swtch() here to enter scheduler
    struct taskstate ts;            //Used by x86 to find stack for interrupt
    struct segdesc gdt[NSEGS];      //x86 global descriptor table
    volatile uint started;          //Has the CPU started?
    int ncli;                       //Depth of pushcli nesting.
    int intena;                     //Were interrupts enabled before pushcli?
    struct cpu *cpu;                // Cpu−local storage variables; see below
    struct proc *proc;              // The currently−running process.
}

so maybe xchg will say that particular cpu has been started and will make common cpu setup. Also will use struct context to switch to the scheduler of that cpu.

3. calls scheduler


scheduler:

// Per−CPU process scheduler.
// Each CPU calls scheduler() after setting itself up.
// Scheduler never returns. It loops, doing:
// − choose a process to run
// − swtch to start running that process
// − eventually that process transfers control
// via swtch back to the scheduler

1. At first, we have disabled all the interrupts. So, we will enable them using sti.
2. acquire lock on process table and loop over all available processes to find out which one is runnable.
3. switch to the process if found. Release of lock should be done by process and reacquire them before coming back to scheduler.
    if not found , release lock on ptable and return.   
    now only initproc is available, so it will switch to it. set per-cpu variable for process(proc) to found process
4. switchuvm will tell hardware to set up target process's table.
    4.1 pushcli?
    4.2 set up TSS(task segment state) to save all kernel registors and kstack and co-processor's data if available.
    4.3 cpu->gdt[].s?
    4.4 set SEG_KDATA and SEG_KSTACK(va) in TSS
    4.5 ltr?
    4.6 lcr3 switch address space. address spce of current process meanimg its pgdir is in p->pgdir which is va. so, need to convert va -> pa by subtracting KERNBASE.
    4.7 popcli?
5. set process's state to running
6. swtch -> saves old context and loads new context. so old = cpu->scheduler(scheduler's context) and new = proc->context
7. switchkvm -> switch to kernel page table after process is done. 
8. when process is done, it should set appropriate state itself  and come back to scheduler
    scheduler set proc variable to 0
    loops again.


initcode.S :

    Its function is to call exec systemcall with arguments init. There is list of system calls maintained with their syscall numbers. So, SYS_exec is macro for sys_exec no.
    How sys_exec works? 

After exec, user-level init will get executed.
init:

1. opens device file named as "console".(need to think?)
2. calls forks
3. child process calls exec with arguments "sh" i.e. shell program.
4. waits for child to terminate

shell:

1. opens three fd i.e. stdin, stdout, stderr.
2. getcmd -> takes input command which you want to execute using stdin.
3. if it is cd, then do chdir and continue
4. else 
    4.1 fork the process
    4.2 parsecmd -> parse the input string 
    4.3 runcmd -> run the parsed command
    4.4 wait for child to complete
    4.5 continue

---------------------------------------------------------------------------------------------------------------------------------

Scheduler is kernel thread and has its own stack but doesn't have a pgdir associated with it, so it is not a process.

Switching context while scheduling:

void swtch(struct context **old, struct context *new);
//Save current register context in old
//and then load register context from new.

Q. what is first two mov instructions doing?
movl 4(%esp), %eax
movl 8(%esp), %edx

eip get changed after ret is called.
need to refer calling convention.

--------------------------------------------------------------------------------------------------------------------------------

fork:

fork the process meaning create a copy of process
1. allocate the process and allocate kstack using allocproc
2. copy process same as parent process uding copyuvm.
3. if allocation fails in step 2, free ksatck and make state of allocated process in process table as UNUSED.
4. set proc->sz, proc->tf same as parent.
5. set proc->parent = parent's process proc
6. set eax reg(return value) to 0 in child's tf
7. now copy open files table(idt should expore this, not needed for now)
8. set proc->state = RUNNABLE if everything went fine.
9. safestrcpy?
10. fork should return pid of child in parent, so proc->pid should be returned.


copyuvm :

create a copy of parent's process table pages
1. cretae kernel mapping of virtual address space using setupkvm.
2. for total parent process's memory using proc->sz, iterate from 0 to proc->sz, pagewise
    2.1 find pte corresponding to that page using walkpgir on parent's pgdir
    2.2 find pa for that pte using macro PTE_ADDR
    2.3 allocate a page using kalloc and move memory contents of pa in 2.2 to this page.
    2.4 now, mapping has to done meaning entry of above allocated page in pgdir of child process using maapages.
3. return allocated pgdir 


exec :

replace the process's memory with given program's image.
1. acquire lock on the inode given bt path using ilock and read the inode from disk.
2. as file is executable, it must be in elf format. So, to ensure file is not corrupted, check for elf->magic_number.
3. create kernel mapping of virtual address space using setupkvm.(why? already there? may be will deallocate the allocated at timeof fork)
4. load the program into memory. Now, as we know, executable has program headers which has rto be loaded into memory. Iterate till ph.num
    4.1 read program segment at ph.off
    4.2 allocate pages amd map them for this program to be loaded using allocuvm
    4.3 load the program segment into pages allocated above using loaduvm.
5. release lock acquired in step1
6. allocate two pages at next page boundary using allocuvm 
    6.1 set first page as guard page and hence non-accessible by user. To do this, clear U flag in pte using clearpteu.
    6.2 use this page for user stack
        6.2.1 set sp point to second page ending from below, as stack grows downwards toward 0.
        Now refer to user stack below to understand further.
        6.2.2 need to refer something?
7. save program name for debugging
8. change proc->pgdir to point to new pgdir allocated above.
9. change proc->esp to sp in 6.2.1
10. change proc->entry to elf.entry
11. proc->sz = sz which is obtained while loading th eprogram in 4. //need to change this for null pointer i think. sz - PGSIZE
12. switch to the process using switchuvm.
13. free the old pgdir and its allocated pages using freevm.

user stack should look like this :

argument 0
...
argument N
0
address of argument 0
...
address of argument N
address of address of
argument 0
argc
0xFFFFFFF
(empty)


allocuvm:

// Allocate page tables and physical memory to grow process from oldsz to
// newsz, which need not be page aligned. Returns new size or 0 on error.
1. iterate for no of pages
    1.1 allocate a page using kalloc
    1.2 clear the page
    1.3 mapp the page.

loaduvm :

// Load a program segment into pgdir. addr must be page−aligned
// and the pages from addr to addr+sz must already be mapped.
1. Iterate over no of pages :
    1.1 locate pte using walkpgdir
    1.2 obtain pa from pte using PTE_ADDR
    1.3 read the inode at offset given into pa. size can be pagesize or less than pagesize for last page.


clearpteu:

clear U flag in pte
1. find pte using walkpgdir
2. unset U flag in that pte.

copyout:


freevm:

free pgdir. First free user part(can be variable in size) and then kernel entries (fixed NPDENTRIES).
1. free user pages using deallocuvm
2. free kernel part. Iterate over NPDENTRIES :
    2.1 get pa from converting va to pa of pte.
    2.2 free it using kfree.
3. free pgdir using kfree.

kfree:

// Free the page of physical memory pointed at by v,
// which normally should have been returned by a
// call to kalloc().
1. fill tha page with garbage to catch dangling ptr refernces.
2. acquire lock on kmem
3. add this page to kmem.freelist
4. release lock.

kalloc :

// Allocate one 4096−byte page of physical memory.
// Returns a pointer that the kernel can use.
// Returns 0 if the memory cannot be allocated.
1. acquire lock on kmem
2. remove this page to kmem.freelist
3. release lock.
4. return ptr to that page,

deallocuvm:

deallocate all user pages from KERNBASE to 0. //need to change to KERNBASE to PAGESIZE
1. find toatl no of pages
2. iterate over total pages
    2.1 find pte in pgdir using walkpgdir
    2.2 compute pa using pte 
    2.3 free that page using kfree
    2.4 set that pte to 0.

------------------------------------------------------------------------------------------------------------------------------------

Trap handling:

initcode.S
We have seen that this code does exec and then user level program init runs which will fork and run shell thereafter.
How exec works?

start:(7707)

1. push arguments to user stack ?
2. mov system call number to eax
3. There are idt for each interrupt which is identified by interrupt no. For syscall, interrupt no is 64(T_SYSCALL). Interrupt can be called using int instruction. write what does int do?(afterwards)

There is table idt (interrupt descriptor table) which has list of interrupt descriptors for interrupt no(256 entries in total). tvinit from main(which calls kinit1, userinit, etc.) sets up all these entries.

tvinit:

1. iterate over 256 entries
    1.1 set interrupt descriptor using SETGATE and dpl = 0(kernel mode)
2. set dpl for syscall interrupt(64) = 1(user mode) so that user code can use syscall using int 64(T_SYSCALL). See note below.

NOTE: xv6 doesn’t allow processes to raise other interrupts (e.g., device interrupts) with int; if they try, they will encounter a general protection exception, which goes to vector 13.

SETGATE(gate, istrap, sel, off, d) : 
    gate = list of interrupt gate descriptors
    istrap = 1:trap 0:interrupt
    sel: Code segment selector for interrupt/trap handler (SEG_KCODE << 3)
    off: Offset in code segment for interrupt/trap handler
    dpl: Descriptor Privilege Level −
        the privilege level required for software to invoke
        this interrupt/trap gate explicitly using an int instruction

Diff bet trap and interrupt ?
    trap occurs when running process make syscall, otherwise for exception or by other devices, interrupt is genetrated. 

When trap/interrupt occurs, if processor is in user mode, it needs to change from user mode to kernel mode(using switchuvm). Also, while switching, registors should be saved on kernel stack not on user stack.

//from the book
When a trap occurs, the processor hardware does the following. If the processor
was executing in user mode, it loads %esp and %ss from the task segment descriptor,
pushes the old user %ss and %esp onto the new stack. If the processor was executing
in kernel mode, none of the above happens. The processor then pushes the %eflags,
%cs, and %eip registers. For some traps, the processor also pushes an error word.
The processor then loads %eip and %cs from the relevant IDT entry.
xv6 uses a Perl script (2950) to generate the entry points that the IDT entries point
to. Each entry pushes an error code if the processor didn’t, pushes the interrupt num­
ber, and then jumps to alltraps.
Syscall records the return value of the system call function in %eax. When the
trap returns to user space, it will load the values from cp­>tf into the machine regis­
ters. Thus, when exec returns, it will return the value that the system call handler re­
turned

Had to see perl script??
skipping stack setup and switching mode for now(alltraps).

what alltraps does some work and then call trap.

trap:

1. It checks whether int T_SYSCALL is used to call trap.
2. calls syscall.

syscall:

call sys[syscall_no]. //system call no is stored in eax in tf.
//list of syscall[] referring to particular system call no

sys_exec:
I don't understand, need to understand setting up user stack.

