# xv6 explaination

**Reference :** MIT6_828F12_xv6-book-rev7.pdf

## Section 1 - Loading the Kernel

BIOS loads first sector(512 bytes)(known as bootloader) into a predefined location (0x7c00 for xv6) and jumps to it. It is responsibility of bootloader to find where is rest of kernel in the device and load it into memory and give control to it(jump to first instruction).
If the system has multiple processors, first processor boots up and initializes other processors.

Start at 8409 :
When system boots, it always starts in 16-bit real mode.
.code16
start:

1. Disable all interrupts
    When interrupt occurs i.e. knocking on INTR pin of processor, it will switch the execution to handler if interrupt bit in flag registor is set to 1. What we want is no disturbance until we set kernel and interrupt handlers, as BIOS has setup its own handlers, it is no longer appropriate or safe to handle interrupts from hardware devices. We want to reinitialize the system completely. So, cli instruction will set interrupt bit to 0 in flag registor. 
2. Load all segments and set them to 0.
    Initially, we want flat address space. So, va = pa.
    Why?
        When it will enter into protected mode, it won't have paging. Segmentation is only there. Segmentation meaning, pa = va + base. For mapping virtual address exactly to physical address(i.e. identical mapping), base = 0. 
3. For entering into protected mode, load GDT. va = pa, initially.
    Why va = pa?
        higher addresses may not be available for some machines.
    load GDT:
        meaning initialize gdt descriptor which has base and limit. base = address of gdt and limit = size of (gdt) - 1 (adress of gdt desc - adress of gdt - 1).
        First descriptor in GDT is always null descriptor.
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


4. Enter protected mode.
    set protected enabled bit in control registor #0 (CR0)
    Note: There are control registore required by processor CR0 - CR4. Each bit in CR has different and special meaning.
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
    2.2 set page boundary pa -= offset % SECTSIZE 
    2.3 offset = (offset / SECTSIZE) + 1; //elf sectors start at 1, sector 0 is bootloader
    2.4 loop to required no, read that sector into memory given by pa using readsect(not required how it works.)
3. Check for magic number in elf header to verify it is not corrupted.
4. Elf has field called elf->phoff which specifies offset of program headers(list of program headers.These specifies where program should be loaded into memory and where it is present in disk and what is it's size). Elf also has field elf->phnum which specifies number of program headers available.
5. Iterate over each program header 
    5.1 pa = (uchar*)ph−>paddr; //get memory location where it should be loaded (Refer the note below)
    5.2 readseg(pa, ph−>filesz, ph−>off); //read disk at offset ph->off into pa, ph->filesz bytes.
    5.3 optimization, ph has two diff field memsz and filesz. So, if(memsz > filsz), set rest to 0. Meaning, disk has filesz of program available, so while loading into memory which is of memsz, set memsz-filesz = 0.
6. get starting point of execution of kernel using elf->entry and cast to function pointer and switch to it.

Note: Kernel generally loads at 0x100000 (pa) because first 1MB is reserved for memory mapped devices.

Q. What are memory mapped devices? 
-->Memory mapped I/O is a way to exchange data and instructions between a CPU and peripheral devices attached to it. Memory mapped IO is one where the processor and the IO device share the same memory location(memory),i.e.,the processor and IO devices are mapped using the memory address.

entry:
    Kernel has to enable paging and setup high virtual adresses.
    elf has given entry point in virtual address space, but we haven't set up va yet. So, V2P_WO will subtract KERNBASE from it(will see later).    
1. Turn on paging
2. set up stack pointer at high address in kernel va.
3. jump to main.

1. Set up large pages for kernel.(There are large pages of size 4MB and small pages of size 4KB, mostly kernel uses large pages and user process uses small pages.)
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
    2GB + 4MB  --> second entry in entrypgdir which will map to 0-4MB in pa space(at index = 512 in entrypgdir)
    2GB
    ...
    4MB        --> first entry in entrypgdir which will map to 0-4MB in pa space(at index = 0 in entrypgdir)
    0

    KERNBASE = 0x80000000 and PDXSHIFT = 22 bits(largs page size is 4MB i.e 22 bits)
    Kernel will run above 2GB(KERNBASE) in va always and user will run from 0 to 2GB. As soon as, we setup kernel address space, kernel will shift to it and remove 0-4MB mapping in va space. This mapping was needed only to setup kernel at KERNBASE.

3. Turn on paging by seeting PG(page enabled) bit in cr0(also WP bit, not imp what it is for now).
4. Set up stack pointer in va space.
    movl $(stack + KSTACKSIZE), %esp 
    stack is variable declared by kernel which has address above KERNBASE. esp = KERNBASE + somthing + KSTACKSIZE and will grow downwards.
5. jump to main which has va, so to shift from different address spaces, pc relative jump don't work. Hence indirect jump to main.

Now, esp and eip both will point above KERNBASE.

## Section 2 - Setting up Kernel page directory!

main:

Every process has two mappings- kernel side and user side. When process is formed, it first get memory in kernel space which will map to pa and that pa will be used to create user side mapping. Also, each process has two stack - user stack and kernel stack.

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
This is to be done!

kvmalloc :
    area required for kernel pgdir is taken from kernel heap.
    1.1 allocate kernel pgdir using setupkvm. 
    1.2 switch to kernel pgdir using switchkvm.

Q. what happens to entrypgdir?
-> It is static array allocated, so can't be deallocted while kpgdir is allocated through kernel heap and hence can be deallocated.
 
setupkvm:
    set up kernel page table which is used by scheduler process. Setupkvm returns pointer to kpgdir which is in va.
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
// lazy allocation meaning don't allocate everything at starting

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
    load cr3 with kernel pgdir(convert va->pa using v2p, remember setupkvm returns pointer in va space i.e. kpgdir)

## Section 3 - Trap handling

There is table idt (interrupt descriptor table) which has list of interrupt descriptors for interrupt no(256 entries in total).Each entry consists of %cs and %ip to be used while handling that trap. Interrupts 0-31 are defined as software interrupts like divide by 0, floating point exception, page fault, etc. 32-63 are used for hardware interrupts and 64 is used for system call.  tvinit from main sets up all these entries but doesn't enable it.

**Note** : Each processor has to enable interrupt handlers which will be done in startothers() and mpmain() which will see later.

// Gate descriptors for interrupts and traps  
struct gatedesc {
uint off_15_0 : 16; // low 16 bits of offset in segment
uint cs : 16;       // code segment selector
uint args : 5;      // # args, 0 for interrupt/trap gates
uint rsv1 : 3;      // reserved(should be zero I guess)
uint type : 4;      // type(STS_{IG32,TG32})
uint s : 1;         // must be 0 (system)
uint dpl : 2;       // descriptor(meaning new) privilege level
uint p : 1;         // Present 
uint off_31_16 : 16;// high bits of offset in segment
};

SETGATE(gate, istrap, sel, off, d) : 
    gate = a particular interrupt descriptor of type gatedesc
    istrap = 1:trap 0:interrupt
    sel: Code segment selector for interrupt/trap handler (SEG_KCODE << 3)
    off: Offset in code segment for interrupt/trap handler(vector in vector table which is set in perl script)
    dpl: Descriptor Privilege Level −
        the privilege level required for software to invoke
        this interrupt/trap gate explicitly using an int instruction

tvinit:

1. iterate over 256 entries
    1.1 set interrupt descriptor using SETGATE and dpl = 0(kernel mode)
2. set dpl for syscall interrupt(64) = 1(user mode) so that user code can use syscall using int 64(T_SYSCALL). See note below.

**Note**: xv6 doesn’t allow processes to raise other interrupts (e.g., device interrupts) with int; if they try, they will encounter a general protection exception, which goes to vector 13.

Q. Diff bet trap and interrupt ?
-> Trap occurs when running process make syscall, otherwise for exception or by other devices, interrupt is genetrated. 

When trap/interrupt occurs, if processor is in user mode, it needs to change from user mode to kernel mode(using switchuvm). Also, while switching, registors should be saved on kernel stack not on user stack.

When a trap occurs, the processor hardware does the following. If the processor was executing in user mode, it loads %esp and %ss from the task segment descriptor, pushes the old user %ss and %esp onto the new stack. If the processor was executing in kernel mode, none of the above happens. The processor then pushes the %eflags, %cs, and %eip registers. For some traps, the processor also pushes an error word.
The processor then loads %eip and %cs from the relevant IDT entry. xv6 uses a Perl script to generate the entry points(vectors) that the IDT entries point to. Each entry pushes an error code if the processor didn’t, pushes the interrupt num­ber, and then jumps to alltraps.

alltraps :

1. build trapframe (meaning push ds, es, fs, gs registors into kstack) i.e. general purpose registors.
2. set up data segments for kernel 
3. push kernel stack pointer
4. call the trap.

trap:

1. Find the trap no in tf->eax.
2. If it is trap no 64 i.e T_SYSCALL, 
    2.1 Check if the processor is killed or not.
    2.2 call syscall.
3. Else find the appropriate handler for trap occured and do the appropritae action.

Q. who sets trapno in trapframe?
-> Trapframe is present in proc structure available for each process. when we push the arguments onto stack, they are pushed in such a way that it matches the stryucture of trapframe.(still, i am not sure!)

trapret :
    
1. Pops offs other user's registors that are not poped by hardware(which are ss, sp, flag, cs, ip) and errocode if any.
2. iret so that hardware can pops off remaining registors and change back to user mode.

Q. What happens when you execute `int n` instruction?
->
1. Fetch nth descriptor from idt. 
2. Priviledge level check! (check if DPL(pl of descriptor) >= CPL(current pl, rememeber high pl has low numerical value)
3. Check for selectors's pl(which is selected by GATE descriptor) < CPL, if it so, that means process is running in user mode so need to save %esp and %ss registors.
4. Load %ss and %sp for kernel stack from TSS(remember, each process has it's own TSS which is used at time of context switching and mode switching).
 TSS descripto to be selected from GDT and index is given by TR registor(done by switchuvm, will see this in detail later).
5. Push %ss, and %esp and load from TSS(this is optional, done only if process is running in user mode.), push %eflag, %cs, %eip. NOw, while handling interrupt, we don't want another interrutp so clear IF(Interrupt flag) bit in %eflag. Also, load %cs(selector) and %ip(offset) using gate descriptor fetched in step 1.
6.%cs:%ip will point to particular entry of vector for example if n = 64:
vector64:
    pushl $0
    pushl $64
    jmp alltraps

It will push 0(or errorcode if any), then trapno i.e 64 in this case, onto the stack. Now it will jump to alltraps.
6. Now, esp is pointing to trap no in kernel stack and we need to pass this trap no as argument to trap function which handle all traps. So, passing arguments through stack. For this, push esp, call trap. After trap returns, pop the pushed esp and you are back to same kernel stack as before. 
7. when returns from trap, it first pops esp pushed, then call trapret.
8. We have stack having contents like this:
ss, esp,eflags, cs, eip, 0 (for error code), 64, ds, es, fs, gs, eax, ecx, edx, ebx, oesp, ebp, esi, edi, esp
So, first pop all pops all general purpose registors from esp to eax, Then pops gs, fs, es and ds. Then pops trap no and errorcode (which is 0 in this case). Then, it calls iret which pops off all remaining user registors and changes mode back to user mode.


## Section 4 - Creating the first process

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

Q. what is kernel and user stack?
-> Each process has two stacks: a user stack and a kernel stack. When the process is executing user in­structions, only its user stack is in use, and its kernel stack is empty. When the pro­ cess enters the kernel (via a system call or interrupt), the kernel code executes on the process’s kernel stack; while a process is in the kernel, its user stack still contains saved data, but isn’t actively used. A process’s thread alternates between actively using the user stack and the kernel stack. The kernel stack is separate (and protected from user code) so that the kernel can execute even if a process has wrecked its user stack.
There is thirs stack which is used by scheduler processes.(we have seen this, in kvmalloc())


Q. What is trapframe?
-> Whenever control transfers into the kernel while a process is running, the hardware and xv6 trap entry code save user registers on the process’s kernel stack. OS writes values at the top of the new stack that look just like those that would be there if the process had entered the kernel via an interrupt, so that the or­ dinary code for returning from the kernel back to the process’s user code will work.
These values are a struct trapframe which stores the user registers.

Q. What is context?
-> When kernel is executing in place of process, the registors it requires get copies from kernel stack which is pointed by p->context.  

Q. If there is context, what is the need for TSS?
-> I think TSS stores user's registors and stack values for every priviledge level and interrupt table while context store kernel's registors.


**Note :** 
1. The ss and sp of kernel stack are stored in TSS.
2. Trapframe is not valid when process is running in user mode as contents of trapframe are going to change.
3. Suppose two processes are running, above KERNBASE, two diff kstacks and two disjoint set of pages(given by respective pgdir).


### Creating the first process :

Code of the first process:
initcode.S and init.c
init.c is compiled into “/init” file During make !
xv6 approach:
Use initcode.S to “exec(“/init”)” and let exec() do rest of the job

Normally, processes fork another process and then exec but we don't have a parent process, we need to build process by hand as if it was created by fork.

1. We need PCB for running any process. So, need to allocate one. Allocproc() does this. It checks UNUSED entry in proc table and if found, 
changed the state to EMBRYO (meaning only PCB is allocated and kernel satck and page directory is not allocated).
2. Allocate kernel stack for this process and if everything goes fine, change the state to UNUSED.
3. Now, we need to setup kernel stack. Kernel stack is setup in following manner. 
4. First, we setup trapframe which is used to save user's registors. 
5. Now, iret instructions pops off only 5 registors - cs, ip, ss, sp and flag, rest others also need to pop off, so this is done by trapret function. By calling convention, trapret return address is stored after trapframe. 
6. forkret is the function which returns to trapret meaning address of forkret is stored just after trapret. This is the case for the forked process but xv6 does not consider special case for first process.
7. Next thing is before doing anything in kernel side, it saves kernel registors in its context.  
8. In between context and forkret, there can be many chain of function calls.

    esp | <- top of new stack
    ... |
    eip |
    ... |
    edi | <- p->tf
 trapret| <- address at which forkret will return
    eip |
    ... |
    edi | <- p->context
  empty | <- p->kstack

9. Now, we need to create a page table for the process with (at first) mappings only for memory that the kernel uses. 
The initial contents of the first process’s memory are the compiled form of init­ code.S.
10. Once the process is initialize, set the state to RUNNABLE so that scheduler can schedule it for running.

userinit:

1. allocproc -> allocates entry in process table and sets up kernel stack.

    1.1 acquire lock on process table
    1.2 searches for first unused entry
    1.3 if found, 
        1.3.1 release lock on process table
        1.3.2 mark that entry's state as EMBRYO and pid to nextpid(which is global int set to 1) and increment nextpid
        1.3.3 allocate kernel stack using kalloc (which allocates one page for kernel stack by checking in free list i.e. kmem and returns physical adress of that page)
        1.3.4 if not enough space
            1.3.3.1 set entry's state as UNUSED and return
        1.3.5 if found
            1.3.5.1 set the sp pointer to virtual address of kernel stack which is p->kstack + KERNBASE
            1.3.5.2 leave space for trapframe i.e. sp = sp - sizeof(trapframe) and p->tf should point end of trapframe which is sp.
            1.3.5.3 leave space for trapret i.e. sp = sp - 4 and put address of trapret onto stack hence sp = sp + 4
            1.3.5.4 leave space for context i.e sp = sp - sizeof(context) and p->context should point to end of context which is sp.
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
    setupkvm -> setup page table for kernel address space at first.

3. init has to execute initcode.S whose binary has to be placed in init's address space which is done by inituvm
    inituvm -> copies binary of initcode.S into process's memory 

        3.1 allocate a page using kalloc
        3.2 clear the page 
        3.3 mappages create PTEs for virtual addresses starting at va that refer to physical addresses starting at pa. 
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
                forkret
                context
            forkret returns to trapret, trapret pops all registers like ueax, ubax, etc. except ucs, ueip, ueflag, uss, usp which are poped by h/w. so ueip should point to beginning of process.
    4.7 set name of directory for debugging purpose.
    4.8 safestrcpy to copy name of process into proc struct.
    4.9 set state of process = RUNNABLE
now next thing is to call scheduler and start executing init.
mpmain calls scheduler.

mpmain:
1. calls idtinit which calls lidt to load idt
2. calls xchg

// Per−CPU state
struct cpu {
    uchar id;                       //Local APIC ID; index into cpus[] below
    struct context *scheduler;      //swtch() here to enter scheduler
    struct taskstate ts;            //Used by x86 to find stack for interrupt
    struct segdesc gdt[NSEGS];      //x86 global descriptor table
    volatile uint started;          //Has the CPU started?
    int ncli;                       //Depth of pushcli nesting.
    int intena;                     //Were interrupts enabled before pushcli
    struct cpu *cpu;                // Cpu−local storage variables; see below
    struct proc *proc;              // The currently−running process.
}

so maybe xchg will say that particular cpu has been started and will make common cpu setup. Also will use struct context to switch to the scheduler of that cpu.

3. calls scheduler

Please refers to scheduler section below.

### How system call works?

1. Push the arguments required by the syscall and then push return address of the function.
2. Put system call no which you want into eax registor.
3. execute int instruction with trap no assigned to make syscall which is 64(macro - T_SYSCALL) in xv6.
Now what does syscall do?
1. it check the syscall no in eax is valid no or not.
2. If it is, call appropriate function.

initcode.S :

    Its function is to call exec systemcall with arguments init. As we know, the format required for arguments of exec - {name of program, arguments .. , NULL}. There is list of system calls maintained with their syscall numbers. So, SYS_exec is macro for sys_exec no.

    The helper functions argint and argptr, argstr retrieve the n’th system call argument, as either an integer, pointer, or a string.
    argint uses the user­space %esp register to locate the n’th argument: %esp points at the return address. The arguments are right above it, at %esp+4. Then the nth argument is at %esp+4+4*n.

    argint calls fetchint to read the value at that address from user memory and write it to *ip. fetchint can simply cast the address to a pointer, because the user and the kernel share the same page table, but the kernel must verify that the pointer by the user is indeed a pointer in the user part of the address space. argstr interprets the nth argument as a pointer. It ensures that the pointer points at a NUL­L terminated string and that the complete string is located below the end of the user part of the address space.

    sys_exec :
        1. fetch 0 argument i.e. name of the program as str into path.
        2. fetch 1 argumnet i.e. {arguments .. , NULL} as int.
        3. now loop over all the arguments :
            3.1 fetch it as int and check validity
            3.2 convert it into str
            3.3 save it into array
        4. call exec with path and array(argv).
    
    exec will execute(will see detailed explanation in Section3)

After exec, user-level init will get executed.
init:

1. opens device file named as "console".
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

## Section 5 - Fork and Exec

fork:

sys_fork()->fork();

fork the process meaning create a copy of process
1. get the current proc using myproc() and then allocate the process and allocate kstack using allocproc
2. copy process same as parent process using copyuvm.
3. if allocation fails in step 2, free ksatck and make state of allocated process in process table as UNUSED.
4. set proc->sz, proc->tf same as parent.
5. set proc->parent = parent's process proc
6. set eax reg(return value) to 0 in child's tf
7. now copy open files table
8. set proc->state = RUNNABLE if everything went fine.
9. safestrcpy copies name of program.
10. fork should return pid of child in parent, so proc->pid should be returned.


copyuvm :

create a copy of parent's process table pages
1. create kernel mapping of virtual address space using setupkvm.
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
3. create kernel mapping of virtual address space using setupkvm.   
4. load the program into memory. Now, as we know, executable has program headers which has to be loaded into memory. Iterate till ph.num
    4.1 read program segment at ph.off
    4.2 allocate pages amd map them for this program to be loaded using allocuvm
    4.3 load the program segment into pages allocated above using loaduvm.
5. release lock acquired in step1
6. allocate two pages at next page boundary using allocuvm 
    6.1 set first page as guard page and hence non-accessible by user. To do this, clear U flag in pte using clearpteu.
    6.2 use this page for user stack
        6.2.1 set sp point to second page ending from below, as stack grows downwards toward 0.
        Now refer to user stack below to understand further.
        6.2.2 loop over arguments:
            6.2.2.1 move new process stack pointer so there's room for current argument
            6.2.2.2 copy argument to actual new process user-stack
            6.2.2.3 make argv vector entry (which is still in temporary ustack variable!) point to current argument that we just pushed to stack
            6.2.2.4 put 0 in last entry of argv vector (which is still in temporary ustack variable!), as is expected by convention
            6.2.2.5 put -1 as return address in appropriate spot in temporary ustack variable
            6.2.2.6 put argc in appropriate spot in temporary ustack variable
            6.2.2.7 put address of where argv will be in new user-stack (but isn't there yet) in appropriate spot in temporary ustack variable
            6.2.2.8 now that all arguments are copied to new user-stack, and we know where to place return address & argc & argv, copy ustack variable to new process user-stack
7. save program name for debugging
8. change proc->pgdir to point to new pgdir allocated above.
9. change proc->esp to sp in 6.2.1
10. change proc->entry to elf.entry
11. proc->sz = sz which is obtained while loading th eprogram in 4. 
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

// Copy len bytes from p to user address va in page table pgdir.

1. get va offset within its page using uva2ka
2. copy data using memmove


uva2ka :

1. Find out if the address given in user sapce or not.
2. If it is, return physical offset in pte for that va.

freevm:

//Free a page table and all the physical memory pages in the user part.
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

deallocate all user pages from KERNBASE to 0. 
1. find total no of pages
2. iterate over total pages
    2.1 find pte in pgdir using walkpgdir
    2.2 compute pa using pte 
    2.3 free that page using kfree
    2.4 set that pte to 0.

## Section 6 - Scheduler

scheduler:

Scheduler is kernel thread and has its own stack but doesn't have a pgdir associated with it, so it is not a process. Every xv6 process has its own kernel stack and register set. Each CPU has a separate scheduler thread for use when it is executing the sched­uler rather than any process’s kernel thread. Switching from one thread to another in­volves saving the old thread’s CPU registers, and restoring previously­ saved registers of the new thread; the fact that %esp and %eip are saved and restored means that the CPU will switch stacks and switch what code it is executing.

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
4. switchuvm will tell hardware to set up target process's table and switch TSS.
    4.1 pushcli - pops eflag and cli
    4.2 set up TSS(task segment state) to save all kernel registors and kstack and co-processor's data if available.
    4.3 cpu->gdt[].s - There are two types of descriptor system and segment which is denoted by s bit in ARB(remember Access Right Bytes in descriptor) of descriptor. So, TSS requires system descriptor. 
    4.4 set SEG_KDATA and SEG_KSTACK(va) in TSS
    4.5 ltr - load task register
    4.6 lcr3 switch address space because we are still in kpgdir created in entry.S. address space of current process meanimg its pgdir is in p->pgdir which is allocated in setupkvm in userinit, which is va. so, need to convert va -> pa by subtracting KERNBASE.
    4.7 popcli - pos eflag and sti
5. set process's state to running
6. swtch -> saves old context and loads new context. so old = cpu->scheduler(scheduler's context) and new = proc->context.(will see this later.)
7. switchkvm -> switch to kernel page table after process is done. 
8. when process is done, it should set appropriate state itself  and come back to scheduler
    scheduler set proc variable to 0
    loops again.

Switching context while scheduling:

Before calling this function, new(p->context) and old(&c->scheduler) are push onto the stack i.e arguments and then ret value of address of scheduler() as per calling conventions.
void swtch(struct context **old, struct context *new);
//Save current register context in old
//and then load register context from new.

Swtch starts by loading its arguments off the stack into the registers %edx(p->context) and %eax(c->scheduler); swtch must do this before it changes the stack pointer and can no longer access the arguments via %esp. Then swtch pushes the register state, creating a context structure on the current stack. Only the callee­ save registers need to be saved; the convention on the x86 is that these are %ebp, %ebx, %esi, %ebp, and %esp.

Now it makes old to point to esp and esp to point to new. Meaning, the stack on which we are since creation is now stored in c->scheduler pointer and current esp is pointing to new one meaning p->context(remember the stack structure of p->kstack). Then, we pop off all the registors and then ret, our stack contains eip which is pointing to forkret, so now we are in forkret now. Remeber, we taked about process has to release the lock on ptable which is done by forkret. Also, if it is the first process, then some initilization which will see later. Now, forkret return to trapret, trap ret pops off all general purpose registors and call iret. Now, cs and ip are pointing to initcode.S executable and pgdir and stack is setup.

sched :

    called from exit(), yield(), sleep() 
    1. get the current proc structure using myproc.
    2. does some error checks
    3. get interrupt enabled status on current CPU
    4. calls swtch. So, this saves the context in p->context and switches to scheduler stck from kernel stack.
    5. after comming back, set the interrupt eanbled status back to previous one.


Combinig all this :

Consider this example, 

1. On a timer interrupt, trap() is called. Stack has changed from user stack to kernel stack.
2. trap() calls yield() which in turn calls sched(.)
3. sched() ->  swtch(&p->context, c->scheduler(). This saves the context in p->context and stack changes to scheduler’s kernel stack. 
4. Switches to location after swtch in scheduler().
5. Now the loop in scheduler()
6. calls switchkvm()
7. Then continues to find next process to run 
8. then calls swtch(&c->scheduler, p->context) (next process's context)
9. Stack changes to process kernel stack. 
10. Process runs the last instruction it was was in i.e. mycpu()->intena = intena; in sched()
11. Then returns to the one who called sched() i.e. exit/sleep, etc
12. Finally returns from it’s own *TRAP* handler and returns to process’s user stack and user code.

## Section 7 - File system

The xv6 file system implementation is organized in seven layers :

File descriptor |
Pathname |
Directory |
Inode |
Logging |
Buffer cache |
Disk |

Each block is of 512 bytes in xv6.

Arrangements of blocks :
Block 0 | Bootloader |
Block 1 | Superblock(contains metadata about file system) |
Block 2 | log |
After log | inodes |
After inodes | bitmap blocks (to indicate which data blocks are free 0-free;1-in use) |
Remaining | Data blocks |

### Disk

The code is in ide.c
idequeue points to the buf now being read/written to the disk. idequeue−>qnext points to the next buf to be processed.
idelock is the spinlock on idequeue.
B_VALID: the buffer data has been read from the disk.
B_DIRTY: the buffer data has been modified and needs to be written back to disk.

Functions : 

1. static int idewait(int checkerr);
--> Wait for IDE disk to become ready.

2. static void idestart(struct buf *b);
--> Start the request for b. Caller must hold idelock.
    2.1 Get sector no.
    2.2 Issue read/write command to IDE controller. 

3. void ideinit(void);
--> caled from main(). Initialized IDE controller by writing to certain ports and initialize idelock.

4. void ideintr(void);
--> Interrupt handler for ide interrupt.
    4.1 Acquire lock on ide
    4.2 If idequeue is empty, return. Else get the first buffer to be processed and mve the queue one step.
    4.3 Read data if needed. Set B_VALID and unset B_DIRTY.
    4.4 wakeup the process who is waiting for this buffer.
    4.5 If idequeue is not empty, call idestart().
    4.6 Release lock.

Wakeup :
    1. acquire lock on ptable
    2. call wakeup1()
    3. Release the lock.

Wakeup1 :
    look for the process in process table which is sleeping on that channel.

5. void iderw(struct buf *b);
--> Sync buf with disk. If B_DIRTY is set, write buf to disk, clear B_DIRTY, set B_VALID. Else if B_VALID is not set, read buf from disk, set B_VALID.
    5.1 Acquire ide lock
    5.2 Append buff b to idequeue
    5.3 call idestart on firts buffer in idequeue.
    5.4 Release lock

### Buffer cache

The code is in bio.c

The buffer cache is a doubly linked list of buf structures holding cached copies of disk block contents. The buffer cache layer consists of bread and bwrite; the former obtains a buf containing a copy of a block which can be read or modified in memory, and the latter writes a modified buffer to the appropriate block on the disk.

Interface:
* To get a buffer for a particular disk block, call bread.
* After changing buffer data, call bwrite to write it to disk.
* When done with the buffer, call brelse.
* Do not use the buffer after calling brelse.
* Only one process at a time can use a buffer, so do not keep them longer than necessary.

struct buf {
  int flags;  // 0 or B_VALID or B_DIRTY
  uint dev;   // device number
  uint blockno; // seq block number on device 
  struct sleeplock lock; // Lock to be held by process using it
  uint refcnt; // Number ofaccesses to the buf 
  struct buf *prev; // LRU cache list
  struct buf *next; // LRU cache list
  struct buf *qnext; // disk queue 
  uchar data[BSIZE];  // data 512 bytes 
};

struct {
struct spinlock lock;
struct buf buf[NBUF];
// Linked list of all buffers, through prev/next.
// head.next is most recently used.
struct buf head;
} bcache;

Functions :

1. void binit(void);
--> called from main(). It creates linked list of buffers.

2. static struct buf* bget(uint dev, uint blockno);
--> Look through buffer cache for block on device dev. If not found, allocate a buffer. In either case, return locked buffer.
    2.1 acquire lock on bcache.
    2.2 loop over bcache to find if the block is already cached meaning buffer is already present for that block no. As head->next is MRU, we search head->next. If found, increment refernce count.
    2.3 If not found in 2.2, loop over bcache to find if any block is unused meaning whose reference count is 0 and B_DIRTY bit is not set, allocate the buffer. As head->prev is LRU, we search head->prev.
    2.4 Returns the looked buffer with no contents.

3. struct buf* bread(uint, uint);
--> Return a locked buf with the contents of the indicated block.
    3.1 get the buffer using bget for that block no.
    3.2 check if the B_VALID bit is set or not.
    3.3 if yes, issue read command using iderw on that buffer.

4. void brelse(struct buf*);
--> Release a locked buffer. Move to the head of the MRU list.
    4.1 Release the lock
    4.2 Decrease the reference count
    4.3 if ref_cnt is 0, then move the buffer to front of head of bcache list so while accessing head->prev, it will be the first one.

5. void bwrite(struct buf*);
--> Write b’s contents to disk. Must be locked.
    3.1 get the buffer using bget for that block no.
    3.2 set the B_DIRTY bit
    3.3 issue write command using iderw on that buffer.  

**Note** : How iderw knows whether to read or write? This is done using B_VALID/B_DIRTY bits. If b_DIRTY is present, issue write command and if B_VALID, issue read command. 

### Logging

The code is in log.c
Xv6 solves the problem of crashes during file system operations with a simple form of logging. An xv6 system call does not directly write the on-disk file system data structures. Instead, it places a description of all the disk writes it wishes to make in a log on the disk. Once the system call has logged all of its writes, it writes a special commit record to the disk indicating that the log contains a complete operation. At that point the system call copies the writes to the on-disk file system data structures. After those writes have completed, the system call erases the log on disk.

1. A system call calls begin_op() at begining and end_op() at end.
2. During the code of system call, whenever a buffer is modified, (and done with) log_write() is called.
3. when finally commit() is called, all modified blocks are copied to disk. 

Log consists of a header block followed by a sequence of updated block copies (*logged blocks*).

A typical use of the log in a system call looks like this:
begin_op();
...
bp = bread(...);
bp->data[...] = ...;
log_write(bp);
...
end_op();

struct logheader {
int n;
int block[LOGSIZE];
};

struct log {
struct spinlock lock;
int start;
int size;
int outstanding; // how many FS sys calls are executing.
int committing; // in commit(), please wait.
int dev;
struct logheader lh;
};

begin_op() :
    After doing some checks, it will increments log.outstanding. 
    1. If some process is commiting, sleep
    2. If log space is exhausted, wait for someone to commit.
    3. Else, increment log.outstanding

end_op() :
    1. Decrement log.outstanding
    2. If someone is commiting, panic as if someone is commiting, begin_op() should sleep so not possible.(I think)
    3. If log.outstanding is 0, then set log.commiting = 1 , else wakeup as begin_op is waiting.
    4. if condition is true in 3, then call commit() and log.commiting = 0 and wakeup.

commit() :
    1. Write modified blocks from cache to log
    2. Write header to disk −− the real commit
    3. Copy committed blocks from log to their home location
    4. Erase the transaction from the log

log_write() :
    Caller has modified b−>data and is done with the buffer. Record the block number and pin in the cache with B_DIRTY. commit() will do the disk write.

### Block allocator

The code is in fs.c
The block allocator provides two functions: balloc allocates a new disk block, and bfree frees a block.
struct superblock {
    uint size; // Size of file system image (blocks)
    uint nblocks; // Number of data blocks
    uint ninodes; // Number of inodes.
    uint nlog; // Number of log blocks
    uint logstart; // Block number of first log block
    uint inodestart; // Block number of first inode block
    uint bmapstart; // Block number of first free map block
};

balloc :
    Allocate a zeroed disk block.
    1. loop over blocks in file system.
    2. read the block in buffer
    3. check if block is free using block bitmap.
    4. if found, set bit to 1, indicating block is in use.
    5. call log_write()
    6. release the buffer using brelse
    7. zero the contents in th eblock
    8. return the block.

bfree :
    Free a disk block.
    1. read the block in buffer using bread.
    2. find the bit in bitmap for that corresponding block and clear it.
    3. call log_write()
    4. release the buffer using brelse







