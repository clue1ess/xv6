# Operating system concepts

**Meta note** : This explanation assume that you have enough understanding of computer organization and assembly language.

**References** : My class notes of professor Abhijit A.M.(Operating system) and professor A.A Sawant(Microprocessor Techniques, Computer organization, Advanced Microprocessor Techniques) and video leactures by Sourav Bansal(IITD) and some web links(mentioned in respective sections.)

## What is OS?

OS = Kernel(will se this later) + System programs(apllication programs like compiler, linker, loader, C library, etc.)

## Need of OS:

1. Easy interaction between the human & computer.
2. Starting computer operation automatically when power in turned on.
3. Loading & scheduling users program.
4. Controlling input & output.
5. Controlling program execution.
6. Managing use of main memory.
7. Providing security to users program.

Reference : https://blogs.siliconindia.com/Jyotionnet/Operating-System--Need--Functions-bid-F941ap7D21007231.html


## Kernel

The kernel is a computer program at the core of a computer's operating system with complete control over everything in the system. It is the "portion of the operating system code that is always resident in memory". It facilitates interactions between hardware and software components. On most systems, it is one of the first programs loaded on startup (after the bootloader) (We will see this later). It handles the rest of startup as well as input/output requests from software, translating them into data-processing instructions for the central processing unit. It handles memory and peripherals like keyboards, monitors, printers, and speakers.

Reference : https://en.wikipedia.org/wiki/Kernel_(operating_system)


## How does processor works

Processor has PC(Program Counter) registor which stores the address of next instruction to execute. Steps involved :

1. Fetch the instruction stored at adress given by PC.
2. Decode the instruction, meaning extract operands and operations to be done from given bit pattern.
3. Execute th einstruction.
4. Increment the PC.

Processor is made to work in two modes:

1. Real addressing mode (single task)
2. Virtual addressing mode (multi task)


## Booting process

1. When computer is turned on, PC will point to a location set by manufacturer(typically in ROM) which is BIOS.
2. BIOS initializes hardware devices and has settings which defines boot order. BIOS will scan for existing devices.
3. On first hit, it will load contents of sector 0(512 bytes) of that device into predefined location in RAM, known as bootloader.
4. Bootloader will select OS to load(e.g., grub).
5. It loads selected OS into memory from secondary storage and jump to it.

## Program vs process

Process is a program in execution. Process is an active entity, while a program is a passive entity. This means that a program can be considered as a bunch of code, or sequence of instructions, whereas a process is any such program that is currently active.
One program can run multiple times as multiple processes.
Os has enough information saved about the processes to know which is running, what resources it is using, etc. 

Multi-programming : Multiple application programs reside in memory simultaneously.
Multi-tasking : Multi-programming + time-sharing.

**Note** : xv6 provides time-sharing.

Reference : https://www.thedailyprogrammer.com/2016/08/processes-in-operating-system.html

## Hardware Interrupts

1. I/O devices are connected to system bus and can raise interrutpt for data-transfer. Interrupt means changing PC to some other pre-defined location which is known by OS(this pre-defined locations are occupied by OS at system boot time), that is breaking the normal execution of CPU. 
2. Before going to that location, i.e before changing PC, we have to save context i.e saving PC, saved registors, stack, etc. so that when we come back nothing is changed. 
3. Now, after saving context, OS runs and after its work is completed, it executes *iret* instruction which again gives the control to the saved context. 

**Note** : Hardware interrupts are generally asynchronous, meaning they can occur at any time.

## Priviledge instruction

For single task OS environment, ony one program is loaded into memory. But for multitasking OS environment, multiple programs is loaded into memory simultaneously but only one is executed at a time(which one is to be executed depends on scheduler).For a partricular time slice, one  program is executed and suspended after some time(if it is not completed) and whole context of that process is saved and some other process is scheduled.
Each program has four segments - *code, data. stack, heap*. No program should spoil other program's segments and also that of OS. Hence, CPU has two modes of operation specified by mode bit:
1. User mode (normal instructions can run in this mode).
2. Kernel mode (priviledge instructions can also run in this mode.)

Acutually, there are four priviledge level represented by two bits as :
00 - kernel mode
01 - os services
10 - device drivers
11 - application programs(user mode)

What exactly are priviledge instructructions?
-> accesing I/O, changing mode of operations, execution of ISR(Interrupt Service Routine), schedulling processes, etc.

## Process life-cycle

Process is typically represented by OS data structure(PCB) which maintains all info about that particular process. 
PCB(Process Control Block) or task-struct contains :
1. Process id(pid) which is unique to each process.
2. Details of allocated memory for various segments.
3. Information about file opened.
4. Current state of process(will come bak to it.)

Life cycle of process:

States of process = {CREATE, READY, RUNNING, TERMINATED, WAIT}

                                        timer interrupt
                                    <------------------
                                    |                  |
                                    |                  |
CREATE(using fork and exec) ----> READY ----------> RUNNING ----------> TERMINATED
                                   |    schduler     |       exit()
                                   |                 |
                                   <---- WAIT  <---- |
                                h/w interrupt    wait for I/O




## System calls
 
They are kind of software interrupts. A system call is a way for programs to interact with the operating system. A computer program makes a system call when it makes a request to the operating system’s kernel. System call provides the services of the operating system to the user programs via Application Program Interface(API). It provides an interface between a process and operating system to allow user-level processes to request services of the operating system. System calls are the only entry points into the kernel system. 


User code-->|           |------->user code
            |(INT)      |
            |           |                           User mode
-------------------------------------------------------
            |           |                           Kernel mode
            |---------->|
               OS code

Reference : https://www.geeksforgeeks.org/introduction-of-system-call/

## Fork 

Fork system call is used for creating a new process, which is called child process, which runs concurrently with the process that makes the fork() call (parent process). After a new child process is created, both processes will execute the next instruction following the fork() system call. A child process uses the same pc(program counter), same CPU registers, same open files which use in the parent process.

It takes no parameters and returns an integer value. Below are different values returned by fork().

Negative Value: creation of a child process was unsuccessful.
Zero: Returned to the newly created child process.
Positive value: Returned to parent or caller. The value contains process ID of newly created child process.

Reference : https://www.geeksforgeeks.org/fork-system-call/

## Exec

Exec system call is a functionality of an operating system that runs an executable file in the context of an already existing process, replacing the previous executable. This act is also referred to as an overlay. It is especially important in Unix-like systems, although exists elsewhere. As a new process is not created, the process identifier (PID) does not change, but the machine code, data, heap, and stack of the process are replaced by those of the new program. 

Reference : https://en.wikipedia.org/wiki/Exec_(system_call)

## Wait and exit

The system call wait() is easy. This function blocks the calling process until one of its child processes exits or a signal(Refer https://www.usna.edu/Users/cs/aviv/classes/ic221/s16/lec/19/lec.html for signals). wait() takes the address of an integer variable and returns the process ID of the completed process. Some flags that indicate the completion status of the child process are passed back with the integer pointer. One of the main purposes of wait() is to wait for completion of child processes. 

A process terminates its execution by making an exit system call. The returned value from exit is saved in PCB by OS. PCB is not normally discarded until prent calls wait() on the child.

Zombie process: 
1. A process which still has entry in process table.
2. Child killed itself but parent did not do wait, so resources allocated to child are not released.

Orphan process :
Parent is dead before child.
Generally, first process created called *init* becomes parent of all orphan process and calls wait() on it.

Reference : http://www.csl.mtu.edu/cs4411.ck/www/NOTES/process/fork/wait.html

## Shell

A shell in a Linux operating system takes input from you in the form of commands, processes it, and then gives an output. It is the interface through which a user works on the programs, commands, and scripts.
Refer this https://oskarth.com/unix01/

# Scheduler

Functions:
1. Save the context of the currently running process.
2. Select the next process from the list of runnable process.
3. Load the process's registors and PC.
4. Give control to that process.

## Physical address space

   Physical address space |
   Memory mapped devices |
   unused |
   Extended memory(RAM) |
   VGA memory |
   BIOS/device |
   Low memory |

## Process address space

Every process has its own memory, registors, stack, data, etc., i.e. every process has its own address space. For multi-programming environment, two constraints are there:
1. No process should access another's process address space or kernel's address space.
2. While context switching (i.e. time sharing between process so that each process feels it is getting it's own cpu and memory.)
    context of current process is to be saved and context of other process is to be loaded.

The virtual address space for a process is the set of virtual memory addresses that it can use. The address space for each process is private and cannot be accessed by other processes unless it is shared. A virtual address does not represent the actual physical location of an object in memory.

Reference : https://docs.microsoft.com/en-us/windows/win32/memory/virtual-address-space

### Segmentation :
   va(virtual address) --> MMU(memory management unit) --> pa(physical address)

   Each process has four segments. 
   In real addressing mode:
   pa = va + base(some predefined address like KERNBASE in xv6)
   In virtual addressing mode:
   The info about starting address of each segment, how much space it needed, what is it priviledge level are stored in descriptor of that segment. Such descriptors are stored in GDT(global descriptor table). GDTR registor is used to find base of GDT. lgdt is instruction used to load base address of GDT into GDTR. 

   Decriptor entry :

   size of descriptor is 8 bytes.
   limit can be max 1MB - 1 i.e 20 bits.
   base is of 32 bits.

   first word -> upper 16 bits of limit 
   second word -> lower 16 bits of base
   fifth byte -> next 8 bits of base
   sixth byte -> ARB (Access right bytes i.e. type or permissions) 
   seventh byte -> next 4 bits of limit and some bits(G, Available.., not important for now)
   eighth byte -> upper 8 bits of base

   CS(code segment), DS(data), ES(extra), SS(stack) are known as selector because this segmnets selects appropriate descriptor in gdt and that descriptor give base(starting address) and limit(max size) of that segment. 

   Let's go through an example:

   Suppose we made call instruction(wants to change CS) 
   call cs, ip i.e call selector, offset
   this selector will select cs descriptor from GDT and check if offset is well within the limit and privlidege level is correct or not(meaning higher pl can access lower pl but to do vice-versa we need some special descriptors like CALL GATE descriptor, not neccesary to know.) If everything goes well, base and limit will give actual pa of that segment.

   This is known as segmentation.


### Paging

Physical address space is divided into pages of 4MB(typically). Page table has entries of the pages which are accessible by that process.  Page directory contains list of page tables. Each process has its own page directory. This is 2-level paging. Address converted by segmentation is known as linear adress. Linear adrees is of 32 bits and let's see how it gives pa.

2-LEVEL PAGING :

10      10      12                  
pde     pte     page

pgdir (2^12 entries and top 10 bits of address
will give particular entry in pgdir and that entry    --> pagetable(2^12 entries and next 10 bits will
will contain base address of page table)                            give particular entry in page table and that  --> page(last 12 bits will
                                                                    entry will point to base address of page)           offset in that page)


## TSS
 
Current process's environment has to saved. So, for that system has provision for TSS(Task State Segment). Such space is made available for every task. When task is suspended, its contents are saved in TSS, then trying to refer new TSS wrt new task. TSS contains CS, DS, ES, SS and much more.
