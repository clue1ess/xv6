# Operating system

**Meta note** : This explanation assume that you have enough understanding of computer organization and assembly language.

**Reference** : My class notes of professor Abhijit A.M.(Operating system) and professor A.A Sawant(Microprocessor Techniques, Computer organization, Advanced Microprocessor Techniques) and some web links(mentioned in respective sections.)

## What is OS?

OS = Kernel(will se this later) + System programs(apllication programs like compiler, linker, loader, C library, etc.)

## Need of OS:

1. Easy interaction between the human & computer.<br>
2. Starting computer operation automatically when power in turned on.<br>
3. Loading & scheduling users program.<br>
4. Controlling input & output.<br>
5. Controlling program execution.<br>
6. Managing use of main memory.<br>
7. Providing security to users program.<br>

Reference : https://blogs.siliconindia.com/Jyotionnet/Operating-System--Need--Functions-bid-F941ap7D21007231.html<br><br>


## Kernel

The kernel is a computer program at the core of a computer's operating system with complete control over everything in the system. It is the "portion of the operating system code that is always resident in memory". It facilitates interactions between hardware and software components. On most systems, it is one of the first programs loaded on startup (after the bootloader) (We will see this later). It handles the rest of startup as well as input/output requests from software, translating them into data-processing instructions for the central processing unit. It handles memory and peripherals like keyboards, monitors, printers, and speakers.<br>

Reference : https://en.wikipedia.org/wiki/Kernel_(operating_system)<br><br>


## How does processor works

Processor has PC(Program Counter) registor which stores the address of next instruction to execute. Steps involved :

1. Fetch the instruction stored at adress given by PC.
2. Decode the instruction, meaning extract operands and operations to be done from given bit pattern.
3. Execute th einstruction.
4. Increment the PC.


## Booting process

1. When computer is turned on, PC will point to a location set by manufacturer(typically in ROM) which is BIOS.
2. BIOS initializes hardware devices and has settings which defines boot order. BIOS will scan for existing devices.
3. On first hit, it will load contents of sector 0 of that device into predefined location in RAM, known as bootloader.
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

## Memory Management

