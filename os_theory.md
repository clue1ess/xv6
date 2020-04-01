# Operating system

**Meta note** : This explanation assume that you have enough understanding of computer organization and assembly language.

**Reference** : My class notes of professor Abhijit Meenakshi and some web links(mentioned in respective sections.)

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

Reference : https://www.thedailyprogrammer.com/2016/08/processes-in-operating-system.html

## Hardware Interrupts

I/O devices are connected to system bus and can raise interrutpt for data-transfer. Interrupt means changing PC to some other pre-defined location which is known by OS(this pre-defined locations are occupied by OS at system boot time), that is breaking the normal execution of CPU. 

## System calls


## Process life-cycle

## Fork and exec

## Shell

Now, we have basic understanding of how fork and exec works, we will see how shell make use of these syscalls.
What shell does is -
Whenever we type any command, it forks the parent process and exec the child process

## Address Space(Segmentation and paging)

