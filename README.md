# Simulino
Another attempt at a simulator for the Arduino family of microcontroller boards, with a specific eye toward TDD.

### Rationale
Test-driven development has always been difficult on an Arduino, because the system on which the tests are running
is likely a 64-bit machine with an operating system and little direct hardware peripheral support, while the
system the tested code will run on is likely an 8-bit microcontroller with no operating system and lots of hardware
peripherals connected by interrupts, such as I/O pins, timers, USARTs, and so on.

This project is an attempt at simulating an Arduino, or more specifically, the Atmel AVR microcontroller at the heart
of an Arduino board, in Scala.  Why Scala?  Because I like Scala, that's why.

### Interior Architecture
In general, this is an event-driven architecture with a scheduled event bus.  Future events, such as executing
instructions, setting registers, changing pin states, and so on are scheduled to particular clock ticks.  When the
simulator runs, the event bus is ticked repeatedly; all events scheduled for the current tick are performed (some
of which will undoubtedly schedule more events for future ticks), then all events for the next tick, and so on.

For example, performing an instruction-pointer-update event, when the next position of the IP is occupied by an `ADD`
instruction--thus executing it--does not directly make any changes in the state of the CPU or the memory; 
but it produces an `IncrementIp` event to move the IP to the next instruction, a `SetMemory` event to replace the 
value in the target register with the calculated sum, and a `SetFlags` event to modify the flags in the status register 
appropriately.  Since the `ADD` instruction does not execute instantaneously, these events are all scheduled for a
time in the future, when the instruction is expected to complete.

The vertical stack of horizontal layers is meant to contain a high-level `Cpu` abstraction that knows only a few things:
for example, instruction pointer, instruction set, and program memory.  A lower layer should know things specific to 
Atmel AVR microcontrollers, such as how the stack is implemented; a layer under that will know things about particular 
AVR microcontroller families, and the low-level specifics beyond that will be taken care of by JSON configuration files.

So far only the two highest levels exist, because most of the specifics I know are for the Atmel ATmega2560.  Hopefully
the third and fourth layers will materialize once the ATmega2560 nears completion and I start learning about other
AVR parts.

Interesting note: the AVR ATtiny4 is a six-pin microcontroller, less than 2mm on a side, that has over fifty of the
instructions in this simulator, sixteen 8-bit registers (numbered 0x10 to 0x1F), 512 bytes (bytes, not kilo or mega)
of flash memory for a program, and 32 bytes (bytes, not kilo or mega) for runtime variables.  Oh--and the last six
CPU registers can be combined in pairs into 16-bit address registers, the better to index into that 32-byte (not 32-bit)
address space.  What does software craftsmanship look like on an architecture like that?  Comment to @dnwiebe on
Twitter.

### Exterior Architecture
Currently, the "handle" class is `Simulator`.  You create a `Simulator` from a configuration stream, then load a `.hex`
file into it, set up and schedule its inputs and outputs, and let it run for awhile.  Eventually, I'd like to make
Simulino a web service, with clients in many languages.  But that's in the future.

### In Progress
Updated 7/7/2015

* Executing an `Instruction` produces an `IncrementIp` event, but `Instruction`s also carry a `.length` field.  This is
duplication of effort.  I don't want to get rid of `.length`, because executing branching instructions doesn't produce
an `IncrementIp` with the instruction length in it; so probably we ought to have `IncrementIp` implied for all
instructions that don't branch, calculate it based on `.length`, and then use `IncrementIp` events only for instructions
that change the IP by some amount other than the length of the instruction.

* Get `BlinkTest` (which runs the Blink demo program that comes with the Arduino IDE) passing without pending.  That is
to say, figure out why it's strobing too fast by a factor of approximately (but not exactly) 10.

### Prioritized Backlog
Updated 7/8/2015

1. The way CPSE is implemented leaves much to be desired. See if there's a better way to detect four-byte instructions.

1. Some instructions have different latencies depending on which MCU they execute on.  Currently there's no way to
represent this in configuration.  There should be.

1. Code for the instructions needs to be a permanent part of Simulino, of course, or at the very least in a linked-in
`.jar` file, but the question of which instructions are active members of the instruction set should be answered by 
configuration information in the JSON file loaded when the `Simulator` object is constructed.

1. There are some serious performance issues here.  A simulated second requires two or three real minutes.  Profile
the code and see where it can be optimized.
