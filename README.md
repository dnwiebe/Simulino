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

The vertical stack of horizontal layers is meant to contain a high-level `Cpu` abstraction that knows only a few things:
for example, instruction pointer, stack pointer, instruction set, and program memory.  A lower layer should know things
specific to Atmel AVR microcontrollers; a layer under that will know things about particular AVR microcontroller
families, and the low-level specifics beyond that will be taken care of by JSON configuration files.

So far only the two highest levels exist, because most of the specifics I know are for the Atmel ATmega2560.  Hopefully
the third and fourth layers will materialize once the ATmega2560 nears completion and I start learning about other
AVR parts.

### Exterior Architecture
Currently, the "handle" class is `Simulator`.  You create a `Simulator` from a configuration stream, then load a `.hex`
file into it, set up and schedule its inputs and outputs, and let it run for awhile.  Eventually, I'd like to make
Simulino a web service, with clients in many languages.  But that's in the future.

### Pressing tasks (updated 5/31/2015)
* Get `BlinkTest` (which runs the Blink demo program that comes with the Arduino IDE) passing without pending.
* `AvrCpu` has a method called `.register` whose name is misleading. It ought to be called `.memory` or `.dataMemory` or
something.
* `Memory` forces the use of `.getData` (which retrieves an array) for all retrievals, even single-byte ones.  This could
be optimized a little.
* Executing an `Instruction` produces an `IncrementIp` event, but `Instruction`s also carry a `.length` field.  This is
duplication of effort.  I don't want to get rid of `.length`, because executing branching instructions doesn't produce
an `IncrementIp` with the instruction length in it; so probably we ought to have `IncrementIp` implied for all
instructions that don't branch, calculate it based on `.length`, and then use `IncrementIp` events only for instructions
that change the IP by some amount other than the length of the instruction.
