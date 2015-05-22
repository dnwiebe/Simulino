# Simulino
Another attempt at a simulator for the Arduino family of microcontroller boards

### Rationale
Test-driven development has always been difficult on an Arduino, because the system on which the tests are running
is likely a 64-bit machine with an operating system and little direct hardware peripheral support, while the
system the tested code will run on is likely an 8-bit microcontroller with no operating system and lots of hardware
peripherals connected by interrupts, such as I/O pins, timers, USARTs, and so on.

This project is an attempt at simulating an Arduino, or more specifically, the Atmel AVR microcontroller at the heart
of an Arduino board, in Scala.  Why Scala?  Because I like Scala, that's why.

### Yet to do
* Get BlinkTest (which runs the Blink demo program that comes with the Arduino IDE) running
* In some places, flash memory and SRAM are confused.  Straighten that out.
