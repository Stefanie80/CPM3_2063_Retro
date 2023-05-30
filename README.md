# CPM3_2063_Retro
The CP/M 3 operating system, ported to John Winans' Z80 Retro board

Check out John's project at https://github.com/Z80-Retro

The sources contained in this repository are meant to be compiled on CP/M 3.
The files that originated from Digital Research are written in 8080 assembly, 
and are to be compiled with RMAC.

RMAC is part of the CP/M 3 distribution package, available from http://www.cpm.z80.de/

The sources written by me, and the parts taken from John's code are written 
in Z80 assembly and compile with ZMAC. I will include the ZMAC.COM binary 
here, because i could not find a source online.

For building the sources, i reccomend the Altair Z80 Simulator from 
http://www.s100computers.com/Software%20Folder/Altair%20Simmulator/Altair%20Software.htm

I have included .SUB files for building CPM3.SYS and the bootloader LDR.BIN

For those who do not wish to tinker with the source, or want to try the system before diving into it, 
i have prepared a package containing a fully running system with an emulator of John's board. You can download it here:
https://drive.google.com/file/d/10RHLt3bqPHN4sBdgkRkXFwICIUTqFL7T/view?usp=drive_link

It includes a binary of the emulator, build from this repository:
https://github.com/EtchedPixels/EmulatorKit

Currently, it only runs on linux. This specific binary was compiled on Linux Mint Debian Edition, but should work on any linux system.
