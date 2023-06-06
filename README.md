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

For building the sources, I use the Altair Z80 Simulator from 
http://www.s100computers.com/Software%20Folder/Altair%20Simmulator/Altair%20Software.htm

This simulator is very handy for CP/M development, because it provides an easy way to transfer files in and out of the simulated system.
Submit-Files (.SUB) will be provided for CPM3.SYS and all the tools. For building CPM3.SYS for example, 
you'd run the command "r cpm3.sub" in the simulator. That will read cpm3.sub and save it to the simulators filesystem.
Then you can run "submit cpm3". The submit file will then read all required sources from the host into the simulator,
compile them, and write the resulting CPM3.SYS back out to the host file system.
For this build process to work, you need to place the following files from the simulator package into the repo folder:
altairz80.exe,cpm3,cpm3.dsk
Once you start the .exe, you have to enter "do cpm3" to start the system.
Once it is booted, switch to drive I: to initialize it, then switch back to A: and copy over some tools to the I: drive.
Specifically, you will need to copy RMAC.COM,ZMAC.COM,R.COM,W.COM,SUBMIT.COM,GENCPM.COM,RENAME.COM and LINK.COM.
You have to use the CP/M tool PIP for this. The general syntax is "PIP dest=source". 
For example: "PIP I:=RMAC.COM" will copy RMAC.COM from the drive you are on to drive I:
To read ZMAC.COM you need to enter "r Tools\ZMAC.COM"
You will also need some files from the CP/M 3 distribution disk.

For those who do not wish to tinker with the source, or want to try the system before diving into it, 
i have prepared a package containing a fully running system with an emulator of John's board. You can download it here:
https://drive.google.com/file/d/10RHLt3bqPHN4sBdgkRkXFwICIUTqFL7T/view?usp=drive_link

It includes a binary of the emulator, build from this repository:
https://github.com/EtchedPixels/EmulatorKit

Currently, it only runs on linux. This specific binary was compiled on Linux Mint Debian Edition.
The file EXIT.ASM is not part of the system itself. Rather, it is a little tool for use with the mentioned emulator. 
All it does is exit the emulator from the inside.

All the manuals for CP/M 3 are available from here: http://www.s100computers.com/Software%20Folder/CPM3%20BIOS%20Installation/CPM3%20FLOPPY%20BIOS%20Software.htm

For using the System, i reccomend reading the User Guide and keeping the Command Guide handy for reference.
If you want to write programs to run on CP/M 3, read the Programmers Guide.
The System Guide is meant for those who port CP/M 3 to a new system. But it is also a great source to learn about the inner workings of CP/M 3.
