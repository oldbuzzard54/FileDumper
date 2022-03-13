# FileDumper is a program for Hercules/Hyperion users to "print" the contents
of sequential files.  There is a subroutine called Snapfr that formats the
data and returns the formatted data in 1 of 3 formats.

INSTALLING FileDumper
=====================
The JCL to install can be found in the file Install.jcl.  This JCL uses
the userid of HERC01 and links the executables into 'SYS2.LINKLIB'.

INSTALLING FileDumper
=====================
The following JCL is required to run FileDump

// job
//FD1  EXEC PGM=FILEDUMP,PARM='parm'
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD input dataset
//SYSUT2   DD SYSOUT=* (or a dataset)

Parm communicates what to do with the file by SYSUT1.
PARM='f,skip,count'
where
f is the desired format code.
f=1 is like the IBM dump format. example:
00000000 6161C8C5 D9C3C5D3 D740D1D6 ....*//HERCELP JOB MSGCLASS=A,MSGLEVE*

f=2 is a double digit with printable chars above it. example:
00000000 / /   E X E C   P G M = I E B G E N E R
00000000 616140C5E7C5C340D7C7D47EC9C5C2C7C5D5C5D9404040404040404040...

f=3 is stacked double digit with printable chars above it.  example:
00000000 // EXEC PGM=IEBGENER
00000000 664CECC4DCD7CCCCCDCD44444444444444444444444444444444444444...
00000000 11057530774E9527555900000000000000000000000000000000000000...

skip=number records to skip in SYSUT1 before starting to print.  0 or
omitted results in no records being skipped.

count=number records to dump.  0 or omitted results in all the records
being printed.

All of the .txt files are compile listings and/or installation listings.

All steps in each job must complete with RC= 0000 except for PL/I 
compiles, which can also complete with RC= 0004.

