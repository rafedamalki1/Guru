
/* r-vms.p */

IF OPSYS = "UNIX" THEN UNIX ls.
ELSE IF OPSYS = "msdos" THEN DOS dir.
ELSE IF OPSYS = "os2" THEN OS2 dir.
ELSE IF OPSYS = "vms" THEN VMS directory.
ELSE IF OPSYS = "btos" then BTOS
       "[sys]<sys>files.run" files.
ELSE DISPLAY OPSYS "is an unsupported operating system".
