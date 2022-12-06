


/* r-unix.p */

IF OPSYS = "UNIX" THEN UNIX ls.
ELSE IF OPSYS = "WIN32" THEN DOS dir.
ELSE DISPLAY OPSYS "is an unsupported operating system".





