
DEFINE VARIABLE err-status AS INTEGER.
DEFINE VARIABLE filename AS CHARACTER LABEL "Enter a file to delete".

UPDATE filename.
OS-DELETE filename.
err-status = OS-ERROR.

IF err-status <> 0 THEN
   CASE err-status:
      WHEN 1 THEN
	  MESSAGE "You are not the owner of this file or directory.".
      WHEN 2 THEN  
	   MESSAGE
              "The file or directory you want to delete does not exist.". 
      OTHERWISE
	   DISPLAY "OS Error #" + STRING(OS-ERROR,"99") FORMAT "x(13)"
              WITH FRAME b.
   END CASE.
