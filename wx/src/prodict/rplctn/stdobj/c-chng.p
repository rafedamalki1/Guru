TRIGGER PROCEDURE FOR CREATE OF replicate-chng.

DEFINE VAR iJulian AS INTEGER NO-UNDO.

ASSIGN 
iJulian = (((integer(today) - integer(date("1/1/90"))) * 24) * 60 * 60).
iJulian = iJulian + (integer(time)).

IF NOT AVAILABLE system-config THEN
  FIND FIRST system-config NO-LOCK NO-ERROR.
  IF NOT AVAILABLE system-config THEN
    RETURN ERROR.
   
ASSIGN
  replicate-chng.gmt-mod-dt = today
  replicate-chng.gmt-mod-tm = string(time,"HH:MM:SS")
  replicate-chng.rec-owner = system-config.system-id
  replicate-chng.gmt-julian =  iJulian
  NO-ERROR.
  
  
   
 
