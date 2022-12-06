/*VATTMAN.P körs ej*/
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(41)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE TEMP-TABLE lonut
   FIELD strang AS CHARACTER FORMAT "X(61)".
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
{AMERICANEUROPEAN.I}
IF globforetag = "VATT" THEN DO:                        
   prognamndat = "/guru/export/lon/lonfr.d".
   prognamnque = "/guru/export/lon/lonfr" + ".q".
END. 
ELSE DO:  
   prognamndat = "D:\delad\pro8\GURU\apptemp\lonfr.d".
   prognamnque = "D:\delad\pro8\GURU\apptemp\lonfr" + ".q".
END.   
IF OPSYS = "UNIX" THEN DO:
   kommando = SEARCH("quoter").
   IF kommando = ? THEN DO:          
      RETURN.       
   END.   
END.
ELSE DO:      
   kommando = SEARCH("quoter.exe").
   IF kommando = ? THEN RETURN.       
END.      
OS-COMMAND SILENT VALUE(kommando) VALUE(prognamndat) > VALUE(prognamnque).
   

IF globforetag = "VATT" THEN DO: 
   INPUT FROM /guru/export/lon/lonfr.q NO-ECHO.
END. 
ELSE IF globforetag = "ELPA" THEN DO: 
   INPUT FROM D:\delad\pro8\GURU\apptemp\lonfr.q NO-ECHO.
END.  
REPEAT TRANSACTION:
  CREATE lonut.
  ASSIGN.
  IMPORT lonut NO-ERROR.
 
END.   
IF globforetag = "VATT" THEN DO: 
   prognamn = "/guru/export/lon/ma".
   prognamn = prognamn  + STRING(TODAY,"999999") + ".d".        
   OS-copy /guru/export/lon/lonfr.d VALUE(prognamn). 
   OUTPUT TO /guru/export/lon/perimp65.dta NO-ECHO.
   FOR EACH lonut BY lonut.strang:   
      PUT lonut.strang SKIP.
   END.   
   OUTPUT CLOSE.    
   prognamn = "/guru/export/lon/so".
   prognamn = prognamn  + STRING(TODAY,"999999") + ".d".
   OS-copy /guru/export/lon/perimp65.dta VALUE(prognamn). 
   OS-COPY /guru/export/lon/tom.d /guru/export/lon/lonfr.d.
END.   
IF globforetag = "ELPA" THEN DO: 
   prognamn = "D:\delad\pro8\GURU\apptemp\ma".
   prognamn = prognamn  + STRING(TODAY,"999999") + ".d".        
   OS-copy D:\delad\pro8\GURU\apptemp\lonfr.d VALUE(prognamn).    
   OUTPUT TO D:\delad\pro8\GURU\apptemp\perimp65.dta NO-ECHO.
   FOR EACH lonut BY lonut.strang:     
      PUT lonut.strang SKIP.       
   END.   
   OUTPUT CLOSE.  
   prognamn = "D:\delad\pro8\GURU\apptemp\so".
   prognamn = prognamn  + STRING(TODAY,"999999") + ".d".
   OS-copy D:\delad\pro8\GURU\apptemp\perimp65.dta VALUE(prognamn).   
   OS-COPY D:\delad\pro8\GURU\apptemp\tom.d D:\delad\pro8\GURU\apptemp\lonfr.d.
                       
END. 
DO TRANSACTION:
   CREATE LOGGOD.
   ASSIGN 
   LOGGOD.FGB = "M"
   LOGGOD.DATUM = TODAY.
END.
{EUROPEANAMERICAN.I}