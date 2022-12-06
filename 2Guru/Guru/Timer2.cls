 
 /*------------------------------------------------------------------------
    File        : Timer2
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Tue Dec 03 09:53:10 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING System.Windows.Forms.*.


CLASS Guru.Timer2:
  DEFINE PUBLIC VARIABLE Root2 AS Guru.Root NO-UNDO.
  DEFINE PUBLIC VARIABLE Timer2 AS System.Windows.Forms.Timer NO-UNDO.
  DEFINE PRIVATE VARIABLE TickNum2 AS INTEGER NO-UNDO.
  DEFINE PRIVATE  PROPERTY proghandl2 AS HANDLE  NO-UNDO
   PRIVATE GET. PRIVATE SET.
  DEFINE PRIVATE  PROPERTY prognamn2 AS CHARACTER  NO-UNDO
   PRIVATE GET. PRIVATE SET.
   
   
  CONSTRUCTOR PUBLIC Timer2(INPUT r AS Guru.Root):
     THIS-OBJECT:Root2 = r.
     Timer2 = NEW System.Windows.Forms.Timer().
  END CONSTRUCTOR.   
  CONSTRUCTOR PUBLIC Timer2(INPUT inprognamn AS CHARACTER, INPUT inproghandl AS HANDLE):
     prognamn2 = inprognamn.
     proghandl2 = inproghandl.
     Timer2 = NEW System.Windows.Forms.Timer().
  END CONSTRUCTOR.   
  
  METHOD PUBLIC VOID StartaTimer(INPUT intervall AS INTEGER):
    THIS-OBJECT:Timer2:Interval = intervall.
    /*Anders Olsson Elpool i Umeå AB  27 mar 2014 16:30:58 
    starta inte om tiden är minder än 1 sek 
    */
    IF intervall > 1000 THEN THIS-OBJECT:Timer2:Start().
  END METHOD.
  
  METHOD PUBLIC VOID TimerEventRun(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
     
     IF VALID-HANDLE(proghandl2) THEN RUN VALUE(prognamn2) IN proghandl2.
     ELSE proghandl2 = ?.
  END METHOD.
  METHOD PUBLIC HANDLE TimerEventHandle():
   
     RETURN proghandl2.
  END METHOD.  
  DESTRUCTOR PUBLIC Timer2 ( ):
     
      Timer2:Dispose().
   END DESTRUCTOR.    
END CLASS.