 
 /*------------------------------------------------------------------------
    File        : Timer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Tue Dec 03 09:53:10 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING System.Windows.Forms.*.


CLASS Guru.TimerMulti:
  DEFINE PUBLIC VARIABLE TimerSys AS System.Windows.Forms.Timer NO-UNDO.
  DEFINE PRIVATE VARIABLE TickNum AS INTEGER NO-UNDO.
  DEFINE PRIVATE PROPERTY proghandl AS HANDLE  NO-UNDO
  PRIVATE GET. PRIVATE SET.
  DEFINE PRIVATE PROPERTY prognamn AS CHARACTER  NO-UNDO
   PRIVATE GET. PRIVATE SET.
   
   
  CONSTRUCTOR PUBLIC TimerMulti(INPUT inprognamn AS CHARACTER, INPUT inproghandl AS HANDLE):
     prognamn = inprognamn.
     proghandl = inproghandl.
     TimerSys = NEW System.Windows.Forms.Timer().
  END CONSTRUCTOR.   
  
  METHOD PUBLIC VOID StartaTimer(INPUT intervall AS INTEGER):
    THIS-OBJECT:TimerSys:Interval = intervall.
    /*Anders Olsson Elpool i Umeå AB  27 mar 2014 16:30:58 
    starta inte om tiden är minder än 1 sek 
    */
    IF intervall > 1000 THEN THIS-OBJECT:TimerSys:Start().
  END METHOD.
  
  METHOD PUBLIC VOID TimerEventRun(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
     IF VALID-HANDLE(proghandl) THEN RUN VALUE(prognamn) IN proghandl.
     ELSE proghandl = ?.
  END METHOD.
  METHOD PUBLIC HANDLE TimerEventHandle():
     RETURN proghandl.
  END METHOD.    
  DESTRUCTOR PUBLIC TimerMulti ( ):
      TimerSys:Dispose().
   END DESTRUCTOR.
END CLASS.