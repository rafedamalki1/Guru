 
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


CLASS Guru.Timer:
  DEFINE PUBLIC VARIABLE Root AS Guru.Root NO-UNDO.
  DEFINE PUBLIC VARIABLE Timer AS System.Windows.Forms.Timer NO-UNDO.
  DEFINE PRIVATE VARIABLE TickNum AS INTEGER NO-UNDO.
  DEFINE PRIVATE  PROPERTY proghandl AS HANDLE  NO-UNDO
     PRIVATE GET. PRIVATE SET.
  DEFINE PRIVATE  PROPERTY prognamn AS CHARACTER  NO-UNDO
     PRIVATE GET. PRIVATE SET.
   
   
   /*Anders Olsson Elpool i Umeå AB  6 jul 2016 11:43:21 
   för cls program 
   */
  CONSTRUCTOR PUBLIC Timer(INPUT r AS Guru.Root):
     THIS-OBJECT:Root = r.
     Timer = NEW System.Windows.Forms.Timer().
  END CONSTRUCTOR. 
  /*Anders Olsson Elpool i Umeå AB  6 jul 2016 11:43:35 
  för w och p program 
  */  
  CONSTRUCTOR PUBLIC Timer(INPUT inprognamn AS CHARACTER, INPUT inproghandl AS HANDLE):
     prognamn = inprognamn.
     proghandl = inproghandl.
     Timer = NEW System.Windows.Forms.Timer().
  END CONSTRUCTOR.   
  
  METHOD PUBLIC VOID StartaTimer(INPUT intervall AS INTEGER):
    THIS-OBJECT:Timer:Interval = intervall.
    /*Anders Olsson Elpool i Umeå AB  27 mar 2014 16:30:58 
    starta inte om tiden är minder än 1 sek 
    */
    IF intervall > 1000 THEN THIS-OBJECT:Timer:Start().
  END METHOD.
  
  METHOD PUBLIC VOID TimerEventRun(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
     IF VALID-HANDLE(proghandl) THEN RUN VALUE(prognamn) IN proghandl.
     ELSE proghandl = ?.
  END METHOD.
  METHOD PUBLIC HANDLE TimerEventHandle():
    
     RETURN proghandl.
  END METHOD.    
  DESTRUCTOR PUBLIC Timer ( ):
     
      Timer:Dispose().
   END DESTRUCTOR.
END CLASS.