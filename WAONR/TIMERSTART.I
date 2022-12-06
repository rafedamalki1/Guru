/*TIMERSTART.I*/
DEFINE VARIABLE Timer  AS Guru.Timer              NO-UNDO.
IF {TIMERDEF.I} THEN DO:
   IF spartidvar > 0 THEN DO:
      /*
      Timer = NEW Guru.Timer("fbtnsnabb_UI",THIS-PROCEDURE).
      */
        Timer = NEW Guru.Timer({&TimerKor}  ,THIS-PROCEDURE).
      Timer:StartaTimer(spartidvar). /*Interval i milisekuder*/
      Timer:Timer:Tick:Subscribe(Timer:TimerEventRun).  
   END.    
END.   
