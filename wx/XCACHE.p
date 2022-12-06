/*XCACHE.P*/
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE progkopia AS CHARACTER FORMAT "X(50)" NO-UNDO.                
/*
prognamn = "\\BEREDNING1\d$\DELAD\klient\pro9\guru\sund.CSH". 
progkopia = "\\NTSERVER2\DELAD\PRO9\GURU\sund9.CSH".

SAVE CACHE COMPLETE SUND9 TO "\\BEREDNING1\d$\DELAD\klient\pro9\guru\sund9.CSH".
OS-COPY VALUE(prognamn) VALUE(progkopia).
 */
prognamn = "\\BEREDNING1\DELAD\klient\pro9\guru\sundn9.CSH". 
progkopia = "\\NTSERVER2\DELAD\PRO9\GURU\sundn9.CSH".

SAVE CACHE COMPLETE SUND9 TO "\\NTSERVER2\DELAD\PRO9\GURU\sund9.CSH".
OS-COPY VALUE(prognamn) VALUE(progkopia).

/*
prognamn = "\\172.20.16.18\d$\DELAD\pro9\guru\UMEA.CSH". 
progkopia = "\\NTSERVER2\DELAD\PRO9\GURU\UMEA.CSH".

SAVE CACHE COMPLETE umea TO "\\172.20.16.18\d$\DELAD\pro9\guru\umea.CSH".
OS-COPY VALUE(prognamn) VALUE(progkopia).
  */
