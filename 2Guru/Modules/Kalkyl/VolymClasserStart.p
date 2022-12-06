/*VolymClasserStart.p FRÅN SCHAKT*/
DEFINE VARIABLE startkalkroot AS Guru.Root NO-UNDO.
DEFINE VARIABLE startkalkdb   AS Modules.Kalkyl.Kalkyldb NO-UNDO.
DEFINE VARIABLE Root          AS Guru.Root NO-UNDO.
DEFINE VARIABLE AppservControll       AS HANDLE  NO-UNDO.  

/*  SCHAKTPRODYN.P  VolymstartDS_UI */

PROCEDURE VolymstartDS_UI :
   startkalkroot = NEW Guru.Root().
   startkalkroot:VolymStartKalkylDb().
   startkalkroot:VolymiSchakt().
   RUN Avsluta_UI.
END PROCEDURE.

PROCEDURE KollVolymStart_UI :
   DEFINE OUTPUT PARAMETER startad AS LOGICAL NO-UNDO.
   IF startkalkroot NE ? THEN DO:
      IF startkalkroot:WindowManager:wnd:Created THEN DO: 
         startad = TRUE.
         MESSAGE "Du måste stänga fönstret med Volymberäkningarna innan du fortsätter!"
         VIEW-AS ALERT-BOX.
         RETURN.
      END.
   END.   
   startad = FALSE.
      
END PROCEDURE.
/*avslutar objectorientering*/
PROCEDURE Avsluta_UI :
   IF startkalkroot NE ? THEN DO:
      startkalkroot:AvsRoot().
      DELETE OBJECT startkalkroot NO-ERROR.
   END.   
   IF VALID-HANDLE(AppservControll) THEN DO:
      RUN avsluta_UI IN AppservControll.
      DELETE PROCEDURE AppservControll NO-ERROR.
   END.
   AppservControll = ?.
END PROCEDURE. 


/*körs ej
PROCEDURE Volymstart_UI :
   DEFINE INPUT  PARAMETER strcTTh AS HANDLE NO-UNDO.
   
   startkalkroot = NEW Guru.Root().
   startkalkroot:VolymStartKalkylDb().
   startkalkroot:VolymiKalkyl(INPUT strcTTh).
   RUN Avsluta_UI.
END PROCEDURE.
*/

   

   