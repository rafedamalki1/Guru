
/*------------------------------------------------------------------------
    File        : BeredAdmHelpStart.p
    Purpose     : 

    Syntax      :run Modules\Beredning\BeredAdmHelpStart.p .

    Description : 

    Author(s)   : 
    Created     : Tue Sep 15 15:39:55 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE VARIABLE Utbytesmtrl   AS Modules.Beredning.UtbytListaAdm NO-UNDO.
DEFINE VARIABLE startutblroot AS Guru.Root NO-UNDO.
DEFINE VARIABLE AppservControll       AS HANDLE  NO-UNDO.   

PROCEDURE Utbytesmtrl_UI :
   RUN StartConAdm_UI.
   Utbytesmtrl = NEW Modules.Beredning.UtbytListaAdm(INPUT startutblroot).
   WAIT-FOR Utbytesmtrl:ShowDialog().
 /*  MESSAGE "SLUT I RUTAN"
   VIEW-AS ALERT-BOX.*/
   RUN Avsluta_UI.
   END PROCEDURE.
PROCEDURE StartConAdm_UI :
   startutblroot = NEW Guru.Root().
   startutblroot:StartUtblDb().
END PROCEDURE.
PROCEDURE Avsluta_UI :
   DELETE OBJECT startutblroot NO-ERROR.
   startutblroot = ?.
   IF VALID-HANDLE(AppservControll) THEN DO:
      RUN avsluta_UI IN AppservControll.
      DELETE PROCEDURE AppservControll NO-ERROR.
   END.
   AppservControll = ?.
   DELETE OBJECT Utbytesmtrl NO-ERROR.
   Utbytesmtrl = ?.
   
   
   DELETE OBJECT startutblroot NO-ERROR.
   startutblroot = ?.
   
   
END PROCEDURE. 
