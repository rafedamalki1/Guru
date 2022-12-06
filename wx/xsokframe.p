/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: XSOKFRAME.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2007.09.20 14:52 ELPAO   
     Modified: 
*/

DEFINE VARIABLE FRAME-SOK AS HANDLE NO-UNDO.
DEFINE VARIABLE BTN_CLOSE AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-ENR AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-BEN AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-KOD AS HANDLE NO-UNDO.
DEFINE VARIABLE ED-SOK AS HANDLE NO-UNDO.

CREATE FRAME FRAME-SOK
   ASSIGN
   DOWN = 1
   OVERLAY = TRUE
   SIDE-LABELS = TRUE
   THREE-D = TRUE
   COL = 20 
   ROW = 8
   WIDTH = 50.5 
   HEIGHT = 9.75
   TITLE = "Sök materiel från vald leverantör"
   SCROLLABLE = FALSE.
   
    /*
     BTN_CLOSE AT ROW 1.04 COL 48.5
     CMB_SOK AT ROW 1.17 COL 21.5 COLON-ALIGNED
     FILL-IN-ENR AT ROW 2.29 COL 21.5 COLON-ALIGNED
     FILL-IN-KOD AT ROW 2.38 COL 21.5 COLON-ALIGNED
     FILL-IN-BEN AT ROW 3.29 COL 21.5 COLON-ALIGNED
     ED_SOK AT ROW 4.5 COL 4.5 NO-LABEL
     IMAGE-4 AT ROW 2.33 COL 1.38
     */


CREATE BUTTON BTN_CLOSE
   ASSIGN
   FRAME = FRAME-SOK
   ROW = 1
   COLUMN = FRAME-SOK:WIDTH - 1.4
   FLAT-BUTTON = FALSE
   SENSITIVE = TRUE
   VISIBLE = TRUE
   FONT = 1
   LABEL = "X"
   HEIGHT-CHARS = 0.65  
   WIDTH-CHARS= 2    
   TRIGGERS:
      ON CHOOSE PERSISTENT RUN sok_close_UI IN THIS-PROCEDURE. 
   END TRIGGERS.

CREATE FILL-IN FILL-IN-ENR
   ASSIGN
   FRAME = FRAME-SOK
   ROW = 2
   COLUMN = 24
   SENSITIVE = TRUE
   VISIBLE = TRUE
   SCREEN-VALUE = "Enr"
   FONT = 1
   HEIGHT-CHARS = 0.85  
   WIDTH-CHARS= 12    
   TRIGGERS:
      ON "MOUSE-SELECT-DBLCLICK" PERSISTENT RUN sok_UI IN THIS-PROCEDURE.
      ON "RETURN" PERSISTENT RUN sok_UI IN THIS-PROCEDURE.               
   END TRIGGERS.

   CREATE FILL-IN FILL-IN-BEN
   ASSIGN
   FRAME = FRAME-SOK
   ROW = 4
   COLUMN = 24
   SENSITIVE = TRUE
   VISIBLE = TRUE
   SCREEN-VALUE = "Benämning"
   FONT = 1
   HEIGHT-CHARS = 0.85  
   WIDTH-CHARS= 25    
   TRIGGERS:
      ON "MOUSE-SELECT-DBLCLICK" PERSISTENT RUN sok_UI IN THIS-PROCEDURE.
      ON "RETURN" PERSISTENT RUN sok_UI IN THIS-PROCEDURE.               
   END TRIGGERS.

   CREATE FILL-IN FILL-IN-KOD
   ASSIGN
   FRAME = FRAME-SOK
   ROW = 2
   COLUMN = 24
   SENSITIVE = TRUE
   VISIBLE = TRUE
   SCREEN-VALUE = "Kod"
   FONT = 1
   HEIGHT-CHARS = 0.85  
   WIDTH-CHARS= 12    
   TRIGGERS:
      ON "MOUSE-SELECT-DBLCLICK" PERSISTENT RUN sok_UI IN THIS-PROCEDURE.
      ON "RETURN" PERSISTENT RUN sok_UI IN THIS-PROCEDURE.               
   END TRIGGERS.

   CREATE EDITOR ED-SOK
   ASSIGN
   FRAME = FRAME-SOK
   ROW = 5.5
   COLUMN = 7
   VISIBLE = TRUE
   FONT = 1
   HEIGHT-CHARS = 4  
   WIDTH-CHARS= 42.

PROCEDURE sok_close_UI :
   MESSAGE "Avsluta" VIEW-AS ALERT-BOX.
END PROCEDURE.
   
/*    ED-SOK = "Söktips: " + chr(10) +                                                                           */
/*                    "För att få ett urvalsresultat kan man utelämna en del av söksträngen." +                  */
/*                    " Ju mer som utelämnas desto större träfflista." +                                         */
/*                    " Om man skriver in ett # tecken så hämtar man upp hela katalogen från vald leverantör." + */
/*                    " Tänk på att det kan ta en liten stund att hämta en hel katalog.".                        */
   
   


/*
ON CHOOSE OF BTN_SOK IN FRAME FRAME-MTRL /* Sök */
DO:
   IF entrysok = FALSE THEN DO:
      ASSIGN 
      FRAME FRAME-SOK:HIDDEN = FALSE.
      FRAME FRAME-SOK:MOVE-TO-TOP ().
      entrysok = TRUE.
   END.
   ELSE DO:
      ASSIGN 
      FRAME FRAME-SOK:HIDDEN = FALSE.
      FRAME FRAME-SOK:MOVE-TO-BOTTOM ().
      entrysok = FALSE.
   END. 
  IF satsvar = FALSE THEN DO:
     ASSIGN 
     FILL-IN-ENR:HIDDEN IN FRAME FRAME-SOK = FALSE
     FILL-IN-KOD:HIDDEN IN FRAME FRAME-SOK = TRUE
     FILL-IN-BEN:HIDDEN IN FRAME FRAME-SOK = FALSE.
     DISPLAY FILL-IN-ENR FILL-IN-BEN WITH FRAME FRAME-SOK.
  END.
  ELSE DO:
     ASSIGN 
     FILL-IN-ENR:HIDDEN IN FRAME FRAME-SOK = TRUE
     FILL-IN-KOD:HIDDEN IN FRAME FRAME-SOK = FALSE
     FILL-IN-BEN:HIDDEN IN FRAME FRAME-SOK = FALSE.
     DISPLAY FILL-IN-KOD FILL-IN-BEN WITH FRAME FRAME-SOK.
  END.

END.

ON ANY-KEY OF FILL-IN-BEN IN FRAME FRAME-SOK /* Benämning */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.      
   END.
END.


ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN IN FRAME FRAME-SOK /* Benämning */
DO:   
   RUN musbendubb_UI.  
END.

PROCEDURE musenrdubb_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   &Scoped-define FRAME-NAME FRAME-SOK
   ASSIGN
   CMB_SOK = INPUT FRAME {&FRAME-NAME} CMB_SOK
   FILL-IN-ENR = INPUT FILL-IN-ENR.
   IF FILL-IN-ENR = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-ENR IN FRAME FRAME-SOK.
      RETURN NO-APPLY.
   END.  
   stjarnvar = INDEX(FILL-IN-ENR,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF CMB_SOK = "Börjar med" THEN DO:
      ASSIGN
      posok = FILL-IN-ENR.
      begvar = TRUE.
   END.
   ELSE IF CMB_SOK = "Innehåller" THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-ENR + "*"
      begvar = FALSE.      
   END.
   ELSE IF CMB_SOK = "Slutar med" THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-ENR.
      begvar = FALSE.
   END.
   RUN initsok_UI (INPUT 2,INPUT posok).
   {musarrow.i}
END PROCEDURE.


PROCEDURE musbendubb_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   &Scoped-define FRAME-NAME FRAME-SOK
   ASSIGN
   CMB_SOK = INPUT FRAME {&FRAME-NAME} CMB_SOK
   FILL-IN-BEN = INPUT FILL-IN-BEN.
   IF FILL-IN-BEN = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BEN IN FRAME FRAME-SOK.
      RETURN NO-APPLY.
   END.
   stjarnvar = INDEX(FILL-IN-BEN,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF CMB_SOK = "Börjar med" THEN DO:
      ASSIGN
      aosok = FILL-IN-BEN.
      begvar = TRUE.
   END.
   ELSE IF CMB_SOK = "Innehåller" THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BEN + "*"
      begvar = FALSE.      
   END.
   ELSE IF CMB_SOK = "Slutar med"  THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BEN.
      begvar = FALSE.
   END.
   RUN initsok_UI (INPUT 1,INPUT aosok).
   {musarrow.i}
END PROCEDURE.

PROCEDURE muskoddubb_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   &Scoped-define FRAME-NAME FRAME-SOK
   ASSIGN
   CMB_SOK = INPUT FRAME {&FRAME-NAME} CMB_SOK
   FILL-IN-KOD = INPUT  FILL-IN-KOD.
   IF FILL-IN-KOD = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-KOD IN FRAME FRAME-SOK.
      RETURN NO-APPLY.
   END.  
   stjarnvar = INDEX(FILL-IN-KOD,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF CMB_SOK = "Börjar med" THEN DO:
      ASSIGN
      posok = FILL-IN-KOD.
      begvar = TRUE.
   END.
   ELSE IF CMB_SOK = "Innehåller" THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-KOD + "*"
      begvar = FALSE.      
   END.
   ELSE IF CMB_SOK = "Slutar med" THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-KOD.
      begvar = FALSE.
   END.
   RUN initsok_UI (INPUT 2,INPUT posok).
   {musarrow.i}
END PROCEDURE.
*/
