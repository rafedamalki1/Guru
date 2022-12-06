&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME FRAME-BRW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS FRAME-BRW 
/*------------------------------------------------------------------------

  File: BERKOPIKALK.W

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
DEFINE VARIABLE extrbefbapph AS HANDLE NO-UNDO.
&Scoped-define SHARED SHARED

{KALKTEMP2.I}
{KONVALTEMP.I}
{KALKBEFBTEMP.I} 
&Scoped-define NEW
{BBLABEL.I}
DEFINE TEMP-TABLE kalk_temp_copy NO-UNDO LIKE kalk_temp.

/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER numvar AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER id2var AS INTEGER NO-UNDO.
DEFINE VARIABLE xapph AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE brwh AS HANDLE NO-UNDO EXTENT 50.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-BRW

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_VAL BTN_KOPPLA-2 Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-INFO RAD_VAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_KOPPLA-2 
     LABEL "Koppla" 
     SIZE 14 BY 1.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-INFO AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Lägg Till", 1,
"Ersätt", 2
     SIZE 12 BY 2.5 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-BRW
     FILL-IN-INFO AT ROW 1 COL 1.5 NO-LABEL WIDGET-ID 8
     RAD_VAL AT ROW 16.5 COL 57 NO-LABEL WIDGET-ID 4
     BTN_KOPPLA-2 AT ROW 18.54 COL 75 WIDGET-ID 2
     Btn_OK AT ROW 20 COL 75
     SPACE(2.24) SKIP(0.62)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kopiera Kalkylkoder"
         DEFAULT-BUTTON Btn_OK WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX FRAME-BRW
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME FRAME-BRW:SCROLLABLE       = FALSE
       FRAME FRAME-BRW:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-INFO IN FRAME FRAME-BRW
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-INFO:READ-ONLY IN FRAME FRAME-BRW        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX FRAME-BRW
/* Query rebuild information for DIALOG-BOX FRAME-BRW
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX FRAME-BRW */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME FRAME-BRW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-BRW FRAME-BRW
ON WINDOW-CLOSE OF FRAME FRAME-BRW /* Kopiera Kalkylkoder */
DO:
  IF VALID-HANDLE(extrbefbapph) THEN DELETE PROCEDURE extrbefbapph. 
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOPPLA-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOPPLA-2 FRAME-BRW
ON CHOOSE OF BTN_KOPPLA-2 IN FRAME FRAME-BRW /* Koppla */
DO:
  RUN koppla_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK FRAME-BRW 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   {ALLSTARTDYN.I}  
   IF Guru.Konstanter:appcon THEN DO:
      RUN BERKOPIKALKAPP.P PERSISTENT SET xapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.      
   END.                             
   ELSE DO:
      RUN BERKOPIKALKAPP.P PERSISTENT SET xapph.   
   END.  
   RUN getLabels_UI IN xapph (OUTPUT TABLE bblabeltemp).  
   RUN enable_UI.   
   {FRMSIZEDF.I}
   RUN skapaui_UI.
   {DIA_M_SLUT.I}            
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI FRAME-BRW 
PROCEDURE allstartbrw_UI :
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRKODBAPP.P PERSISTENT SET extrbefbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN EXTRKODBAPP.P PERSISTENT SET extrbefbapph.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI FRAME-BRW  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME FRAME-BRW.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI FRAME-BRW  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY FILL-IN-INFO RAD_VAL 
      WITH FRAME FRAME-BRW.
  ENABLE RAD_VAL BTN_KOPPLA-2 Btn_OK 
      WITH FRAME FRAME-BRW.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-BRW}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fixastorlek_UI FRAME-BRW 
PROCEDURE fixastorlek_UI :
brwh[1]:HEIGHT = FRAME FRAME-BRW:HEIGHT / 2 + 5.
   brwh[1]:WIDTH = FRAME FRAME-BRW:WIDTH / 2.
   brwh[2]:COLUMN = brwh[1]:COLUMN + brwh[1]:WIDTH + 2.     
   brwh[2]:HEIGHT = FRAME FRAME-BRW:HEIGHT / 2 + 5.
   brwh[2]:WIDTH = FRAME FRAME-BRW:WIDTH / 2 - 7.
   RAD_VAL:ROW = brwh[2]:HEIGHT + 2.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kalkAdd_UI FRAME-BRW 
PROCEDURE kalkAdd_UI :
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kalkReplace_UI FRAME-BRW 
PROCEDURE kalkReplace_UI :
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE koppla_UI FRAME-BRW 
PROCEDURE koppla_UI :
DEFINE VARIABLE KonstNum AS INTEGER NO-UNDO.
   DEFINE VARIABLE KalkNum AS INTEGER NO-UNDO.
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DEFINE VARIABLE j AS INTEGER NO-UNDO.
   DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
   DEFINE VARIABLE crow AS ROWID NO-UNDO.
   DEFINE VARIABLE fraga AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lastegen AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE kalk_temp_copy NO-ERROR.   
   IF RAD_VAL:SCREEN-VALUE IN FRAME FRAME-BRW = "1" THEN fraga = "Är du säker på att du vill du Lägga Till valda Kalkylkoder på valda Konstruktioner?".
   ELSE fraga = "Är du säker på att du vill du Ersätta Kalkylkoder på valda Konstruktioner med valda Kalkylkoder?".   
   MESSAGE fraga VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL TITLE "Utföra åtgärd?" UPDATE svar AS LOGICAL.    
   IF svar THEN DO:
      KalkNum = brwh[1]:NUM-SELECTED-ROWS. 
      KonstNum = brwh[2]:NUM-SELECTED-ROWS. 
      IF KonstNum = 0 OR KalkNum = 0 THEN DO:
         MESSAGE "Du måste välja ur båda listorna!"
         VIEW-AS ALERT-BOX.
         RETURN.
      END.   
      j = 1.
      DO WHILE j LE KalkNum: /* Samla ihop valda kalkylkoder*/
         RUN fetrowid_UI IN brwproc[1] (INPUT j, OUTPUT crow).
         FIND kalk_temp WHERE ROWID(kalk_temp) = crow NO-LOCK NO-ERROR.   
         CREATE kalk_temp_copy.
         BUFFER-COPY kalk_temp TO kalk_temp_copy.      
         j = j + 1.   
      END.      
      FIND LAST kalk_temp WHERE kalk_temp.ARBKOD = "EGEN" NO-LOCK NO-ERROR.
      IF AVAILABLE kalk_temp THEN DO:
         lastegen = kalk_temp.LOPNR.    
      END.   
      i = 1.   
      DO WHILE i LE KonstNum: 
         RUN fetrowid_UI IN brwproc[2] (INPUT i, OUTPUT crow).            
         FIND kon_display WHERE ROWID(kon_display) = crow NO-LOCK NO-ERROR.
         IF RAD_VAL:SCREEN-VALUE IN FRAME FRAME-BRW = "1" THEN DO: /* Lägg till */
            FOR EACH kalk_temp_copy NO-LOCK:
               IF kalk_temp_copy.ARBKOD = "EGEN" THEN DO:
                  lastegen = lastegen + 1.
                  RUN copyegenkalkb_UI IN extrbefbapph (INPUT valaonr,INPUT valomrade,
                  INPUT kalk_temp_copy.NUM,INPUT kon_display.NUM,INPUT kalk_temp_copy.LOPNR, INPUT lastegen).                 
                  CREATE kalk_temp.
                  BUFFER-COPY kalk_temp_copy TO kalk_temp.
                  kalk_temp.NUM = kon_display.NUM.
                  kalk_temp.LOPNR = lastegen.
               END.
               ELSE DO:
                  FIND FIRST kalk_temp WHERE kalk_temp.NUM = kon_display.NUM AND kalk_temp.ARBKOD = kalk_temp_copy.ARBKOD AND kalk_temp.LOPNR = kalk_temp_copy.LOPNR NO-ERROR.
                  IF AVAILABLE kalk_temp THEN DO:
                     IF kon_display.NUM NE numvar THEN /* Inte plussa på sig själv */
                        kalk_temp.ANTAL = kalk_temp.ANTAL + kalk_temp_copy.ANTAL. 
                  END.
                  ELSE DO:
                     CREATE kalk_temp.
                     BUFFER-COPY kalk_temp_copy TO kalk_temp.
                     kalk_temp.NUM = kon_display.NUM.
                  END.
               END.
            END.
         END.
         ELSE DO: /* Ersätt */   
            IF kalk_temp_copy.ARBKOD = "EGEN" THEN DO:
               MESSAGE "EGNA Koder går inte att ersätta!"
               VIEW-AS ALERT-BOX.
            END.       
            ELSE DO:
               FOR EACH kalk_temp WHERE kalk_temp.NUM = kon_display.NUM NO-LOCK:
                  DELETE kalk_temp.         
               END. 
               FOR EACH kalk_temp_copy NO-LOCK:
                  CREATE kalk_temp.
                  BUFFER-COPY kalk_temp_copy TO kalk_temp.
                  kalk_temp.NUM = kon_display.NUM.
               END.
            END.
         END.                           
         i = i + 1.
      END.   
      MESSAGE "Åtgärden utförd" VIEW-AS ALERT-BOX.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectKonst_UI FRAME-BRW 
PROCEDURE selectKonst_UI :
DEFINE VARIABLE crow AS ROWID NO-UNDO.

   RUN fetrowid_UI IN brwproc[2] (INPUT 1, OUTPUT crow).
   FIND kon_display WHERE ROWID(kon_display) = crow NO-LOCK NO-ERROR.
   IF AVAILABLE kon_display THEN DO:         
      FIND FIRST bblabeltemp WHERE bblabeltemp.KONSKOD = kon_display.GRUPP
         USE-INDEX KOD NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      FIND FIRST bblabeltemp USE-INDEX KOD NO-LOCK NO-ERROR.
   END.     
   RUN rubriker_UI IN brwproc[2] (INPUT TEMP-TABLE bblabeltemp:DEFAULT-BUFFER-HANDLE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapabrw_UI FRAME-BRW 
PROCEDURE skapabrw_UI :
DEFINE INPUT  PARAMETER brvnr AS INTEGER NO-UNDO.  
   DEFINE INPUT  PARAMETER tthandlez AS HANDLE NO-UNDO.
   DEFINE VARIABLE brwantal AS INTEGER NO-UNDO.
   DEFINE VARIABLE x-multi  AS DECIMAL NO-UNDO.
   DEFINE VARIABLE y-multi  AS DECIMAL NO-UNDO. 
   DEFINE VARIABLE radvar   AS DECIMAL NO-UNDO.
   DEFINE VARIABLE colvar   AS DECIMAL NO-UNDO.   
   RUN DYNBRWINIT.P PERSISTENT SET brwproc[brvnr].
   RUN newbrw_UI IN brwproc[brvnr] (INPUT THIS-PROCEDURE).
   RUN brw_UI IN brwproc[brvnr] (OUTPUT brwh[brvnr]).
   IF brvnr = 1 THEN DO:
      RUN brwegenskap_UI IN brwproc[brvnr] (INPUT tthandlez,INPUT "BRW_KALK", INPUT 124, INPUT 10, INPUT TRUE, INPUT TRUE, INPUT "Välj kalkylkoder att koppla", INPUT FRAME FRAME-BRW:HANDLE).
      brwh[brvnr]:PRIVATE-DATA = "ejh".
      RUN getsizf_UI IN framesizeh (OUTPUT x-multi, OUTPUT y-multi).
      /*                                     bredd     höjd     kol      rad*/
      RUN brwstorlek_UI IN brwproc[brvnr](INPUT 32, INPUT 10, INPUT 1.5, INPUT 1.9, INPUT x-multi, INPUT y-multi).
      FIND FIRST kalk_temp NO-LOCK NO-ERROR.     
      RUN fieldinit_UI IN brwproc[brvnr].              
      RUN createfields_UI IN brwproc[brvnr] (INPUT "ARBKOD", INPUT "Arbkod", INPUT 5, INPUT "x(256)", INPUT TRUE).
      RUN createfields_UI IN brwproc[brvnr] (INPUT "LOPNR", INPUT "Löpnr", INPUT 4, INPUT ">>>", INPUT TRUE).
      RUN createfields_UI IN brwproc[brvnr] (INPUT "ENHET", INPUT "Enhet", INPUT 2, INPUT "x(256)", INPUT TRUE).
      RUN createfields_UI IN brwproc[brvnr] (INPUT "BENAMNING", INPUT "Benämning", INPUT 15, INPUT "x(256)", INPUT TRUE).      
      RUN brwsetupstop_UI IN brwproc[brvnr] (INPUT 1).  
   END.
   ELSE IF brvnr = 2 THEN DO:
      RUN brwegenskap_UI IN brwproc[brvnr] (INPUT tthandlez,INPUT "BRW_KONST", INPUT 124, INPUT 10, INPUT TRUE, INPUT TRUE, INPUT "Välj konstruktioner att kopiera till", INPUT FRAME FRAME-BRW:HANDLE).
      brwh[brvnr]:PRIVATE-DATA = "ejh".
      RUN getsizf_UI IN framesizeh (OUTPUT x-multi, OUTPUT y-multi).
      /*                                     bredd     höjd     kol      rad*/
      RUN brwstorlek_UI IN brwproc[brvnr](INPUT 32, INPUT 10, INPUT 1.5, INPUT 1.9, INPUT x-multi, INPUT y-multi).              
      RUN fieldinit_UI IN brwproc[brvnr].                             
      FIND FIRST bblabeltemp USE-INDEX KOD NO-LOCK NO-ERROR.
      RUN createfields_UI IN brwproc[brvnr] (INPUT "ID2", INPUT bblabeltemp.ID2, INPUT 5, INPUT "x(256)", INPUT TRUE).
      RUN createfields_UI IN brwproc[brvnr] (INPUT "EXTRA1", INPUT "Fri id", INPUT 8, INPUT "x(256)", INPUT TRUE).
      RUN createfields_UI IN brwproc[brvnr] (INPUT "F1", INPUT bblabeltemp.F1, INPUT 5, INPUT "x(256)", INPUT TRUE).      
      RUN vcextra_UI IN brwproc[brvnr] (INPUT "selectKonst_UI", INPUT THIS-PROCEDURE).
      RUN brwsetupstop_UI IN brwproc[brvnr] (INPUT 1).         
   END.
   
   RUN fieldslut_UI IN brwproc[brvnr].
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapaui_UI FRAME-BRW 
PROCEDURE skapaui_UI :
FIND FIRST bblabeltemp USE-INDEX KOD NO-LOCK NO-ERROR.
   FILL-IN-INFO:SCREEN-VALUE IN FRAME FRAME-BRW = "Kalkylkoder för konstruktion med " + bblabeltemp.ID2 + " " + string(id2var).
   RUN skapabrw_UI (INPUT 1, TEMP-TABLE kalk_temp:DEFAULT-BUFFER-HANDLE).
   RUN skapabrw_UI (INPUT 2, TEMP-TABLE kon_display:DEFAULT-BUFFER-HANDLE).
   
   RUN setcolsortvar_UI IN brwproc[1] (INPUT " WHERE NUM = '" + STRING(numvar) + "'").
   RUN openbdynspec_UI IN brwproc[1].
   RUN fixastorlek_UI.
   
   RUN columnstotempdyn_UI IN brwproc[2].
   RUN openbdynspec_UI IN brwproc[2].   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

