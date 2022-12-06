&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1


/* Temp-Table and Buffer definitions                                    */




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/07/05 - 10:41 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
&Scoped-define SHARED SHARED 
{DIRDEF.I}
{AONRDEF.I}
{AVTAONRTEMP.I}
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE VARIABLE berkalkrec AS RECID NO-UNDO.
DEFINE VARIABLE ortssok AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE omrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE kopberapph AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_KALK

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES beredningtemp

/* Definitions for BROWSE BRW_KALK                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KALK beredningtemp.OMRADE ~
beredningtemp.BERNR beredningtemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KALK 
&Scoped-define QUERY-STRING-BRW_KALK FOR EACH beredningtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KALK OPEN QUERY BRW_KALK FOR EACH beredningtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KALK beredningtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KALK beredningtemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-22 FILL-IN_KALKNR TOG_ALLAOMR ~
FILL-IN_KALKYL FILL-IN_KALKB BTN_REG BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_AONR FILL-IN_DELNR FILL-IN_ORT ~
FILL-IN_KALKNR TOG_ALLAOMR FILL-IN_KALKYL FILL-IN_KALKB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_REG 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT ">99" INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_KALKB AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 21.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_KALKNR AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Bered. nr" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY 1.

DEFINE VARIABLE FILL-IN_KALKYL AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Bered. nr" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_ORT AS CHARACTER FORMAT "x(40)" 
     LABEL "Ort/Benämning" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47.5 BY 2.83
     BGCOLOR 8 .

DEFINE VARIABLE TOG_ALLAOMR AS LOGICAL INITIAL no 
     LABEL "Alla Områden" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.5 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_KALK FOR 
      beredningtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_KALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KALK DIALOG-1 _STRUCTURED
  QUERY BRW_KALK NO-LOCK DISPLAY
      beredningtemp.OMRADE FORMAT "x(6)":U
      beredningtemp.BERNR COLUMN-LABEL "Beredningsnr" FORMAT "->>>>>>9":U
      beredningtemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 47.5 BY 17.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_AONR AT ROW 2 COL 15.5 COLON-ALIGNED
     FILL-IN_DELNR AT ROW 2 COL 31 COLON-ALIGNED
     FILL-IN_ORT AT ROW 3.5 COL 15.5 COLON-ALIGNED
     FILL-IN_KALKNR AT ROW 5 COL 15.5 COLON-ALIGNED
     BRW_KALK AT ROW 5 COL 28
     TOG_ALLAOMR AT ROW 7.25 COL 11.5 WIDGET-ID 2
     FILL-IN_KALKYL AT ROW 23.13 COL 50.5 COLON-ALIGNED
     FILL-IN_KALKB AT ROW 24.25 COL 50.5 COLON-ALIGNED
     BTN_REG AT ROW 26.25 COL 46.5
     BTN_AVS AT ROW 26.25 COL 61.5
     "Sök på:" VIEW-AS TEXT
          SIZE 9.75 BY .67 AT ROW 23 COL 29
     RECT-22 AT ROW 22.75 COL 28
     SPACE(3.74) SKIP(2.66)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Koppla aonr till beredning":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: beredningtemp T "?" NO-UNDO temp-db beredningtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_KALK FILL-IN_KALKNR DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_KALK IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_KALK:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_KALK:MAX-DATA-GUESS IN FRAME DIALOG-1         = 1000
       BRW_KALK:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-1 = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_AONR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_AONR:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_DELNR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_DELNR:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_ORT IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KALK
/* Query rebuild information for BROWSE BRW_KALK
     _TblList          = "Temp-Tables.beredningtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.beredningtemp.OMRADE
     _FldNameList[2]   > Temp-Tables.beredningtemp.BERNR
"BERNR" "Beredningsnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.beredningtemp.BENAMNING
"BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KALK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Koppla aonr till beredning */
DO:
  {BORTBRWPROC.I}
  IF VALID-HANDLE(kopberapph) THEN DELETE PROCEDURE kopberapph.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Koppla aonr till beredning */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KALK
&Scoped-define SELF-NAME BRW_KALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KALK DIALOG-1
ON VALUE-CHANGED OF BRW_KALK IN FRAME DIALOG-1
DO:
   ASSIGN FILL-IN_KALKNR = beredningtemp.BERNR.
   DISPLAY FILL-IN_KALKNR WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON CHOOSE OF BTN_AVS IN FRAME DIALOG-1 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   DEFINE VARIABLE nyttbernr AS INTEGER NO-UNDO.
   DEFINE VARIABLE nyttomrade AS CHARACTER NO-UNDO.
   {muswait.i} 
   FILL-IN_KALKNR = INPUT FILL-IN_KALKNR.  
   IF FILL-IN_KALKNR NE ? THEN DO TRANSACTION:      
      IF beredningtemp.OMRADE = uppaonrtemp.OMRADE THEN DO: 
         TOG_ALLAOMR:CHECKED = FALSE.
         APPLY "VALUE-CHANGED" TO TOG_ALLAOMR.
      END.
      IF TOG_ALLAOMR:CHECKED = TRUE THEN DO:
         /* görs i value-changed för brw_kalk */         
      END.
      ELSE DO:
         FIND FIRST beredningtemp WHERE beredningtemp.OMRADE = uppaonrtemp.OMRADE AND
         beredningtemp.BERNR = FILL-IN_KALKNR AND beredningtemp.AONR = ? 
         USE-INDEX BERNR EXCLUSIVE-LOCK NO-ERROR.
      END.         
      IF NOT AVAILABLE beredningtemp THEN DO:
         MESSAGE 
         "Det finns ingen beredning med nummer " FILL-IN_KALKNR VIEW-AS ALERT-BOX.         
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN_KALKNR IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END. 
      EMPTY TEMP-TABLE felmeddtemp NO-ERROR.          
      RUN btnreg IN kopberapph (INPUT aonrvar,INPUT delnrvar,
                                INPUT beredningtemp.BERNR,
                                INPUT beredningtemp.OMRADE,
                                INPUT beredningtemp.BERAONR,
                                OUTPUT TABLE felmeddtemp, OUTPUT nyttbernr, OUTPUT nyttomrade).
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
         IF felmeddtemp.VAL = 1 OR felmeddtemp.VAL = 4 OR felmeddtemp.VAL = 5 OR felmeddtemp.VAL = 6 THEN DO:
            DELETE felmeddtemp.
            APPLY "ENTRY" TO FILL-IN_KALKNR IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
         IF felmeddtemp.VAL = 2 THEN DO:
            DELETE felmeddtemp.
            APPLY "ENTRY" TO FILL-IN_KALKNR IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.          
         END.
         IF felmeddtemp.VAL = 3 THEN DO:
            DELETE felmeddtemp.
            APPLY "ENTRY" TO FILL-IN_KALKNR IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.  
         END.      
      END.      
      ASSIGN 
      beredningtemp.AONR = aonrvar
      beredningtemp.DELNR = delnrvar.      
      IF TOG_ALLAOMR:CHECKED = TRUE THEN   DO:
         beredningtemp.BERNR = nyttbernr.
         beredningtemp.OMRADE = nyttomrade.       
         MESSAGE "Ditt nya beredningsnummer är " nyttbernr  VIEW-AS ALERT-BOX.
      END.
   END.   
   ELSE DO:
      MESSAGE "Felaktigt beredningsnr!" VIEW-AS ALERT-BOX TITLE "Meddelande".
      APPLY "ENTRY" TO FILL-IN_KALKNR IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.    
   END.   
   {musarrow.i}   
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON GO OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(kopberapph) THEN DELETE PROCEDURE kopberapph.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_KALKB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_KALKB DIALOG-1
ON ANY-KEY OF FILL-IN_KALKB IN FRAME DIALOG-1 /* Benämning */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_KALKB IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_KALKB DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_KALKB IN FRAME DIALOG-1 /* Benämning */
DO:
   FILL-IN_KALKB = INPUT FILL-IN_KALKB.
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "BENAMNING", INPUT FILL-IN_KALKB).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_KALKYL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_KALKYL DIALOG-1
ON ANY-KEY OF FILL-IN_KALKYL IN FRAME DIALOG-1 /* Bered. nr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_KALKYL IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_KALKYL DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_KALKYL IN FRAME DIALOG-1 /* Bered. nr */
DO:
   FILL-IN_KALKYL = INPUT FILL-IN_KALKYL.
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "BERNR", INPUT FILL-IN_KALKYL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ALLAOMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ALLAOMR DIALOG-1
ON VALUE-CHANGED OF TOG_ALLAOMR IN FRAME DIALOG-1 /* Alla Områden */
DO:  
   IF TOG_ALLAOMR:CHECKED = TRUE THEN DO:
      RUN berhmt IN kopberapph (INPUT "",INPUT-OUTPUT TABLE beredningtemp).
      RUN setcolsortvar_UI IN brwproc[1] (INPUT " AKTIV = TRUE AND AONR = ? USE-INDEX BERNR").  
      RUN openbdynspec_UI IN brwproc[1].
   END.
   ELSE DO:
      RUN berhmt IN kopberapph (INPUT uppaonrtemp.OMRADE,INPUT-OUTPUT TABLE beredningtemp).
      RUN setcolsortvar_UI IN brwproc[1] (INPUT " AKTIV = TRUE AND OMRADE = '" + omrvar + "' AND AONR = ? USE-INDEX BERNR").  
      RUN openbdynspec_UI IN brwproc[1].
   END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:  
   {DIA_M_START.I}
   {ALLSTARTDYN.I} 
   &Scoped-define FORMATNAMN FILL-IN_AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN_DELNR   
   {DELNRFORMAT.I}
   FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
   uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
   omrvar = uppaonrtemp.OMRADE.
   RUN berhmt IN kopberapph (INPUT uppaonrtemp.OMRADE,INPUT-OUTPUT TABLE beredningtemp).
   ASSIGN FRAME {&FRAME-NAME}:TITLE = "Koppla "+ LC(Guru.Konstanter:gaok) + " till beredning".
   RUN grundtid_UI.      
   RUN enable_UI.         
   ENABLE BRW_KALK WITH FRAME {&FRAME-NAME}. 
   FIND FIRST beredningtemp WHERE beredningtemp.OMRADE = omrvar AND
   beredningtemp.AKTIV = TRUE AND beredningtemp.AONR = ? 
   USE-INDEX BERNR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE beredningtemp THEN DO:
      TOG_ALLAOMR:CHECKED = TRUE.
      APPLY "VALUE-CHANGED" TO TOG_ALLAOMR IN FRAME {&FRAME-NAME}.
      FIND FIRST beredningtemp WHERE beredningtemp.OMRADE = omrvar AND
      beredningtemp.AKTIV = TRUE AND beredningtemp.AONR = ? USE-INDEX BERNR NO-LOCK NO-ERROR.           
      IF NOT AVAILABLE beredningtemp THEN 
      DO:         
         MESSAGE "Det finns inga aktiva beredningar som ej är kopplade i systemet" VIEW-AS ALERT-BOX.
         LEAVE MAIN-BLOCK.
      END.             
   END.
   ELSE DO:            
      berkalkrec = RECID(beredningtemp). 
      RUN setcolsortvar_UI IN brwproc[1] (INPUT " AKTIV = TRUE AND OMRADE = '" + omrvar + "' AND AONR = ? USE-INDEX BERNR").  
      RUN openbdynspec_UI IN brwproc[1].        
      REPOSITION BRW_KALK TO RECID berkalkrec.
      RUN repo_UI (INPUT 1, INPUT berkalkrec).
      status-ok = BRW_KALK:SELECT-FOCUSED-ROW() NO-ERROR.  
      APPLY "VALUE-CHANGED" TO BRW_KALK IN FRAME {&FRAME-NAME}.      
   END.
   FILL-IN_AONR:LABEL = Guru.Konstanter:gaok.
   
   FILL-IN_ORT:LABEL = Guru.Konstanter:gaonamnk.
   ASSIGN FRAME {&FRAME-NAME}:TITLE = "Koppla " + LC(Guru.Konstanter:gaok) + " till beredning".            
   {FRMSIZED.I}
   {musarrow.i}  
   my1hand = FILL-IN_KALKNR:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BRW_KALK:MOVE-AFTER-TAB-ITEM(my1hand).
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_KALK:HANDLE IN FRAME {&FRAME-NAME}).
   IF Guru.Konstanter:appcon THEN DO:
      RUN KOPBERAPP.P PERSISTENT SET kopberapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:varforetypchar[48]). 
   END.
   ELSE DO:
      RUN KOPBERAPP.P PERSISTENT SET kopberapph (INPUT Guru.Konstanter:varforetypchar[48]).
   END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN_AONR FILL-IN_DELNR FILL-IN_ORT FILL-IN_KALKNR TOG_ALLAOMR 
          FILL-IN_KALKYL FILL-IN_KALKB 
      WITH FRAME DIALOG-1.
  ENABLE RECT-22 FILL-IN_KALKNR TOG_ALLAOMR FILL-IN_KALKYL FILL-IN_KALKB 
         BTN_REG BTN_AVS 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE grundtid_UI DIALOG-1 
PROCEDURE grundtid_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/
  ASSIGN
  FILL-IN_AONR = uppaonrtemp.AONR
  FILL-IN_DELNR = uppaonrtemp.DELNR   
  FILL-IN_ORT = uppaonrtemp.ORT.          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo_UI DIALOG-1 
PROCEDURE repo_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER brwvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER browrec AS RECID NO-UNDO.
   IF brwvar = 1 THEN DO:
      &Scoped-define BROWSE-NAME BRW_KALK
      {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
      REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

