&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/03 -  9:26 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER plannrvar AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER artalvar AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER omr AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER fast AS LOGICAL NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE persproch AS HANDLE NO-UNDO.     /* PERSONALAPP.P */
   {JURPERST.I}
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}
 
/*{NRSERIETEMP.I}*/
&Scoped-define SHARED SHARED
{PLANNRTEMP.I}
{SOKDEF.I}
{OMRTEMPW.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE inplannr AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE raknaupp AS LOGICAL NO-UNDO.
DEFINE VARIABLE uppar AS INTEGER NO-UNDO. 
DEFINE VARIABLE slutar AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-2

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_FAST CMB_OMR CMB_ARTAL FILL-IN_PLANNR ~
BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_FAST CMB_OMR CMB_ARTAL FILL-IN_PLANNR 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_ARTAL AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Årtal" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utförande omr" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.57 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_PLANNR AS CHARACTER FORMAT "X(7)" 
     LABEL "Plannr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE {&NEW} SHARED VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga plannr", no,
"Fasta plannr", yes
     SIZE 40 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-2
     RAD_FAST AT ROW 1.5 COL 1.43 NO-LABEL
     CMB_OMR AT ROW 2.62 COL 14.57 COLON-ALIGNED
     CMB_ARTAL AT ROW 2.62 COL 41.28
     FILL-IN_PLANNR AT ROW 3.92 COL 14.57 COLON-ALIGNED
     BTN_OK AT ROW 6.15 COL 27.29
     BTN_AVB AT ROW 6.15 COL 42.29
     SPACE(0.33) SKIP(0.26)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Nytt plannummer":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-2
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-2:SCROLLABLE       = FALSE
       FRAME DIALOG-2:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_ARTAL IN FRAME DIALOG-2
   ALIGN-L                                                              */
/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME DIALOG-2
   SHARED                                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-2
/* Query rebuild information for DIALOG-BOX DIALOG-2
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-2 DIALOG-2
ON END-ERROR OF FRAME DIALOG-2 /* Nytt plannummer */
DO:
   IF VALID-HANDLE(persproch) THEN DELETE PROCEDURE persproch NO-ERROR.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-2 DIALOG-2
ON ENDKEY OF FRAME DIALOG-2 /* Nytt plannummer */
DO:
   musz = TRUE.
   IF VALID-HANDLE(persproch) THEN DELETE PROCEDURE persproch NO-ERROR.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-2
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-2 /* Avbryt */
DO:   
   IF VALID-HANDLE(persproch) THEN DELETE PROCEDURE persproch NO-ERROR.
   musz = TRUE. 
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-2
ON CHOOSE OF BTN_OK IN FRAME DIALOG-2 /* Ok */
DO: 
   ASSIGN 
   CMB_ARTAL = INPUT CMB_ARTAL
   artalvar = CMB_ARTAL
   FILL-IN_PLANNR = INPUT FILL-IN_PLANNR
   inplannr = FILL-IN_PLANNR. 
   CMB_OMR = INPUT CMB_OMR.
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
   USE-INDEX OMRNAMN NO-LOCK NO-ERROR.
   ASSIGN omr = omrtemp.OMRADE.

   IF FILL-IN_PLANNR = ? THEN DO:
      MESSAGE Guru.Konstanter:gpll " kan inte vara okänt!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF FILL-IN_PLANNR = "0" THEN DO:
      MESSAGE Guru.Konstanter:gpll + " kan inte vara 0!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY. 
   END.
   IF FILL-IN_PLANNR = "" THEN DO:
      MESSAGE Guru.Konstanter:gpll " kan inte vara blankt!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY. 
   END.
   RUN appat_UI (INPUT 1).
   FOR EACH felmeddtemp:
      musz = TRUE.
      IF felmeddtemp.VAL = 1 THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Planummer finns redan" UPDATE svar AS LOGICAL.         
         IF svar THEN DO:
            RUN appat_UI (INPUT 2).
            APPLY "ENTRY" TO FILL-IN_PLANNR IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
         ELSE DO:            
            APPLY "ENTRY" TO FILL-IN_PLANNR IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
         DELETE felmeddtemp.
      END.
      ELSE DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
      END.
   END.
   {muswait.i}   
   ASSIGN 
   fast = RAD_FAST
   plannrvar = INTEGER(FILL-IN_PLANNR).
   FRAME DIALOG-2:HIDDEN = TRUE.
   musz = FALSE.
   IF VALID-HANDLE(persproch) THEN DELETE PROCEDURE persproch NO-ERROR.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_ARTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ARTAL DIALOG-2
ON VALUE-CHANGED OF CMB_ARTAL IN FRAME DIALOG-2 /* Årtal */
DO:
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL
   artalvar = CMB_ARTAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR DIALOG-2
ON VALUE-CHANGED OF CMB_OMR IN FRAME DIALOG-2 /* Utförande omr */
DO:
   {muswait.i}
   CMB_OMR = INPUT CMB_OMR.
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
   USE-INDEX OMRNAMN NO-LOCK NO-ERROR.
   ASSIGN omr = omrtemp.OMRADE.
   RUN appat_UI (INPUT 2). 
   ASSIGN FILL-IN_PLANNR = STRING(plannrvar).
   DISPLAY FILL-IN_PLANNR WITH FRAME {&FRAME-NAME}.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_PLANNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_PLANNR DIALOG-2
ON ENTRY OF FILL-IN_PLANNR IN FRAME DIALOG-2 /* Plannr */
DO:
   {muswait.i}
   CMB_OMR = INPUT CMB_OMR.
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
   USE-INDEX OMRNAMN NO-LOCK NO-ERROR.
   ASSIGN omr = omrtemp.OMRADE.
   RUN appat_UI (INPUT 2). 
   ASSIGN FILL-IN_PLANNR = STRING(plannrvar).
   DISPLAY FILL-IN_PLANNR WITH FRAME {&FRAME-NAME}.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_PLANNR DIALOG-2
ON LEAVE OF FILL-IN_PLANNR IN FRAME DIALOG-2 /* Plannr */
DO:
   ASSIGN FILL-IN_PLANNR = INPUT FILL-IN_PLANNR.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_FAST DIALOG-2
ON VALUE-CHANGED OF RAD_FAST IN FRAME DIALOG-2
DO:
   RAD_FAST = INPUT RAD_FAST.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-2 


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
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK TRANSACTION:   
   {DIA_M_START.I}
   {muswait.i}   
   {TILFSTPL.I}
   IF Guru.Konstanter:appcon THEN DO:
      RUN PERSONALAPP.P PERSISTENT SET persproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.   
      
   END.
   ELSE DO:
      RUN PERSONALAPP.P PERSISTENT SET persproch.  
   END.
   RUN jurp_UI IN persproch (INPUT Guru.Konstanter:globanv,OUTPUT TABLE jurperstemp,OUTPUT TABLE judavdtemp).
   CMB_OMR:LIST-ITEMS = "". 
   FOR EACH judavdtemp,         
   EACH omrtemp WHERE omrtemp.AVDELNINGNR = judavdtemp.AVDELNINGNR.
      status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN). 
      CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.
   END.
   ASSIGN
   CMB_OMR:LABEL = Guru.Konstanter:gutfk + " " + LC(Guru.Konstanter:gomrk) 
   FILL-IN_PLANNR:LABEL = Guru.Konstanter:gplk     
   FRAME {&FRAME-NAME}:TITLE = "Nytt " + LC(Guru.Konstanter:gplk).   
   &Scoped-define FORMATNAMN FILL-IN_PLANNR   
   {AOFORMAT3.I}
   FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE omrtemp THEN DO:
      FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
   END.
   ASSIGN  CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.
   omr = omrtemp.OMRADE. 
   ASSIGN
   status-ok = CMB_ARTAL:DELETE("0")
   uppar = YEAR(TODAY) - 3
   slutar = YEAR(TODAY) + 4.
   status-ok = CMB_ARTAL:ADD-LAST(STRING(uppar,"9999")).             
   DO WHILE uppar < slutar: 
       uppar = uppar + 1.             
       status-ok = CMB_ARTAL:ADD-LAST(STRING(uppar,"9999")).    
   END.  
   ASSIGN
   CMB_ARTAL:SCREEN-VALUE = STRING(YEAR(TODAY),"9999")
   CMB_ARTAL = INPUT CMB_ARTAL.

   RUN enable_UI.       
   {FRMSIZED.I} 
   ASSIGN
   CMB_OMR = INPUT CMB_OMR. 
   /*APPLY "LEAVE" TO CMB_OMR IN FRAME {&FRAME-NAME}.*/
   IF Guru.Konstanter:globforetag = "BORL" THEN CMB_OMR:HIDDEN = TRUE.
   {musarrow.i}
   {DIA_M_SLUT.I}
   APPLY "ENTRY" TO FILL-IN_PLANNR.
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE appat_UI DIALOG-2 
PROCEDURE appat_UI :
/* -----------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
   IF Guru.Konstanter:appcon THEN DO:
      RUN NRPLANAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT vart,INPUT omr,INPUT-OUTPUT raknaupp,
      INPUT-OUTPUT inplannr,INPUT-OUTPUT artalvar,INPUT-OUTPUT plannrvar,
      OUTPUT TABLE felmeddtemp). 
   END.
   ELSE DO:
      RUN NRPLANAPP.P
      (INPUT vart,INPUT omr,INPUT-OUTPUT raknaupp,
      INPUT-OUTPUT inplannr,INPUT-OUTPUT artalvar,INPUT-OUTPUT plannrvar,
      OUTPUT TABLE felmeddtemp).  
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-2  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-2.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-2  _DEFAULT-ENABLE
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
  DISPLAY RAD_FAST CMB_OMR CMB_ARTAL FILL-IN_PLANNR 
      WITH FRAME DIALOG-2.
  ENABLE RAD_FAST CMB_OMR CMB_ARTAL FILL-IN_PLANNR BTN_OK BTN_AVB 
      WITH FRAME DIALOG-2.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-2}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valomr_UI DIALOG-2 
PROCEDURE valomr_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF SUBSTRING(Guru.Konstanter:globomr,1,1) = "4" THEN DO:
      IF SUBSTRING(omrtemp.OMRADE,1,1) = "4" THEN DO:  
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN) IN FRAME {&FRAME-NAME}.
      END.
   END.
   ELSE status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

