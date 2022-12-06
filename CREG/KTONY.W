&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-3 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/04 -  1:19 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ny AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER kto AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ktonr AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER jpers AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE ktoproch AS HANDLE NO-UNDO. /* Procedure handle till KTOAPP.P */
DEFINE VARIABLE aokaonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE aokdelnr AS INTEGER NO-UNDO.
DEFINE VARIABLE ksbenamn AS CHARACTER NO-UNDO.

DEFINE SHARED TEMP-TABLE ktotemp
   FIELD KONTO AS CHARACTER
   FIELD KONTONR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD JUDID AS CHARACTER
   FIELD VIJUDID AS CHARACTER
   FIELD OMRADE AS CHARACTER
   FIELD AVDELNINGNR AS INTEGER
   FIELD AKTIV AS LOGICAL
   FIELD RECTIDVIS AS RECID
   INDEX KONTO KONTO KONTONR.
DEFINE SHARED TEMP-TABLE kbtemp
   FIELD K1 AS CHARACTER
   FIELD K2 AS CHARACTER
   FIELD K3 AS CHARACTER
   FIELD K4 AS CHARACTER
   FIELD K5 AS CHARACTER.
DEFINE SHARED TEMP-TABLE jurtemp
   FIELD JUDID AS CHARACTER
   FIELD NAMN AS CHARACTER
   FIELD VIJUDID AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-3

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN_KONTONR FILL-IN_BENAMNING ~
FILL-IN-AKTIV BTN_OK BTN_AVBRYT 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_KONTONR FILL-IN_BENAMNING ~
FILL-IN-AKTIV FILL-IN-TEXT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVBRYT 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_JURP AS CHARACTER FORMAT "X(256)":U 
     LABEL "Jur pers" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AKTIV AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Aktiv" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "För kontodel" 
      VIEW-AS TEXT 
     SIZE 30 BY .96
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN_BENAMNING AS CHARACTER FORMAT "X(25)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.

DEFINE VARIABLE FILL-IN_KONTONR AS CHARACTER FORMAT "x(256)" 
     LABEL "Kod" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     FILL-IN_KONTONR AT ROW 3.46 COL 10 COLON-ALIGNED
     FILL-IN_BENAMNING AT ROW 5.08 COL 10 COLON-ALIGNED
     CMB_JURP AT ROW 6.75 COL 10 COLON-ALIGNED
     FILL-IN-AKTIV AT ROW 8.38 COL 10 COLON-ALIGNED
     BTN_OK AT ROW 9.88 COL 9
     BTN_AVBRYT AT ROW 9.88 COL 24
     FILL-IN-TEXT AT ROW 1.5 COL 1.5 NO-LABEL
     SPACE(7.62) SKIP(8.86)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Konto":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE
       FRAME DIALOG-3:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_JURP IN FRAME DIALOG-3
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_JURP:HIDDEN IN FRAME DIALOG-3           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TEXT IN FRAME DIALOG-3
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON END-ERROR OF FRAME DIALOG-3 /* Konto */
DO:
   {muswait.i}
   musz = TRUE.
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON ENDKEY OF FRAME DIALOG-3 /* Konto */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVBRYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVBRYT DIALOG-3
ON CHOOSE OF BTN_AVBRYT IN FRAME DIALOG-3 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-3
ON CHOOSE OF BTN_OK IN FRAME DIALOG-3 /* Ok */
DO:
   {muswait.i}
   ASSIGN
   FILL-IN_KONTONR = INPUT FILL-IN_KONTONR
   FILL-IN-AKTIV = INPUT FILL-IN-AKTIV
   FILL-IN_BENAMNING = INPUT FILL-IN_BENAMNING.
   IF INPUT FILL-IN_KONTONR = "" THEN DO:
      MESSAGE "Kontofältet kan inte vara blankt." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.   
   /*konto jurp*/
   IF Guru.Konstanter:varforetypval[18] = 1 THEN DO:   
      CMB_JURP = INPUT CMB_JURP.
      FIND FIRST jurtemp WHERE jurtemp.NAMN = CMB_JURP NO-ERROR.
      IF AVAILABLE jurtemp THEN DO:
         FIND FIRST ktotemp WHERE ktotemp.KONTO = kto AND ktotemp.KONTONR = FILL-IN_KONTONR AND 
         ktotemp.VIJUDID = jurtemp.VIJUDID AND ktonr NE FILL-IN_KONTONR USE-INDEX KONTO NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST ktotemp WHERE ktotemp.KONTO = kto AND ktotemp.KONTONR = FILL-IN_KONTONR AND 
         ktotemp.VIJUDID = CMB_JURP AND ktonr NE FILL-IN_KONTONR USE-INDEX KONTO NO-LOCK NO-ERROR.
      END.
      
   END.
   ELSE DO:
      FIND FIRST ktotemp WHERE ktotemp.KONTO = kto AND ktotemp.KONTONR = FILL-IN_KONTONR
         AND ktonr NE FILL-IN_KONTONR USE-INDEX KONTO NO-LOCK NO-ERROR.
   END.       
   IF AVAILABLE ktotemp THEN DO:
      MESSAGE "Det finns redan ett konto med denna beteckning.".
      RETURN NO-APPLY.
   END.  
   IF ny = TRUE THEN DO:
      CREATE ktotemp.
      ASSIGN
      ktotemp.RECTIDVIS = ?
      ktotemp.AKTIV = TRUE.
   END.
   ELSE FIND FIRST ktotemp WHERE ktotemp.KONTO = kto AND 
      ktotemp.KONTONR = ktonr NO-LOCK NO-ERROR.
   musz = FALSE.   
  /*konto jurp*/
   jpers = "".
   IF Guru.Konstanter:varforetypval[18] = 1 THEN DO:   
      FIND FIRST jurtemp WHERE jurtemp.NAMN = CMB_JURP NO-ERROR.
      ASSIGN
      ktotemp.JUDID = jurtemp.JUDID
      ktotemp.VIJUDID = jurtemp.VIJUDID
      jpers = ktotemp.VIJUDID.
   END.
   ASSIGN
   ktotemp.KONTONR = FILL-IN_KONTONR
   ktotemp.BENAMNING = FILL-IN_BENAMNING
   ktotemp.AKTIV = FILL-IN-AKTIV
   ktotemp.KONTO = kto
   ktonr = FILL-IN_KONTONR.
   IF ny = TRUE THEN DO:
      RUN nykontou_UI IN ktoproch 
         (INPUT ktotemp.KONTO, INPUT ktotemp.KONTONR,INPUT jpers, INPUT TABLE ktotemp).
   END.
   ELSE DO:
      RUN andrakontou_UI IN ktoproch 
         (INPUT ktotemp.KONTO, INPUT ktotemp.KONTONR,INPUT jpers, INPUT TABLE ktotemp).
   END.                       
   {BORTBRWPROC.I}
   APPLY "GO" TO FRAME {&FRAME-NAME}.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AKTIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AKTIV DIALOG-3
ON MOUSE-SELECT-CLICK OF FILL-IN-AKTIV IN FRAME DIALOG-3 /* Aktiv */
DO:
   IF INPUT FILL-IN-AKTIV = TRUE THEN FILL-IN-AKTIV = FALSE.
   IF INPUT FILL-IN-AKTIV = FALSE THEN FILL-IN-AKTIV = TRUE.
   DISPLAY FILL-IN-AKTIV WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-3 


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
   /*konto jurp*/
   IF Guru.Konstanter:varforetypval[18] = 1 THEN DO:   
      CMB_JURP:DELETE("").
      FOR EACH jurtemp:      
         CMB_JURP:ADD-LAST(jurtemp.NAMN).              
      END.
   END.
   FIND FIRST jurtemp NO-ERROR.
   IF AVAILABLE jurtemp THEN ASSIGN CMB_JURP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = jurtemp.NAMN.
   
   FIND FIRST kbtemp NO-ERROR.
   IF kto = "K1" THEN FILL-IN-TEXT = FILL-IN-TEXT + " " + 
      CAPS(SUBSTRING(kbtemp.K1,1,1)) + LC(SUBSTRING(kbtemp.K1,2)).
   ELSE IF kto = "K2" THEN FILL-IN-TEXT = FILL-IN-TEXT + " " + 
      CAPS(SUBSTRING(kbtemp.K2,1,1)) + LC(SUBSTRING(kbtemp.K2,2)).
   ELSE IF kto = "K3" THEN FILL-IN-TEXT = FILL-IN-TEXT + " " + 
      CAPS(SUBSTRING(kbtemp.K3,1,1)) + LC(SUBSTRING(kbtemp.K3,2)).
   ELSE IF kto = "K4" THEN FILL-IN-TEXT = FILL-IN-TEXT + " " + 
      CAPS(SUBSTRING(kbtemp.K4,1,1)) + LC(SUBSTRING(kbtemp.K4,2)).
   ELSE IF kto = "K5" THEN FILL-IN-TEXT = FILL-IN-TEXT + " " + 
      CAPS(SUBSTRING(kbtemp.K5,1,1)) + LC(SUBSTRING(kbtemp.K5,2)).
   DISPLAY FILL-IN-TEXT WITH FRAME {&FRAME-NAME}.
   IF ny = TRUE THEN DO:
      FILL-IN-AKTIV = TRUE.
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ny".   
   END.
   ELSE DO:
      FIND FIRST ktotemp WHERE ktotemp.KONTO = kto AND ktotemp.KONTONR = ktonr AND ktotemp.VIJUDID = jpers NO-LOCK NO-ERROR.
      FIND FIRST jurtemp WHERE jurtemp.VIJUDID = ktotemp.VIJUDID NO-ERROR.
      IF AVAILABLE jurtemp THEN DO:
         CMB_JURP = jurtemp.NAMN.
         ASSIGN  CMB_JURP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = jurtemp.NAMN.
      END.
      ELSE DO:
         CMB_JURP = ktotemp.VIJUDID.
         ASSIGN  CMB_JURP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ktotemp.VIJUDID.
      END.
      ASSIGN      
      FILL-IN_KONTONR = ktotemp.KONTONR      
      FILL-IN_BENAMNING = ktotemp.BENAMNING
      FILL-IN-AKTIV = ktotemp.AKTIV.           
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ändra-" + ktotemp.BENAMNING.
   END.    
   RUN enable_UI.       
   /*konto jurp*/
   IF Guru.Konstanter:varforetypval[18] = 1 THEN DO:   
      ENABLE CMB_JURP WITH FRAME {&FRAME-NAME}.
   END.
   IF ny = FALSE THEN DO:
      DISABLE FILL-IN_KONTONR WITH FRAME {&FRAME-NAME}.
   END.
   FIND FIRST jurtemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE jurtemp THEN DO:
      CMB_JURP:HIDDEN = TRUE.
   END.
   {FRMSIZED.I}
   {musarrow.i}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-3  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-3.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-3  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN_KONTONR FILL-IN_BENAMNING FILL-IN-AKTIV FILL-IN-TEXT 
      WITH FRAME DIALOG-3.
  ENABLE FILL-IN_KONTONR FILL-IN_BENAMNING FILL-IN-AKTIV BTN_OK BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

