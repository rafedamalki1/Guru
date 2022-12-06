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
&Scoped-define NEW 
{KALKKATTEMP.I}
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ekalkkattemp.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}


DEFINE SHARED VARIABLE brec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-3

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_TYP FILL-IN_RADNR FILL-IN_VINAMN ~
FILL-IN_PRIS FILL-IN_OPRIS BTN_OK BTN_AVBRYT 
&Scoped-Define DISPLAYED-OBJECTS CMB_TYP FILL-IN_RADNR FILL-IN_VINAMN ~
FILL-IN_PRIS FILL-IN_OPRIS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVBRYT AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_TYP AS CHARACTER FORMAT "X(256)":U 
     LABEL "Val av typer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "t" 
     DROP-DOWN-LIST
     SIZE 11.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_OPRIS AS DECIMAL FORMAT "->>>>9.99" INITIAL 0 
     LABEL "Övertidspris" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FILL-IN_PRIS AS DECIMAL FORMAT "->>>>9.99<" INITIAL 0 
     LABEL "Pris/Enh" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FILL-IN_RADNR AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Sortering" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FILL-IN_VINAMN AS CHARACTER FORMAT "X(256)" 
     LABEL "Kategori" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     CMB_TYP AT ROW 1.5 COL 13.38 COLON-ALIGNED
     FILL-IN_RADNR AT ROW 2.96 COL 13.38 COLON-ALIGNED
     FILL-IN_VINAMN AT ROW 4.38 COL 13.38 COLON-ALIGNED
     FILL-IN_PRIS AT ROW 5.79 COL 13.38 COLON-ALIGNED
     FILL-IN_OPRIS AT ROW 7.21 COL 13.38 COLON-ALIGNED
     BTN_OK AT ROW 8.75 COL 12.38
     BTN_AVBRYT AT ROW 8.75 COL 27.38
     SPACE(1.24) SKIP(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kategori":L.


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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVBRYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVBRYT DIALOG-3
ON CHOOSE OF BTN_AVBRYT IN FRAME DIALOG-3 /* Avbryt */
DO:
  {muswait.i}
  musz = TRUE.
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-3
ON CHOOSE OF BTN_OK IN FRAME DIALOG-3 /* Ok */
DO:
   {muswait.i}
  
  ASSIGN
  FILL-IN_RADNR  = INPUT FILL-IN_RADNR
  CMB_TYP        = INPUT CMB_TYP
  FILL-IN_VINAMN = INPUT FILL-IN_VINAMN
  FILL-IN_OPRIS  = INPUT FILL-IN_OPRIS
  FILL-IN_PRIS   = INPUT FILL-IN_PRIS.
  IF FILL-IN_VINAMN = "" THEN DO:
     MESSAGE "Kategori kan inte vara blank!" VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  FIND FIRST ekalkkattemp NO-LOCK NO-ERROR.
  IF NOT AVAILABLE ekalkkattemp THEN DO:          
     FIND FIRST kalkkattemp WHERE kalkkattemp.TYP = CMB_TYP AND kalkkattemp.NAMN = FILL-IN_VINAMN NO-LOCK NO-ERROR.
     IF AVAILABLE kalkkattemp THEN DO:
        MESSAGE "Byt namn på denna kategori!" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.        
     END.
     CREATE ekalkkattemp.
     ASSIGN
     ekalkkattemp.TYP = CMB_TYP 
     ekalkkattemp.NAMN = FILL-IN_VINAMN.
  END.
  ASSIGN 
  ekalkkattemp.RADNR     = FILL-IN_RADNR   
  ekalkkattemp.TYP       = CMB_TYP         
  ekalkkattemp.VINAMN    = FILL-IN_VINAMN  
  ekalkkattemp.OPRIS     = FILL-IN_OPRIS   
  ekalkkattemp.PRIS      = FILL-IN_PRIS.     
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_TYP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_TYP DIALOG-3
ON VALUE-CHANGED OF CMB_TYP IN FRAME DIALOG-3 /* Val av typer */
DO:
   CMB_TYP = INPUT CMB_TYP.
   
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
   FIND FIRST kalkkattyptemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE kalkkattyptemp THEN DO:
      musz = TRUE.
      LEAVE MAIN-BLOCK.
   END.
   FOR EACH kalkkattyptemp NO-LOCK:
      status-ok = CMB_TYP:ADD-LAST(kalkkattyptemp.TYP).
   END.
   status-ok = CMB_TYP:DELETE("T").
   FIND FIRST ekalkkattemp NO-LOCK NO-ERROR.
   IF AVAILABLE ekalkkattemp THEN DO:
      ASSIGN 
      FILL-IN_RADNR  = ekalkkattemp.RADNR     
      CMB_TYP        = ekalkkattemp.TYP       
      FILL-IN_VINAMN = ekalkkattemp.VINAMN    
      FILL-IN_OPRIS  = ekalkkattemp.OPRIS     
      FILL-IN_PRIS   = ekalkkattemp.PRIS.      
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "ÄNDRA-" + ekalkkattemp.NAMN.
      CMB_TYP:SCREEN-VALUE = ekalkkattemp.TYP.
      CMB_TYP = INPUT CMB_TYP.
      DISABLE CMB_TYP WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ny".
      FIND FIRST kalkkattyptemp NO-LOCK NO-ERROR.
      CMB_TYP:SCREEN-VALUE = kalkkattyptemp.TYP.
      CMB_TYP = INPUT CMB_TYP.   
   END.
   RUN enable_UI.            
   IF AVAILABLE ekalkkattemp THEN DO:
      DISABLE CMB_TYP WITH FRAME {&FRAME-NAME}.
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
  DISPLAY CMB_TYP FILL-IN_RADNR FILL-IN_VINAMN FILL-IN_PRIS FILL-IN_OPRIS 
      WITH FRAME DIALOG-3.
  ENABLE CMB_TYP FILL-IN_RADNR FILL-IN_VINAMN FILL-IN_PRIS FILL-IN_OPRIS BTN_OK 
         BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

