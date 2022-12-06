&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 12/18/96 - 10:13 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER konstvalvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER nybild AS LOGICAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER rowidvar AS ROWID NO-UNDO.
DEFINE OUTPUT PARAMETER filnamn AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define SHARED SHARED
{KONSTRMTRL.I}
{BERBILD.I}
DEFINE NEW SHARED VARIABLE blobproch AS HANDLE NO-UNDO.    

DEFINE SHARED VARIABLE typkod AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.   



DEFINE VARIABLE sparkod AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ord AS INTEGER NO-UNDO.

DEFINE VARIABLE initdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
DEFINE VARIABLE nybildapph AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_MAPP FILL-IN-BEN BTN_OK BTN_VISA BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-BEN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_MAPP 
     IMAGE-UP FILE "BILDER\folder.ico":U
     LABEL "Btn 1" 
     SIZE 5 BY 1.5.

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(25)":U 
     LABEL "Filnamn" 
     VIEW-AS FILL-IN 
     SIZE 26.5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BTN_MAPP AT ROW 1.83 COL 41.5
     FILL-IN-BEN AT ROW 2.08 COL 12 COLON-ALIGNED
     BTN_OK AT ROW 4 COL 3
     BTN_VISA AT ROW 4 COL 18
     BTN_AVB AT ROW 4 COL 33
     SPACE(0.62) SKIP(0.45)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

ASSIGN 
       BTN_MAPP:AUTO-RESIZE IN FRAME DIALOG-1      = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1
DO:
   IF VALID-HANDLE(nybildapph) THEN DELETE PROCEDURE nybildapph.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MAPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MAPP DIALOG-1
ON CHOOSE OF BTN_MAPP IN FRAME DIALOG-1 /* Btn 1 */
DO:
   SYSTEM-DIALOG GET-FILE filnamn
   TITLE          "Välj den bildfil som skall länkas samman med konstruktion."
   FILTERS        "Bilder (*.bmp,*.gif,*.jpg,*.pdf)" "*.bmp,*.gif,*.jpg,*.pdf"                  
   INITIAL-DIR    initdir
   MUST-EXIST         
   USE-FILENAME
   UPDATE OKvald.
   IF OKvald = TRUE THEN DO:
      FILL-IN-BEN:SCREEN-VALUE = SUBSTRING(filnamn,(R-INDEX(filnamn, "\") + 1)).
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:   
   {muswait.i}  
   ASSIGN 
   felmedd = ""
   FILL-IN-BEN = INPUT FILL-IN-BEN.   
   IF FILL-IN-BEN = "" THEN DO:
      MESSAGE "Filnamn kan inte vara blankt!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.     
   RUN btnok_UI IN nybildapph (INPUT-OUTPUT rowidvar,INPUT nybild,INPUT konstvalvar,INPUT FILL-IN-BEN,
                 INPUT sparkod,OUTPUT felmedd,INPUT-OUTPUT TABLE berbildtemp).
   IF felmedd NE "" THEN DO:
      MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
      felmedd = "".
      APPLY "ENTRY" TO FILL-IN-BEN.
   END.
   ELSE DO:      
      IF VALID-HANDLE(nybildapph) THEN DELETE PROCEDURE nybildapph.      
      APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON GO OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nybildapph) THEN DELETE PROCEDURE nybildapph.
   musz = FALSE.   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISA DIALOG-1
ON CHOOSE OF BTN_VISA IN FRAME DIALOG-1 /* Visa */
DO:
   DEFINE VARIABLE hjfilvar AS CHARACTER NO-UNDO.
   FILL-IN-BEN = INPUT FILL-IN-BEN.
   hjfilvar = initdir + FILL-IN-BEN.
   IF INDEX(FILL-IN-BEN, ".") = 0 THEN DO:     
      IF SEARCH(hjfilvar + ".jpg") NE ? THEN hjfilvar = hjfilvar + ".jpg".
      ELSE IF SEARCH(hjfilvar + ".gif") NE ? THEN hjfilvar = hjfilvar + ".gif".
      ELSE IF SEARCH(hjfilvar + ".bmp") NE ? THEN hjfilvar = hjfilvar + ".bmp".
      ELSE IF SEARCH(hjfilvar + ".pdf") NE ? THEN hjfilvar = hjfilvar + ".pdf".      
   END.   
   RUN OPENDOC.P (hjfilvar,"","",NO).      
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
   initdir = Guru.Konstanter:gurubilder.
   IF nybild = TRUE THEN DO:      
      FIND FIRST konsttemp WHERE konsttemp.KTYPKOD = konstvalvar NO-LOCK NO-ERROR.
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Nyupplägg av bild".         
   END.
   ELSE DO:
      FIND FIRST konsttemp WHERE konsttemp.KTYPKOD = konstvalvar NO-LOCK NO-ERROR.
      FIND FIRST berbildtemp WHERE berbildtemp.BBROW = rowidvar NO-LOCK NO-ERROR.
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ändring av bild".   
      ASSIGN
      FILL-IN-BEN = berbildtemp.FILNAMN 
      sparkod = berbildtemp.FILNAMN.
   END.   
   RUN enable_UI.       
   {FRMSIZED.I} 
   {musarrow.i}
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
  Purpose: 
  Parameters:  
  Notes:       
-------------------------------------------------------------*/    
   IF Guru.Konstanter:appcon THEN DO:
      RUN NYBILDAPP.P PERSISTENT SET nybildapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NYBILDAPP.P PERSISTENT SET nybildapph.
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
  DISPLAY FILL-IN-BEN 
      WITH FRAME DIALOG-1.
  ENABLE BTN_MAPP FILL-IN-BEN BTN_OK BTN_VISA BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

