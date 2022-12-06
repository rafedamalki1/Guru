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

  Created: 96/01/28 -  6:27 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}

DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE regstartsek AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE regslutsek AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE SHARED VARIABLE arnr AS INTEGER FORMAT "9999" NO-UNDO. 
DEFINE SHARED VARIABLE manadnr AS INTEGER FORMAT "99" NO-UNDO.
DEFINE SHARED VARIABLE manadnamn AS CHARACTER NO-UNDO.

DEFINE VARIABLE vtidrec AS RECID.
DEFINE VARIABLE veckrec AS RECID.
DEFINE VARIABLE losen AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-2

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_AR CMB_MANAD BTN_START BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS CMB_AR CMB_MANAD 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_START 
     LABEL "OK" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_AR AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "År" 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 7.38 BY 1
     FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE CMB_MANAD AS CHARACTER FORMAT "X(9)":U INITIAL "januari" 
     LABEL "Månad" 
     VIEW-AS COMBO-BOX INNER-LINES 13
     LIST-ITEMS "januari","februari","mars","april","maj","juni","juli","augusti","september","oktober","november","december","hela året" 
     DROP-DOWN-LIST
     SIZE 12.5 BY 1
     FGCOLOR 0  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-2
     CMB_AR AT ROW 1.58 COL 3.63 COLON-ALIGNED
     CMB_MANAD AT ROW 1.58 COL 19.5 COLON-ALIGNED
     BTN_START AT ROW 3.25 COL 5
     BTN_AVS AT ROW 3.25 COL 20
     SPACE(0.12) SKIP(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Månadssaldo flex".


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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-2
ON CHOOSE OF BTN_AVS IN FRAME DIALOG-2 /* Avbryt */
DO:
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_START DIALOG-2
ON CHOOSE OF BTN_START IN FRAME DIALOG-2 /* OK */
DO:
   IF CMB_MANAD = "januari" THEN manadnr = 1.
   IF CMB_MANAD = "februari" THEN manadnr = 2.
   IF CMB_MANAD = "mars" THEN manadnr = 3.
   IF CMB_MANAD = "april" THEN manadnr = 4.
   IF CMB_MANAD = "maj" THEN manadnr = 5.
   IF CMB_MANAD = "juni" THEN manadnr = 6.
   IF CMB_MANAD = "juli" THEN manadnr = 7.
   IF CMB_MANAD = "augusti" THEN manadnr = 8.
   IF CMB_MANAD = "september" THEN manadnr = 9.
   IF CMB_MANAD = "oktober" THEN manadnr = 10.
   IF CMB_MANAD = "november" THEN manadnr = 11.
   IF CMB_MANAD = "december" THEN manadnr = 12.         
   IF CMB_MANAD = "hela året" THEN manadnr = 99.
   ASSIGN
   manadnamn = CMB_MANAD
   arnr = CMB_AR.
   IF musz = FALSE THEN DO:             
      IF manadnr = 99 THEN musz = musz.
      ELSE IF manadnr > 12 OR manadnr < 1 THEN DO:
         MESSAGE "Orimlig månad" manadnr "!" VIEW-AS ALERT-BOX. 
         RETURN NO-APPLY.
      END.  
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN NO-APPLY.
      END.   

      APPLY "GO" TO BTN_AVS IN FRAME {&FRAME-NAME}.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_AR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AR DIALOG-2
ON LEAVE OF CMB_AR IN FRAME DIALOG-2 /* År */
DO:
   CMB_MANAD = INPUT CMB_MANAD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AR DIALOG-2
ON VALUE-CHANGED OF CMB_AR IN FRAME DIALOG-2 /* År */
DO:
   CMB_AR = INPUT CMB_AR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_MANAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_MANAD DIALOG-2
ON LEAVE OF CMB_MANAD IN FRAME DIALOG-2 /* Månad */
DO:
   CMB_MANAD = INPUT CMB_MANAD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_MANAD DIALOG-2
ON VALUE-CHANGED OF CMB_MANAD IN FRAME DIALOG-2 /* Månad */
DO:
   CMB_MANAD = INPUT CMB_MANAD.
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
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:   
   {DIA_M_START.I}
   ASSIGN  /*LADDAR ÅR I CMB_AR*/
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) - 14,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) - 13,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) - 12,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) - 11,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) - 10,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) - 9,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) - 8,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) - 7,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) - 6,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) - 5,"9999"))  
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) - 4,"9999"))  
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) - 3,"9999"))  
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) - 2,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) - 1,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY),"9999"))
   status-ok = CMB_AR:DELETE("0")
   CMB_AR:SCREEN-VALUE = STRING(YEAR(TODAY),"9999").                    
   IF MONTH(TODAY) = 01 THEN DO:
      CMB_MANAD:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING("januari").
      CMB_MANAD = INPUT CMB_MANAD.
   END.   
   ELSE IF MONTH(TODAY) = 02 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("februari").
      CMB_MANAD = INPUT CMB_MANAD.
   END.  
   ELSE IF MONTH(TODAY) = 03 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("mars").
      CMB_MANAD = INPUT CMB_MANAD.
   END.
   ELSE IF MONTH(TODAY) = 04 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("april").
      CMB_MANAD = INPUT CMB_MANAD.
   END.  
   ELSE IF MONTH(TODAY) = 05 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("maj").
      CMB_MANAD = INPUT CMB_MANAD.
   END.
   ELSE IF MONTH(TODAY) = 06 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("juni").
      CMB_MANAD = INPUT CMB_MANAD.
   END.  
   ELSE IF MONTH(TODAY) = 07 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("juli").
      CMB_MANAD = INPUT CMB_MANAD.
   END.
   ELSE IF MONTH(TODAY) = 08 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("augusti").
      CMB_MANAD = INPUT CMB_MANAD.
   END.  
   ELSE IF MONTH(TODAY) = 09 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("september").
      CMB_MANAD = INPUT CMB_MANAD.
   END.
   ELSE IF MONTH(TODAY) = 10 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("oktober").
      CMB_MANAD = INPUT CMB_MANAD.
   END.  
   ELSE IF MONTH(TODAY) = 11 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("november").
      CMB_MANAD = INPUT CMB_MANAD.
   END.
   ELSE IF MONTH(TODAY) = 12 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("december").
      CMB_MANAD = INPUT CMB_MANAD.
   END.       
   CMB_AR = INPUT CMB_AR.
   RUN enable_UI.       
   {FRMSIZED.I}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY CMB_AR CMB_MANAD 
      WITH FRAME DIALOG-2.
  ENABLE CMB_AR CMB_MANAD BTN_START BTN_AVS 
      WITH FRAME DIALOG-2.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-2}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

