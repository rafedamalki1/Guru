&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
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
{GLOBVAR2DEL1.I}
{REGVAR.I}
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE man AS INTEGER FORMAT "99" NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE vtidrec AS RECID.
DEFINE VARIABLE veckrec AS RECID.
DEFINE VARIABLE losen AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE ar AS INTEGER FORMAT "9999" NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-2

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_MANAD BTN_START BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS CMB_MANAD 

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
     LABEL "Starta körning" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_MANAD AS CHARACTER FORMAT "X(9)":U INITIAL "januari" 
     LABEL "Vilken månad avser månadskörningen ?" 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "januari","februari","mars","april","maj","juni","juli","augusti","september","oktober","november","december" 
     DROP-DOWN-LIST
     SIZE 12.5 BY 1
     FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-MAN AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 62.5 BY 4.5
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-MANAD AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Vilken månad avser månadskörningen?(1,2 osv)" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-2
     FILL-IN-MAN AT ROW 2 COL 2 NO-LABEL
     CMB_MANAD AT ROW 3.88 COL 50 COLON-ALIGNED
     BTN_START AT ROW 7.5 COL 35.5
     BTN_AVS AT ROW 7.5 COL 50.5
     FILL-IN-MANAD AT ROW 9.33 COL 50.5 COLON-ALIGNED
     SPACE(1.37) SKIP(0.45)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Månadskörning".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-2
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-2:SCROLLABLE       = FALSE
       FRAME DIALOG-2:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-MAN IN FRAME DIALOG-2
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       FILL-IN-MAN:HIDDEN IN FRAME DIALOG-2           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-MANAD IN FRAME DIALOG-2
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-MANAD:HIDDEN IN FRAME DIALOG-2           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-2
ON CHOOSE OF BTN_AVS IN FRAME DIALOG-2 /* Avbryt */
DO:
   musz = FALSE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_START DIALOG-2
ON CHOOSE OF BTN_START IN FRAME DIALOG-2 /* Starta körning */
DO:
   IF CMB_MANAD = "januari" THEN FILL-IN-MANAD = 1.
   IF CMB_MANAD = "februari" THEN FILL-IN-MANAD = 2.
   IF CMB_MANAD = "mars" THEN FILL-IN-MANAD = 3.
   IF CMB_MANAD = "april" THEN FILL-IN-MANAD = 4.
   IF CMB_MANAD = "maj" THEN FILL-IN-MANAD = 5.
   IF CMB_MANAD = "juni" THEN FILL-IN-MANAD = 6.
   IF CMB_MANAD = "juli" THEN FILL-IN-MANAD = 7.
   IF CMB_MANAD = "augusti" THEN FILL-IN-MANAD = 8.
   IF CMB_MANAD = "september" THEN FILL-IN-MANAD = 9.
   IF CMB_MANAD = "oktober" THEN FILL-IN-MANAD = 10.
   IF CMB_MANAD = "november" THEN FILL-IN-MANAD = 11.
   IF CMB_MANAD = "december  " THEN FILL-IN-MANAD = 12.         
   man = FILL-IN-MANAD.
   IF musz = FALSE THEN DO:             
      IF FILL-IN-MANAD > 12 OR FILL-IN-MANAD < 1 THEN DO:
         MESSAGE "Orimlig månad." FILL-IN-MANAD "!" VIEW-AS ALERT-BOX. 
         RETURN NO-APPLY.
      END.
      ar = YEAR(TODAY).
      IF MONTH(TODAY) = FILL-IN-MANAD + 1 THEN ar = ar.     
      ELSE ar = ar - 1 .
      FILL-IN-MANAD:HIDDEN = TRUE.
      FILL-IN-MAN = "MÅNADSKÖRNINGEN ÄR STARTAD".
      DISPLAY FILL-IN-MAN WITH FRAME {&FRAME-NAME}.
      FILL-IN-MAN:HIDDEN = FALSE. 
      
      {muswait.i}
      RUN manad_UI.
      {musarrow.i} 
      MESSAGE "Månadskörningen är färdig." VIEW-AS ALERT-BOX. 
      APPLY "GO" TO BTN_AVS IN FRAME {&FRAME-NAME}.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_MANAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_MANAD DIALOG-2
ON ENTRY OF CMB_MANAD IN FRAME DIALOG-2 /* Vilken månad avser månadskörningen ? */
DO:
   {musarrow.i} 
   MESSAGE "Vill du verkligen starta månadskörning nu ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
   CASE val:
      WHEN TRUE THEN DO:
         musz = FALSE.              
      END.
      WHEN FALSE THEN APPLY "GO" TO BTN_AVS IN FRAME {&FRAME-NAME}.
   END CASE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_MANAD DIALOG-2
ON LEAVE OF CMB_MANAD IN FRAME DIALOG-2 /* Vilken månad avser månadskörningen ? */
DO:
   CMB_MANAD = INPUT CMB_MANAD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_MANAD DIALOG-2
ON VALUE-CHANGED OF CMB_MANAD IN FRAME DIALOG-2 /* Vilken månad avser månadskörningen ? */
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
   IF MONTH(TODAY) = 02 THEN DO:
      CMB_MANAD:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING("januari").
      CMB_MANAD = INPUT CMB_MANAD.
   END.   
   ELSE IF MONTH(TODAY) = 03 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("februari").
      CMB_MANAD = INPUT CMB_MANAD.
   END.  
   ELSE IF MONTH(TODAY) = 04 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("mars").
      CMB_MANAD = INPUT CMB_MANAD.
   END.
   ELSE IF MONTH(TODAY) = 05 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("april").
      CMB_MANAD = INPUT CMB_MANAD.
   END.  
   ELSE IF MONTH(TODAY) = 06 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("maj").
      CMB_MANAD = INPUT CMB_MANAD.
   END.
   ELSE IF MONTH(TODAY) = 07 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("juni").
      CMB_MANAD = INPUT CMB_MANAD.
   END.  
   ELSE IF MONTH(TODAY) = 08 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("juli").
      CMB_MANAD = INPUT CMB_MANAD.
   END.
   ELSE IF MONTH(TODAY) = 09 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("augusti").
      CMB_MANAD = INPUT CMB_MANAD.
   END.  
   ELSE IF MONTH(TODAY) = 10 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("september").
      CMB_MANAD = INPUT CMB_MANAD.
   END.
   ELSE IF MONTH(TODAY) = 11 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("oktober").
      CMB_MANAD = INPUT CMB_MANAD.
   END.  
   ELSE IF MONTH(TODAY) = 12 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("november").
      CMB_MANAD = INPUT CMB_MANAD.
   END.
   ELSE IF MONTH(TODAY) = 01 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("december").
      CMB_MANAD = INPUT CMB_MANAD.
   END.       
  
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
  DISPLAY CMB_MANAD 
      WITH FRAME DIALOG-2.
  ENABLE CMB_MANAD BTN_START BTN_AVS 
      WITH FRAME DIALOG-2.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-2}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE manad_UI DIALOG-2 
PROCEDURE manad_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE VARIABLE samvar AS CHARACTER NO-UNDO.
   IF NOT Guru.Konstanter:appcon THEN DO:
      MESSAGE "Du har startat Guru på fel sätt och kan inte köra denna funktion!"
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
   IF Guru.Konstanter:globforetag = "elpa" THEN DO:
      samvar = "\\pc122\delad\pro9s\elLESAMM.TXT".
   END.   
   IF Guru.Konstanter:globforetag = "GRAN" THEN samvar = "\\granguru\guru_ser\server\delad\pro9s\GRLESAMM.TXT".
   IF Guru.Konstanter:globforetag = "GRIT" THEN samvar = "\\granguru\guru_ser\server\delad\pro9s\ITLESAMM.TXT". 
   IF Guru.Konstanter:globforetag = "GADM" THEN samvar = "\\granguru\guru_ser\server\delad\pro9s\ADLESAMM.TXT".
   IF Guru.Konstanter:globforetag = "GKAL" THEN samvar = "\\goliat\DELAD\server\delad\pro9s\KALESAMM.TXT".
   IF Guru.Konstanter:globforetag = "GKRVA" THEN samvar = "\\granguru\guru_ser\server\delad\pro9s\KRVASAMM.TXT".   
   
        

   IF Guru.Konstanter:globforetag = 'GRAN'   OR 
      Guru.Konstanter:globforetag = "GKAL"   THEN DO:
      RUN GRANMAN.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum, INPUT gvisatidpermanad, INPUT Guru.Konstanter:globforetag,INPUT man).           
      RUN GRANFR2.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum, INPUT gvisatidpermanad, INPUT Guru.Konstanter:globforetag,INPUT man).           
   END.
   RUN GRANFLYTTA.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT
   (INPUT samvar,INPUT Guru.Konstanter:globforetag).           
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

