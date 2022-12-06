&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}

&Scoped-define NEW
{TIDPERS.I}
{GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE tomdat AS DATE NO-UNDO.
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

DEFINE SHARED VARIABLE manad AS INTEGER NO-UNDO.


DEFINE VARIABLE vtidrec AS RECID.
DEFINE VARIABLE veckrec AS RECID.
DEFINE VARIABLE losen AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE datar AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

DEFINE INPUT PARAMETER RAD_ALLVAL AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS SEL_LIST BTN_START BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS SEL_LIST 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "AvSLUTA":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_START 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE SEL_LIST AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 45.88 BY 7.67 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     SEL_LIST AT ROW 1.25 COL 1.5 NO-LABEL
     BTN_START AT ROW 9.25 COL 18.38
     BTN_AVS AT ROW 9.25 COL 33.38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16.27.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Listor Flex"
         HEIGHT             = 9.54
         WIDTH              = 47.38
         MAX-HEIGHT         = 16.25
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16.25
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Listor Flex */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Listor Flex */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS C-Win
ON CHOOSE OF BTN_AVS IN FRAME DEFAULT-FRAME /* AvSLUTA */
DO:
   /*musz = TRUE.*/
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_START C-Win
ON CHOOSE OF BTN_START IN FRAME DEFAULT-FRAME /* Visa */
DO:
   RUN visa_UI.
   IF musz = FALSE THEN DO:             
      APPLY "GO" TO BTN_AVS IN FRAME {&FRAME-NAME}.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_LIST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_LIST C-Win
ON MOUSE-SELECT-DBLCLICK OF SEL_LIST IN FRAME DEFAULT-FRAME
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_LIST C-Win
ON VALUE-CHANGED OF SEL_LIST IN FRAME DEFAULT-FRAME
DO:
   SEL_LIST = INPUT SEL_LIST.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {WIN_M_START.I}
   {muswait.i}
   status-ok = SEL_LIST:ADD-LAST("Kompledighet").
   status-ok = SEL_LIST:ADD-LAST("Sjukdom").
   status-ok = SEL_LIST:ADD-LAST("Flexuttag").
   status-ok = SEL_LIST:ADD-LAST("Glömd registrering").
   status-ok = SEL_LIST:ADD-LAST("Annat in/ut").
   status-ok = SEL_LIST:ADD-LAST("Korrigerad tid").
   status-ok = SEL_LIST:ADD-LAST("Periodregistreringar").
   status-ok = SEL_LIST:ADD-LAST("Veckovila").
   status-ok = SEL_LIST:ADD-LAST("Friskvård").
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN status-ok = SEL_LIST:ADD-LAST("Arbetstidsförkortning 160 hela året").
   IF Guru.Konstanter:globforetag = "MISV" THEN status-ok = SEL_LIST:ADD-LAST("Arbetstidsförkortning 161 hela året").
   RUN enable_UI.   
   {FRMSIZE.I}  
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY SEL_LIST 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE SEL_LIST BTN_START BTN_AVS 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI C-Win 
PROCEDURE visa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF SEL_LIST = "Kompledighet" THEN DO:  
      MESSAGE "Vill du se rapport för ej körd flextid klicka på JA." SKIP
      "Vill du se rapport för förra körningen av flextid klicka på NEJ." VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL UPDATE val AS LOGICAL.
      CASE val:
         WHEN TRUE THEN DO:
             manad = 1.  
             RUN FLKOMP.W (INPUT RAD_ALLVAL , INPUT datar,  INPUT manad , INPUT "170" ,INPUT tomdat , INPUT "Kompledighet ej körda registreringar") .
         END.
         WHEN FALSE THEN DO:
            manad = 0.        
            RUN FLKOMP.W (INPUT RAD_ALLVAL , INPUT datar,  INPUT manad , INPUT "170" , INPUT tomdat , INPUT "Kompledighet för senast körda registreringar").
         END.
      END CASE.          
   
   END.
   ELSE IF SEL_LIST = "Sjukdom" THEN DO:
      MESSAGE "Vill du se rapport för ej körd flextid klicka på JA." SKIP
      "Vill du se rapport för förra körningen av flextid klicka på NEJ." VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL UPDATE val1 AS LOGICAL.
      CASE val1:
          WHEN TRUE THEN DO:
             manad = 1.  
             RUN FLKOMP.W (INPUT RAD_ALLVAL , INPUT datar,  INPUT manad , INPUT "110" ,INPUT tomdat , INPUT "Sjukdom ej körda registreringar") .
         END.
         WHEN FALSE THEN DO:
            manad = 0.        
            RUN FLKOMP.W (INPUT RAD_ALLVAL , INPUT datar,  INPUT manad , INPUT "110" , INPUT tomdat , INPUT "Sjukdom för senast körda registreringar").
         END.         
      END CASE.          
   END.   
   ELSE IF SEL_LIST = "Flexuttag" THEN DO:
      MESSAGE "Vill du se rapport för ej körd flextid klicka på JA." SKIP
      "Vill du se rapport för förra körningen av flextid klicka på NEJ." VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL UPDATE val2 AS LOGICAL.
      CASE val2:
         WHEN TRUE THEN DO:
             manad = 1.  
             RUN FLKOMP.W (INPUT RAD_ALLVAL , INPUT datar,  INPUT manad , INPUT "155" ,INPUT tomdat , INPUT "Flexuttag ej körda registreringar") .
         END.
         WHEN FALSE THEN DO:
            manad = 0.        
            RUN FLKOMP.W (INPUT RAD_ALLVAL , INPUT datar,  INPUT manad , INPUT "155" , INPUT tomdat , INPUT "Flexuttag för senast körda registreringar").
         END.         
      END CASE.
   END.   
   ELSE IF SEL_LIST = "Glömd registrering" THEN DO:
      MESSAGE "Vill du se rapport för ej körd flextid klicka på JA." SKIP
      "Vill du se rapport för förra körningen av flextid klicka på NEJ." VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL UPDATE val3 AS LOGICAL.
      CASE val3:
         WHEN TRUE THEN DO:
             manad = 1.  
             RUN FLKOMP.W (INPUT RAD_ALLVAL , INPUT datar,  INPUT manad , INPUT "GLOM" ,INPUT tomdat , INPUT "Glömd registrering ej körda registreringar") .
         END.
         WHEN FALSE THEN DO:
            manad = 0.        
            RUN FLKOMP.W (INPUT RAD_ALLVAL , INPUT datar, INPUT manad , INPUT "GLOM" , INPUT tomdat , INPUT "Glömd registrering för senast körda registreringar").
         END.
         
      END CASE.
   END.   
   ELSE IF SEL_LIST = "Annat in/ut" THEN DO:
      MESSAGE "Vill du se rapport för ej körd flextid klicka på JA." SKIP
      "Vill du se rapport för förra körningen av flextid klicka på NEJ." VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL UPDATE val4 AS LOGICAL.
      CASE val4:
         WHEN TRUE THEN DO:
             manad = 1.  
             RUN FLKOMP.W (INPUT RAD_ALLVAL , INPUT datar,  INPUT manad , INPUT "ANNAT" ,INPUT tomdat , INPUT "Annat in /ut ej körda registreringar") .
         END.
         WHEN FALSE THEN DO:
            manad = 0.        
            RUN FLKOMP.W (INPUT RAD_ALLVAL , INPUT datar,  INPUT manad , INPUT "ANNAT" , INPUT tomdat , INPUT "Annat in /ut för senast körda registreringar").
         END.
         
      END CASE.
   END.   
   ELSE IF SEL_LIST = "Korrigerad tid" THEN DO:
      MESSAGE "Vill du se rapport för ej körd flextid klicka på JA." SKIP
      "Vill du se rapport för förra körningen av flextid klicka på NEJ." VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL UPDATE val5 AS LOGICAL.
      CASE val5:
         WHEN TRUE THEN DO:
             manad = 1.  
             RUN FLKOMP.W (INPUT RAD_ALLVAL , INPUT datar,  INPUT manad , INPUT "Korr" ,INPUT tomdat , INPUT "Korrigerad tid ej körda registreringar") .
         END.
         WHEN FALSE THEN DO:
            manad = 0.        
            RUN FLKOMP.W (INPUT RAD_ALLVAL , INPUT datar,  INPUT manad , INPUT "Korr" , INPUT tomdat , INPUT "Korrigerad tid för senast körda registreringar").
         END.
         
      END CASE.
   END.   
   ELSE IF SEL_LIST = "Periodregistreringar" THEN DO:
      MESSAGE "Vill du se rapport för ej körd flextid klicka på JA." SKIP
      "Vill du se rapport för förra körningen av flextid klicka på NEJ." VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL UPDATE val6 AS LOGICAL.
      CASE val6:
         WHEN TRUE THEN DO:
             manad = 1.  
             RUN FLKOMP.W (INPUT RAD_ALLVAL , INPUT datar,  INPUT manad , INPUT "PERI" ,INPUT tomdat , INPUT "Periodregistreringar ej körda registreringar") .
         END.
         WHEN FALSE THEN DO:
            manad = 0.        
            RUN FLKOMP.W (INPUT RAD_ALLVAL , INPUT datar,  INPUT manad , INPUT "PERI" , INPUT tomdat , INPUT "Periodregistreringar för senast körda registreringar").
         END.         
      END CASE.
   END.
   ELSE IF SEL_LIST = "Veckovila" THEN DO:
      MESSAGE "Vill du se rapport för ej körd flextid klicka på JA." SKIP
      "Vill du se rapport för förra körningen av flextid klicka på NEJ." VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL UPDATE val7 AS LOGICAL.
      CASE val7:
         WHEN TRUE THEN DO:
             manad = 1.  
             RUN FLKOMP.W (INPUT RAD_ALLVAL , INPUT datar,  INPUT manad , INPUT "171" ,INPUT tomdat , INPUT "Veckovila ej körda registreringar") .
         END.
         WHEN FALSE THEN DO:
            manad = 0.        
            RUN FLKOMP.W (INPUT RAD_ALLVAL , INPUT datar,  INPUT manad , INPUT "171" , INPUT tomdat , INPUT "Veckovila för senast körda registreringar").
         END.
         
      END CASE.
   END.
   ELSE IF SEL_LIST = "Friskvård" THEN DO:
      MESSAGE "Vill du se rapport för ej körd flextid klicka på JA." SKIP
      "Vill du se rapport för förra körningen av flextid klicka på NEJ." VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL UPDATE val8 AS LOGICAL.
      CASE val8:
         WHEN TRUE THEN DO:
             manad = 1.  
             RUN FLKOMP.W (INPUT RAD_ALLVAL , INPUT datar,  INPUT manad , INPUT "140" ,INPUT tomdat , INPUT "Friskvård ej körda registreringar") .
         END.
         WHEN FALSE THEN DO:
            manad = 0.        
            RUN FLKOMP.W (INPUT RAD_ALLVAL , INPUT datar,  INPUT manad , INPUT "140" , INPUT tomdat , INPUT "Friskvård för senast körda registreringar").
         END.
         
      END CASE.
   END.
   ELSE IF SEL_LIST = "Arbetstidsförkortning 160 hela året" THEN DO:      
      manad = 160.  
      RUN FLKOMP.W (INPUT RAD_ALLVAL , INPUT datar,  INPUT manad , INPUT "160" ,INPUT tomdat , INPUT "Arbetstidsförkortning 160 hela året") .         
   END.
   ELSE IF SEL_LIST = "Arbetstidsförkortning 161 hela året" THEN DO:      
      manad = 161.  
      RUN FLKOMP.W (INPUT RAD_ALLVAL , INPUT datar,  INPUT manad , INPUT "161" ,INPUT tomdat , INPUT "Arbetstidsförkortning 161 hela året") .         
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

