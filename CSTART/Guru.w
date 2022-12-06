&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME GURUW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS GURUW 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters: guru.w
      <none>

  Author: 

  Created: 95/10/11 -  1:49 pm

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
/*DEFINE SHARED TEMP-TABLE foretemp
   FIELD ATRHOME AS CHARACTER 
   FIELD FORETAG AS CHARACTER
   FIELD GRAFTECK AS LOGICAL
   FIELD PROGRAM AS CHARACTER
   FIELD VERSION AS CHARACTER
   FIELD PLUSD AS LOGICAL.
DEFIN INPUT PARAMETER TABLE FOR foretemp.
*/
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{FORETEMP.I}


DEFINE VARIABLE storkollbredd AS INTEGER NO-UNDO.
DEFINE VARIABLE storkollhojd AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE felnrregler AS CHARACTER  NO-UNDO.
DEFINE VARIABLE alltOK AS LOGICAL NO-UNDO.
{GLOBVAR2DEL1.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-C

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOG_LOS FILL-IN_ANVANDARE FILL-IN_AV-LOSEN ~
BTN_LOSEN BTN_OK BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS TOG_LOS FILL-IN_ANVANDARE FILL-IN_AV-LOSEN ~
FILL-IN-GURU 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR GURUW AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_LOSEN 
     LABEL "Nytt lösenord":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-GURU AS CHARACTER FORMAT "X(256)":U INITIAL "Inloggningsrutin till" 
      VIEW-AS TEXT 
     SIZE 49 BY 1.21
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN_ANVANDARE AS CHARACTER FORMAT "X(256)" 
     LABEL "Användare" 
     VIEW-AS FILL-IN 
     SIZE 26.5 BY 1.

DEFINE VARIABLE FILL-IN_AV-LOSEN AS CHARACTER FORMAT "X(256)" 
     LABEL "Lösenord" 
     VIEW-AS FILL-IN 
     SIZE 26.5 BY 1.

DEFINE VARIABLE FILL-IN_NY-LOSEN AS CHARACTER FORMAT "X(256)" 
     LABEL "Nytt lösenord" 
     VIEW-AS FILL-IN 
     SIZE 26.5 BY 1.

DEFINE VARIABLE FILL-IN_NY-LOSEN-2 AS CHARACTER FORMAT "X(256)" 
     LABEL "Nytt lösenord igen" 
     VIEW-AS FILL-IN 
     SIZE 26.5 BY 1.

DEFINE VARIABLE TOG_LOS AS LOGICAL INITIAL no 
     LABEL "Lösenordet i klartext" 
     VIEW-AS TOGGLE-BOX
     SIZE 26.13 BY .88 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-C
     TOG_LOS AT ROW 3 COL 1.5
     FILL-IN_ANVANDARE AT ROW 4.42 COL 19.25 COLON-ALIGNED
     FILL-IN_AV-LOSEN AT ROW 6.42 COL 19.25 COLON-ALIGNED BLANK 
     BTN_LOSEN AT ROW 8 COL 48.75
     FILL-IN_NY-LOSEN AT ROW 8.42 COL 19.25 COLON-ALIGNED BLANK 
     FILL-IN_NY-LOSEN-2 AT ROW 10.42 COL 19.25 COLON-ALIGNED BLANK 
     BTN_OK AT ROW 11.75 COL 33.75
     BTN_AVS AT ROW 11.75 COL 48.75
     FILL-IN-GURU AT ROW 1.25 COL 1.5 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 62.88 BY 12.17.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW GURUW ASSIGN
         HIDDEN             = YES
         TITLE              = "GURU Elpool i Umeå AB 090/184540"
         HEIGHT             = 12.17
         WIDTH              = 63.13
         MAX-HEIGHT         = 22.88
         MAX-WIDTH          = 87.25
         VIRTUAL-HEIGHT     = 22.88
         VIRTUAL-WIDTH      = 87.25
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW GURUW
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-C
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN FILL-IN-GURU IN FRAME FRAME-C
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN_NY-LOSEN IN FRAME FRAME-C
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_NY-LOSEN:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_NY-LOSEN-2 IN FRAME FRAME-C
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_NY-LOSEN-2:HIDDEN IN FRAME FRAME-C           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(GURUW)
THEN GURUW:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS GURUW
ON CHOOSE OF BTN_AVS IN FRAME FRAME-C /* Avbryt */
DO:       
  /* IF Guru.Konstanter:globforetag = "SOLE" THEN QUIT.*/
   /*
   musz = TRUE.
   RETURN.
   */
   QUIT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_LOSEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_LOSEN GURUW
ON CHOOSE OF BTN_LOSEN IN FRAME FRAME-C /* Nytt lösenord */
DO:   
   RUN nytt_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      APPLY "ENTRY" TO FILL-IN_ANVANDARE IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   ELSE DO:
      APPLY "ENTRY" TO FILL-IN_NY-LOSEN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK GURUW
ON CHOOSE OF BTN_OK IN FRAME FRAME-C /* Ok */
DO:
   RUN koll_UI.
   APPLY "ENTRY" TO FILL-IN_ANVANDARE IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK GURUW
ON END-ERROR OF BTN_OK IN FRAME FRAME-C /* Ok */
DO:
   FILL-IN_AV-LOSEN = FILL-IN_AV-LOSEN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK GURUW
ON ENDKEY OF BTN_OK IN FRAME FRAME-C /* Ok */
DO:
   FILL-IN_AV-LOSEN = FILL-IN_AV-LOSEN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_ANVANDARE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ANVANDARE GURUW
ON ENTRY OF FILL-IN_ANVANDARE IN FRAME FRAME-C /* Användare */
DO:
   FILL-IN_ANVANDARE = FILL-IN_ANVANDARE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_AV-LOSEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AV-LOSEN GURUW
ON ANY-KEY OF FILL-IN_AV-LOSEN IN FRAME FRAME-C /* Lösenord */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:      
      RUN koll_UI.
      APPLY "ENTRY" TO FILL-IN_ANVANDARE IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.   
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AV-LOSEN GURUW
ON LEAVE OF FILL-IN_AV-LOSEN IN FRAME FRAME-C /* Lösenord */
DO:
   FILL-IN_AV-LOSEN = INPUT FILL-IN_AV-LOSEN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_NY-LOSEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_NY-LOSEN GURUW
ON LEAVE OF FILL-IN_NY-LOSEN IN FRAME FRAME-C /* Nytt lösenord */
DO:
   FILL-IN_NY-LOSEN = INPUT FILL-IN_NY-LOSEN.
   IF FILL-IN_NY-LOSEN = "" THEN DO:
      MESSAGE "Lösenordet kan inte vara blankt." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_NY-LOSEN-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_NY-LOSEN-2 GURUW
ON ANY-KEY OF FILL-IN_NY-LOSEN-2 IN FRAME FRAME-C /* Nytt lösenord igen */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      RUN koll_UI.
      APPLY "ENTRY" TO FILL-IN_NY-LOSEN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.            
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_NY-LOSEN-2 GURUW
ON LEAVE OF FILL-IN_NY-LOSEN-2 IN FRAME FRAME-C /* Nytt lösenord igen */
DO:
   FILL-IN_NY-LOSEN-2 = INPUT FILL-IN_NY-LOSEN-2.    
   IF FILL-IN_NY-LOSEN-2 = "" THEN DO:
      MESSAGE "Lösenordet kan inte vara blankt." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_LOS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_LOS GURUW
ON VALUE-CHANGED OF TOG_LOS IN FRAME FRAME-C /* Lösenordet i klartext */
DO:
   TOG_LOS = INPUT TOG_LOS.
   IF TOG_LOS = TRUE THEN DO:
      FILL-IN_AV-LOSEN:PASSWORD-FIELD  = FALSE.
      FILL-IN_NY-LOSEN:PASSWORD-FIELD  = FALSE.
      FILL-IN_NY-LOSEN-2:PASSWORD-FIELD  = FALSE.
      DISPLAY FILL-IN_AV-LOSEN WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      FILL-IN_AV-LOSEN:PASSWORD-FIELD  = TRUE.
      FILL-IN_NY-LOSEN:PASSWORD-FIELD  = TRUE.
      FILL-IN_NY-LOSEN-2:PASSWORD-FIELD  = TRUE.
      DISPLAY FILL-IN_AV-LOSEN WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK GURUW 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {WIN_M_START.I}
   {muswait.i}
   FILL-IN_AV-LOSEN:BLANK  = FALSE.
   FILL-IN_NY-LOSEN:BLANK  = FALSE.
   FILL-IN_NY-LOSEN-2:BLANK  = FALSE.
   FILL-IN_AV-LOSEN:PASSWORD-FIELD  = TRUE.
   FILL-IN_NY-LOSEN:PASSWORD-FIELD  = TRUE.
   FILL-IN_NY-LOSEN-2:PASSWORD-FIELD  = TRUE.
   RUN enable_UI.
   GURUW:TITLE = " Elpool i Umeå AB 090/184540".
   {FRMSIZE.I}
   FIND FIRST foretemp NO-LOCK NO-ERROR.
   FILL-IN-GURU = FILL-IN-GURU + " " + SUBSTRING(foretemp.VERSION,1,10).
   DISPLAY FILL-IN-GURU WITH FRAME {&FRAME-NAME}.
   APPLY "ENTRY" TO FILL-IN_ANVANDARE.
   my1hand = FILL-IN_ANVANDARE:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = FILL-IN_AV-LOSEN:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = FILL-IN_AV-LOSEN:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = FILL-IN_NY-LOSEN:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = FILL-IN_NY-LOSEN:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = FILL-IN_NY-LOSEN-2:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = FILL-IN_NY-LOSEN-2:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BTN_OK:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = BTN_OK:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BTN_LOSEN:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = BTN_LOSEN:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BTN_AVS:MOVE-AFTER-TAB-ITEM(my1hand).
   {musarrow.i}
    {KRYSSBORT.I}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI GURUW  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(GURUW)
  THEN DELETE WIDGET GURUW.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI GURUW  _DEFAULT-ENABLE
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
  DISPLAY TOG_LOS FILL-IN_ANVANDARE FILL-IN_AV-LOSEN FILL-IN-GURU 
      WITH FRAME FRAME-C IN WINDOW GURUW.
  ENABLE TOG_LOS FILL-IN_ANVANDARE FILL-IN_AV-LOSEN BTN_LOSEN BTN_OK BTN_AVS 
      WITH FRAME FRAME-C IN WINDOW GURUW.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-C}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE koll_UI GURUW 
PROCEDURE koll_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   ASSIGN    
   FILL-IN_AV-LOSEN = INPUT FRAME {&FRAME-NAME} FILL-IN_AV-LOSEN
  /* FILL-IN_AV-LOSEN = FILL-IN_KOLL*/
   FILL-IN_ANVANDARE = INPUT FRAME {&FRAME-NAME} FILL-IN_ANVANDARE   
   FILL-IN_NY-LOSEN = INPUT FRAME {&FRAME-NAME} FILL-IN_NY-LOSEN
   FILL-IN_NY-LOSEN-2 = INPUT FRAME {&FRAME-NAME} FILL-IN_NY-LOSEN-2.
   IF FILL-IN_NY-LOSEN NE FILL-IN_NY-LOSEN-2 THEN DO:
      ASSIGN
      FILL-IN_NY-LOSEN = ""
      FILL-IN_NY-LOSEN-2 = "".
      MESSAGE "Det nya lösenordet blev felaktigt angivet." VIEW-AS ALERT-BOX.               
      RETURN.     
   END.
   IF FILL-IN_NY-LOSEN = FILL-IN_AV-LOSEN THEN DO:
      ASSIGN
      FILL-IN_NY-LOSEN = ""
      FILL-IN_NY-LOSEN-2 = "".
      MESSAGE "Det nya lösenordet får inte vara samma som det gamla!" VIEW-AS ALERT-BOX.               
      RETURN.     
   END.
  /* alla sätts 1 då ingen autoinloggning vid byte user/lösen.*/
   Guru.Konstanter:varforetypval[51] = 1.
   /*ÄR USER KOPPLAD TILL PERSONALTAB*/
   RUN REGLERFORLOSEN.P (INPUT FILL-IN_ANVANDARE,INPUT FILL-IN_AV-LOSEN,INPUT FILL-IN_NY-LOSEN,OUTPUT felnrregler,OUTPUT alltOK).
   IF alltOK = FALSE THEN DO:  
      ASSIGN
      /*FILL-IN_KOLL = " "*/
      musz = FALSE
      FILL-IN_ANVANDARE = ""
      FILL-IN_AV-LOSEN = ""
      FILL-IN_NY-LOSEN = ""
      FILL-IN_NY-LOSEN-2 = "".
      DISPLAY FILL-IN_ANVANDARE FILL-IN_AV-LOSEN WITH FRAME {&FRAME-NAME}.  
      IF FILL-IN_NY-LOSEN:HIDDEN = FALSE THEN DISPLAY FILL-IN_NY-LOSEN FILL-IN_NY-LOSEN-2 WITH FRAME {&FRAME-NAME}.                 
      RETURN.         
   END.
   APPLY "WINDOW-CLOSE":U TO GURUW.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nytt_UI GURUW 
PROCEDURE nytt_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
   FILL-IN_AV-LOSEN = INPUT FRAME {&FRAME-NAME} FILL-IN_AV-LOSEN 
   FILL-IN_ANVANDARE = INPUT FILL-IN_ANVANDARE.
   IF FILL-IN_ANVANDARE = "FLEX" OR FILL-IN_ANVANDARE = "SELNDEPA" THEN DO:
      MESSAGE "Du är inte behörig att byta lösen på denna inloggning." VIEW-AS ALERT-BOX.       
      musz = TRUE.       
      RETURN.
   END.
   ENABLE FILL-IN_NY-LOSEN FILL-IN_NY-LOSEN-2 WITH FRAME {&FRAME-NAME}.
   DISPLAY FILL-IN_NY-LOSEN FILL-IN_NY-LOSEN-2 WITH FRAME {&FRAME-NAME}.
   ASSIGN
   FILL-IN_NY-LOSEN:HIDDEN = FALSE
   FILL-IN_NY-LOSEN-2:HIDDEN = FALSE. 
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

