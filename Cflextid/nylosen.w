&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME GURUW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS GURUW 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
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

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE storkollbredd AS INTEGER NO-UNDO.
DEFINE VARIABLE storkollhojd AS INTEGER NO-UNDO.
DEFINE VARIABLE losenwebh AS HANDLE NO-UNDO.
DEFINE VARIABLE felNr AS INTEGER NO-UNDO.
DEFINE VARIABLE alltOK AS LOGICAL NO-UNDO.
DEFINE VARIABLE meddelandevar AS CHARACTER NO-UNDO.
&Scoped-define NEW   
&Scoped-define SHARED
{ANVPERS.I}
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO. 
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR anvandartemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-C

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOG_LOS FILL-IN_NY-LOSEN FILL-IN_NY-LOSEN-2 ~
BTN_OK BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS TOG_LOS 

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

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_KOLL AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 15.72 BY .96 NO-UNDO.

DEFINE VARIABLE FILL-IN_NY-LOSEN AS CHARACTER FORMAT "X(256)" 
     LABEL "Nytt lösenord" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.

DEFINE VARIABLE FILL-IN_NY-LOSEN-2 AS CHARACTER FORMAT "X(256)" 
     LABEL "Ange nytt lösenord igen" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.

DEFINE VARIABLE TOG_LOS AS LOGICAL INITIAL no 
     LABEL "Lösenordet i klartext" 
     VIEW-AS TOGGLE-BOX
     SIZE 26.14 BY .88 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-C
     TOG_LOS AT ROW 3.31 COL 1.57 WIDGET-ID 2
     FILL-IN_KOLL AT ROW 4.58 COL 2.86 NO-LABEL
     FILL-IN_NY-LOSEN AT ROW 7.77 COL 24.57 COLON-ALIGNED BLANK 
     FILL-IN_NY-LOSEN-2 AT ROW 9.38 COL 24.57 COLON-ALIGNED BLANK 
     BTN_OK AT ROW 11.5 COL 32
     BTN_AVS AT ROW 11.5 COL 47
     "Byte av lösenord" VIEW-AS TEXT
          SIZE 32.57 BY 1.54 AT ROW 1.19 COL 1.57
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 63.13 BY 12.17.


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
         HEIGHT             = 12.31
         WIDTH              = 63.14
         MAX-HEIGHT         = 22.88
         MAX-WIDTH          = 87.29
         VIRTUAL-HEIGHT     = 22.88
         VIRTUAL-WIDTH      = 87.29
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
/* SETTINGS FOR FILL-IN FILL-IN_KOLL IN FRAME FRAME-C
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       FILL-IN_KOLL:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_NY-LOSEN IN FRAME FRAME-C
   NO-DISPLAY                                                           */
/* SETTINGS FOR FILL-IN FILL-IN_NY-LOSEN-2 IN FRAME FRAME-C
   NO-DISPLAY                                                           */
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
   IF VALID-HANDLE(losenwebh) THEN DELETE PROCEDURE losenwebh NO-ERROR.
   losenwebh = ?.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK GURUW
ON CHOOSE OF BTN_OK IN FRAME FRAME-C /* Ok */
DO:
   RUN koll_UI.
  /* APPLY "ENTRY" TO FILL-IN_ANVANDARE IN FRAME {&FRAME-NAME}.*/
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK GURUW
ON END-ERROR OF BTN_OK IN FRAME FRAME-C /* Ok */
DO:
   /*FILL-IN_NY-LOSEN = FILL-IN_NY-LOSEN.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK GURUW
ON ENDKEY OF BTN_OK IN FRAME FRAME-C /* Ok */
DO:
  /* FILL-IN_NY-LOSEN = FILL-IN_NY-LOSEN. */
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
ON ANY-KEY OF FILL-IN_NY-LOSEN-2 IN FRAME FRAME-C /* Ange nytt lösenord igen */
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
ON LEAVE OF FILL-IN_NY-LOSEN-2 IN FRAME FRAME-C /* Ange nytt lösenord igen */
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
      FILL-IN_NY-LOSEN:PASSWORD-FIELD  = FALSE.
      FILL-IN_NY-LOSEN:PASSWORD-FIELD  = FALSE.
      FILL-IN_NY-LOSEN-2:PASSWORD-FIELD  = FALSE.
      DISPLAY FILL-IN_NY-LOSEN WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      FILL-IN_NY-LOSEN:PASSWORD-FIELD  = TRUE.
      FILL-IN_NY-LOSEN:PASSWORD-FIELD  = TRUE.
      FILL-IN_NY-LOSEN-2:PASSWORD-FIELD  = TRUE.
      DISPLAY FILL-IN_NY-LOSEN WITH FRAME {&FRAME-NAME}.
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
   FILL-IN_NY-LOSEN:BLANK  = FALSE.
   FILL-IN_NY-LOSEN-2:BLANK  = FALSE.
   FILL-IN_NY-LOSEN:PASSWORD-FIELD  = TRUE.
   FILL-IN_NY-LOSEN-2:PASSWORD-FIELD  = TRUE.
   IF Guru.Konstanter:appcon THEN DO:
      IF NOT VALID-HANDLE(losenwebh) THEN RUN LOSENKOLLWEB.P PERSISTENT SET losenwebh ON Guru.Konstanter:apphand TRANSACTION DISTINCT . 
   END.
   ELSE DO:
      IF NOT VALID-HANDLE(losenwebh) THEN RUN LOSENKOLLWEB.P PERSISTENT SET losenwebh .    
   END.
   RUN AppSpringSet_UI  IN losenwebh (INPUT  Guru.Konstanter:AppSpringSet).
   FIND FIRST anvandartemp WHERE anvandartemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   RUN enable_UI.   
   {FRMSIZE.I}  
   APPLY "ENTRY" TO FILL-IN_NY-LOSEN. 
   /*status-ok = FILL-IN_NY-LOSEN:MOVE-AFTER-TAB-ITEM(my1hand).*/
   my1hand = FILL-IN_NY-LOSEN:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = FILL-IN_NY-LOSEN-2:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = FILL-IN_NY-LOSEN-2:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BTN_OK:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = BTN_OK:HANDLE IN FRAME {&FRAME-NAME}.   
   status-ok = BTN_AVS:MOVE-AFTER-TAB-ITEM(my1hand).
   {musarrow.i}
   {WIN_M_SLUT.I}
   RUN CenterWindow IN Guru.Konstanter:hpApi (INPUT CURRENT-WINDOW).
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
  DISPLAY TOG_LOS 
      WITH FRAME FRAME-C IN WINDOW GURUW.
  ENABLE TOG_LOS FILL-IN_NY-LOSEN FILL-IN_NY-LOSEN-2 BTN_OK BTN_AVS 
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
   DEFINE VARIABLE uppdatlosen AS CHARACTER NO-UNDO.
   ASSIGN  
   FILL-IN_NY-LOSEN = INPUT FRAME {&FRAME-NAME} FILL-IN_NY-LOSEN
   FILL-IN_NY-LOSEN-2 = INPUT FRAME {&FRAME-NAME} FILL-IN_NY-LOSEN-2.
   /*NYA LÖSEN ÄR INTE LIKA*/
   IF FILL-IN_NY-LOSEN NE FILL-IN_NY-LOSEN-2 THEN DO:
      ASSIGN
      FILL-IN_NY-LOSEN = ""
      FILL-IN_NY-LOSEN-2 = "".
      MESSAGE "Det nya lösenordet blev felaktigt angivet." VIEW-AS ALERT-BOX.               
      RETURN.     
   END.
   /*NYA LÖSEN ÄR SAMMA SOM GAMLA*/
   IF Guru.GlobalaVariabler:Kryptonit = TRUE THEN DO:
      IF Guru.Konstanter:SaltRetur(FILL-IN_NY-LOSEN) = anvandartemp.AV-LOSEN  THEN DO:
         ASSIGN
         FILL-IN_NY-LOSEN = ""
         FILL-IN_NY-LOSEN-2 = "".
         MESSAGE "Det nya lösenordet får inte vara samma som det gamla!" VIEW-AS ALERT-BOX.               
         RETURN.     
      END.
   END.   
   ELSE DO:
      IF FILL-IN_NY-LOSEN = anvandartemp.AV-LOSEN  THEN DO:
         ASSIGN
         FILL-IN_NY-LOSEN = ""
         FILL-IN_NY-LOSEN-2 = "".
         MESSAGE "Det nya lösenordet får inte vara samma som det gamla!" VIEW-AS ALERT-BOX.               
         RETURN.     
      END.
   END.  
   
   uppdatlosen = "". 
   RUN losenReglerKoll_UI IN losenwebh (INPUT anvandartemp.ANVANDARE,INPUT-OUTPUT uppdatlosen,INPUT FILL-IN_NY-LOSEN, OUTPUT felNr, OUTPUT alltOk,OUTPUT meddelandevar).
   IF alltOk = FALSE THEN DO:
      MESSAGE meddelandevar
      VIEW-AS ALERT-BOX.
      ASSIGN
      FILL-IN_NY-LOSEN = ""
      FILL-IN_NY-LOSEN-2 = "".
      DISPLAY FILL-IN_NY-LOSEN FILL-IN_NY-LOSEN-2 WITH FRAME {&FRAME-NAME}.
      RETURN.
   END.   
   IF Guru.GlobalaVariabler:Kryptonit = TRUE THEN DO:
      anvandartemp.AV-LOSEN = uppdatlosen.
   END.   
   ELSE DO:
      anvandartemp.AV-LOSEN = uppdatlosen.
   END.         
   IF VALID-HANDLE(losenwebh) THEN DELETE PROCEDURE losenwebh NO-ERROR.
   losenwebh = ?.      
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
  
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

