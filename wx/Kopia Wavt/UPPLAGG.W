&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WINDOW-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 11/24/95 -  6:05 pm

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
DEFINE NEW GLOBAL SHARED VARIABLE hpApi AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE hpWinFunc AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE retvalkoll AS LOGICAL NO-UNDO.



DEFINE NEW SHARED VARIABLE globDefaultstorh AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE globDefaultstorb AS INTEGER NO-UNDO.





DEFINE NEW SHARED VARIABLE guruvarmellan AS CHARACTER NO-UNDO.
 



&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
&Scoped-define NEW NEW
{windows.i}
/*{EGENBVAR.I}*/
{GLOBVAR2DEL1.I}
{ALLDEF.I}


DEFINE NEW SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.


DEFINE  VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE  VARIABLE muszval AS INTEGER NO-UNDO.

DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-C

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS SEL_TABB BTN_ANST BTN_STR BTN_BERTAB ~
BTN_TRAAV BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS SEL_TABB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ANST 
     LABEL "Nytt anstformavtal" 
     SIZE 20 BY 1.59.

DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta" 
     SIZE 22 BY 3.5.

DEFINE BUTTON BTN_BERTAB 
     LABEL "Nytt bertabavtal" 
     SIZE 20 BY 1.59.

DEFINE BUTTON BTN_STR 
     LABEL "Starta guru" 
     SIZE 19 BY 4.5.

DEFINE BUTTON BTN_TRAAV 
     LABEL "Nytt traavtal" 
     SIZE 20 BY 1.59.

DEFINE VARIABLE SEL_TABB AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 17.5 BY 11.5
     BGCOLOR 8  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-C
     SEL_TABB AT ROW 3.5 COL 30.5 NO-LABEL
     BTN_ANST AT ROW 5.59 COL 53.13
     BTN_STR AT ROW 6.5 COL 7
     BTN_BERTAB AT ROW 7.82 COL 53.13
     BTN_TRAAV AT ROW 10 COL 53.13
     BTN_AVS AT ROW 16.5 COL 29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82 BY 22.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Window 1"
         COLUMN             = 14.63
         ROW                = 2.64
         HEIGHT             = 22
         WIDTH              = 82
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 82
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 82
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


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-1
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-C
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_ANST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANST WINDOW-1
ON CHOOSE OF BTN_ANST IN FRAME FRAME-C /* Nytt anstformavtal */
DO:
  RUN ANSTAVTAL.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-1
ON CHOOSE OF BTN_AVS IN FRAME FRAME-C /* Avsluta */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BERTAB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BERTAB WINDOW-1
ON CHOOSE OF BTN_BERTAB IN FRAME FRAME-C /* Nytt bertabavtal */
DO:
  RUN BERAVTAL.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_STR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_STR WINDOW-1
ON CHOOSE OF BTN_STR IN FRAME FRAME-C /* Starta guru */
DO:
   RUN GURUST.P.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TRAAV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TRAAV WINDOW-1
ON CHOOSE OF BTN_TRAAV IN FRAME FRAME-C /* Nytt traavtal */
DO:
  RUN TRAAVTAL.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_TABB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_TABB WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF SEL_TABB IN FRAME FRAME-C
DO:
   
   SEL_TABB = INPUT SEL_TABB.
   IF SEL_TABB = "AODRIFT" THEN RUN AODRIFT.W.
   IF SEL_TABB = "AVDELNING" THEN RUN   AVDEL.W.
   IF SEL_TABB = "ANSTFORMTAB" THEN RUN ANSTFORM.W.
   IF SEL_TABB = "AUTOMREG" THEN  RUN AUTOMREG.W.
   IF SEL_TABB = "AVDRAGMALTID" THEN  RUN AVDRAGM.W.
   IF SEL_TABB = "BEFATTNINGSTAB" THEN  RUN BEFATT.W.
   IF SEL_TABB = "BEREDSKAPAV" THEN  RUN BEREDAV.W.
   IF SEL_TABB = "BEREDSKAPSTART" THEN  RUN BERST.W.
   IF SEL_TABB = "BEREDSKAPTAB" THEN  RUN BERETAB.W.
   IF SEL_TABB = "BERHOJN" THEN  RUN BERHOJN.W.
   IF SEL_TABB = "BERKOD" THEN  RUN BERKOD.W.
   IF SEL_TABB = "BERTAB" THEN  RUN BERTAB.W.
   IF SEL_TABB = "BHOJ" THEN  RUN BHOJ.W.
   IF SEL_TABB = "BILFORARE" THEN  RUN BILF.W.
   IF SEL_TABB = "BORTPERS" THEN  RUN BORTP.W.
   IF SEL_TABB = "EKONOMIKONTO" THEN  RUN EKONOMI.W.
   IF SEL_TABB = "ERSATTNING" THEN  RUN ERSATT.W.
   IF SEL_TABB = "LAGBAS" THEN  RUN LAGBAS.W.   
   IF SEL_TABB = "FVARO" THEN  RUN FVARO.W.
   IF SEL_TABB = "FRDEL" THEN  RUN FRDEL.W.   
   IF SEL_TABB = "FLEXREG" THEN  RUN FLREG.W.   
   IF SEL_TABB = "INDEL" THEN  RUN INDEL.W.
   IF SEL_TABB = "KALKTYP" THEN  RUN KALKTYP.W.
   IF SEL_TABB = "KALKKATEGORI" THEN  RUN KKATGORI.W.
   IF SEL_TABB = "KFORMAN" THEN  RUN KFORMAN.W.
   IF SEL_TABB = "LONFLER" THEN  RUN LONFLER.W.
   IF SEL_TABB = "LONKORT" THEN  RUN LONKORT.W.
   IF SEL_TABB = "LONTILL" THEN  RUN LONTILL.W.
   IF SEL_TABB = "MALKOD" THEN  RUN MALKOD.W.
   IF SEL_TABB = "MALTAB" THEN  RUN MALTAB.W.
   IF SEL_TABB = "MALTIDTAB" THEN  RUN MALTTAB.W.
   IF SEL_TABB = "NFALL" THEN  RUN NFALL.W.
   IF SEL_TABB = "OBAVTAB" THEN  RUN OBAV.W.
   IF SEL_TABB = "OBTAB" THEN  RUN OBTAB.W.
   IF SEL_TABB = "ORDARB" THEN  RUN ORDARB.W.
   IF SEL_TABB = "OVERAVTAB" THEN  RUN OVERAVT.W.
   IF SEL_TABB = "OVERKOD" THEN  RUN OVERKOD.W.
   IF SEL_TABB = "OVERTIDAVIK" THEN  RUN OTIDAVIK.W.
   IF SEL_TABB = "OVERTIDTAB" THEN  RUN OTIDTAB.W.
   IF SEL_TABB = "PROGVAL" THEN  RUN PROGVAL.W.
   IF SEL_TABB = "RESTIDTAB" THEN  RUN RESTAB.W.
   IF SEL_TABB = "TIDREGITAB" THEN  RUN TIDREGIT.W.
   IF SEL_TABB = "TIMKOSTNADSTAB" THEN  RUN TIMKOST.W.
   IF SEL_TABB = "TRAAVTAB" THEN  RUN TRAAVTAB.W.
   IF SEL_TABB = "TRAKTASTART" THEN  RUN TRAST.W.
   IF SEL_TABB = "TRAKTATAB" THEN  RUN TRATAB.W.
   IF SEL_TABB = "TRAKTFLER" THEN  RUN TRAFLER.W.
   IF SEL_TABB = "TRAKTREGLER" THEN  RUN TREGLER.W.
   IF SEL_TABB = "UTRTAB" THEN  RUN UTRTAB.W.
   IF SEL_TABB = "UTRYCKNING" THEN  RUN UTRYCKN.W.
   IF SEL_TABB = "VKORN" THEN  RUN VKORN.W.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


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
   FOR EACH _FILE  NO-LOCK:
      IF SUBSTRING(_FILE._FILE-NAME,1,1) NE "_" THEN DO:
         status-ok = SEL_TABB:ADD-LAST(_FILE._FILE-NAME).
      END.
   END.
   FIND FIRST FORETAG NO-LOCK NO-ERROR.
   Guru.Konstanter:globforetag = FORETAG.FORETAG.
   RUN enable_UI.
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI WINDOW-1 
PROCEDURE andra_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*?NDRA*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI WINDOW-1 
PROCEDURE bort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*BORT*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1 _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
  THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1 _DEFAULT-ENABLE
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
  DISPLAY SEL_TABB 
      WITH FRAME FRAME-C IN WINDOW WINDOW-1.
  ENABLE SEL_TABB BTN_ANST BTN_STR BTN_BERTAB BTN_TRAAV BTN_AVS 
      WITH FRAME FRAME-C IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-C}
  VIEW WINDOW-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny_UI WINDOW-1 
PROCEDURE ny_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*NY*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


