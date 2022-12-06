&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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

  Created: 95/05/16 -  7:28 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER valvardnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER fasthetbet AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER marknrvar AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}

&Scoped-define NEW 
&Scoped-define SHARED SHARED 
{FASTIGHET.I}

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
/*DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.*/
DEFINE VARIABLE radnrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE procent AS INTEGER NO-UNDO.
DEFINE VARIABLE adel1 AS INTEGER NO-UNDO.
DEFINE VARIABLE adel2 AS INTEGER NO-UNDO.
DEFINE VARIABLE markprocapph AS HANDLE NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-ANDEL SEL_ANDEL BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ANDEL SEL_ANDEL FILL-IN_MARKNR ~
FILL-IN_MARKAGARE 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-ANDEL AS CHARACTER FORMAT "X(256)":U 
     LABEL "Andel" 
     VIEW-AS FILL-IN 
     SIZE 8.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_MARKAGARE AS CHARACTER FORMAT "X(50)" 
     LABEL "Markägare" 
     VIEW-AS FILL-IN 
     SIZE 36.25 BY 1.

DEFINE VARIABLE FILL-IN_MARKNR AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Markägarnummer" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE FILL-IN_PROCENT AS INTEGER FORMAT ">>9" INITIAL 100 
     LABEL "Procent" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE SEL_ANDEL AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "1/1","1/2","1/3","2/3","1/4","2/4","3/4","1/5","2/5","3/5","4/5","1/6","2/6","3/6","4/6","5/6","1/7","2/7","3/7","4/7","5/7","6/7","1/8","2/8","3/8","4/8","5/8","6/8","7/8","1/9","2/9","3/9","4/9","5/9","6/9","7/9","8/9","1/10","2/10","3/10","4/10","5/10","6/10","7/10","8/10","9/10" 
     SIZE 8.5 BY 21.25 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-ANDEL AT ROW 6.83 COL 15.5 COLON-ALIGNED
     SEL_ANDEL AT ROW 1.5 COL 58.25 NO-LABEL
     FILL-IN_PROCENT AT ROW 9.08 COL 15.5 COLON-ALIGNED
     BTN_OK AT ROW 23.25 COL 37.75
     FILL-IN_MARKNR AT ROW 4.21 COL 15.5 COLON-ALIGNED
     FILL-IN_MARKAGARE AT ROW 5.54 COL 15.5 COLON-ALIGNED
     BTN_AVB AT ROW 23.25 COL 52.75
     "Hur stor andel av fastigheten äger denna markägare?" VIEW-AS TEXT
          SIZE 55.5 BY 2.5 AT ROW 1.08 COL 2
          FONT 17
     SPACE(10.62) SKIP(20.83)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "MARKÄGARPROCENT":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_MARKAGARE IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_MARKNR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_PROCENT IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_PROCENT:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* MARKÄGARPROCENT */
DO:
   IF VALID-HANDLE(markprocapph) THEN DELETE PROCEDURE markprocapph.
   musz = TRUE.
   RETURN.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* MARKÄGARPROCENT */
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


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO: 
   
   ASSIGN   
   FILL-IN-ANDEL = INPUT FILL-IN-ANDEL.
   IF FILL-IN-ANDEL = "" THEN DO:
      MESSAGE "Det är obligatoriskt att ange hur stor andel markägaren äger, anges tex 1/2 " VIEW-AS ALERT-BOX.
      RETURN.
   END.
   IF INDEX(FILL-IN-ANDEL,"/") = 0 THEN DO:
      MESSAGE "Andel anges tex 1/2 " VIEW-AS ALERT-BOX.
      RETURN.
   END.
   IF INDEX(FILL-IN-ANDEL,".") NE 0 THEN DO:
      MESSAGE "Andel anges tex 1/2" VIEW-AS ALERT-BOX.         
      RETURN.
   END.
   adel1 = INTEGER(SUBSTRING(FILL-IN-ANDEL,1,INDEX(FILL-IN-ANDEL,"/") - 1)) .
   adel2 = INTEGER(SUBSTRING(FILL-IN-ANDEL,INDEX(FILL-IN-ANDEL,"/") + 1)) .
   IF adel1 > adel2 THEN DO:
      MESSAGE "Andel anges tex 1/2 " VIEW-AS ALERT-BOX.
      RETURN.
   END.
   /* Om de vill ha med en väghållare som inte ska ha ersättning men det ska skrivas avtal
   måste han kunna registreras med 0/0. Då går det bara att ta ut upplåtelseavtal.
   IF adel1 > 0 AND  adel2 > 0 THEN adel1 = adel1.
   ELSE DO:   
      MESSAGE "Andel anges tex 1/2 " VIEW-AS ALERT-BOX.
      RETURN.
   END.*/
   CREATE valmarkfast2.
   BUFFER-COPY markagaretemp TO valmarkfast2.
   ASSIGN 
   valmarkfast2.BETECKNING = fastighettemp.BETECKNING 
   valmarkfast2.MARKNR = markagaretemp.MARKNR
   valmarkfast2.ANDEL = FILL-IN-ANDEL
   valmarkfast2.ANDEL1 = adel1 
   valmarkfast2.ANDEL2 = adel2
   valmarkfast2.PROCENT = 100 * valmarkfast2.ANDEL1 / valmarkfast2.ANDEL2
   valmarkfast2.PRODEL = 100 * valmarkfast2.ANDEL1 / valmarkfast2.ANDEL2.   
   RUN btnok2 IN markprocapph (INPUT valmarkfast2.PROCENT,INPUT fasthetbet,INPUT marknrvar,
                              INPUT TABLE valmarkfast2,OUTPUT TABLE felmeddtemp).
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.     
      RETURN NO-APPLY.
   END.
   FIND FIRST valmarkfast2 WHERE valmarkfast2.BETECKNING = fasthetbet AND 
   valmarkfast2.MARKNR = marknrvar NO-LOCK NO-ERROR.
   IF AVAILABLE valmarkfast2 THEN DO:
      ASSIGN 
      valmarkfast2.MARKAGARE = FILL-IN_MARKAGARE
      valmarkfast2.ANDEL = FILL-IN-ANDEL
      valmarkfast2.PROCENT = procent     
      valmarkfast2.ANDEL1 = adel1 
      valmarkfast2.ANDEL2 = adel2
      valmarkfast2.PROCENT = 100 * valmarkfast2.ANDEL1 / valmarkfast2.ANDEL2
      valmarkfast2.PRODEL = 100 * valmarkfast2.ANDEL1 / valmarkfast2.ANDEL2.
      DISPLAY FILL-IN-ANDEL WITH FRAME {&FRAME-NAME}.
   END.

   APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON GO OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
   IF VALID-HANDLE(markprocapph) THEN DELETE PROCEDURE markprocapph.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ANDEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ANDEL DIALOG-1
ON LEAVE OF FILL-IN-ANDEL IN FRAME DIALOG-1 /* Andel */
DO:
  FILL-IN-ANDEL = INPUT FILL-IN-ANDEL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_PROCENT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_PROCENT DIALOG-1
ON LEAVE OF FILL-IN_PROCENT IN FRAME DIALOG-1 /* Procent */
DO:
  FILL-IN_PROCENT = INPUT FILL-IN_PROCENT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_ANDEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_ANDEL DIALOG-1
ON VALUE-CHANGED OF SEL_ANDEL IN FRAME DIALOG-1
DO:
   SEL_ANDEL = INPUT SEL_ANDEL.
   FILL-IN-ANDEL = SEL_ANDEL.   
   DISPLAY FILL-IN-ANDEL WITH FRAME {&FRAME-NAME}.
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
   /*FIND FIRST valmarkfast2 WHERE valmarkfast2.BETECKNING = fasthetbet AND 
   valmarkfast2.MARKNR = marknrvar NO-LOCK NO-ERROR.*/
   FILL-IN-ANDEL = "1/1".
   FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = fasthetbet NO-LOCK NO-ERROR.
   FIND FIRST markagaretemp WHERE markagaretemp.MARKNR = marknrvar USE-INDEX MARKNR NO-LOCK NO-ERROR.
   ASSIGN FILL-IN_MARKAGARE = markagaretemp.MARKAGARE
   FILL-IN_MARKNR = markagaretemp.MARKNR.
   RUN enable_UI.       
   {FRMSIZED.I}
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
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   IF Guru.Konstanter:appcon THEN DO:
      RUN MARKPROCAPP.P PERSISTENT SET markprocapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MARKPROCAPP.P PERSISTENT SET markprocapph.
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
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
  DISPLAY FILL-IN-ANDEL SEL_ANDEL FILL-IN_MARKNR FILL-IN_MARKAGARE 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-ANDEL SEL_ANDEL BTN_OK BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

