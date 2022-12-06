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

  Created: 10/15/96 -  2:40 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER visaigen AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER igendat AS DATE NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}

&Scoped-define NEW
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE datkoll AS DATE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_LISTA BTN_VISA BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_LISTA 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-GO 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE {&NEW} SHARED VARIABLE RAD_LISTA AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "", 1
     SIZE 61.38 BY 10.29 NO-UNDO.

DEFINE BUTTON BTN_FVE-2 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-2 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE VARIABLE FILL-IN-DAG AS CHARACTER FORMAT "X(7)":U 
     LABEL "Dag" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     RAD_LISTA AT ROW 3.21 COL 1.5 NO-LABEL
     BTN_VISA AT ROW 7.75 COL 66
     BTN_AVB AT ROW 20.5 COL 66 WIDGET-ID 2
     "Vilken lista vill du se?:" VIEW-AS TEXT
          SIZE 48.5 BY 1.75 AT ROW 1.21 COL 1.5
          FONT 17
     SPACE(31.24) SKIP(19.20)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Val av typ-kontroll".

DEFINE FRAME FRAME-DAT
     BTN_NVE-2 AT ROW 5 COL 33.63 WIDGET-ID 4
     FILL-IN-STARTDAT AT ROW 5.42 COL 7.63 COLON-ALIGNED WIDGET-ID 8
     FILL-IN-DAG AT ROW 5.42 COL 22.5 COLON-ALIGNED WIDGET-ID 6
     BTN_FVE-2 AT ROW 5.83 COL 33.63 WIDGET-ID 2
     "Ange till och med vilket datum" VIEW-AS TEXT
          SIZE 33.5 BY 1.33 AT ROW 1.58 COL 1.5 WIDGET-ID 10
          FONT 17
     "som tidsedlarna skall kontrolleras" VIEW-AS TEXT
          SIZE 35.5 BY 1.33 AT ROW 3.17 COL 1.5 WIDGET-ID 12
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.13 ROW 14.5
         SIZE 52 BY 7.25 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* REPARENT FRAME */
ASSIGN FRAME FRAME-DAT:FRAME = FRAME DIALOG-1:HANDLE.

/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_LISTA IN FRAME DIALOG-1
   SHARED                                                               */
/* SETTINGS FOR FRAME FRAME-DAT
                                                                        */
ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME FRAME-DAT           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   /*musz = TRUE.*/
   visaigen = FALSE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-DAT
&Scoped-define SELF-NAME BTN_FVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-2 DIALOG-1
ON CHOOSE OF BTN_FVE-2 IN FRAME FRAME-DAT /* - */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT - 1.     
   IF MONTH(FILL-IN-STARTDAT) NE MONTH(datkoll) THEN 
   FILL-IN-STARTDAT = DATE(MONTH(datkoll),01,YEAR(datkoll)).
   RUN dag_UI.       
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-2 DIALOG-1
ON CHOOSE OF BTN_NVE-2 IN FRAME FRAME-DAT /* + */
DO:   
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT + 1.  
   IF MONTH(FILL-IN-STARTDAT) NE MONTH(datkoll) THEN DO:
      IF MONTH(datkoll) = 12 THEN FILL-IN-STARTDAT = DATE(MONTH(datkoll),31,YEAR(datkoll)). 
      ELSE FILL-IN-STARTDAT = DATE(MONTH(datkoll) + 1,01,YEAR(datkoll)) - 1.      
   END.
   RUN dag_UI.      
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define SELF-NAME BTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISA DIALOG-1
ON CHOOSE OF BTN_VISA IN FRAME DIALOG-1 /* Visa */
DO:
   {muswait.i}  
   RAD_LISTA = INPUT RAD_LISTA.
    visaigen = TRUE.   
   igendat = regdatum.
   IF FRAME FRAME-DAT:HIDDEN = FALSE THEN DO:
      avdatum = regdatum.
   END.     
   APPLY "GO" TO FRAME {&FRAME-NAME}.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-DAT
&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT DIALOG-1
ON LEAVE OF FILL-IN-STARTDAT IN FRAME FRAME-DAT /* Datum */
DO:
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
   
   RUN dag_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME FRAME-DAT /* Datum */
DO:
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STARTDAT.
   RUN AlmanBtn.w.
   FILL-IN-STARTDAT = Guru.GlobalaVariabler:regdatum.
   RUN dag_UI.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define SELF-NAME RAD_LISTA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_LISTA DIALOG-1
ON VALUE-CHANGED OF RAD_LISTA IN FRAME DIALOG-1
DO:
   RAD_LISTA = INPUT RAD_LISTA.
   IF (RAD_LISTA = 8 OR RAD_LISTA = 9) THEN ASSIGN FRAME FRAME-DAT:HIDDEN = TRUE.
   ELSE DO:
      IF MONTH(TODAY) = MONTH(avdatum) AND YEAR(TODAY) = YEAR(avdatum)
      THEN DO:
         ASSIGN FRAME FRAME-DAT:HIDDEN = FALSE.
      END.
   END.     
   /*{muswait.i} 
   APPLY "GO" TO FRAME {&FRAME-NAME}.*/ 
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
  {musarrow.i}
  status-ok = RAD_LISTA:DELETE("").
  status-ok = RAD_LISTA:ADD-LAST("Övertid vald månad och innevarande år", 11).
  status-ok = RAD_LISTA:ADD-LAST("Övertid mer än 200 timmar hela valda året", 16).
  status-ok = RAD_LISTA:ADD-LAST("Aktuellt flexsaldo", 8). 
  status-ok = RAD_LISTA:ADD-LAST("Uttagen arbetstidsförkortning under året", 9).    
  status-ok = RAD_LISTA:ADD-LAST("Sjukdom vald månad", 2).
  status-ok = RAD_LISTA:ADD-LAST("Sammanställning status", 17).
  status-ok = RAD_LISTA:ADD-LAST("Ej tolkad övertid vald månad och innevarande år", 18).
  status-ok = RAD_LISTA:ADD-LAST("Kontroll sjuk månadsskifte", 3).
  status-ok = RAD_LISTA:ADD-LAST("Beredskap-sjukdom kontroll", 4).
  status-ok = RAD_LISTA:ADD-LAST("Ob-sjukdom kontroll", 13).
  status-ok = RAD_LISTA:ADD-LAST("Vård av barn-personnummer,Enskild angelägenhet,Tjled m lön -kommentar", 6).   
  status-ok = RAD_LISTA:ADD-LAST("Kontroll registrerad tid", 1). 
   
   
  IF MONTH(regdatum) = 12 THEN avdatum = DATE(12,31,YEAR(regdatum)). 
  ELSE avdatum = DATE((MONTH(regdatum) + 1),01,YEAR(regdatum)) - 1.
  IF MONTH(TODAY) = MONTH(avdatum) AND YEAR(TODAY) = YEAR(avdatum)
  THEN DO:
     IF visaigen = FALSE THEN DO:
        IF MONTH(TODAY) = MONTH(TODAY - 1 ) THEN regdatum = TODAY - 1.
        ELSE regdatum = TODAY.
     END.   
     ASSIGN FRAME FRAME-DAT:HIDDEN = FALSE.
     IF (RAD_LISTA = 8 OR RAD_LISTA = 9) THEN ASSIGN FRAME FRAME-DAT:HIDDEN = TRUE.  
  END.   
  ELSE ASSIGN FRAME FRAME-DAT:HIDDEN = TRUE.
  
  ASSIGN
  FILL-IN-STARTDAT = regdatum
  datkoll = regdatum.  
  RUN dag_UI.
  RUN enable_UI.
  {DIA_M_SLUT.I}
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dag_UI DIALOG-1 
PROCEDURE dag_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   regdatum = FILL-IN-STARTDAT.
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG WITH FRAME FRAME-DAT.
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
  HIDE FRAME FRAME-DAT.
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
  DISPLAY RAD_LISTA 
      WITH FRAME DIALOG-1.
  ENABLE RAD_LISTA BTN_VISA BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
  DISPLAY FILL-IN-STARTDAT FILL-IN-DAG 
      WITH FRAME FRAME-DAT.
  ENABLE BTN_NVE-2 FILL-IN-STARTDAT FILL-IN-DAG BTN_FVE-2 
      WITH FRAME FRAME-DAT.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-DAT}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

