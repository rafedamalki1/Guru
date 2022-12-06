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

  Created: 95/10/09 -  9:30 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER tidalltrec AS RECID NO-UNDO.
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
&Scoped-define SHARED SHARED
{PHMT.I}
{TIDALLT.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_PERSONALKOD 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FVE-2 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-3 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-2 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-3 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE {&NEW} SHARED VARIABLE CMB_PERIOD AS LOGICAL FORMAT "Dag/Period":U INITIAL NO 
     LABEL "Dag/Period" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Yes","No" 
     DROP-DOWN-LIST
     SIZE 11.63 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-DATUM AS INTEGER FORMAT ">9":U INITIAL ? 
     LABEL "Från den" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-DATUM-2 AS INTEGER FORMAT ">9":U INITIAL ? 
     LABEL "Till den" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-MANAD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_PERSONALKOD AS CHARACTER FORMAT "X(8)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.13.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_PERSONALKOD AT ROW 3.67 COL 12.25 COLON-ALIGNED
     CMB_PERIOD AT ROW 3.67 COL 33.13 COLON-ALIGNED
     BTN_NVE-3 AT ROW 6.04 COL 55.25
     BTN_NVE-2 AT ROW 6.08 COL 31.88
     FILL-IN-DATUM-2 AT ROW 6.38 COL 48.13 COLON-ALIGNED
     FILL-IN-DATUM AT ROW 6.42 COL 25.88 COLON-ALIGNED
     FILL-IN-MANAD AT ROW 6.46 COL 2.88 NO-LABEL
     BTN_FVE-3 AT ROW 6.92 COL 55.25
     BTN_FVE-2 AT ROW 6.96 COL 31.88
     BTN_OK AT ROW 9.75 COL 28.75
     BTN_AVB AT ROW 9.75 COL 43.75
     " Vill du ta bort beredskap för en hel vecka eller dag?" VIEW-AS TEXT
          SIZE 57 BY 1.5 AT ROW 1.5 COL 1.5
          FONT 17
     SPACE(0.00) SKIP(7.78)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Borttag beredskap vecka eller dag":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BTN_FVE-2 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_FVE-2:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR BUTTON BTN_FVE-3 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_FVE-3:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR BUTTON BTN_NVE-2 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NVE-2:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR BUTTON BTN_NVE-3 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NVE-3:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_PERIOD IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       CMB_PERIOD:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DATUM IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN-DATUM:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DATUM-2 IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN-DATUM-2:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-MANAD IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE ALIGN-L                                  */
ASSIGN 
       FILL-IN-MANAD:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_PERSONALKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Borttag beredskap vecka eller dag */
DO:
   musz = TRUE.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Borttag beredskap vecka eller dag */
DO:
   musz = TRUE.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   musz = TRUE.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-2 DIALOG-1
ON CHOOSE OF BTN_FVE-2 IN FRAME DIALOG-1 /* - */
DO: 
   ASSIGN
   FILL-IN-DATUM = INPUT FILL-IN-DATUM.   
   FILL-IN-DATUM = FILL-IN-DATUM - 1.   
   IF FILL-IN-DATUM = 0 THEN FILL-IN-DATUM = 1.
   IF tillochmeddatum NE ? THEN DO:
      IF DAY(tillochmeddatum) >= FILL-IN-DATUM THEN DO:
         FILL-IN-DATUM = DAY(tillochmeddatum + 1).
      END.
   END.   
   DISPLAY FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-3 DIALOG-1
ON CHOOSE OF BTN_FVE-3 IN FRAME DIALOG-1 /* - */
DO: 
   ASSIGN
   FILL-IN-DATUM-2 = INPUT FILL-IN-DATUM-2.   
   FILL-IN-DATUM-2 = FILL-IN-DATUM-2 - 1.   
   IF FILL-IN-DATUM-2 = 0 THEN FILL-IN-DATUM-2 = 1.
   IF tillochmeddatum NE ? THEN DO:
      IF DAY(tillochmeddatum) >= FILL-IN-DATUM-2 THEN DO:
         FILL-IN-DATUM-2 = DAY(tillochmeddatum + 1).
      END.
   END.   
   DISPLAY FILL-IN-DATUM-2 WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-2 DIALOG-1
ON CHOOSE OF BTN_NVE-2 IN FRAME DIALOG-1 /* + */
DO:   
   ASSIGN
   FILL-IN-DATUM = INPUT FILL-IN-DATUM.   
   FILL-IN-DATUM = FILL-IN-DATUM + 1.
   IF MONTH(tidallt.DATUM) = 12 THEN DO:
      datkoll = DATE(12,31,YEAR(tidallt.DATUM)).
   END.
   ELSE DO:   
      datkoll = DATE((MONTH(tidallt.DATUM) + 1),01,YEAR(tidallt.DATUM)) - 1.
   END.
   IF FILL-IN-DATUM > DAY(datkoll)THEN DO:    
      FILL-IN-DATUM = DAY(datkoll).
   END.      
   DISPLAY FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-3 DIALOG-1
ON CHOOSE OF BTN_NVE-3 IN FRAME DIALOG-1 /* + */
DO:   
   ASSIGN
   FILL-IN-DATUM-2 = INPUT FILL-IN-DATUM-2.   
   FILL-IN-DATUM-2 = FILL-IN-DATUM-2 + 1.
   IF MONTH(tidallt.DATUM) = 12 THEN DO:
      datkoll = DATE(12,31,YEAR(tidallt.DATUM)).
   END.
   ELSE DO:   
      datkoll = DATE((MONTH(tidallt.DATUM) + 1),01,YEAR(tidallt.DATUM)) - 1.
   END.
   IF FILL-IN-DATUM-2 > DAY(datkoll)THEN DO:    
      FILL-IN-DATUM-2 = DAY(datkoll).
   END.      
   DISPLAY FILL-IN-DATUM-2 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:   
   IF CMB_PERIOD = FALSE THEN DO:      
      ASSIGN
      FILL-IN-DATUM = INPUT FILL-IN-DATUM.         
      IF MONTH(tidallt.DATUM) = 12 THEN DO:
         datkoll = DATE(12,31,YEAR(tidallt.DATUM)).
      END.
      ELSE DO:   
         datkoll = DATE((MONTH(tidallt.DATUM) + 1),01,YEAR(tidallt.DATUM)) - 1.
      END.
      IF FILL-IN-DATUM > DAY(datkoll) THEN DO:
         MESSAGE "Felaktigt angivet datum. Denna månad har bara" DAY(datkoll) "dagar."
         VIEW-AS ALERT-BOX.         
         APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}.         
         RETURN NO-APPLY.
      END.            
      IF FILL-IN-DATUM <= 0 THEN DO:
         MESSAGE "Felaktigt angivet datum. Datum kan ej vara mindre än 1." 
         VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}.            
         RETURN NO-APPLY.
      END.            
      IF tillochmeddatum NE ? THEN DO:
         IF DAY(tillochmeddatum) >= FILL-IN-DATUM THEN DO:
            MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med"
            tillochmeddatum VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}. 
            RETURN NO-APPLY.
         END.            
      END.
      ASSIGN
      FILL-IN-DATUM-2 = INPUT FILL-IN-DATUM-2.         
      IF MONTH(tidallt.DATUM) = 12 THEN DO:
         datkoll = DATE(12,31,YEAR(tidallt.DATUM)).
      END.
      ELSE DO:   
         datkoll = DATE((MONTH(tidallt.DATUM) + 1),01,YEAR(tidallt.DATUM)) - 1.
      END.
      IF FILL-IN-DATUM-2 > DAY(datkoll) THEN DO:
         MESSAGE "Felaktigt angivet datum. Denna månad har bara" DAY(datkoll) "dagar."
         VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-DATUM-2 IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.            
      IF FILL-IN-DATUM-2 <= 0 THEN DO:
         MESSAGE "Felaktigt angivet datum. Datum kan ej vara mindre än 1." 
         VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-DATUM-2 IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.            
      IF tillochmeddatum NE ? THEN DO:
         IF DAY(tillochmeddatum) >= FILL-IN-DATUM-2 THEN DO:
            MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med"
            tillochmeddatum VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-DATUM-2 IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.            
      END.                              
      IF FILL-IN-DATUM-2 < FILL-IN-DATUM THEN DO:
         MESSAGE "Felaktigt angivet datum." VIEW-AS ALERT-BOX.           
         APPLY "ENTRY" TO FILL-IN-DATUM-2 IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.             
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_PERIOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_PERIOD DIALOG-1
ON VALUE-CHANGED OF CMB_PERIOD IN FRAME DIALOG-1 /* Dag/Period */
DO:
   CMB_PERIOD = INPUT CMB_PERIOD.
   IF CMB_PERIOD = TRUE THEN DO:
      ASSIGN
      BTN_FVE-2:HIDDEN = TRUE 
      BTN_NVE-2:HIDDEN = TRUE
      BTN_FVE-3:HIDDEN = TRUE 
      BTN_NVE-3:HIDDEN = TRUE  
      FILL-IN-DATUM-2:HIDDEN = TRUE
      FILL-IN-DATUM:HIDDEN = TRUE.
   END.
   ELSE DO:
      DISPLAY FILL-IN-MANAD label "Månad" FILL-IN-DATUM FILL-IN-DATUM-2 WITH FRAME {&FRAME-NAME}.
      ENABLE FILL-IN-DATUM FILL-IN-DATUM-2 BTN_FVE-2 BTN_NVE-2 BTN_FVE-3 BTN_NVE-3
      WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATUM DIALOG-1
ON LEAVE OF FILL-IN-DATUM IN FRAME DIALOG-1 /* Från den */
DO:
   musz = musz.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATUM-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATUM-2 DIALOG-1
ON LEAVE OF FILL-IN-DATUM-2 IN FRAME DIALOG-1 /* Till den */
DO:
   musz = musz.
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
   FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod 
   NO-LOCK NO-ERROR. 
   FIND tidallt WHERE RECID(tidallt) = tidalltrec NO-LOCK NO-ERROR.
   ASSIGN FILL-IN-DATUM = DAY(tidallt.DATUM).
   IF tillochmeddatum NE ? THEN DO:
      IF DAY(tillochmeddatum) >= FILL-IN-DATUM THEN DO:
         FILL-IN-DATUM = DAY(tillochmeddatum + 1).
      END.
   END.
   FILL-IN-DATUM-2 = FILL-IN-DATUM.      
   ASSIGN
   FILL-IN-MANAD = regmannamn
   FILL-IN_PERSONALKOD = tidallt.PERSONALKOD.
   RUN enable_UI.       
   {FRMSIZED.I}
   DISPLAY CMB_PERIOD WITH FRAME {&FRAME-NAME}.
   ENABLE CMB_PERIOD WITH FRAME {&FRAME-NAME}.
   
   {musarrow.i} 
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY FILL-IN_PERSONALKOD 
      WITH FRAME DIALOG-1.
  ENABLE BTN_OK BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

