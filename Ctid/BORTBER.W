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

  Created: 95/05/03 -  2:05 pm

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
&Scoped-define NEW NEW
DEFINE SHARED VARIABLE placerarec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE bustart3 AS DECIMAL NO-UNDO. 
DEFINE SHARED VARIABLE brwbdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE brwavdatum AS DATE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO. 
DEFINE VARIABLE tidtabrecspar AS RECID NO-UNDO.
DEFINE VARIABLE nyber AS INTEGER NO-UNDO.

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
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_PERSONALKOD FILL-IN-BERSTART ~
FILL-IN-BERSLUT FILL-IN_BEREDSKAP FILL-IN_BERANTAL 

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

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE {&NEW} SHARED VARIABLE CMB_DAG AS LOGICAL FORMAT "Dag/Vecka":U INITIAL NO 
     LABEL "Dag/Vecka" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Yes","No" 
     DROP-DOWN-LIST
     SIZE 11.63 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE CMB_PERIOD AS LOGICAL FORMAT "Dag/Period":U INITIAL NO 
     LABEL "Dag/Period" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Yes","No" 
     DROP-DOWN-LIST
     SIZE 11.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BERSLUT AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Slut tid" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BERSTART AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Start tid" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

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

DEFINE VARIABLE FILL-IN_BERANTAL AS DECIMAL FORMAT "->>>>9.99" INITIAL 0 
     LABEL "Antal" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_BEREDSKAP AS CHARACTER FORMAT "X(4)" 
     LABEL "Löneart" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_DAG AS CHARACTER FORMAT "X(8)" 
     LABEL "Dag" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_PERSONALKOD AS CHARACTER FORMAT "X(8)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_PERSONALKOD AT ROW 3.38 COL 12 COLON-ALIGNED
     FILL-IN_DAG AT ROW 3.38 COL 51 COLON-ALIGNED
     FILL-IN-MANAD AT ROW 3.46 COL 32.88 COLON-ALIGNED NO-LABEL
     FILL-IN-DATUM AT ROW 4.88 COL 12 COLON-ALIGNED
     FILL-IN-DATUM-2 AT ROW 4.88 COL 32.88 COLON-ALIGNED
     FILL-IN-BERSTART AT ROW 6.46 COL 11.88 COLON-ALIGNED
     FILL-IN-BERSLUT AT ROW 6.46 COL 32.88 COLON-ALIGNED
     FILL-IN_BEREDSKAP AT ROW 7.83 COL 11.88 COLON-ALIGNED
     FILL-IN_BERANTAL AT ROW 7.96 COL 32.88 COLON-ALIGNED
     CMB_PERIOD AT ROW 9.42 COL 43.38 COLON-ALIGNED
     CMB_DAG AT ROW 10.58 COL 43.38 COLON-ALIGNED
     BTN_OK AT ROW 11.75 COL 28
     BTN_AVB AT ROW 11.75 COL 43
     "  Vill du verkligen ta bort denna beredskap?" VIEW-AS TEXT
          SIZE 65.13 BY 1.5 AT ROW 1.5 COL 1.5
          FONT 17
     SPACE(0.00) SKIP(9.95)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Borttag av beredskap":L.


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

/* SETTINGS FOR COMBO-BOX CMB_DAG IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       CMB_DAG:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_PERIOD IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       CMB_PERIOD:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-BERSLUT IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-BERSTART IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-DATUM IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN-DATUM:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DATUM-2 IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN-DATUM-2:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-MANAD IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
/* SETTINGS FOR FILL-IN FILL-IN_BERANTAL IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_BEREDSKAP IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_DAG IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_DAG:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_PERSONALKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
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
ON END-ERROR OF FRAME DIALOG-1 /* Borttag av beredskap */
DO:
   ASSIGN
   tidalltrec = ?
   musz = TRUE.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Borttag av beredskap */
DO:
   ASSIGN
   tidalltrec = ?
   musz = TRUE.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   ASSIGN
   tidalltrec = ?
   musz = TRUE.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:   
   {muswait.i}    
   IF CMB_PERIOD = TRUE THEN DO TRANSACTION:
      /*ENDAST DENNA POST*/
      IF tillochmeddatum NE ? THEN DO:
         IF tillochmeddatum >= tidallt.DATUM THEN DO: 
            RETURN NO-APPLY.
         END.
      END.
      FOR EACH extratidallt:
         DELETE extratidallt.
      END.
      CREATE extratidallt.
      BUFFER-COPY tidallt TO extratidallt.
      IF tidallt.TIDLOG = FALSE THEN DO:
         DELETE tidallt.
      END.
      ELSE DO:
         ASSIGN 
         tidallt.VILART = ""
         tidallt.BEREDSKAP = "" 
         tidallt.BERANTAL = 0.
      END.
      IF Guru.Konstanter:appcon THEN DO:
         RUN TIDBORT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT Guru.Konstanter:globanv,INPUT 3,INPUT brwbdatum,INPUT brwavdatum,INPUT TABLE extratidallt,
         OUTPUT placerarec,OUTPUT TABLE tidallt APPEND).
      END.
      ELSE DO: 
         RUN TIDBORT.P
         (INPUT Guru.Konstanter:globanv,INPUT 3,INPUT brwbdatum,INPUT brwavdatum,INPUT TABLE extratidallt,
         OUTPUT placerarec,OUTPUT TABLE tidallt APPEND).
      END.       
   END.
   ELSE DO:
      bdatum = DATE(MONTH(bdatum),FILL-IN-DATUM,YEAR(bdatum)).           
      avdatum = bdatum.
      avdatum = DATE(MONTH(avdatum),FILL-IN-DATUM-2,YEAR(avdatum)).
      FOR EACH extratidallt:
         DELETE extratidallt.
      END.
      CREATE extratidallt.
      BUFFER-COPY tidallt TO extratidallt.
      FOR EACH tidallt WHERE tidallt.DATUM >= bdatum AND tidallt.DATUM <= avdatum AND 
      tidallt.BEREDSKAP NE "":
         IF tidallt.TIDLOG = FALSE THEN DO:
            DELETE tidallt.
         END.
         ELSE DO:
            ASSIGN 
            tidallt.VILART = ""
            tidallt.BEREDSKAP = "" 
            tidallt.BERANTAL = 0.
         END.
      END.
      IF Guru.Konstanter:appcon THEN DO:
         RUN TIDBORT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT Guru.Konstanter:globanv,INPUT 31,INPUT bdatum,INPUT avdatum,INPUT TABLE extratidallt,
         OUTPUT placerarec,OUTPUT TABLE tidallt APPEND).
      END.
      ELSE DO: 
         RUN TIDBORT.P
         (INPUT Guru.Konstanter:globanv,INPUT 31,INPUT bdatum,INPUT avdatum,INPUT TABLE extratidallt,
         OUTPUT placerarec,OUTPUT TABLE tidallt APPEND).
      END.            
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
   ASSIGN
   FILL-IN-MANAD = regmannamn
   FILL-IN_DAG = tidallt.DAG
   FILL-IN_BEREDSKAP = tidallt.VILART
   FILL-IN_BERANTAL = tidallt.BERANTAL
   FILL-IN_PERSONALKOD = tidallt.PERSONALKOD 
   FILL-IN-BERSLUT = tidallt.BEREDSKAPSLUT
   FILL-IN-BERSTART = tidallt.BEREDSKAPSTART
   CMB_DAG = TRUE.
   CMB_PERIOD = TRUE.
   {AVBGOMD.I}
   RUN BORTBERV.W (INPUT tidalltrec,INPUT tidallt.PERSONALKOD).
   {AVBFRAMD.I}
   
   IF musz = TRUE THEN DO:     
      LEAVE MAIN-BLOCK.
   END.   
   RUN enable_UI.       
   {FRMSIZED.I}
   ASSIGN
   FILL-IN-MANAD:HIDDEN = FALSE
   FILL-IN-DATUM:HIDDEN = FALSE.
   
   IF CMB_PERIOD = FALSE THEN DO:    
      ASSIGN
      FILL-IN-MANAD:HIDDEN = FALSE
      FILL-IN-DATUM:HIDDEN = FALSE 
      FILL-IN-DATUM-2:HIDDEN = FALSE
      FILL-IN-BERSLUT:HIDDEN = TRUE 
      FILL-IN-BERSTART:HIDDEN = TRUE 
      FILL-IN_BERANTAL:HIDDEN = TRUE
      FILL-IN_BEREDSKAP:HIDDEN = TRUE 
      FILL-IN_DAG:HIDDEN = TRUE. 
      FILL-IN-DATUM:LABEL = "Från den".        
      DISPLAY FILL-IN-MANAD LABEL "Månad" FILL-IN-DATUM FILL-IN-DATUM-2
      WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      FILL-IN-DATUM:LABEL = "Datum".
      DISPLAY FILL-IN-MANAD LABEL "Månad" FILL-IN_DAG FILL-IN-DATUM  
      WITH FRAME {&FRAME-NAME}.
   END.  
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
  DISPLAY FILL-IN_PERSONALKOD FILL-IN-BERSTART FILL-IN-BERSLUT FILL-IN_BEREDSKAP 
          FILL-IN_BERANTAL 
      WITH FRAME DIALOG-1.
  ENABLE BTN_OK BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

