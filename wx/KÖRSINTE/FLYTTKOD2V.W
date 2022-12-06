&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
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
{GLOBVAR2DEL1.I}
{LOP.I}


DEFINE NEW SHARED VARIABLE katvar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE skatvar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE prec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE typ_val AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE andra AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(5)" NO-UNDO.
DEFINE VARIABLE ortsok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE arbkodvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE kalkproch   AS HANDLE NO-UNDO. /* flyttapp.p. */

/*DEFINE QUERY lop1q FOR LOP1.
DEFINE QUERY lop2q FOR LOP2.
DEFINE QUERY lop3q FOR LOP3.*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_P1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ptemp1 ptemp2 ptemp3

/* Definitions for BROWSE BRW_P1                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_P1 ptemp1.ARBKOD ptemp1.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_P1 
&Scoped-define QUERY-STRING-BRW_P1 FOR EACH ptemp1 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_P1 OPEN QUERY BRW_P1 FOR EACH ptemp1 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_P1 ptemp1
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_P1 ptemp1


/* Definitions for BROWSE BRW_P2                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_P2 ptemp2.ARBKOD ptemp2.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_P2 
&Scoped-define QUERY-STRING-BRW_P2 FOR EACH ptemp2 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_P2 OPEN QUERY BRW_P2 FOR EACH ptemp2 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_P2 ptemp2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_P2 ptemp2


/* Definitions for BROWSE BRW_P3                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_P3 ptemp3.ARBKOD ptemp3.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_P3 
&Scoped-define QUERY-STRING-BRW_P3 FOR EACH ptemp3 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_P3 OPEN QUERY BRW_P3 FOR EACH ptemp3 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_P3 ptemp3
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_P3 ptemp3


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-22 RAD_VAL BRW_P3 BRW_P2 BRW_P1 ~
BTN_KOPI FILL-IN-KOD FILL-IN-BEN BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_VAL FILL-IN-KOD FILL-IN-BEN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_KOPI 
     LABEL "Kopiera" 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 25.25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Arbetskod" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .83 NO-UNDO.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Typ 1", 1,
"Typ 2", 2,
"Typ 3", 3
     SIZE 52.63 BY .92 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 67 BY 1.21
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_P1 FOR 
      ptemp1 SCROLLING.

DEFINE QUERY BRW_P2 FOR 
      ptemp2 SCROLLING.

DEFINE QUERY BRW_P3 FOR 
      ptemp3 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_P1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_P1 C-Win _STRUCTURED
  QUERY BRW_P1 NO-LOCK DISPLAY
      ptemp1.ARBKOD FORMAT "X(5)":U
      ptemp1.BENAMNING FORMAT "X(40)":U WIDTH 54.5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 67 BY 11.58
         TITLE "Arbetskoder typ 1".

DEFINE BROWSE BRW_P2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_P2 C-Win _STRUCTURED
  QUERY BRW_P2 NO-LOCK DISPLAY
      ptemp2.ARBKOD FORMAT "X(5)":U
      ptemp2.BENAMNING FORMAT "X(40)":U WIDTH 53.5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 67 BY 11.58
         TITLE "Arbetskoder typ 2".

DEFINE BROWSE BRW_P3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_P3 C-Win _STRUCTURED
  QUERY BRW_P3 NO-LOCK DISPLAY
      ptemp3.ARBKOD FORMAT "X(5)":U
      ptemp3.BENAMNING FORMAT "X(40)":U WIDTH 53.5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 67 BY 11.58
         TITLE "Arbetskoder typ 3".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     RAD_VAL AT ROW 1.83 COL 1.5 NO-LABEL
     BRW_P3 AT ROW 3.17 COL 1.5
     BRW_P2 AT ROW 3.17 COL 1.5
     BRW_P1 AT ROW 3.17 COL 1.5
     BTN_KOPI AT ROW 15.25 COL 29.13
     FILL-IN-KOD AT ROW 17.13 COL 20.63 COLON-ALIGNED
     FILL-IN-BEN AT ROW 17.13 COL 40.75 COLON-ALIGNED
     BTN_AVB AT ROW 18.5 COL 54.5
     "Sök på:" VIEW-AS TEXT
          SIZE 8.63 BY .83 AT ROW 17.13 COL 2.88
     RECT-22 AT ROW 16.96 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 69.13 BY 18.83.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: ptemp1 T "?" NO-UNDO temp-db ptemp1
      TABLE: ptemp2 T "?" NO-UNDO temp-db ptemp2
      TABLE: ptemp3 T "?" NO-UNDO temp-db ptemp3
      TABLE: ptemp5 T "?" NO-UNDO temp-db ptemp5
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Kopiera kalkylkod från ett år till ett annat"
         HEIGHT             = 18.79
         WIDTH              = 68.63
         MAX-HEIGHT         = 22.96
         MAX-WIDTH          = 91.75
         VIRTUAL-HEIGHT     = 22.96
         VIRTUAL-WIDTH      = 91.75
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
/* BROWSE-TAB BRW_P3 RAD_VAL DEFAULT-FRAME */
/* BROWSE-TAB BRW_P2 BRW_P3 DEFAULT-FRAME */
/* BROWSE-TAB BRW_P1 BRW_P2 DEFAULT-FRAME */
ASSIGN 
       BRW_P1:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_P1:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000.

ASSIGN 
       BRW_P2:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_P2:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000.

ASSIGN 
       BRW_P3:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_P3:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_P1
/* Query rebuild information for BROWSE BRW_P1
     _TblList          = "Temp-Tables.ptemp1"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.ptemp1.ARBKOD
     _FldNameList[2]   > Temp-Tables.ptemp1.BENAMNING
"ptemp1.BENAMNING" ? ? "character" ? ? ? ? ? ? no ? no no "54.5" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_P1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_P2
/* Query rebuild information for BROWSE BRW_P2
     _TblList          = "Temp-Tables.ptemp2"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.ptemp2.ARBKOD
     _FldNameList[2]   > Temp-Tables.ptemp2.BENAMNING
"ptemp2.BENAMNING" ? ? "character" ? ? ? ? ? ? no ? no no "53.5" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_P2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_P3
/* Query rebuild information for BROWSE BRW_P3
     _TblList          = "Temp-Tables.ptemp3"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.ptemp3.ARBKOD
     _FldNameList[2]   > Temp-Tables.ptemp3.BENAMNING
"ptemp3.BENAMNING" ? ? "character" ? ? ? ? ? ? no ? no no "53.5" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_P3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Kopiera kalkylkod från ett år till ett annat */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kopiera kalkylkod från ett år till ett annat */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_P1
&Scoped-define SELF-NAME BRW_P1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_P1 C-Win
ON VALUE-CHANGED OF BRW_P1 IN FRAME DEFAULT-FRAME /* Arbetskoder typ 1 */
DO:
   ASSIGN
   status-ok = BRW_P1:SELECT-FOCUSED-ROW()
   prec = RECID(ptemp1). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_P2
&Scoped-define SELF-NAME BRW_P2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_P2 C-Win
ON VALUE-CHANGED OF BRW_P2 IN FRAME DEFAULT-FRAME /* Arbetskoder typ 2 */
DO:
   ASSIGN
   status-ok = BRW_P2:SELECT-FOCUSED-ROW()
   prec = RECID(ptemp2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_P3
&Scoped-define SELF-NAME BRW_P3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_P3 C-Win
ON VALUE-CHANGED OF BRW_P3 IN FRAME DEFAULT-FRAME /* Arbetskoder typ 3 */
DO:
   ASSIGN
   status-ok = BRW_P3:SELECT-FOCUSED-ROW()
   prec = RECID(ptemp3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOPI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOPI C-Win
ON CHOOSE OF BTN_KOPI IN FRAME DEFAULT-FRAME /* Kopiera */
DO:
   {muswait.i}
   IF typ_val = 1 THEN arbkodvar = ptemp1.ARBKOD.
   ELSE IF typ_val = 2 THEN arbkodvar = ptemp2.ARBKOD.
   ELSE arbkodvar = ptemp3.ARBKOD.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN FLYTTKOD.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT typ_val, INPUT katvar, INPUT katvar - 1, INPUT arbkodvar).
   END.
   ELSE DO:
      RUN FLYTTKOD.P 
      (INPUT typ_val, INPUT katvar, INPUT katvar - 1, INPUT arbkodvar).
   END.
   MESSAGE "Flytt av kod klart." VIEW-AS ALERT-BOX.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN C-Win
ON ANY-KEY OF FILL-IN-BEN IN FRAME DEFAULT-FRAME /* Benämning */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN IN FRAME DEFAULT-FRAME /* Benämning */
DO:
   FILL-IN-BEN = INPUT FILL-IN-BEN.
   IF RAD_VAL = 1 THEN DO:
      status-ok = BRW_P1:SELECT-FOCUSED-ROW().
      APPLY "VALUE-CHANGED" TO BRW_P1.
      prec = RECID(ptemp1).
      IF FILL-IN-BEN = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      ortsok = '*' + FILL-IN-BEN + '*'.   
      FIND ptemp1 WHERE RECID(ptemp1) = prec NO-LOCK NO-ERROR.
      FIND NEXT ptemp1 WHERE ptemp1.BENAMNING MATCHES ortsok 
      AND ptemp1.KATAR = katvar USE-INDEX BEN NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE ptemp1 THEN DO:      
         FIND FIRST ptemp1 WHERE ptemp1.BENAMNING MATCHES ortsok
         AND ptemp1.KATAR = katvar USE-INDEX BEN NO-LOCK NO-ERROR.      
         IF NOT AVAILABLE ptemp1 THEN DO:
            MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
      IF AVAILABLE ptemp1 THEN DO:      
         RUN repo_UI (INPUT 1, INPUT RECID(ptemp1)).
         status-ok = BRW_P1:SELECT-FOCUSED-ROW().
      END.
   END.
   ELSE IF RAD_VAL = 2 THEN DO:
      status-ok = BRW_P2:SELECT-FOCUSED-ROW().
      APPLY "VALUE-CHANGED" TO BRW_P2.
      prec = RECID(ptemp2).
      IF FILL-IN-BEN = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      ortsok = '*' + FILL-IN-BEN + '*'.   
      FIND ptemp2 WHERE RECID(ptemp2) = prec NO-LOCK NO-ERROR.
      FIND NEXT ptemp2 WHERE ptemp2.BENAMNING MATCHES ortsok 
      AND ptemp2.KATAR = katvar USE-INDEX BEN NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE ptemp2 THEN DO:      
         FIND FIRST ptemp2 WHERE ptemp2.BENAMNING MATCHES ortsok
         AND ptemp2.KATAR = katvar USE-INDEX BEN NO-LOCK NO-ERROR.      
         IF NOT AVAILABLE ptemp2 THEN DO:
            MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
      IF AVAILABLE ptemp2 THEN DO:
         RUN repo_UI (INPUT 2, INPUT RECID(ptemp2)).      
         status-ok = BRW_P2:SELECT-FOCUSED-ROW().
      END.
   END.  
   ELSE IF RAD_VAL = 3 THEN DO:
      status-ok = BRW_P3:SELECT-FOCUSED-ROW().
      APPLY "VALUE-CHANGED" TO BRW_P3.
      prec = RECID(ptemp3).
      IF FILL-IN-BEN = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      ortsok = '*' + FILL-IN-BEN + '*'.   
      FIND ptemp3 WHERE RECID(ptemp3) = prec NO-LOCK NO-ERROR.
      FIND NEXT ptemp3 WHERE ptemp3.BENAMNING MATCHES ortsok 
      AND ptemp3.KATAR = katvar USE-INDEX BEN NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE ptemp3 THEN DO:      
         FIND FIRST ptemp3 WHERE ptemp3.BENAMNING MATCHES ortsok
         AND ptemp3.KATAR = katvar USE-INDEX BEN NO-LOCK NO-ERROR.      
         IF NOT AVAILABLE ptemp3 THEN DO:
            MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
      IF AVAILABLE ptemp3 THEN DO:
         RUN repo_UI (INPUT 3, INPUT RECID(ptemp3)).      
         status-ok = BRW_P3:SELECT-FOCUSED-ROW().
      END.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD C-Win
ON ANY-KEY OF FILL-IN-KOD IN FRAME DEFAULT-FRAME /* Arbetskod */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-KOD IN FRAME DEFAULT-FRAME /* Arbetskod */
DO:
   FILL-IN-KOD = INPUT FILL-IN-KOD.
   IF RAD_VAL = 1 THEN DO:
      status-ok = BRW_P1:SELECT-FOCUSED-ROW().
      APPLY "VALUE-CHANGED" TO BRW_P1.
      prec = RECID(ptemp1).
      IF FILL-IN-KOD = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      aosok = '*' + FILL-IN-KOD + '*'.   
      FIND ptemp1 WHERE RECID(ptemp1) = prec NO-LOCK NO-ERROR.
      FIND NEXT ptemp1 WHERE ptemp1.ARBKOD MATCHES aosok 
      AND ptemp1.KATAR = katvar USE-INDEX AR NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE ptemp1 THEN DO:      
         FIND FIRST ptemp1 WHERE ptemp1.ARBKOD MATCHES aosok
         AND ptemp1.KATAR = katvar USE-INDEX AR NO-LOCK NO-ERROR.      
         IF NOT AVAILABLE ptemp1 THEN DO:
            MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
      IF AVAILABLE ptemp1 THEN DO:
         RUN repo_UI (INPUT 1, INPUT RECID(ptemp1)).      
         status-ok = BRW_P1:SELECT-FOCUSED-ROW().
      END.
   END.
   ELSE IF RAD_VAL = 2 THEN DO:
      status-ok = BRW_P2:SELECT-FOCUSED-ROW().
      APPLY "VALUE-CHANGED" TO BRW_P2.
      prec = RECID(ptemp2).
      IF FILL-IN-KOD = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      aosok = '*' + FILL-IN-KOD + '*'.   
      FIND ptemp2 WHERE RECID(ptemp2) = prec NO-LOCK NO-ERROR.
      FIND NEXT ptemp2 WHERE ptemp2.ARBKOD MATCHES aosok 
      AND ptemp2.KATAR = katvar USE-INDEX AR NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE ptemp2 THEN DO:      
         FIND FIRST ptemp2 WHERE ptemp2.ARBKOD MATCHES aosok
         AND ptemp2.KATAR = katvar USE-INDEX AR NO-LOCK NO-ERROR.      
         IF NOT AVAILABLE ptemp2 THEN DO:
            MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
      IF AVAILABLE ptemp2 THEN DO:
         RUN repo_UI (INPUT 2, INPUT RECID(ptemp2)).      
         status-ok = BRW_P2:SELECT-FOCUSED-ROW().
      END.
   END.  
   ELSE IF RAD_VAL = 3 THEN DO:
      status-ok = BRW_P3:SELECT-FOCUSED-ROW().
      APPLY "VALUE-CHANGED" TO BRW_P3.
      prec = RECID(ptemp3).
      IF FILL-IN-KOD = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      aosok = '*' + FILL-IN-KOD + '*'.   
      FIND ptemp3 WHERE RECID(ptemp3) = prec NO-LOCK NO-ERROR.
      FIND NEXT ptemp3 WHERE ptemp3.ARBKOD MATCHES aosok 
      AND ptemp3.KATAR = katvar USE-INDEX AR NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE ptemp3 THEN DO:      
         FIND FIRST ptemp3 WHERE ptemp3.ARBKOD MATCHES aosok
         AND ptemp3.KATAR = katvar USE-INDEX AR NO-LOCK NO-ERROR.      
         IF NOT AVAILABLE ptemp3 THEN DO:
            MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
      IF AVAILABLE ptemp3 THEN DO:
         RUN repo_UI (INPUT 3, INPUT RECID(ptemp3)).      
         status-ok = BRW_P3:SELECT-FOCUSED-ROW().
      END.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL C-Win
ON VALUE-CHANGED OF RAD_VAL IN FRAME DEFAULT-FRAME
DO:
   ASSIGN
   RAD_VAL = INPUT RAD_VAL
   typ_val = RAD_VAL.
   IF RAD_VAL = 1 THEN DO:
      ASSIGN
      BRW_P1:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BRW_P2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_P3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
      OPEN QUERY BRW_P1 FOR EACH ptemp1 WHERE ptemp1.KATAR = katvar USE-INDEX AR NO-LOCK INDEXED-REPOSITION.       
      APPLY "VALUE-CHANGED" TO BRW_P1 IN FRAME {&FRAME-NAME}.
   END.
   ELSE IF RAD_VAL = 2 THEN DO:
      ASSIGN
      BRW_P1:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_P2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BRW_P3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.     
      OPEN QUERY BRW_P2 FOR EACH ptemp2 WHERE ptemp2.KATAR = katvar USE-INDEX AR NO-LOCK INDEXED-REPOSITION.   
      APPLY "VALUE-CHANGED" TO BRW_P2 IN FRAME {&FRAME-NAME}.
   END.
   ELSE IF RAD_VAL = 3 THEN DO:
      ASSIGN
      BRW_P1:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_P2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_P3:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.     
      OPEN QUERY BRW_P3 FOR EACH ptemp3 WHERE ptemp3.KATAR = katvar USE-INDEX AR NO-LOCK INDEXED-REPOSITION.        
      APPLY "VALUE-CHANGED" TO BRW_P3 IN FRAME {&FRAME-NAME}.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_P1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO: 
   {BORTBRWPROC.I}
   RUN disable_UI.
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
   {ALLSTARTDYN.I}
   RUN lophm_UI IN kalkproch (OUTPUT TABLE ptemp1, OUTPUT TABLE ptemp2, OUTPUT TABLE ptemp3, OUTPUT TABLE ebrpristemp).
   FIND LAST ebrpristemp USE-INDEX AR  NO-ERROR.
   /*IF AVAILABLE ebrpristemp THEN katvar = ebrpristemp.ARTAL - 1.   */
   IF AVAILABLE ebrpristemp THEN katvar = ebrpristemp.ARTAL. 
   RUN enable_UI.   
   {FRMSIZE.I}  
   ASSIGN
   RAD_VAL = 1 
   typ_val = 1
   BRW_P1:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
   BRW_P2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_P3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.   
   OPEN QUERY BRW_P1 FOR EACH ptemp1 WHERE ptemp1.KATAR = katvar USE-INDEX AR NO-LOCK INDEXED-REPOSITION.
   APPLY "VALUE-CHANGED" TO BRW_P1 IN FRAME {&FRAME-NAME}.   
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/       
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_P1:HANDLE IN FRAME {&FRAME-NAME}).         
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_P2:HANDLE IN FRAME {&FRAME-NAME}).         
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_P3:HANDLE IN FRAME {&FRAME-NAME}).   
   IF Guru.Konstanter:appcon THEN DO:
      RUN FLYTTAPP.P PERSISTENT SET kalkproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN FLYTTAPP.P PERSISTENT SET kalkproch.
   END.     
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE byt_UI C-Win 
PROCEDURE byt_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF RAD_VAL = 1 THEN DO:
      APPLY "VALUE-CHANGED" TO BRW_P1 IN FRAME {&FRAME-NAME}.
   END.  
   ELSE IF RAD_VAL = 2 THEN DO:
      APPLY "VALUE-CHANGED" TO BRW_P2 IN FRAME {&FRAME-NAME}.
   END. 
   ELSE IF RAD_VAL = 3 THEN DO:
      APPLY "VALUE-CHANGED" TO BRW_P3 IN FRAME {&FRAME-NAME}.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY RAD_VAL FILL-IN-KOD FILL-IN-BEN 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-22 RAD_VAL BRW_P3 BRW_P2 BRW_P1 BTN_KOPI FILL-IN-KOD FILL-IN-BEN 
         BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo_UI C-Win 
PROCEDURE repo_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER brwvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER browrec AS RECID NO-UNDO.
   IF brwvar = 1 THEN DO:
      &Scoped-define BROWSE-NAME BRW_P1
      {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
      REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   END.
   IF brwvar = 2 THEN DO:
      &Scoped-define BROWSE-NAME BRW_P2
      {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
      REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   END.
   IF brwvar = 3 THEN DO:
      &Scoped-define BROWSE-NAME BRW_P3
      {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
      REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

