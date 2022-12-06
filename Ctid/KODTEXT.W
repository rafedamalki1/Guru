&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
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

  Created: 07/15/96 -  2:55 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER vartkod AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
&Scoped-define SHARED SHARED
{PHMT.I}
{GLOBVAR2DEL1.I}
{FLEXTAB.I}
{LONTILLAGG.I}
{BEREDSKAP.I}
{TRAKTTILLAGG.I}
{UPPGHMT.I}
{SOKDEF.I}
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.   
DEFINE VARIABLE str AS CHARACTER FORMAT "X(80)" NO-UNDO.
DEFINE VARIABLE anstkod AS CHARACTER  NO-UNDO.
DEFINE VARIABLE tempvar AS CHARACTER NO-UNDO.
{TIDUTTTNEW.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_BER

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES berkodtemp lontilltemp overkodtemp ~
traktatabtemp

/* Definitions for BROWSE BRW_BER                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_BER berkodtemp.VILART ~
berkodtemp.LONKODTEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BER berkodtemp.VILART 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_BER berkodtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_BER berkodtemp
&Scoped-define QUERY-STRING-BRW_BER FOR EACH berkodtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BER OPEN QUERY BRW_BER FOR EACH berkodtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BER berkodtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BER berkodtemp


/* Definitions for BROWSE BRW_LON                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_LON lontilltemp.VILART ~
lontilltemp.LONKODTEXT lontilltemp.ENHET 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LON lontilltemp.VILART 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_LON lontilltemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_LON lontilltemp
&Scoped-define QUERY-STRING-BRW_LON FOR EACH lontilltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_LON OPEN QUERY BRW_LON FOR EACH lontilltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_LON lontilltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LON lontilltemp


/* Definitions for BROWSE BRW_OVER                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_OVER overkodtemp.VILART ~
overkodtemp.LONKODTEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OVER overkodtemp.VILART 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_OVER overkodtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_OVER overkodtemp
&Scoped-define QUERY-STRING-BRW_OVER FOR EACH overkodtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_OVER OPEN QUERY BRW_OVER FOR EACH overkodtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_OVER overkodtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OVER overkodtemp


/* Definitions for BROWSE BRW_TRAK                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_TRAK traktatabtemp.VILART ~
traktatabtemp.FORKL 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TRAK traktatabtemp.VILART 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_TRAK traktatabtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_TRAK traktatabtemp
&Scoped-define QUERY-STRING-BRW_TRAK FOR EACH traktatabtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_TRAK OPEN QUERY BRW_TRAK FOR EACH traktatabtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_TRAK traktatabtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TRAK traktatabtemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 RAD_ALLTID BRW_TRAK BRW_LON BRW_BER ~
BRW_OVER BTN_SKRIV FILL-IN-KOD BTN_AVSL 
&Scoped-Define DISPLAYED-OBJECTS RAD_ALLTID FILL-IN-KOD 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVSL AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "x(4)" 
     LABEL "Löneart" 
     VIEW-AS FILL-IN 
     SIZE 6.5 BY .83.

DEFINE VARIABLE RAD_ALLTID AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Lönetillägg", 2,
"Bered", 3,
"Trakt", 4,
"Över", 5
     SIZE 47.25 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 52 BY 1.21
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BER FOR 
      berkodtemp SCROLLING.

DEFINE QUERY BRW_LON FOR 
      lontilltemp SCROLLING.

DEFINE QUERY BRW_OVER FOR 
      overkodtemp SCROLLING.

DEFINE QUERY BRW_TRAK FOR 
      traktatabtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BER DIALOG-1 _STRUCTURED
  QUERY BRW_BER NO-LOCK DISPLAY
      berkodtemp.VILART COLUMN-LABEL "Löneart" FORMAT "X(5)":U
      berkodtemp.LONKODTEXT COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 40
  ENABLE
      berkodtemp.VILART
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 52 BY 10
         TITLE "Beredskap".

DEFINE BROWSE BRW_LON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LON DIALOG-1 _STRUCTURED
  QUERY BRW_LON NO-LOCK DISPLAY
      lontilltemp.VILART COLUMN-LABEL "Löneart" FORMAT "X(8)":U
      lontilltemp.LONKODTEXT COLUMN-LABEL "Text" FORMAT "x(256)":U
            WIDTH 35
      lontilltemp.ENHET COLUMN-LABEL "Sort" FORMAT "x(2)":U
  ENABLE
      lontilltemp.VILART
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 52 BY 10
         TITLE "Lönetillägg".

DEFINE BROWSE BRW_OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OVER DIALOG-1 _STRUCTURED
  QUERY BRW_OVER NO-LOCK DISPLAY
      overkodtemp.VILART FORMAT "X(8)":U
      overkodtemp.LONKODTEXT COLUMN-LABEL "Text" FORMAT "x(256)":U
            WIDTH 40
  ENABLE
      overkodtemp.VILART
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 52 BY 10
         TITLE "Övertidskoder".

DEFINE BROWSE BRW_TRAK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TRAK DIALOG-1 _STRUCTURED
  QUERY BRW_TRAK NO-LOCK DISPLAY
      traktatabtemp.VILART FORMAT "X(8)":U
      traktatabtemp.FORKL COLUMN-LABEL "Text" FORMAT "X(256)":U
            WIDTH 40
  ENABLE
      traktatabtemp.VILART
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 52 BY 10
         TITLE "Traktamenten och resetillägg".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     RAD_ALLTID AT ROW 1.75 COL 1.5 NO-LABEL
     BRW_TRAK AT ROW 3.25 COL 1.5
     BRW_LON AT ROW 3.25 COL 1.5
     BRW_BER AT ROW 3.25 COL 1.5
     BRW_OVER AT ROW 3.25 COL 1.5
     BTN_SKRIV AT ROW 7 COL 57.5
     FILL-IN-KOD AT ROW 13.71 COL 18.38 COLON-ALIGNED
     BTN_AVSL AT ROW 13.75 COL 57.5
     "Sök på:" VIEW-AS TEXT
          SIZE 8.25 BY .83 AT ROW 13.75 COL 2.5
     RECT-3 AT ROW 13.54 COL 1.5
     SPACE(18.49) SKIP(0.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Klartext till koder".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: berkodtemp T "?" NO-UNDO temp-db berkodtemp
      TABLE: lontilltemp T "?" NO-UNDO temp-db lontilltemp
      TABLE: overkodtemp T "?" NO-UNDO temp-db overkodtemp
      TABLE: traktatabtemp T "?" NO-UNDO temp-db traktatabtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_TRAK RAD_ALLTID DIALOG-1 */
/* BROWSE-TAB BRW_LON BRW_TRAK DIALOG-1 */
/* BROWSE-TAB BRW_BER BRW_LON DIALOG-1 */
/* BROWSE-TAB BRW_OVER BRW_BER DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

ASSIGN 
       BRW_BER:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-1 = TRUE
       BRW_BER:COLUMN-RESIZABLE IN FRAME DIALOG-1       = TRUE.

ASSIGN 
       BRW_LON:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-1 = TRUE
       BRW_LON:COLUMN-RESIZABLE IN FRAME DIALOG-1       = TRUE.

ASSIGN 
       BRW_OVER:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-1 = TRUE
       BRW_OVER:COLUMN-RESIZABLE IN FRAME DIALOG-1       = TRUE.

ASSIGN 
       BRW_TRAK:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-1 = TRUE
       BRW_TRAK:COLUMN-RESIZABLE IN FRAME DIALOG-1       = TRUE.

ASSIGN 
       BTN_SKRIV:HIDDEN IN FRAME DIALOG-1           = TRUE.

ASSIGN 
       FILL-IN-KOD:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BER
/* Query rebuild information for BROWSE BRW_BER
     _TblList          = "Temp-Tables.berkodtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.berkodtemp.VILART
"berkodtemp.VILART" "Löneart" "X(5)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.berkodtemp.LONKODTEXT
"berkodtemp.LONKODTEXT" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BER */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LON
/* Query rebuild information for BROWSE BRW_LON
     _TblList          = "Temp-Tables.lontilltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.lontilltemp.VILART
"lontilltemp.VILART" "Löneart" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.lontilltemp.LONKODTEXT
"lontilltemp.LONKODTEXT" "Text" "x(256)" "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.lontilltemp.ENHET
"lontilltemp.ENHET" "Sort" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LON */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OVER
/* Query rebuild information for BROWSE BRW_OVER
     _TblList          = "Temp-Tables.overkodtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.overkodtemp.VILART
"overkodtemp.VILART" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.overkodtemp.LONKODTEXT
"overkodtemp.LONKODTEXT" "Text" "x(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_OVER */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TRAK
/* Query rebuild information for BROWSE BRW_TRAK
     _TblList          = "Temp-Tables.traktatabtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.traktatabtemp.VILART
"traktatabtemp.VILART" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.traktatabtemp.FORKL
"traktatabtemp.FORKL" "Text" "X(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TRAK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON WINDOW-CLOSE OF FRAME DIALOG-1 /* Klartext till koder */
DO:
  {BORTBRWPROC.I}
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_OVER
&Scoped-define SELF-NAME BRW_OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_OVER DIALOG-1
ON VALUE-CHANGED OF BRW_OVER IN FRAME DIALOG-1 /* Övertidskoder */
DO: 
/*    MESSAGE overkodtemp.VILART vartkod STRING(ROWID(overkodtemp)) VIEW-AS ALERT-BOX. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSL DIALOG-1
ON CHOOSE OF BTN_AVSL IN FRAME DIALOG-1 /* Avsluta */
DO:   
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV DIALOG-1
ON CHOOSE OF BTN_SKRIV IN FRAME DIALOG-1 /* Skriv ut */
DO: 
   FOR EACH tidut.
      DELETE tidut.
   END.   
   RUN huvud_UI.
   RUN kod_UI.
   RUN SKRIVVAL.W (INPUT FALSE).
   IF musz = TRUE THEN musz = FALSE.
   ELSE RUN EKLOGS.P.
   {musarrow.i}         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV DIALOG-1
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME DIALOG-1 /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD DIALOG-1
ON ANY-KEY OF FILL-IN-KOD IN FRAME DIALOG-1 /* Löneart */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-KOD IN FRAME DIALOG-1 /* Löneart */
DO:
   FILL-IN-KOD = INPUT FILL-IN-KOD.
   IF FILL-IN-KOD = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.    
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "VILART", INPUT FILL-IN-KOD).
   RUN sokurvaldyn_UI IN brwproc[2] (INPUT "VILART", INPUT FILL-IN-KOD).
   RUN sokurvaldyn_UI IN brwproc[3] (INPUT "VILART", INPUT FILL-IN-KOD).
   RUN sokurvaldyn_UI IN brwproc[4] (INPUT "VILART", INPUT FILL-IN-KOD).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_ALLTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_ALLTID DIALOG-1
ON VALUE-CHANGED OF RAD_ALLTID IN FRAME DIALOG-1
DO:
   ASSIGN RAD_ALLTID = INPUT RAD_ALLTID.
   ASSIGN
   BRW_LON:HIDDEN = TRUE 
   BRW_TRAK:HIDDEN = TRUE
   BRW_BER:HIDDEN = TRUE      
   BRW_OVER:HIDDEN = TRUE.  
   IF RAD_ALLTID = 2 THEN DO:            
      BRW_LON:HIDDEN = FALSE. 
      RUN openbdynspec_UI IN brwproc[1].
   END.
   IF RAD_ALLTID = 3 THEN DO:
      BRW_BER:HIDDEN = FALSE.           
      RUN openbdynspec_UI IN brwproc[2].
   END.        
   IF RAD_ALLTID = 4 THEN DO:
      BRW_TRAK:HIDDEN = FALSE.
      RUN openbdynspec_UI IN brwproc[3].
   END.
   IF RAD_ALLTID = 5 THEN DO:
      BRW_OVER:HIDDEN = FALSE.
      RUN openbdynspec_UI IN brwproc[4].
   END.                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BER
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
   {muswait.i} 
   RAD_ALLTID = vart.
   RUN enable_UI.       
   {FRMSIZED.I} 
   RUN anst_UI.
   RUN goma_UI.
   IF vart = 2 THEN DO:
      FIND FIRST lontilltemp WHERE lontilltemp.VILART = vartkod NO-LOCK NO-ERROR.
      IF AVAILABLE lontilltemp THEN RUN selectbyrowid_UI IN brwproc[1] (INPUT ROWID(lontilltemp)).
   END.
   ELSE IF vart = 3 THEN DO:
      FIND FIRST berkodtemp WHERE berkodtemp.VILART = vartkod NO-LOCK NO-ERROR.
      IF AVAILABLE berkodtemp THEN RUN selectbyrowid_UI IN brwproc[2] (INPUT ROWID(berkodtemp)).
   END.
   ELSE IF vart = 4 THEN DO:
      FIND FIRST traktatabtemp WHERE traktatabtemp.VILART = vartkod NO-LOCK NO-ERROR.
      IF AVAILABLE traktatabtemp THEN RUN selectbyrowid_UI IN brwproc[3] (INPUT ROWID(traktatabtemp)).
   END.
   ELSE IF vart = 5 THEN DO:       
      IF vartkod = pkod THEN RUN openbdynspec_UI IN brwproc[4].
      ELSE DO:      
         FIND FIRST overkodtemp WHERE overkodtemp.VILART = vartkod AND 
         overkodtemp.KOD = anstkod NO-LOCK NO-ERROR.
         
         IF AVAILABLE overkodtemp THEN DO:
            RUN selectbyrowid_UI IN brwproc[4] (INPUT ROWID(overkodtemp)).
         END.
      END.      
   END.
   {musarrow.i}
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
   lontilltemp.VILART:READ-ONLY IN BROWSE BRW_LON = TRUE.
   berkodtemp.VILART:READ-ONLY IN BROWSE BRW_BER = TRUE.
   traktatabtemp.VILART:READ-ONLY IN BROWSE BRW_TRAK = TRUE.
   overkodtemp.VILART:READ-ONLY IN BROWSE BRW_OVER = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_LON:HANDLE IN FRAME {&FRAME-NAME}).            
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_BER:HANDLE IN FRAME {&FRAME-NAME}).   
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_TRAK:HANDLE IN FRAME {&FRAME-NAME}).            
   RUN DYNBRW.P PERSISTENT SET brwproc[4]
      (INPUT BRW_OVER:HANDLE IN FRAME {&FRAME-NAME}).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anst_UI DIALOG-1 
PROCEDURE anst_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod
   NO-LOCK NO-ERROR. 
   FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.
   anstkod = ansttemp.KOD.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN TRAKODHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT personaltemp.PERSONALKOD,OUTPUT TABLE traktatabtemp).
      RUN BERKODHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT personaltemp.PERSONALKOD,OUTPUT TABLE berkodtemp).
      RUN LONKODHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT personaltemp.PERSONALKOD,OUTPUT TABLE lontilltemp,OUTPUT TABLE lonkorttemp).
   END.
   ELSE DO:
      RUN TRAKODHMT.P  
      (INPUT personaltemp.PERSONALKOD,OUTPUT TABLE traktatabtemp).
      RUN BERKODHMT.P  
      (INPUT personaltemp.PERSONALKOD,OUTPUT TABLE berkodtemp).
      RUN LONKODHMT.P  
      (INPUT personaltemp.PERSONALKOD,OUTPUT TABLE lontilltemp,OUTPUT TABLE lonkorttemp).
   END.
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
   tempvar = "berkodtemp.BEREDSKAPSAVTAL = """ + STRING(personaltemp.BEREDSKAPSAVTAL) + """".
   RUN setcolsortvar_UI IN brwproc[2] (INPUT tempvar).
   RUN setcolsortvar_UI IN brwproc[3] (INPUT "").
   tempvar = "overkodtemp.KOD = """ + STRING(ansttemp.KOD) + """".
   RUN setcolsortvar_UI IN brwproc[4] (INPUT tempvar).
   RUN openbdynspec_UI IN brwproc[1].
   RUN openbdynspec_UI IN brwproc[2].
   RUN openbdynspec_UI IN brwproc[3].
   RUN openbdynspec_UI IN brwproc[4].   
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
  DISPLAY RAD_ALLTID FILL-IN-KOD 
      WITH FRAME DIALOG-1.
  ENABLE RECT-3 RAD_ALLTID BRW_TRAK BRW_LON BRW_BER BRW_OVER BTN_SKRIV 
         FILL-IN-KOD BTN_AVSL 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goma_UI DIALOG-1 
PROCEDURE goma_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   ASSIGN
   BRW_LON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE    
   BRW_BER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE    
   BRW_TRAK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE    
   BRW_OVER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF RAD_ALLTID = 2 THEN BRW_LON:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   IF RAD_ALLTID = 3 THEN BRW_BER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   IF RAD_ALLTID = 4 THEN BRW_TRAK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   IF RAD_ALLTID = 5 THEN BRW_OVER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI DIALOG-1 
PROCEDURE huvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   str = "=====.==============================.====".                         
   CREATE tidut.
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 21
   soktemp.SOKCHAR[1] = pkod
   soktemp.SOKINT[1] = RAD_ALLTID.
   {SOKANROP.I}
   IF RAD_ALLTID = 2 THEN DO:
      SUBSTRING(tidut.UT,1) = "LÖNETILLÄGG".       
   END.
   IF RAD_ALLTID = 3 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,1) = "BEREDSKAPSTILLÄGG".       
   END. 
   IF RAD_ALLTID = 4 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,1) = "TRAKTAMENTSTILLÄGG". 
   END.                       
   IF RAD_ALLTID = 5 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,1) = "ÖVERTIDSTILLÄGG".       
   END.
   ASSIGN
   SUBSTRING(tidut.UT,25) = soktemp.SOKCHAR[1]
   SUBSTRING(tidut.UT,62) = STRING(TODAY)
   SUBSTRING(tidut.UT,73) = STRING(TIME,"HH:MM:SS").
   CREATE tidut.      
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,1) = "KOD" 
   SUBSTRING(tidut.UT,7) = "KLARTEXT"
   SUBSTRING(tidut.UT,38) = "SORT". 
   CREATE tidut.
   SUBSTRING(tidut.UT,1) = str.                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kod_UI DIALOG-1 
PROCEDURE kod_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/          
   IF RAD_ALLTID = 2 THEN DO:
      GET FIRST BRW_LON NO-LOCK.
      DO WHILE AVAILABLE(lontilltemp):           
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = lontilltemp.VILART 
         SUBSTRING(tidut.UT,7) = lontilltemp.LONKODTEXT
         SUBSTRING(tidut.UT,38) = lontilltemp.ENHET.         
         GET NEXT BRW_LON NO-LOCK.
      END. 
   END.                                               
   IF RAD_ALLTID = 3 THEN DO:      
      GET FIRST BRW_BER NO-LOCK.
      DO WHILE AVAILABLE(berkodtemp):          
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = berkodtemp.VILART 
         SUBSTRING(tidut.UT,7) = berkodtemp.LONKODTEXT
         SUBSTRING(tidut.UT,38) = berkodtemp.ENHET.
         GET NEXT BRW_BER NO-LOCK.         
      END. 
   END.
   IF RAD_ALLTID = 4 THEN DO:
      GET FIRST BRW_TRAK NO-LOCK.
      DO WHILE AVAILABLE(traktatabtemp):           
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = traktatabtemp.VILART 
         SUBSTRING(tidut.UT,7) = traktatabtemp.FORKL
         SUBSTRING(tidut.UT,38) = "ST".
         GET NEXT BRW_TRAK NO-LOCK.         
      END. 
   END.
   IF RAD_ALLTID = 5 THEN DO:      
      GET FIRST BRW_OVER NO-LOCK.
      DO WHILE AVAILABLE(overkodtemp):           
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = overkodtemp.VILART 
         SUBSTRING(tidut.UT,7) = overkodtemp.LONKODTEXT
         SUBSTRING(tidut.UT,38) = overkodtemp.ENHET.
         GET NEXT BRW_OVER NO-LOCK.         
      END. 
   END.                       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

