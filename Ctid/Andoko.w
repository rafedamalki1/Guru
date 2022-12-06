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

  Created: 95/07/05 - 10:41 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER varaonr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER vardelnr AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{AVDTEMP.I}
{ALLDEF.I}
{LONTILLAGG.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}
{RESDEF.I}
&Scoped-define SHARED SHARED
{FLEXTAB.I}
{DIRDEF.I}
{PHMT.I}
{OMRTEMPW.I}

DEFINE NEW SHARED VARIABLE bustart3 AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE klocka AS DECIMAL NO-UNDO. 
DEFINE SHARED VARIABLE ovrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE sparomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE nyttaoapph AS HANDLE NO-UNDO.                     /* NYTTAOAPP.P */
DEFINE VARIABLE omravdand AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_AONR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES utsokaonr lontilltemp

/* Definitions for BROWSE BRW_AONR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_AONR utsokaonr.OMRADE utsokaonr.AONR ~
utsokaonr.DELNR utsokaonr.ORT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AONR utsokaonr.OMRADE 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_AONR utsokaonr
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_AONR utsokaonr
&Scoped-define QUERY-STRING-BRW_AONR FOR EACH utsokaonr NO-LOCK ~
    BY utsokaonr.OMRADE ~
       BY utsokaonr.AONR ~
        BY utsokaonr.DELNR
&Scoped-define OPEN-QUERY-BRW_AONR OPEN QUERY BRW_AONR FOR EACH utsokaonr NO-LOCK ~
    BY utsokaonr.OMRADE ~
       BY utsokaonr.AONR ~
        BY utsokaonr.DELNR.
&Scoped-define TABLES-IN-QUERY-BRW_AONR utsokaonr
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AONR utsokaonr


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


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN_VILART FILL-IN_LONTILLANTAL ~
FILL-IN-MOMS FILL-IN-AONR FILL-IN-DELNR FILL-IN-DATUM BTN_REG BTN_NVE-3 ~
BTN_FVE-3 CMB_OMR BTN_AVS CMB_AVD FILL-IN-RESMAL RECT-22 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_VILART FILL-IN-SORT ~
FILL-IN_LONTILLANTAL FILL-IN-MOMS FILL-IN-AONR FILL-IN-DELNR FILL-IN-DATUM ~
FILL-IN-RUBRIK FILL-IN-TEXT CMB_OMR FILL-IN-PKOD FILL-IN-SKP CMB_AVD ~
FILL-IN-RESMAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FVE-3 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-3 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_REG 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(6)":U 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DATUM AS DATE FORMAT "99/99/99":U 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MOMS AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Varav moms" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-RESMAL AS CHARACTER FORMAT "X(40)" 
     LABEL "Kommentar" 
     VIEW-AS FILL-IN 
     SIZE 42.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-RUBRIK AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39.5 BY 1.25
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-SKP AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-SORT AS CHARACTER FORMAT "X(2)":U 
     LABEL "Sort" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Visa aonr för:" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONRS AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_LONTILLAGG AS CHARACTER FORMAT "X(4)" 
     LABEL "Lönetillägg" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_LONTILLANTAL AS DECIMAL FORMAT "->>>>9.99" INITIAL 0 
     LABEL "Antal" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_ORTS AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_VILART AS CHARACTER FORMAT "X(4)" 
     LABEL "Lönetillägg" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 34.25 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 61 BY 1.21
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      utsokaonr SCROLLING.

DEFINE QUERY BRW_LON FOR 
      lontilltemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AONR DIALOG-1 _STRUCTURED
  QUERY BRW_AONR NO-LOCK DISPLAY
      utsokaonr.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      utsokaonr.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      utsokaonr.DELNR COLUMN-LABEL "Del!nr" FORMAT "999":U
      utsokaonr.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(40)":U
  ENABLE
      utsokaonr.OMRADE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 61 BY 10.63
         TITLE "Aktiva arbetsordernummer".

DEFINE BROWSE BRW_LON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LON DIALOG-1 _STRUCTURED
  QUERY BRW_LON NO-LOCK DISPLAY
      lontilltemp.VILART COLUMN-LABEL "Löneart" FORMAT "X(8)":U
      lontilltemp.LONKODTEXT COLUMN-LABEL "Text" FORMAT "x(40)":U
      lontilltemp.ENHET COLUMN-LABEL "Sort" FORMAT "x(2)":U
  ENABLE
      lontilltemp.VILART
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 61 BY 12.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_VILART AT ROW 5.88 COL 13 COLON-ALIGNED
     BRW_LON AT ROW 5.17 COL 28.5
     FILL-IN-SORT AT ROW 7.38 COL 13 COLON-ALIGNED
     FILL-IN_LONTILLANTAL AT ROW 8.88 COL 13 COLON-ALIGNED
     FILL-IN-MOMS AT ROW 10.38 COL 13 COLON-ALIGNED
     FILL-IN-AONR AT ROW 11.88 COL 13 COLON-ALIGNED
     FILL-IN-DELNR AT ROW 13.38 COL 13 COLON-ALIGNED
     BRW_AONR AT ROW 5.17 COL 28.5
     FILL-IN-DATUM AT ROW 14.88 COL 13 COLON-ALIGNED
     BTN_REG AT ROW 20.5 COL 60.63
     BTN_NVE-3 AT ROW 14.5 COL 25.13
     BTN_FVE-3 AT ROW 15.38 COL 25.13
     FILL-IN-RUBRIK AT ROW 1.5 COL 1.5 COLON-ALIGNED NO-LABEL
     FILL-IN-TEXT AT ROW 1.75 COL 65.5 COLON-ALIGNED NO-LABEL
     CMB_OMR AT ROW 4 COL 65 COLON-ALIGNED NO-LABEL
     RAD_FAST AT ROW 4.04 COL 28.5 NO-LABEL
     FILL-IN-PKOD AT ROW 4.38 COL 13 COLON-ALIGNED
     FILL-IN-SKP AT ROW 17.46 COL 31.25 COLON-ALIGNED NO-LABEL
     FILL-IN_LONTILLAGG AT ROW 17.38 COL 16.13 COLON-ALIGNED
     FILL-IN_AONRS AT ROW 17.46 COL 46.5 COLON-ALIGNED
     FILL-IN_ORTS AT ROW 17.46 COL 67 COLON-ALIGNED
     BTN_AVS AT ROW 20.5 COL 75.63
     CMB_AVD AT ROW 2.79 COL 65 COLON-ALIGNED NO-LABEL
     FILL-IN-RESMAL AT ROW 18.71 COL 13 COLON-ALIGNED
     RECT-22 AT ROW 17.29 COL 28.5
     SPACE(0.74) SKIP(3.20)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändra eller registrera övrig ersättning":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: ersattningtemp T "?" NO-UNDO temp-db ersattningtemp
      TABLE: lontilltemp T "?" NO-UNDO temp-db lontilltemp
      TABLE: utsokaonr T "?" NO-UNDO temp-db utsokaonr
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* BROWSE-TAB BRW_LON FILL-IN_VILART DIALOG-1 */
/* BROWSE-TAB BRW_AONR FILL-IN-DELNR DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_AONR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_AONR:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_AONR:MAX-DATA-GUESS IN FRAME DIALOG-1         = 1000.

/* SETTINGS FOR BROWSE BRW_LON IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_LON:HIDDEN  IN FRAME DIALOG-1                = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-PKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-RESMAL:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-RUBRIK IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-SKP IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-SORT IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-TEXT IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_AONRS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_AONRS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_LONTILLAGG IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_LONTILLAGG:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_ORTS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_ORTS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_FAST:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AONR
/* Query rebuild information for BROWSE BRW_AONR
     _TblList          = "Temp-Tables.utsokaonr"
     _Options          = "NO-LOCK "
     _OrdList          = "Temp-Tables.utsokaonr.OMRADE|yes,Temp-Tables.utsokaonr.AONR|yes,Temp-Tables.utsokaonr.DELNR|yes"
     _FldNameList[1]   > Temp-Tables.utsokaonr.OMRADE
"utsokaonr.OMRADE" "Område" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.utsokaonr.AONR
"utsokaonr.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.utsokaonr.DELNR
"utsokaonr.DELNR" "Del!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.utsokaonr.ORT
"utsokaonr.ORT" "Ort/Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AONR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LON
/* Query rebuild information for BROWSE BRW_LON
     _TblList          = "Temp-Tables.lontilltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.lontilltemp.VILART
"lontilltemp.VILART" "Löneart" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.lontilltemp.LONKODTEXT
"lontilltemp.LONKODTEXT" "Text" "x(40)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.lontilltemp.ENHET
"lontilltemp.ENHET" "Sort" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LON */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Ändra eller registrera övrig ersättning */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Ändra eller registrera övrig ersättning */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AONR
&Scoped-define SELF-NAME BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AONR DIALOG-1
ON VALUE-CHANGED OF BRW_AONR IN FRAME DIALOG-1 /* Aktiva arbetsordernummer */
DO:
   IF musz = FALSE THEN DO:      
      ASSIGN
      FILL-IN-AONR = utsokaonr.AONR
      FILL-IN-DELNR = utsokaonr.DELNR.
      DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.     
   END.
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_LON
&Scoped-define SELF-NAME BRW_LON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LON DIALOG-1
ON VALUE-CHANGED OF BRW_LON IN FRAME DIALOG-1
DO:                                
   IF AVAILABLE lontilltemp THEN DO:
      ASSIGN
      FILL-IN-SORT = lontilltemp.ENHET.      
      ASSIGN
      FILL-IN_VILART = lontilltemp.VILART
      FILL-IN_LONTILLAGG = lontilltemp.LONTILLAGG.   
      DISPLAY FILL-IN-SORT FILL-IN_VILART WITH FRAME {&FRAME-NAME}.
      RUN fillinmoms_UI.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON CHOOSE OF BTN_AVS IN FRAME DIALOG-1 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-3 DIALOG-1
ON CHOOSE OF BTN_FVE-3 IN FRAME DIALOG-1 /* - */
DO: 
   ASSIGN
   FILL-IN-DATUM = INPUT FILL-IN-DATUM.   
   FILL-IN-DATUM = FILL-IN-DATUM - 1.        
   IF FILL-IN-DATUM < bdatum THEN FILL-IN-DATUM = bdatum. 
   DISPLAY FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-3 DIALOG-1
ON CHOOSE OF BTN_NVE-3 IN FRAME DIALOG-1 /* + */
DO:   
   ASSIGN
   FILL-IN-DATUM = INPUT FILL-IN-DATUM.   
   FILL-IN-DATUM = FILL-IN-DATUM + 1.    
   IF FILL-IN-DATUM > avdatum THEN FILL-IN-DATUM = avdatum.    
   DISPLAY FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   {muswait.i} 
   ASSIGN
   FILL-IN-AONR = INPUT FILL-IN-AONR
   FILL-IN-DELNR = INPUT FILL-IN-DELNR
   FILL-IN_VILART = INPUT FILL-IN_VILART
   FILL-IN-MOMS = INPUT FILL-IN-MOMS
   FILL-IN-DATUM = INPUT FILL-IN-DATUM
   FILL-IN_LONTILLANTAL = INPUT FILL-IN_LONTILLANTAL
   FILL-IN-RESMAL = INPUT FILL-IN-RESMAL.
   IF FILL-IN-DATUM = ? THEN DO:
      MESSAGE "Datum måste ha ett värde!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF FILL-IN-RESMAL = " " THEN DO:
      MESSAGE "Obligatorisk kommentar" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   musz = FALSE.            
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND 
   utsokaonr.DELNR = FILL-IN-DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
   IF NOT AVAILABLE utsokaonr THEN DO:
      MESSAGE Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "finns inte." VIEW-AS ALERT-BOX.      
      RETURN NO-APPLY.
   END.
   ELSE DO:                          
      {AOKOLLERS.I}
      IF utsokaonr.AONRAVDATUM = 01/01/1991 OR
      utsokaonr.AONRAVDATUM >= regdatum THEN FILL-IN-DELNR = FILL-IN-DELNR.
      ELSE DO:
         MESSAGE Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är redan avslutat." VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.

   IF musz = TRUE THEN musz = FALSE.
   ELSE DO:     
      FIND FIRST lontilltemp WHERE lontilltemp.ERSFINNS = TRUE AND 
      lontilltemp.KOD = ansttemp.KOD  AND
      lontilltemp.VILART = FILL-IN_VILART USE-INDEX VILART NO-LOCK NO-ERROR.
      IF NOT AVAILABLE lontilltemp THEN DO:
         MESSAGE "Lönetillägg" fill-in_VILART "finns inte." VIEW-AS ALERT-BOX.         
         RETURN NO-APPLY.
      END.    
      ELSE DO:
         ASSIGN FILL-IN_LONTILLAGG = lontilltemp.LONTILLAGG.
      END.
      IF lontilltemp.MOMS = FALSE AND FILL-IN-MOMS > 0 THEN DO:
         MESSAGE "Lönetillägg" FILL-IN_VILART "har inte moms." VIEW-AS ALERT-BOX.                 
         RETURN NO-APPLY.
      END.    
      
      IF Guru.Konstanter:globforetag = "ELPA" THEN DO:   
         IF FILL-IN_VILART = "791" THEN DO:
            IF FILL-IN-MOMS = 0 THEN DO:
               MESSAGE "Har du glömt att fylla i momsen?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.        
               IF val1 = TRUE THEN DO:                  
                  RETURN NO-APPLY.
               END.             
            END.
            MESSAGE "Obs! Kvitton måste redovisas till lönekontoret före den 10:e påföljande månad " SKIP
                    "med blanketten som finns på Intranätet under Personal / Mallar / Kvittoredovisning" VIEW-AS ALERT-BOX.          
         END.
         IF FILL-IN_VILART = "792" THEN DO:           
            MESSAGE "Momsen registreras tillsammans med utlägget!" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
         END.   
      END.
      IF Guru.Konstanter:globforetag = "GKAL" THEN DO:   
         IF FILL-IN_LONTILLAGG = "764"  THEN DO:    /*FILL-IN_VILART = "825"*/
            IF FILL-IN-MOMS = 0 THEN DO:
               MESSAGE "Har du glömt att fylla i momsen?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val2 AS LOGICAL.        
               IF val2 = TRUE THEN DO:                  
                  RETURN NO-APPLY.
               END.
            END.
            MESSAGE "Obs! Kvitton måste redovisas till lönekontoret före den 10:e påföljande månad " SKIP
                    "med blanketten som finns på Intranätet under Personal / Mallar / Kvittoredovisning" VIEW-AS ALERT-BOX.          
            
         END.      
         IF FILL-IN_VILART = "MOMS" THEN DO:           
            MESSAGE "Momsen registreras tillsammans med utlägget!" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
         END.
      END.
      DO TRANSACTION:  
         FIND okost WHERE RECID(okost) = ovrec EXCLUSIVE-LOCK NO-ERROR.  
         regdatum = FILL-IN-DATUM.
         RUN REGVEC.P.
         RUN REGDAG.P.
         ASSIGN         
         okost.AONR = FILL-IN-AONR
         okost.DELNR = FILL-IN-DELNR
         okost.LONTILLAGG = FILL-IN_LONTILLAGG
         okost.LONTILLANTAL = FILL-IN_LONTILLANTAL
         SUBSTRING(okost.LONKODTEXT,1,40) = lontilltemp.LONKODTEXT
         SUBSTRING(okost.LONKODTEXT,41) = FILL-IN-RESMAL
         okost.MOMS = FILL-IN-MOMS
         okost.DATUM = FILL-IN-DATUM
         okost.VECKONUMMER = regvnr
         okost.DAG = regdagnamn.                       
      END.
   END.
   musz = FALSE.
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON ENDKEY OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   musz = FALSE.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON GO OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AVD DIALOG-1
ON VALUE-CHANGED OF CMB_AVD IN FRAME DIALOG-1
DO:
   CMB_AVD = INPUT CMB_AVD.   
   RUN nycolsortprep_UI (INPUT 2).
   RUN openbdynspec_UI IN brwproc[1].
   {CMB_AVDB2.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR DIALOG-1
ON VALUE-CHANGED OF CMB_OMR IN FRAME DIALOG-1
DO:
   CMB_OMR = INPUT CMB_OMR.            
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
   USE-INDEX OMRNAMN NO-LOCK NO-ERROR.           
   IF CMB_OMR = Guru.Konstanter:gomrk + " : alla" THEN sparomrade = sparomrade.
   ELSE sparomrade = CMB_OMR.
   RUN nycolsortprep_UI (INPUT 1).
   RUN openbdynspec_UI IN brwproc[1].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR DIALOG-1
ON ENTRY OF FILL-IN-AONR IN FRAME DIALOG-1 /* Aonr */
DO:
   ASSIGN
   RECT-22:HIDDEN = FALSE
   CMB_AVD:HIDDEN = FALSE
   CMB_OMR:HIDDEN = FALSE
   FILL-IN-SKP:HIDDEN = FALSE
   FILL-IN-TEXT:HIDDEN = FALSE 
   FILL-IN-SKP = "Sök på:"  
   BRW_LON:HIDDEN = TRUE.
   ENABLE  FILL-IN_AONRS FILL-IN_ORTS RAD_FAST WITH FRAME {&FRAME-NAME}.
   ASSIGN
   RAD_FAST:HIDDEN = FALSE   
   FILL-IN_AONRS:HIDDEN = FALSE
   FILL-IN_AONRS:HIDDEN = FALSE 
   FILL-IN_ORTS:HIDDEN = FALSE.   
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND utsokaonr.DELNR = FILL-IN-DELNR AND
   utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
   IF AVAILABLE utsokaonr THEN DO:
      IF Guru.Konstanter:globallao = FALSE AND utsokaonr.FASTAAONR = TRUE AND utsokaonr.OMRADE = " " THEN DO:
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
         USE-INDEX OMR NO-LOCK NO-ERROR.           
         aonrrec = RECID(utsokaonr).
         RAD_FAST = utsokaonr.FASTAAONR.
         ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN NO-ERROR.                        
         IF CMB_OMR:SCREEN-VALUE = ? THEN DO:
            CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
         END.
      END.
      ELSE IF Guru.Konstanter:globallao = FALSE AND utsokaonr.FASTAAONR = FALSE AND utsokaonr.OMRADE = " " THEN DO:
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
         USE-INDEX OMR NO-LOCK NO-ERROR.           
         aonrrec = RECID(utsokaonr).
         RAD_FAST = utsokaonr.FASTAAONR.
         ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN NO-ERROR.                        
         IF CMB_OMR:SCREEN-VALUE = ? THEN DO:
            CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
         END.
      END.
      ELSE DO:        
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = utsokaonr.OMRADE 
         USE-INDEX OMR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE omrtemp THEN DO:
            ASSIGN 
            CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
            CMB_OMR = INPUT CMB_OMR.
         END.
         ELSE ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN. 
      END.
      ASSIGN
      aonrrec = RECID(utsokaonr)
      RAD_FAST = utsokaonr.FASTAAONR.
   END.
   ELSE DO: 
      ASSIGN 
      CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
      CMB_OMR = INPUT CMB_OMR.
      DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.                  
      ASSIGN 
      aonrrec = 0
      RAD_FAST = FALSE.
   END.
   DISPLAY FILL-IN-SKP RAD_FAST WITH FRAME {&FRAME-NAME}.
   ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.
   BRW_AONR:HIDDEN = FALSE.    
   RUN nycolsortprep_UI (INPUT 1).
   RUN openbdynspec_UI IN brwproc[1].
   IF FILL-IN-AONR NE "" THEN DO:
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND utsokaonr.DELNR = FILL-IN-DELNR AND
      utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
      IF AVAILABLE utsokaonr THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(utsokaonr)).
         RUN lastselectdyn_UI IN brwproc[1].
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATUM DIALOG-1
ON LEAVE OF FILL-IN-DATUM IN FRAME DIALOG-1 /* Datum */
DO:
   FILL-IN-DATUM = INPUT FILL-IN-DATUM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATUM DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-DATUM IN FRAME DIALOG-1 /* Datum */
DO:
   ASSIGN
   FILL-IN-DATUM = INPUT FILL-IN-DATUM
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-DATUM.
   RUN AlmanBtn.w.
   FILL-IN-DATUM = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DELNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DELNR DIALOG-1
ON LEAVE OF FILL-IN-DELNR IN FRAME DIALOG-1 /* Delnr */
DO:
  
   IF INPUT FILL-IN-AONR = "" AND INPUT FILL-IN-DELNR = 0 THEN DO:
      ASSIGN
      FILL-IN-AONR = INPUT FILL-IN-AONR
      FILL-IN-DELNR = INPUT FILL-IN-DELNR.
   END.    
   ELSE DO:                            
      ASSIGN
      FILL-IN-AONR = INPUT FILL-IN-AONR
      FILL-IN-DELNR = INPUT FILL-IN-DELNR.               
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND 
      utsokaonr.DELNR = FILL-IN-DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
      IF NOT AVAILABLE utsokaonr THEN DO:
         MESSAGE Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "finns inte." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
      ELSE DO:                                             
         IF utsokaonr.AONRAVDATUM = 01/01/1991 OR
         utsokaonr.AONRAVDATUM >= regdatum THEN FILL-IN-DELNR = FILL-IN-DELNR.
         ELSE DO:
            MESSAGE Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är redan avslutat." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
      musz = FALSE.
      DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-RESMAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-RESMAL DIALOG-1
ON LEAVE OF FILL-IN-RESMAL IN FRAME DIALOG-1 /* Kommentar */
DO:
   FILL-IN-RESMAL = INPUT FILL-IN-RESMAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_AONRS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONRS DIALOG-1
ON ANY-KEY OF FILL-IN_AONRS IN FRAME DIALOG-1 /* Aonr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_AONRS IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONRS DIALOG-1
ON LEAVE OF FILL-IN_AONRS IN FRAME DIALOG-1 /* Aonr */
DO:
   FILL-IN_AONRS = INPUT FILL-IN_AONRS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONRS DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_AONRS IN FRAME DIALOG-1 /* Aonr */
DO:
   FILL-IN_AONRS = INPUT FILL-IN_AONRS.
   IF FILL-IN_AONRS = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_AONRS IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "AONR", INPUT FILL-IN_AONRS).
   RUN fillinupdate_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_LONTILLANTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LONTILLANTAL DIALOG-1
ON ENTRY OF FILL-IN_LONTILLANTAL IN FRAME DIALOG-1 /* Antal */
DO:
   FILL-IN_VILART = INPUT FILL-IN_VILART.
   FIND FIRST lontilltemp WHERE lontilltemp.ERSFINNS = TRUE AND lontilltemp.KOD = ansttemp.KOD AND
   lontilltemp.VILART = FILL-IN_VILART NO-LOCK NO-ERROR.
   IF NOT AVAILABLE lontilltemp THEN DO:     
      MESSAGE "Lönearten finns ej." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_VILART IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   ASSIGN 
   FILL-IN_LONTILLAGG = lontilltemp.LONTILLAGG
   FILL-IN-SORT = lontilltemp.ENHET.   
   DISPLAY FILL-IN-SORT WITH FRAME {&FRAME-NAME}.          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LONTILLANTAL DIALOG-1
ON LEAVE OF FILL-IN_LONTILLANTAL IN FRAME DIALOG-1 /* Antal */
DO:
   FILL-IN_LONTILLANTAL = INPUT FILL-IN_LONTILLANTAL.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_ORTS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORTS DIALOG-1
ON ANY-KEY OF FILL-IN_ORTS IN FRAME DIALOG-1 /* Benämning */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_ORTS IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORTS DIALOG-1
ON LEAVE OF FILL-IN_ORTS IN FRAME DIALOG-1 /* Benämning */
DO:
   FILL-IN_ORTS = INPUT FILL-IN_ORTS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORTS DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_ORTS IN FRAME DIALOG-1 /* Benämning */
DO:
   FILL-IN_ORTS = INPUT FILL-IN_ORTS.
   IF FILL-IN_ORTS = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_ORTS IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "ORT", INPUT FILL-IN_ORTS).
   RUN fillinupdate_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_VILART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_VILART DIALOG-1
ON ANY-KEY OF FILL-IN_VILART IN FRAME DIALOG-1 /* Lönetillägg */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "LEAVE" TO FILL-IN_VILART IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_VILART DIALOG-1
ON ENTRY OF FILL-IN_VILART IN FRAME DIALOG-1 /* Lönetillägg */
DO:                            
   ASSIGN
   FILL-IN-SKP:HIDDEN = TRUE
   RECT-22:HIDDEN = TRUE
   CMB_AVD:HIDDEN = TRUE
   CMB_OMR:HIDDEN = TRUE
   FILL-IN-TEXT:HIDDEN = TRUE
   BRW_AONR:HIDDEN = TRUE 
   RAD_FAST:HIDDEN = TRUE   
   FILL-IN_AONRS:HIDDEN = TRUE 
   FILL-IN_ORTS:HIDDEN = TRUE.
   ENABLE BRW_LON  WITH FRAME {&FRAME-NAME}.
   FIND FIRST lontilltemp WHERE lontilltemp.ERSFINNS = TRUE AND lontilltemp.KOD = ansttemp.KOD AND 
   lontilltemp.VILART = FILL-IN_VILART USE-INDEX VILART NO-LOCK NO-ERROR.
   IF NOT AVAILABLE lontilltemp THEN DO:      
      APPLY "HOME" TO BRW_LON.
      status-ok = BRW_LON:SELECT-FOCUSED-ROW() NO-ERROR.
   END.
   ELSE DO: 
      RUN fillinmoms_UI.
      IF FILL-IN_VILART NE "" THEN RUN sokurvaldyn_UI IN brwproc[2] (INPUT "VILART", INPUT FILL-IN_VILART).
   END.
   BRW_LON:HIDDEN = FALSE.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_VILART DIALOG-1
ON LEAVE OF FILL-IN_VILART IN FRAME DIALOG-1 /* Lönetillägg */
DO: 
   FILL-IN_VILART = INPUT FILL-IN_VILART.   
   RUN fillinmoms_UI.
   IF FILL-IN_VILART NE "" THEN RUN sokurvaldyn_UI IN brwproc[2] (INPUT "VILART", INPUT FILL-IN_VILART).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_FAST DIALOG-1
ON VALUE-CHANGED OF RAD_FAST IN FRAME DIALOG-1
DO:
   RAD_FAST = INPUT RAD_FAST.   
   IF RAD_FAST = FALSE THEN DO:
      CMB_OMR = sparomrade.
      CMB_OMR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = sparomrade.
      FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
      USE-INDEX OMRNAMN NO-LOCK NO-ERROR.
   END.  
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
      IF RAD_FAST = TRUE THEN DO:
         ASSIGN 
         sparomrade = CMB_OMR. 
         CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
         CMB_OMR = INPUT CMB_OMR.      
      END.
   END.
   RUN nycolsortprep_UI (INPUT 1).
   RUN openbdynspec_UI IN brwproc[1].
   IF AVAILABLE utsokaonr THEN DO:   
      status-ok = BRW_AONR:DESELECT-FOCUSED-ROW() NO-ERROR.
   END.
   DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AONR
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
   ASSIGN
   BRW_AONR:TITLE = "Aktiva " + LC(Guru.Konstanter:gaol)
   FILL-IN_AONRS:LABEL = Guru.Konstanter:gaok 
   FILL-IN-AONR:LABEL = Guru.Konstanter:gaok 
   FILL-IN-TEXT = "Visa " + LC(Guru.Konstanter:gaok) + " för:". 
   {TILLFAST.I}
   FIND FIRST utsokaonr WHERE utsokaonr.FASTAAONR = TRUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE utsokaonr THEN DO:
      status-ok = RAD_FAST:DELETE(Guru.Konstanter:gfastl + " " + LC(Guru.Konstanter:gaok)).
   END.
   &Scoped-define FORMATNAMN FILL-IN_AONRS   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-DELNR   
   {DELNRFORMAT.I}
   FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.      
   FILL-IN-RUBRIK = "Övriga ersättningar".
   FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = varaonr AND utsokaonr.DELNR = vardelnr
   NO-LOCK NO-ERROR.
   FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.      
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN LONKODHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT personaltemp.PERSONALKOD,OUTPUT TABLE lontilltemp,OUTPUT TABLE lonkorttemp).
   END.
   ELSE DO:
      RUN LONKODHMT.P  
      (INPUT personaltemp.PERSONALKOD,OUTPUT TABLE lontilltemp,OUTPUT TABLE lonkorttemp).
   END.
   CMB_AVD:DELIMITER = "$". 
   status-ok = CMB_AVD:ADD-LAST(Guru.Konstanter:gavdk + " : alla").   
   status-ok = CMB_OMR:ADD-LAST(Guru.Konstanter:gomrk + " : alla").   
   {ANVAVDSO.I}     
   FOR EACH eavdtemp,         
   EACH avdelningtemp WHERE avdelningtemp.AVDELNINGNR = eavdtemp.AVDELNINGNR.
      status-ok = CMB_AVD:ADD-LAST(avdelningtemp.AVDELNINGNAMN).
   END.   
   
   CMB_AVD:SCREEN-VALUE= Guru.Konstanter:gavdk + " : alla".
   FIND okost WHERE RECID(okost) = ovrec NO-LOCK NO-ERROR.
   RUN grundtid_UI. 
   RUN enable_UI.       
   {FRMSIZED.I}            
   ASSIGN 
   FILL-IN-SKP:HIDDEN = TRUE
   RECT-22:HIDDEN = TRUE
   CMB_AVD:HIDDEN = TRUE
   CMB_OMR:HIDDEN = TRUE
   FILL-IN-TEXT:HIDDEN = TRUE.   
   tempvar = "lontilltemp.ERSFINNS = TRUE AND lontilltemp.KOD = """ + STRING(ansttemp.KOD) + """ ".
   RUN setcolsortvar_UI IN brwproc[2] (INPUT tempvar).
   RUN openbdynspec_UI IN brwproc[2].
   ENABLE BRW_LON WITH FRAME {&FRAME-NAME}.
   GET FIRST BRW_LON NO-LOCK.
   IF NOT AVAILABLE lontilltemp THEN DO:
      APPLY "HOME" TO BRW_LON.
      status-ok = BRW_LON:SELECT-FOCUSED-ROW() NO-ERROR.
   END.     
   {musarrow.i}  
   APPLY "ENTRY" TO FILL-IN_VILART IN FRAME {&FRAME-NAME}.
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
   utsokaonr.OMRADE:READ-ONLY IN BROWSE BRW_AONR = TRUE.
   lontilltemp.VILART:READ-ONLY IN BROWSE BRW_LON = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_AONR:HANDLE IN FRAME {&FRAME-NAME}).            
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_LON:HANDLE IN FRAME {&FRAME-NAME}).
   RUN sattindex_UI IN brwproc[1] (INPUT "OMRADE").
   IF Guru.Konstanter:appcon THEN DO:      
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph.
   END.   
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).
   RUN dynprogextra IN brwproc[1]  (INPUT "omrvisa_UI",INPUT THIS-PROCEDURE).
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
  DISPLAY FILL-IN_VILART FILL-IN-SORT FILL-IN_LONTILLANTAL FILL-IN-MOMS 
          FILL-IN-AONR FILL-IN-DELNR FILL-IN-DATUM FILL-IN-RUBRIK FILL-IN-TEXT 
          CMB_OMR FILL-IN-PKOD FILL-IN-SKP CMB_AVD FILL-IN-RESMAL 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN_VILART FILL-IN_LONTILLANTAL FILL-IN-MOMS FILL-IN-AONR 
         FILL-IN-DELNR FILL-IN-DATUM BTN_REG BTN_NVE-3 BTN_FVE-3 CMB_OMR 
         BTN_AVS CMB_AVD FILL-IN-RESMAL RECT-22 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillinmoms_UI DIALOG-1 
PROCEDURE fillinmoms_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/    
   IF FILL-IN_VILART NE "" THEN DO:
      FIND FIRST lontilltemp WHERE lontilltemp.ERSFINNS = TRUE AND lontilltemp.KOD = ansttemp.KOD AND
      lontilltemp.VILART = FILL-IN_VILART NO-LOCK NO-ERROR.
      IF NOT AVAILABLE lontilltemp THEN DO:     
         MESSAGE "Lönearten finns ej." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_LONTILLAGG IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.        
      ASSIGN FILL-IN_LONTILLAGG = lontilltemp.LONTILLAGG.
      IF lontilltemp.MOMS = TRUE THEN ASSIGN FILL-IN-MOMS:HIDDEN = FALSE.
      ELSE ASSIGN FILL-IN-MOMS:HIDDEN = TRUE.     
   END.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillinupdate_UI DIALOG-1 
PROCEDURE fillinupdate_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/    
   IF AVAILABLE utsokaonr THEN DO:
      ASSIGN
      FILL-IN-AONR = utsokaonr.AONR
      FILL-IN-DELNR = utsokaonr.DELNR.
      DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}. 
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE grundtid_UI DIALOG-1 
PROCEDURE grundtid_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/  
   IF vart = "NYA" THEN DO:     
      IF AVAILABLE utsokaonr THEN DO:
         ASSIGN
         FILL-IN-AONR = utsokaonr.AONR
         FILL-IN-DELNR = utsokaonr.DELNR.
      END.
      ASSIGN     
      FILL-IN-DATUM = bdatum.         
   END.   
   ELSE DO:        
      ASSIGN
      FILL-IN-AONR = okost.AONR
      FILL-IN-DELNR = okost.DELNR
      FILL-IN_LONTILLAGG = okost.LONTILLAGG    
      FILL-IN_LONTILLANTAL = okost.LONTILLANTAL
      FILL-IN-MOMS = okost.MOMS
      FILL-IN-DATUM = okost.DATUM.  
      FIND FIRST lontilltemp WHERE lontilltemp.ERSFINNS = TRUE AND 
      lontilltemp.KOD = ansttemp.KOD AND 
      lontilltemp.LONTILLAGG = FILL-IN_LONTILLAGG
      NO-LOCK NO-ERROR.
      IF AVAILABLE lontilltemp THEN DO:
         ASSIGN FILL-IN_VILART = lontilltemp.VILART FILL-IN-SORT = lontilltemp.ENHET.
      END.   
      FILL-IN-RESMAL = SUBSTRING(okost.LONKODTEXT,41). 
   END.        
   FILL-IN-PKOD = personaltemp.PERSONALKOD.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nycolsortprep_UI DIALOG-1 
PROCEDURE nycolsortprep_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   {NYCOL.I}   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE omrvisa_UI DIALOG-1 
PROCEDURE omrvisa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER TABLE FOR coltemp.
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.
   &Scoped-define FORMATNAMN utsokaonr.AONR
   &Scoped-define FORMATNAMNOMR utsokaonr.OMRADE
   &Scoped-define BROWSE-NAME BRW_AONR
   {OMRAOFORMAT.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
