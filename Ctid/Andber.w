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
{TIDALLT.I}
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR extratidallt.
/* Local Variable Definitions ---                                       */
{AVDTEMP.I}
{ALLDEF.I}
{BEREDSKAP.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{DIRDEF.I}
{PHMT.I}
{OMRTEMPW.I}

DEFINE NEW SHARED VARIABLE bustart3 AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE klocka AS DECIMAL NO-UNDO. 
DEFINE SHARED VARIABLE placerarec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE datkoll AS DATE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE regdagspar AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.
DEFINE VARIABLE sok1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sok3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE sparomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE omravdand AS INTEGER NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE nyttaoapph AS HANDLE NO-UNDO.                     /* NYTTAOAPP.P */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_AONR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES utsokaonr berkodtemp

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


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN_VILART FILL-IN-BERSTART ~
FILL-IN-BERSLUT FILL-IN_BERBEORD FILL-IN-AONR FILL-IN-DELNR BTN_REG BTN_AVB ~
CMB_OMR BRW_BER CMB_AVD RECT-22 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-PKOD FILL-IN_VILART ~
FILL-IN-BERSTART FILL-IN-BERSLUT FILL-IN_BERANTAL FILL-IN_BERBEORD ~
FILL-IN-AONR FILL-IN-DELNR FILL-IN-VPVA CMB_OMR FILL-IN-SKP CMB_AVD 

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

DEFINE BUTTON BTN_FVE-2 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-2 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_REG 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_DAG AS CHARACTER FORMAT "X(3)":U 
     LABEL "Dag" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "mån ","tis","ons","tor","fre","lör","sön" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(6)":U 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BERSLUT AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Slut tid" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BERSTART AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Start tid" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DATUM AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MANAD AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SKP AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-VPVA AS CHARACTER FORMAT "X(256)":U INITIAL "Visa personal för:" 
     VIEW-AS FILL-IN 
     SIZE 24.25 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONRS AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_BERANTAL AS DECIMAL FORMAT "->>>>9.99" INITIAL 0 
     LABEL "Antal" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_BERBEORD AS LOGICAL FORMAT "Ja/Nej" INITIAL NO 
     LABEL "Beordrad" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_BEREDSKAP AS CHARACTER FORMAT "X(4)" 
     LABEL "Löneart" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_LONS AS CHARACTER FORMAT "X(6)" 
     LABEL "Löneart" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_ORTS AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_TEXT AS CHARACTER FORMAT "x(40)" 
     LABEL "Text" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_VILART AS CHARACTER FORMAT "X(4)" 
     LABEL "Löneart" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 32.63 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 54 BY 1.21
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      utsokaonr SCROLLING.

DEFINE QUERY BRW_BER FOR 
      berkodtemp SCROLLING.
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
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 54 BY 10.08
         TITLE "Aktiva arbetsordernummer".

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
    WITH NO-ROW-MARKERS SIZE 54 BY 9.88
         TITLE "Beredskapslönearter".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_AONR AT ROW 4.63 COL 33
     FILL-IN-PKOD AT ROW 2.46 COL 13.5 COLON-ALIGNED
     CMB_DAG AT ROW 5.08 COL 13.5 COLON-ALIGNED
     FILL-IN-MANAD AT ROW 3.79 COL 13.5 COLON-ALIGNED NO-LABEL
     FILL-IN-DATUM AT ROW 5.17 COL 13.5 COLON-ALIGNED
     FILL-IN_VILART AT ROW 6.63 COL 13.5 COLON-ALIGNED
     FILL-IN-BERSTART AT ROW 7.79 COL 13.5 COLON-ALIGNED
     FILL-IN-BERSLUT AT ROW 9.08 COL 13.5 COLON-ALIGNED
     FILL-IN_BERANTAL AT ROW 10.42 COL 13.5 COLON-ALIGNED
     FILL-IN_BERBEORD AT ROW 11.67 COL 13.5 COLON-ALIGNED
     FILL-IN-AONR AT ROW 13 COL 13.5 COLON-ALIGNED
     FILL-IN-DELNR AT ROW 14.25 COL 13.5 COLON-ALIGNED
     BTN_REG AT ROW 16.5 COL 73
     BTN_AVB AT ROW 16.5 COL 88
     FILL-IN_LONS AT ROW 15.04 COL 49.88 COLON-ALIGNED
     FILL-IN_AONRS AT ROW 15.04 COL 49.88 COLON-ALIGNED
     FILL-IN_ORTS AT ROW 15.04 COL 70 COLON-ALIGNED
     FILL-IN_TEXT AT ROW 15.04 COL 70 COLON-ALIGNED
     FILL-IN-VPVA AT ROW 1.38 COL 60.5 COLON-ALIGNED NO-LABEL
     RAD_FAST AT ROW 3.58 COL 31 NO-LABEL
     CMB_OMR AT ROW 3.5 COL 62.25 COLON-ALIGNED NO-LABEL
     BRW_BER AT ROW 4.63 COL 33
     FILL-IN-SKP AT ROW 15.04 COL 32.38 COLON-ALIGNED NO-LABEL
     FILL-IN_BEREDSKAP AT ROW 16.5 COL 19.38 COLON-ALIGNED
     BTN_NVE-2 AT ROW 4.92 COL 24.25
     BTN_FVE-2 AT ROW 5.79 COL 24.25
     CMB_AVD AT ROW 2.38 COL 62.25 COLON-ALIGNED NO-LABEL
     RECT-22 AT ROW 14.79 COL 33
     SPACE(15.37) SKIP(1.74)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändra eller registrera beredskap":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: berkodtemp T "?" NO-UNDO temp-db berkodtemp
      TABLE: utsokaonr T "?" NO-UNDO temp-db utsokaonr
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE Custom                                                   */
/* BROWSE-TAB BRW_AONR 1 DIALOG-1 */
/* BROWSE-TAB BRW_BER CMB_OMR DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_AONR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_AONR:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_AONR:MAX-DATA-GUESS IN FRAME DIALOG-1         = 1000.

ASSIGN 
       BRW_BER:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-1 = TRUE
       BRW_BER:COLUMN-RESIZABLE IN FRAME DIALOG-1       = TRUE.

/* SETTINGS FOR BUTTON BTN_FVE-2 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_FVE-2:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR BUTTON BTN_NVE-2 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NVE-2:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_DAG IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_DAG:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DATUM IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-DATUM:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-MANAD IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-MANAD:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-PKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-SKP IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-VPVA IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_AONRS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_AONRS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_BERANTAL IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_BEREDSKAP IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_BEREDSKAP:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_LONS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_LONS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_ORTS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_ORTS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_TEXT IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_TEXT:HIDDEN IN FRAME DIALOG-1           = TRUE.

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
"utsokaonr.OMRADE" "Område" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.utsokaonr.AONR
"utsokaonr.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.utsokaonr.DELNR
"utsokaonr.DELNR" "Del!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.utsokaonr.ORT
"utsokaonr.ORT" "Ort/Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AONR */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Ändra eller registrera beredskap */
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
ON ENDKEY OF FRAME DIALOG-1 /* Ändra eller registrera beredskap */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AONR
&Scoped-define SELF-NAME BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AONR DIALOG-1
ON MOUSE-MENU-CLICK OF BRW_AONR IN FRAME DIALOG-1 /* Aktiva arbetsordernummer */
DO:
  ASSIGN
   sok1 = utsokaonr.AONR       
   sok2 = utsokaonr.DELNR
   sok4 = "".
   RUN nyupp_UI (INPUT 20).
   IF LENGTH(sok3) > 0 THEN DO:
      MESSAGE sok3 VIEW-AS ALERT-BOX TITLE "Arbetsuppgift".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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


&Scoped-define BROWSE-NAME BRW_BER
&Scoped-define SELF-NAME BRW_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BER DIALOG-1
ON VALUE-CHANGED OF BRW_BER IN FRAME DIALOG-1 /* Beredskapslönearter */
DO:
   IF AVAILABLE berkodtemp THEN DO:
      FILL-IN_BEREDSKAP = berkodtemp.BEREDSKAP.
      FILL-IN_VILART = berkodtemp.VILART.
      DISPLAY FILL-IN_VILART WITH FRAME {&FRAME-NAME}.
   END.
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


&Scoped-define SELF-NAME BTN_NVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-2 DIALOG-1
ON CHOOSE OF BTN_NVE-2 IN FRAME DIALOG-1 /* + */
DO:   
   ASSIGN
   FILL-IN-DATUM = INPUT FILL-IN-DATUM.   
   FILL-IN-DATUM = FILL-IN-DATUM + 1.
   IF MONTH(extratidallt.DATUM) = 12 THEN DO:
      datkoll = DATE(12,31,YEAR(extratidallt.DATUM)).
   END.
   ELSE DO:   
      datkoll = DATE((MONTH(extratidallt.DATUM) + 1),01,YEAR(extratidallt.DATUM)) - 1.
   END.
   IF FILL-IN-DATUM > DAY(datkoll)THEN DO:    
      FILL-IN-DATUM = DAY(datkoll).
   END.      
   DISPLAY FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   {muswait.i}  
   nytid = FILL-IN-BERSTART.
   RUN TIMSEK.P.
   ASSIGN
   regstartsek = sekunder
   nytid = FILL-IN-BERSLUT.
   RUN TIMSEK.P.
   ASSIGN
   regslutsek = sekunder
   sekunder = regslutsek - regstartsek.
   RUN SEKTIM.P.
   FILL-IN_BERANTAL = ROUND(nytid,2).
   DISPLAY FILL-IN_BERANTAL WITH FRAME {&FRAME-NAME}.
   ASSIGN
   FILL-IN-DATUM = INPUT FILL-IN-DATUM.         
   IF MONTH(extratidallt.DATUM) = 12 THEN DO:
      datkoll = DATE(12,31,YEAR(extratidallt.DATUM)).
   END.
   ELSE DO:   
      datkoll = DATE((MONTH(extratidallt.DATUM) + 1),01,YEAR(extratidallt.DATUM)) - 1.
   END.
   IF FILL-IN-DATUM > DAY(datkoll) THEN DO:
      MESSAGE "Felaktigt angivet datum. Denna månad har bara" DAY(datkoll) "dagar."
      VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.            
   IF FILL-IN-DATUM <= 0 THEN DO:
      MESSAGE "Felaktigt angivet datum. Datum kan ej vara mindre än 1." 
      VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.
   IF tillochmeddatum NE ? THEN DO:
      IF DAY(tillochmeddatum) >= FILL-IN-DATUM THEN DO:
         MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med"
         tillochmeddatum VIEW-AS ALERT-BOX.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
         RETURN.   
      END.            
   END.    
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar).
   RUN REGVEC.P.
   RUN REGDAG.P.
   ASSIGN
   CMB_DAG = regdagnamn.     
   RUN VECOKOLL.P. 
   regdagnamn = CMB_DAG.
   musz = FALSE.
   IF Guru.Konstanter:globforetag = "LULE"    THEN DO:
      ASSIGN
      FILL-IN-AONR = INPUT FILL-IN-AONR 
      FILL-IN-DELNR = INPUT FILL-IN-DELNR.
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND 
      utsokaonr.DELNR = FILL-IN-DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
      IF NOT AVAILABLE utsokaonr THEN DO:
         MESSAGE Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "finns inte." VIEW-AS ALERT-BOX.         
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.
      ELSE DO:                    
         {AOKOLLERS.I}
         IF utsokaonr.AONRAVDATUM = 01/01/1991 OR
         utsokaonr.AONRAVDATUM >= regdatum THEN FILL-IN-DELNR = FILL-IN-DELNR.
         ELSE DO:
            MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är redan avslutat." VIEW-AS ALERT-BOX.         
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
   END.   
   ELSE DO:
      musz = musz.      
   END.
   IF musz = TRUE THEN musz = FALSE.
   ELSE DO:         
      ASSIGN      
      FILL-IN-BERSLUT = INPUT FILL-IN-BERSLUT 
      FILL-IN-BERSTART = INPUT FILL-IN-BERSTART 
      FILL-IN_BERANTAL = INPUT FILL-IN_BERANTAL  
      FILL-IN_VILART = INPUT FILL-IN_VILART
      FILL-IN_BERBEORD = INPUT FILL-IN_BERBEORD
      regdagnamn = CMB_DAG.
      ASSIGN
      regdatumspar = regdatum
      regdagspar = regdagnamn.
      FIND FIRST berkodtemp WHERE berkodtemp.BEREDSKAPSAVTAL = personaltemp.BEREDSKAPSAVTAL AND 
      berkodtemp.VILART = FILL-IN_VILART USE-INDEX VILART NO-LOCK NO-ERROR.
      IF NOT AVAILABLE berkodtemp THEN DO:
         MESSAGE "Beredskapskod" fill-in_VILART "finns inte." VIEW-AS ALERT-BOX.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN_VILART IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.
      ELSE IF berkodtemp.VALBAR = FALSE THEN DO:
         MESSAGE "Beredskapslöneart" fill-in_VILART "går ej att använda." VIEW-AS ALERT-BOX.         
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN_VILART IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         ASSIGN FILL-IN_BEREDSKAP = berkodtemp.BEREDSKAP.
      END.   
      ASSIGN
      sok1 = personaltemp.PERSONALKOD    
      sok2 = 0
      sok3 = FILL-IN_BEREDSKAP
      sok4 = ""
      sok5 = 0.      
      IF Guru.Konstanter:appcon THEN DO: 
         RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 21,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.
      ELSE DO:
         RUN FLEXTIDH.P 
         (INPUT 21,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.      
      IF sok3 NE "XXX" THEN DO:
         IF sok2 > 0 THEN persrec = persrec.   
         ELSE DO:
            nytid = FILL-IN-BERSTART.
            RUN TIMSEK.P.
            ASSIGN
            regstartsek = sekunder
            nytid = FILL-IN-BERSLUT.
            RUN TIMSEK.P.
            ASSIGN
            regslutsek = sekunder
            sekunder = regslutsek - regstartsek.
            RUN SEKTIM.P.
            IF FILL-IN_BERANTAL NE ROUND(nytid,2) THEN DO:
               MESSAGE "Antalet eller start och sluttid stämmer inte!" VIEW-AS ALERT-BOX.
               status-mus2 = SESSION:SET-WAIT-STATE("").
               APPLY "ENTRY" TO FILL-IN_BERANTAL IN FRAME {&FRAME-NAME}.
               APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
            END.
         END.      
      END.
      ELSE DO:
         ASSIGN
         sok1 = personaltemp.PERSONALKOD    
         sok2 = 0
         sok3 = FILL-IN_BEREDSKAP
         sok4 = ""
         sok5 = 0.
         IF Guru.Konstanter:appcon THEN DO: 
            RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT 22,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
            INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
         END.
         ELSE DO:
            RUN FLEXTIDH.P 
            (INPUT 22,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
            INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
         END.         
         IF sok3 NE "XXX" THEN DO:
            IF sok2 > 0 THEN persrec = persrec.
            ELSE DO:
               nytid = FILL-IN-BERSTART.
               RUN TIMSEK.P.
               ASSIGN
               regstartsek = sekunder
               nytid = FILL-IN-BERSLUT.
               RUN TIMSEK.P.
               ASSIGN
               regslutsek = sekunder
               sekunder = regslutsek - regstartsek.
               RUN SEKTIM.P.
               IF FILL-IN_BERANTAL NE ROUND(nytid,2) THEN DO:
                  MESSAGE "Antalet eller start och sluttid stämmer inte!" VIEW-AS ALERT-BOX.
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  APPLY "ENTRY" TO FILL-IN_BERANTAL IN FRAME {&FRAME-NAME}.
                  APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
               END.
            END.
         END.
         ELSE DO:
            /*LENA 10/14/2002 SOK1-SOK5*/
            ASSIGN
            sok1 = personaltemp.PERSONALKOD    
            sok2 = 0
            sok3 = ""
            sok4 = ""
            sok5 = 0.            
            /*SPECIALKODER SOM INTE FINNS I TOLKNINGEN*/
            IF Guru.Konstanter:appcon THEN DO: 
               RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT 23,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
               INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
            END.
            ELSE DO:
               RUN FLEXTIDH.P 
               (INPUT 23,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
               INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
            END.            
            IF sok3 NE "XXX" THEN DO:
               IF sok2 > 0 THEN persrec = persrec.   
               ELSE DO:
                  nytid = FILL-IN-BERSTART.
                  RUN TIMSEK.P.
                  ASSIGN
                  regstartsek = sekunder
                  nytid = FILL-IN-BERSLUT.
                  RUN TIMSEK.P.  
                  ASSIGN
                  regslutsek = sekunder
                  sekunder = regslutsek - regstartsek.
                  RUN SEKTIM.P.
                  IF FILL-IN_BERANTAL NE ROUND(nytid,2) THEN DO:
                     MESSAGE "Antalet eller start och sluttid stämmer inte!" VIEW-AS ALERT-BOX.
                     status-mus2 = SESSION:SET-WAIT-STATE("").
                     APPLY "ENTRY" TO FILL-IN_BERANTAL IN FRAME {&FRAME-NAME}.
                     APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
                  END.
               END.      
            END.
            ELSE DO:
               MESSAGE "Nu är det fel på din beredskapsdatabas ta kontakt med ansvarig."
               VIEW-AS ALERT-BOX.
               status-mus2 = SESSION:SET-WAIT-STATE("").
               APPLY "ENTRY" TO FILL-IN_VILART IN FRAME {&FRAME-NAME}.
               APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
            END.
         END.
      END.      
      IF extratidallt.RECTIDVIS = ? THEN DO:
         CREATE tidallt.
         BUFFER-COPY extratidallt TO tidallt.
         tidallt.PERSONALKOD = "".               
      END.
      ELSE DO:
         FIND FIRST tidallt WHERE tidallt.RECTIDVIS = extratidallt.RECTIDVIS NO-ERROR.
         BUFFER-COPY extratidallt TO tidallt.      
         tidallt.PERSONALKOD = "".                 
      END.
      DO TRANSACTION:  
         ASSIGN
         regdagnamn = CMB_DAG.
         ASSIGN
         tidallt.PERSONALKOD = ""
         tidallt.AONR = FILL-IN-AONR
         tidallt.DELNR = FILL-IN-DELNR
         tidallt.DATUM = regdatum         
         tidallt.DAG = CMB_DAG
         tidallt.VECKONUMMER = regvnr
         tidallt.SLUT = 7.00
         tidallt.START = 7.00 
         tidallt.ANVANDARE = Guru.Konstanter:globanv
         tidallt.BERBEORD = FILL-IN_BERBEORD
         tidallt.BEREDSKAP = FILL-IN_BEREDSKAP
         tidallt.BERANTAL = FILL-IN_BERANTAL 
         tidallt.BEREDSKAPSLUT = FILL-IN-BERSLUT 
         tidallt.BEREDSKAPSTART = FILL-IN-BERSTART.
         /*godöver persotb.GKANDVEMNAR = SUBSTRING(TIDREGITAB.PROGRAM,159) Lena 20200603 */
         ASSIGN SUBSTRING(tidallt.PROGRAM,1,158) = "ANDBER" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv.                  
      END.      
      /*KORT SLUT*/            
      ASSIGN
      bdatum = regdatum
      avdatum = regdatum.
      EMPTY TEMP-TABLE extratidallt NO-ERROR.       
      FOR EACH tidallt WHERE tidallt.PERSONALKOD = "":
         CREATE extratidallt.
         BUFFER-COPY tidallt TO extratidallt.
         DELETE tidallt.
      END.
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN BEROVER0.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT Guru.Konstanter:globanv,INPUT pkod,INPUT bdatum, INPUT avdatum,INPUT TABLE extratidallt,
          OUTPUT placerarec,OUTPUT TABLE tidallt APPEND).
      END.
      ELSE DO:
         RUN BEROVER0.P 
         (INPUT Guru.Konstanter:globanv,INPUT pkod,INPUT bdatum, INPUT avdatum,INPUT TABLE extratidallt,
          OUTPUT placerarec,OUTPUT TABLE tidallt APPEND).
      END.      
   END.   
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.
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
   omravdand = 1.
   RUN nycolsortprep_UI (INPUT 2).
   RUN openbdynspec_UI IN brwproc[1].
   {CMB_AVDB2.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_DAG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_DAG DIALOG-1
ON LEAVE OF CMB_DAG IN FRAME DIALOG-1 /* Dag */
DO:
  CMB_DAG = INPUT CMB_DAG.
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
   omravdand = 2.
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
   BRW_BER:HIDDEN = TRUE       
   /*RECT-22:HIDDEN = FALSE*/
   CMB_AVD:HIDDEN = FALSE
   CMB_OMR:HIDDEN = FALSE
   FILL-IN-VPVA:HIDDEN = FALSE.
   ASSIGN    
   FILL-IN_LONS:HIDDEN = TRUE
   FILL-IN_TEXT:HIDDEN = TRUE.   
   ASSIGN 
   FILL-IN-VPVA = "Visa aonr för:".   
   DISPLAY FILL-IN-VPVA WITH FRAME {&FRAME-NAME}.   
   ENABLE FILL-IN_AONRS FILL-IN_ORTS RAD_FAST WITH FRAME {&FRAME-NAME}.  
   ASSIGN
   RAD_FAST:HIDDEN = FALSE   
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
      RAD_FAST = utsokaonr.FASTAAONR.
   END.
   ELSE DO:                   
      ASSIGN 
      CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
      CMB_OMR = INPUT CMB_OMR.
      DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
      RAD_FAST = FALSE.
   END.
   DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}.
   ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.
   BRW_AONR:HIDDEN = FALSE. 
   omravdand = 2.
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR DIALOG-1
ON LEAVE OF FILL-IN-AONR IN FRAME DIALOG-1 /* Aonr */
DO:
   ASSIGN
   FILL-IN-AONR = INPUT FILL-IN-AONR
   FILL-IN-DELNR = INPUT FILL-IN-DELNR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BERSLUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BERSLUT DIALOG-1
ON LEAVE OF FILL-IN-BERSLUT IN FRAME DIALOG-1 /* Slut tid */
DO:
   FILL-IN-BERSLUT = INPUT FILL-IN-BERSLUT.
   IF FILL-IN-BERSLUT > 24.00 THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END. 
    IF SUBSTRING(STRING(FILL-IN-BERSLUT,"99.99"),4 ,2) > "59" THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.    
   IF FILL-IN-BERSLUT = FILL-IN-BERSTART THEN DO:
      MESSAGE "Start och slut kan ej vara lika." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.         
   
   IF FILL-IN-BERSTART > FILL-IN-BERSLUT THEN DO:   
      MESSAGE "Start kan inte vara större än slut." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.   
   END.   
   nytid = FILL-IN-BERSTART.
   RUN TIMSEK.P.
   ASSIGN
   regstartsek = sekunder
   nytid = FILL-IN-BERSLUT.
   RUN TIMSEK.P.
   ASSIGN
   regslutsek = sekunder
   sekunder = regslutsek - regstartsek.
   RUN SEKTIM.P.
   FILL-IN_BERANTAL = ROUND(nytid,2).
   DISPLAY FILL-IN_BERANTAL WITH FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BERSLUT DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-BERSLUT IN FRAME DIALOG-1 /* Slut tid */
DO:
   klocka = INPUT FILL-IN-BERSLUT.
   {AVBGOMD.I}
   RUN KLOCKAN.W.
   {AVBFRAMD.I}
   IF klocka = 00.00 THEN klocka = 24.00.
   FILL-IN-BERSLUT = klocka.
   DISPLAY FILL-IN-BERSLUT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BERSTART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BERSTART DIALOG-1
ON LEAVE OF FILL-IN-BERSTART IN FRAME DIALOG-1 /* Start tid */
DO:
   FILL-IN-BERSTART = INPUT FILL-IN-BERSTART.
   IF FILL-IN-BERSTART > 24.00 THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.      
   IF SUBSTRING(STRING(FILL-IN-BERSTART,"99.99"),4 ,2) > "59" THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.         
   nytid = FILL-IN-BERSTART.
   RUN TIMSEK.P.
   ASSIGN
   regstartsek = sekunder
   nytid = FILL-IN-BERSLUT.
   RUN TIMSEK.P.
   ASSIGN
   regslutsek = sekunder
   sekunder = regslutsek - regstartsek.
   RUN SEKTIM.P.
   FILL-IN_BERANTAL = ROUND(nytid,2).
   DISPLAY FILL-IN_BERANTAL WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BERSTART DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-BERSTART IN FRAME DIALOG-1 /* Start tid */
DO:
   klocka = INPUT FILL-IN-BERSTART.
  {AVBGOMD.I}
   RUN KLOCKAN.W.
   {AVBFRAMD.I}
   FILL-IN-BERSTART = klocka.
   DISPLAY FILL-IN-BERSTART WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATUM DIALOG-1
ON LEAVE OF FILL-IN-DATUM IN FRAME DIALOG-1 /* Datum */
DO:
   musz = musz.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DELNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DELNR DIALOG-1
ON LEAVE OF FILL-IN-DELNR IN FRAME DIALOG-1 /* Delnr */
DO:  
                 
   ASSIGN
   FILL-IN-AONR = INPUT FILL-IN-AONR
   FILL-IN-DELNR = INPUT FILL-IN-DELNR.      
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


&Scoped-define SELF-NAME FILL-IN_BERANTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BERANTAL DIALOG-1
ON LEAVE OF FILL-IN_BERANTAL IN FRAME DIALOG-1 /* Antal */
DO:
   FILL-IN_BERANTAL = INPUT FILL-IN_BERANTAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_BERBEORD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BERBEORD DIALOG-1
ON LEAVE OF FILL-IN_BERBEORD IN FRAME DIALOG-1 /* Beordrad */
DO:
   FILL-IN_BERBEORD = INPUT FILL-IN_BERBEORD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BERBEORD DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN_BERBEORD IN FRAME DIALOG-1 /* Beordrad */
DO:
   IF INPUT FILL-IN_BERBEORD = TRUE THEN FILL-IN_BERBEORD = FALSE.
   IF INPUT FILL-IN_BERBEORD = FALSE THEN FILL-IN_BERBEORD = TRUE.
   DISPLAY FILL-IN_BERBEORD WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_LONS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LONS DIALOG-1
ON ANY-KEY OF FILL-IN_LONS IN FRAME DIALOG-1 /* Löneart */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_LONS IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LONS DIALOG-1
ON LEAVE OF FILL-IN_LONS IN FRAME DIALOG-1 /* Löneart */
DO:
   FILL-IN_LONS = INPUT FILL-IN_LONS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LONS DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_LONS IN FRAME DIALOG-1 /* Löneart */
DO:
   FILL-IN_LONS = INPUT FILL-IN_LONS.
   IF FILL-IN_LONS = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_LONS IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   RUN sokurvaldyn_UI IN brwproc[2] (INPUT "VILART", INPUT FILL-IN_LONS).
   IF AVAILABLE berkodtemp THEN DO:
      FILL-IN_BEREDSKAP = berkodtemp.BEREDSKAP.
      FILL-IN_VILART = berkodtemp.VILART.
      DISPLAY FILL-IN_VILART WITH FRAME {&FRAME-NAME}.
   END.
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


&Scoped-define SELF-NAME FILL-IN_TEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_TEXT DIALOG-1
ON ANY-KEY OF FILL-IN_TEXT IN FRAME DIALOG-1 /* Text */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_TEXT IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_TEXT DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_TEXT IN FRAME DIALOG-1 /* Text */
DO:
   FILL-IN_TEXT = INPUT FILL-IN_TEXT. 
   IF FILL-IN_TEXT = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_TEXT IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   RUN sokurvaldyn_UI IN brwproc[2] (INPUT "LONKODTEXT", INPUT FILL-IN_TEXT).  
   IF AVAILABLE berkodtemp THEN DO:
      FILL-IN_BEREDSKAP = berkodtemp.BEREDSKAP.
      FILL-IN_VILART = berkodtemp.VILART.
      DISPLAY FILL-IN_VILART WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_VILART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_VILART DIALOG-1
ON ENTRY OF FILL-IN_VILART IN FRAME DIALOG-1 /* Löneart */
DO:
   FILL-IN-SKP = "Sök på:".
   DISPLAY FILL-IN-SKP WITH FRAME {&FRAME-NAME}.
   ENABLE BRW_BER  FILL-IN_LONS FILL-IN_TEXT WITH FRAME {&FRAME-NAME}.
   FIND FIRST berkodtemp WHERE berkodtemp.BEREDSKAPSAVTAL = personaltemp.BEREDSKAPSAVTAL AND 
   berkodtemp.VILART = FILL-IN_VILART USE-INDEX VILART NO-LOCK NO-ERROR.
   IF NOT AVAILABLE berkodtemp THEN DO:
      APPLY "HOME" TO BRW_BER.
      status-ok = BRW_BER:SELECT-FOCUSED-ROW() NO-ERROR.
   END.
   ELSE DO:                
      IF FILL-IN_VILART NE "" THEN RUN sokurvaldyn_UI IN brwproc[2] (INPUT "VILART", INPUT FILL-IN_VILART).
   END. 
   ASSIGN
   BRW_BER:HIDDEN = FALSE
   RAD_FAST:HIDDEN = TRUE
   BRW_AONR:HIDDEN = TRUE 
   /*RECT-22:HIDDEN = FALSE*/
   CMB_AVD:HIDDEN = TRUE
   CMB_OMR:HIDDEN = TRUE
   FILL-IN-VPVA:HIDDEN = TRUE
   FILL-IN_AONRS:HIDDEN = TRUE 
   FILL-IN_ORTS:HIDDEN = TRUE 
   FILL-IN_TEXT:HIDDEN = FALSE 
   FILL-IN_LONS:HIDDEN = FALSE    
   FILL-IN-VPVA = "Visa personal för:".
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
   FILL-IN-AONR:LABEL = Guru.Konstanter:gaok.
   {TILLFAST.I}.
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
   /*&Scoped-define FORMATNAMN utsokaonr.AONR
   &Scoped-define FORMATNAMNOMR utsokaonr.OMRADE
   &Scoped-define BROWSE-NAME BRW_AONR
   {OMRAOFORMAT.I}
   */ 
   FIND FIRST extratidallt NO-LOCK NO-ERROR.   
   RUN anst_UI.
   FILL-IN-MANAD = regmannamn.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN GODKOLLA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN GODKOLLA.P  
      (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   FIND FIRST felmeddtemp NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DELETE felmeddtemp.   
   RUN grundtid_UI.
   RUN enable_UI.       
   {FRMSIZED.I}
   ASSIGN 
   /*RECT-22:HIDDEN = TRUE*/
   CMB_AVD:HIDDEN = TRUE
   CMB_OMR:HIDDEN = TRUE
   FILL-IN-VPVA:HIDDEN = TRUE.   
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:
      CMB_AVD:DELIMITER = "$". 
      status-ok = CMB_AVD:ADD-LAST(Guru.Konstanter:gavdk + " : alla").            
      status-ok = CMB_OMR:ADD-LAST(Guru.Konstanter:gomrk + " : alla").      
      {ANVAVDSO.I}
      
      FOR EACH eavdtemp,         
      EACH avdelningtemp WHERE avdelningtemp.AVDELNINGNR = eavdtemp.AVDELNINGNR.
         status-ok = CMB_AVD:ADD-LAST(avdelningtemp.AVDELNINGNAMN).
      END.         
      CMB_AVD:SCREEN-VALUE= Guru.Konstanter:gavdk + " : alla".
      IF AVAILABLE extratidallt THEN DO:
         ASSIGN
         FILL-IN-AONR = extratidallt.AONR
         FILL-IN-DELNR = extratidallt.DELNR.
      END.
      DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.   
   END.
   ELSE DO:
      ASSIGN
      RAD_FAST:HIDDEN = TRUE
      FILL-IN-AONR:HIDDEN = TRUE 
      FILL-IN-DELNR:HIDDEN = TRUE.
   END.
   OPEN QUERY BRW_BER FOR EACH berkodtemp WHERE 
   USE-INDEX VILART NO-LOCK .    
   ASSIGN
   FILL-IN_TEXT:HIDDEN = FALSE 
   FILL-IN_LONS:HIDDEN = FALSE    
   FILL-IN-SKP = "Sök på:".
   DISPLAY FILL-IN-SKP WITH FRAME {&FRAME-NAME}.
   ENABLE BRW_BER  FILL-IN_LONS FILL-IN_TEXT WITH FRAME {&FRAME-NAME}.
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
   utsokaonr.OMRADE:READ-ONLY IN BROWSE BRW_AONR = TRUE.
   berkodtemp.VILART:READ-ONLY IN BROWSE BRW_BER = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_AONR:HANDLE IN FRAME {&FRAME-NAME}).            
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_BER:HANDLE IN FRAME {&FRAME-NAME}).   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anst_UI DIALOG-1 
PROCEDURE anst_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod
   NO-LOCK NO-ERROR. 
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN BERKODHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT personaltemp.PERSONALKOD,OUTPUT TABLE berkodtemp).
   END.
   ELSE DO:
      RUN BERKODHMT.P  
      (INPUT personaltemp.PERSONALKOD,OUTPUT TABLE berkodtemp).
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
  DISPLAY FILL-IN-PKOD FILL-IN_VILART FILL-IN-BERSTART FILL-IN-BERSLUT 
          FILL-IN_BERANTAL FILL-IN_BERBEORD FILL-IN-AONR FILL-IN-DELNR 
          FILL-IN-VPVA CMB_OMR FILL-IN-SKP CMB_AVD 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN_VILART FILL-IN-BERSTART FILL-IN-BERSLUT FILL-IN_BERBEORD 
         FILL-IN-AONR FILL-IN-DELNR BTN_REG BTN_AVB CMB_OMR BRW_BER CMB_AVD 
         RECT-22 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
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
      ASSIGN
      regdagnamn = extratidallt.DAG.
      
      ASSIGN FILL-IN-DATUM = DAY(extratidallt.DATUM).         
      IF tillochmeddatum NE ? THEN DO:
         IF DAY(tillochmeddatum) >= FILL-IN-DATUM THEN DO:
            FILL-IN-DATUM = DAY(tillochmeddatum + 1).
         END.
      END.         
      DISPLAY FILL-IN-DATUM FILL-IN-MANAD WITH FRAME {&FRAME-NAME}.
      ENABLE FILL-IN-DATUM BTN_FVE-2 BTN_NVE-2 WITH FRAME {&FRAME-NAME}.
      ASSIGN
      FILL-IN_BERBEORD = FALSE     
      FILL-IN_BEREDSKAP = ""
      FILL-IN_VILART = ""
      FILL-IN-BERSLUT = 00.00 
      FILL-IN-BERSTART = 00.00
      FILL-IN_BERANTAL = 0.
   END.   
   ELSE DO:
      ASSIGN  FILL-IN-DATUM = DAY(extratidallt.DATUM).
      DISPLAY FILL-IN-DATUM FILL-IN-MANAD WITH FRAME {&FRAME-NAME}.         
      ASSIGN
      regdatum = extratidallt.DATUM
      regvnr = extratidallt.VECKONUMMER
      FILL-IN_BERBEORD = extratidallt.BERBEORD
      FILL-IN-BERSLUT = extratidallt.BEREDSKAPSLUT 
      FILL-IN-BERSTART = extratidallt.BEREDSKAPSTART     
      FILL-IN_BEREDSKAP = extratidallt.BEREDSKAP
      FILL-IN_BERANTAL = extratidallt.BERANTAL.  
      FIND FIRST berkodtemp WHERE berkodtemp.BEREDSKAPSAVTAL = personaltemp.BEREDSKAPSAVTAL AND
      berkodtemp.BEREDSKAP = FILL-IN_BEREDSKAP NO-LOCK NO-ERROR.
      IF AVAILABLE berkodtemp THEN ASSIGN FILL-IN_VILART = berkodtemp.VILART.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nyupp_UI DIALOG-1 
PROCEDURE nyupp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER sok0 AS INTEGER NO-UNDO.
   IF Guru.Konstanter:appcon THEN DO: 
      RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT sok0,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.
   ELSE DO:
      RUN FLEXTIDH.P 
      (INPUT sok0,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.  
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

