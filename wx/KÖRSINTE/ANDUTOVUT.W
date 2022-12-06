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
&Scoped-define NEW    
&Scoped-define SHARED 
{AVDTEMP.I}
{FLEXTAB.I}
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR extratemp.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}
{OTIDBEORD.I}
DEFINE NEW SHARED VARIABLE bustart3 AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE flrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE flrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE fldrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE klocka AS DECIMAL NO-UNDO. 
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE sparomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE flexkvst AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexkvsl AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexkvslspar AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexmost AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexmostspar AS DECIMAL NO-UNDO.
DEFINE VARIABLE flexmosl AS DECIMAL NO-UNDO.     
DEFINE VARIABLE seku AS INTEGER NO-UNDO.
DEFINE VARIABLE pkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sok3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE nyttaoapph AS HANDLE NO-UNDO.                     /* NYTTAOAPP.P */
DEFINE VARIABLE otbeordapph AS HANDLE NO-UNDO.
DEFINE VARIABLE omravdand AS INTEGER NO-UNDO.   /*används i NYCOL.I*/
DEFINE VARIABLE sptid AS DECIMAL NO-UNDO.
DEFINE VARIABLE hjtid AS DECIMAL NO-UNDO.
{OMRTEMPW.I}
&Scoped-define NEW   
&Scoped-define SHARED
{ANVPERS.I}
&Scoped-define NEW   
&Scoped-define SHARED SHARED
{DIRDEF.I}
/*{EGENBEN.I}*/
{AVDELNINGTEMPT.I}

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
&Scoped-define INTERNAL-TABLES utsokaonr otidbeordtemp

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


/* Definitions for BROWSE BRW_OTBRD                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_OTBRD otidbeordtemp.PERSONALKOD ~
otidbeordtemp.FORNAMN otidbeordtemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OTBRD 
&Scoped-define QUERY-STRING-BRW_OTBRD FOR EACH otidbeordtemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_OTBRD OPEN QUERY BRW_OTBRD FOR EACH otidbeordtemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_OTBRD otidbeordtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OTBRD otidbeordtemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DIALOG-1 ~
    ~{&OPEN-QUERY-BRW_OTBRD}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN_TID CMB_KNAPP FILL-IN_AONR ~
FILL-IN_DELNR FILL-IN_ORSAK BTN_REG BTN_AVB CMB_AVD CMB_OMR CMB_OVERUT ~
RECT-22 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_TID CMB_KNAPP FILL-IN_AONR ~
FILL-IN_DELNR FILL-IN_ORSAK FILL-IN-TEXT CMB_AVD CMB_OMR ~
FILL-IN_PERSONALKOD FILL-IN_DATUM FILL-IN-SKP 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD klock100 DIALOG-1 
FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD klock60 DIALOG-1 
FUNCTION klock60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_REG 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_KNAPP AS CHARACTER FORMAT "X(256)":U 
     LABEL "Knapp" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "In","Ut","Lunch ut","Lunch in","Övertid ut","Övertid in","Annat in","Annat ut","Flex in","Flex ut" 
     DROP-DOWN-LIST
     SIZE 13.38 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OVERUT AS CHARACTER FORMAT "X(4)":U 
     LABEL "Övertiduttag" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Komp,","Över,","Flex,","Ejöv" 
     DROP-DOWN-LIST
     SIZE 7.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_PRISTYP AS CHARACTER FORMAT "X(9)":U 
     LABEL "Debitering" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-OTBRD AS CHARACTER FORMAT "X(256)":U 
     LABEL "Övertidsbeordrare" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-OVER AS CHARACTER FORMAT "X(1)":U 
     LABEL "Övertiduttag" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS AS DECIMAL FORMAT "->>>>9.99":U INITIAL ? 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SKP AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Visa aonr för:" 
     VIEW-AS FILL-IN 
     SIZE 22.75 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_AONRS AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_DATUM AS DATE FORMAT "99/99/99" 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT "999" INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_ORSAK AS CHARACTER FORMAT "X(68)" 
     LABEL "Kommentar" 
     VIEW-AS FILL-IN 
     SIZE 33.5 BY 1.

DEFINE VARIABLE FILL-IN_ORTS AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 14.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_PERSONALKOD AS CHARACTER FORMAT "x(5)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_TID AS DECIMAL FORMAT "99.99" INITIAL 0 
     LABEL "Övertidens slut" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

DEFINE VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 34 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.13 BY 1.21
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      utsokaonr SCROLLING.

DEFINE QUERY BRW_OTBRD FOR 
      otidbeordtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AONR DIALOG-1 _STRUCTURED
  QUERY BRW_AONR NO-LOCK DISPLAY
      utsokaonr.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      utsokaonr.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      utsokaonr.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      utsokaonr.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(40)":U
  ENABLE
      utsokaonr.OMRADE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 49.13 BY 10.25
         TITLE "Aktiva arbetsordernummer".

DEFINE BROWSE BRW_OTBRD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OTBRD DIALOG-1 _STRUCTURED
  QUERY BRW_OTBRD NO-LOCK DISPLAY
      otidbeordtemp.PERSONALKOD COLUMN-LABEL "Enhet" FORMAT "x(5)":U
      otidbeordtemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 10
      otidbeordtemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 24.25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 43 BY 7.75
         TITLE "Övertidsbeordrare" EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_OTBRD AT ROW 8 COL 42
     FILL-IN_TID AT ROW 10.17 COL 18.5 COLON-ALIGNED
     CMB_KNAPP AT ROW 11.88 COL 18.5 COLON-ALIGNED
     FILL-IN_AONR AT ROW 22 COL 11 COLON-ALIGNED
     FILL-IN_DELNR AT ROW 23.67 COL 11 COLON-ALIGNED
     FILL-IN_ORSAK AT ROW 16.79 COL 18.5 COLON-ALIGNED
     BTN_REG AT ROW 24 COL 68.13
     BTN_AVB AT ROW 24 COL 83.13
     FILL-IN_AONRS AT ROW 15.79 COL 61.63 COLON-ALIGNED
     FILL-IN_ORTS AT ROW 15.79 COL 80.13 COLON-ALIGNED
     FILL-IN-TEXT AT ROW 1.25 COL 67.5 COLON-ALIGNED NO-LABEL
     CMB_AVD AT ROW 2.21 COL 67.88 COLON-ALIGNED NO-LABEL
     RAD_FAST AT ROW 3.67 COL 34.75 NO-LABEL
     BRW_AONR AT ROW 4.75 COL 48
     CMB_OMR AT ROW 3.38 COL 67.88 COLON-ALIGNED NO-LABEL
     FILL-IN_PERSONALKOD AT ROW 6.75 COL 18.5 COLON-ALIGNED
     FILL-IN_DATUM AT ROW 8.46 COL 18.5 COLON-ALIGNED
     FILL-IN-SKP AT ROW 15.79 COL 47.13 COLON-ALIGNED NO-LABEL
     FILL-IN-OTBRD AT ROW 15.21 COL 18.5 COLON-ALIGNED
     CMB_OVERUT AT ROW 13.54 COL 18.5 COLON-ALIGNED
     CMB_PRISTYP AT ROW 8.25 COL 43 COLON-ALIGNED
     FILL-IN-PRIS AT ROW 9.5 COL 43 COLON-ALIGNED
     FILL-IN-OVER AT ROW 6.75 COL 43 COLON-ALIGNED HELP
          "ÖVERTID = Ö, KOMP = K, INGEN ERSÄTTNING = I"
     RECT-22 AT ROW 15.58 COL 48
     SPACE(0.61) SKIP(9.12)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändra eller registrera flextid":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: otidbeordtemp T "?" NO-UNDO temp-db otidbeordtemp
      TABLE: utsokaonr T "?" NO-UNDO temp-db utsokaonr
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE Custom                                                   */
/* BROWSE-TAB BRW_OTBRD 1 DIALOG-1 */
/* BROWSE-TAB BRW_AONR RAD_FAST DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_AONR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_AONR:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_AONR:MAX-DATA-GUESS IN FRAME DIALOG-1         = 1000.

/* SETTINGS FOR BROWSE BRW_OTBRD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_OTBRD:HIDDEN  IN FRAME DIALOG-1                = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_OVERUT IN FRAME DIALOG-1
   NO-DISPLAY                                                           */
/* SETTINGS FOR COMBO-BOX CMB_PRISTYP IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_PRISTYP:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-OTBRD IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-OTBRD:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-OVER IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-OVER:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-PRIS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-PRIS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SKP IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-TEXT IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_AONRS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_AONRS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_DATUM IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_ORTS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_ORTS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_PERSONALKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
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
"utsokaonr.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.utsokaonr.ORT
"utsokaonr.ORT" "Ort/Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AONR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OTBRD
/* Query rebuild information for BROWSE BRW_OTBRD
     _TblList          = "Temp-Tables.otidbeordtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.otidbeordtemp.PERSONALKOD
"otidbeordtemp.PERSONALKOD" "Enhet" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.otidbeordtemp.FORNAMN
"otidbeordtemp.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.otidbeordtemp.EFTERNAMN
"otidbeordtemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "24.25" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_OTBRD */
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
ON END-ERROR OF FRAME DIALOG-1 /* Ändra eller registrera flextid */
DO:
   musz = TRUE.
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END.
   IF VALID-HANDLE(otbeordapph) THEN DO:
       RUN borthandle_UI IN otbeordapph.
       DELETE PROCEDURE otbeordapph NO-ERROR.
       otbeordapph = ?.
   END.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Ändra eller registrera flextid */
DO:     
   RETURN.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AONR
&Scoped-define SELF-NAME BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AONR DIALOG-1
ON VALUE-CHANGED OF BRW_AONR IN FRAME DIALOG-1 /* Aktiva arbetsordernummer */
DO:
   IF musz = FALSE THEN DO: 
      IF AVAILABLE utsokaonr THEN DO:
         ASSIGN
         FILL-IN_AONR = utsokaonr.AONR
         FILL-IN_DELNR = utsokaonr.DELNR.
         DISPLAY  FILL-IN_AONR FILL-IN_DELNR WITH FRAME {&FRAME-NAME}.    
      END.
   END.
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_OTBRD
&Scoped-define SELF-NAME BRW_OTBRD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_OTBRD DIALOG-1
ON VALUE-CHANGED OF BRW_OTBRD IN FRAME DIALOG-1 /* Övertidsbeordrare */
DO:  
  
   IF AVAILABLE otidbeordtemp THEN DO:
      ASSIGN FILL-IN-OTBRD = otidbeordtemp.PERSONALKOD.
      DISPLAY FILL-IN-OTBRD WITH FRAME {&FRAME-NAME}.
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   musz = TRUE.
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END.
   IF VALID-HANDLE(otbeordapph) THEN DO:
       RUN borthandle_UI IN otbeordapph.
       DELETE PROCEDURE otbeordapph NO-ERROR.
       otbeordapph = ?.
   END.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:

   {muswait.i}     
   ASSIGN
   sptid = extratemp.TID
   FILL-IN_DATUM = INPUT FILL-IN_DATUM
   FILL-IN_TID = INPUT FILL-IN_TID  
   CMB_KNAPP = INPUT CMB_KNAPP
   /*FILL-IN_AONR = INPUT FILL-IN_AONR
   FILL-IN_DELNR = INPUT FILL-IN_DELNR*/
   FILL-IN_ORSAK = INPUT FILL-IN_ORSAK
   FILL-IN-OTBRD = INPUT FILL-IN-OTBRD
   CMB_OVERUT = INPUT CMB_OVERUT
   regdatum = FILL-IN_DATUM.
   
   FILL-IN_ORSAK = SUBSTRING(FILL-IN_ORSAK,1,158). 
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      SUBSTRING(FILL-IN_ORSAK,159,6) = SUBSTRING(FILL-IN-OTBRD,1,6).
   END.            

   IF CMB_OVERUT = "Komp" THEN FILL-IN-OVER = "K".
   IF CMB_OVERUT = "Över" THEN FILL-IN-OVER = "Ö".
   IF CMB_OVERUT = "Flex" THEN FILL-IN-OVER = "F". 
   IF CMB_OVERUT = "Ejöv" THEN FILL-IN-OVER = "I". 
   IF CMB_OVERUT = "Ledi" THEN FILL-IN-OVER = "L".
   IF CMB_OVERUT = "Komb" THEN FILL-IN-OVER = "L".
   IF FILL-IN_TID > 24.00 THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.   
   IF ( FILL-IN_TID - TRUNCATE(FILL-IN_TID,0)) > 0.59 THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.
   RUN REGDAG.P.
   RUN REGVEC.P.
   {SLFLARBW.I}.   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN GODKOLLA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT pkod,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN GODKOLLA.P  
      (INPUT pkod,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
      RETURN NO-APPLY.      
   END.      
   IF FILL-IN_ORSAK = "" THEN DO:
      MESSAGE "Kommentar är obligatorik vid övertid" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.   
   IF CMB_OVERUT = "Flex" THEN DO:
      MESSAGE "Övertid kan inte lägga som flex." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.

   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:      
      IF FILL-IN-OTBRD NE ""  THEN DO:      
         FIND FIRST otidbeordtemp WHERE otidbeordtemp.PERSONALKOD = FILL-IN-OTBRD AND otidbeordtemp.AKTIV = TRUE  NO-LOCK NO-ERROR.
         IF NOT AVAILABLE otidbeordtemp  THEN DO:
            MESSAGE "Angiven övertidsbeordrare finns inte"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         IF FILL-IN-OTBRD = personaltemp.PERSONALKOD THEN DO:                  
            MESSAGE "Man kan inte berodra sig själv"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.            
         END.
      END.
      /*IF FILL-IN-OVER = "F" OR personaltemp.OVERTIDUTTAG = "I" THEN DO:
         IF FILL-IN-OTBRD NE "" THEN DO:
            MESSAGE "Övertidsbeordrare skall skall bara användas för övertid"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.      
      END.         */
      IF personaltemp.OVERTIDUTTAG = "I" THEN DO:
         IF FILL-IN-OTBRD NE "" THEN DO:
            MESSAGE "Övertidsbeordrare skall skall bara användas för övertid"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.      
      END.
      ELSE DO:      
         IF FILL-IN_TID > regslut  THEN DO:                       
            IF FILL-IN-OTBRD = "" THEN DO:
               MESSAGE "Övertidsbeordrare måste anges vid övertid. Välj från vallista!"  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.      
            IF FILL-IN_ORSAK = "" THEN DO:
               MESSAGE "Kommentar är obligatorik vid övertid"  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
         END.
         ELSE IF FILL-IN_TID LE regslut  THEN DO:                       
            IF FILL-IN-OTBRD NE "" THEN DO:
               MESSAGE "Övertidsbeordrare skall skall bara användas för övertid"  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.      
         END.
      END.    
   END.   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
      sok2 = 0.
      IF personaltemp.OVERTIDUTTAG = "I" THEN musz = musz.
      /*ELSE IF FILL-IN-START GE regstart AND FILL-IN-SLUT LE regslut  THEN musz = musz.*/
      ELSE DO:                        
         ASSIGN
         sok1 = personaltemp.PERSONALKOD
         sok4 = STRING(FILL-IN_DATUM)
         sok5 = regstart.

         IF Guru.Konstanter:appcon THEN DO: 
            RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT 47,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
            INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
         END.
         ELSE DO:
            RUN FLEXTIDH.P 
            (INPUT 47,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
            INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
         END.
      END.                  
      hjtid = klock60( klock100(sok2 / 100) + klock100(FILL-IN_TID) - klock100(sptid)).     
      MESSAGE hjtid sok2 FILL-IN_TID sptid      VIEW-AS ALERT-BOX.
      IF hjtid < 0 THEN DO:
         MESSAGE "Full arbetstid är inte uppnådd med denna sluttid, alltså är detta ingen övertid" VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.
   END.
   FIND FIRST flexregtemp WHERE flexregtemp.KOD = flexavttemp.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.
   IF AVAILABLE flexregtemp THEN DO:
      flexkvst = flexregtemp.KVSTART.
      flexkvsl = flexregtemp.KVSLUT.
      IF MONTH(regdatum) > MONTH(flexregtemp.SOMMARST) AND 
         MONTH(regdatum) < MONTH(flexregtemp.SOMMARSL) THEN DO:
         flexkvst = flexregtemp.KVSOST.
         flexkvsl = flexregtemp.KVSOSL.
      END.
      ELSE IF MONTH(regdatum) = MONTH(flexregtemp.SOMMARSL) AND 
         DAY(regdatum) <= DAY(flexregtemp.SOMMARSL) THEN DO:
         flexkvst = flexregtemp.KVSOST.
         flexkvsl = flexregtemp.KVSOSL.
      END.
      ELSE IF MONTH(regdatum) = MONTH(flexregtemp.SOMMARST) AND 
         DAY(regdatum) >= DAY(flexregtemp.SOMMARST) THEN DO: 
         flexkvst = flexregtemp.KVSOST.
         flexkvsl = flexregtemp.KVSOSL.
      END.
      ASSIGN
      flexmost = flexregtemp.MOSTART
      flexmosl = flexregtemp.MOSLUT.
      /*hårdkodning för att slippa ändra flexreg.MOSLUT efter sommartid OBS- måste ändras till vintertid en gång till*/
      IF MONTH(TODAY) > MONTH(flexregtemp.SOMMARST) AND MONTH(TODAY) < MONTH(flexregtemp.SOMMARSL) THEN DO:
         IF flexavttemp.FLEXKOD = "K" THEN regdatum = regdatum.               
         ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.
         /*ELSE IF flexavttemp.FLEXKOD = "KV" AND flexmosl = 8 THEN ASSIGN flexmosl = 7.30.*/       
      END.
      ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARSL) AND DAY(TODAY) <= DAY(flexregtemp.SOMMARSL) THEN DO:
         IF flexavttemp.FLEXKOD = "K" THEN regdatum = regdatum.
         ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.
         /*ELSE IF flexavttemp.FLEXKOD = "KV" AND flexmosl = 8 THEN ASSIGN flexmosl = 7.30.*/
      END.
      ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARST) AND DAY(TODAY) >= DAY(flexregtemp.SOMMARST) THEN DO: 
         IF flexavttemp.FLEXKOD = "K" THEN regdatum = regdatum.
         ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.
         /*ELSE IF flexavttemp.FLEXKOD = "KV" AND flexmosl = 8 THEN ASSIGN flexmosl = 7.30.*/
      END.
      ELSE DO:
         IF flexavttemp.FLEXKOD = "K" THEN regdatum = regdatum.
         ELSE  IF flexmosl = 8.30 THEN ASSIGN flexmosl = 9.00.                       
         /*ELSE IF flexavttemp.FLEXKOD = "KV" AND flexmosl = 7.3 THEN ASSIGN flexmosl = 8.*/  
      END.  
      IF personaltemp.DELTID = TRUE THEN DO:
         sok1 = personaltemp.ANSTALLNING.
         IF Guru.Konstanter:appcon THEN DO: 
            RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT 6,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
            INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
         END.
         ELSE DO:
            RUN FLEXTIDH.P 
            (INPUT 6,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
            INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
         END.   
         IF sok2 = 1 THEN sok2 = sok2.
         ELSE DO:
            musz = FALSE.
            FIND FIRST flexregtemp WHERE flexregtemp.KOD = flexavttemp.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.
            IF AVAILABLE flexregtemp THEN DO:            
               IF MONTH(TODAY) > MONTH(flexregtemp.SOMMARST) AND 
               MONTH(TODAY) < MONTH(flexregtemp.SOMMARSL) THEN musz = TRUE.
               ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARSL) AND 
               DAY(TODAY) <= DAY(flexregtemp.SOMMARSL) THEN musz = TRUE.               
               ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARST) AND 
               DAY(TODAY) >= DAY(flexregtemp.SOMMARST) THEN musz = TRUE.
            END.            
            IF musz = TRUE THEN DO:
               musz = FALSE.
               IF sok4 NE "" THEN DO:
                  nytid = DECIMAL(sok4).
               END.
               ELSE DO:
                  sekunder = sok5.
                  RUN SEKTIM.P.
               END.
            END.
            ELSE DO:         
               sekunder = sok5.
               RUN SEKTIM.P.
            END.                        
            IF nytid > regslut THEN DO:                
               flexkvslspar = flexkvsl.
               nytid = flexkvsl.
               RUN TIMSEK.P.
               seku = sekunder.
               nytid = regslut.
               RUN TIMSEK.P.
               sekunder = seku - sok5 + sekunder.
               RUN SEKTIM.P.
               flexkvsl = nytid.
               nytid = flexkvst.
               RUN TIMSEK.P.
               seku = sekunder.
               nytid = regslut.
               RUN TIMSEK.P.
               sekunder = seku - sok5 + sekunder.
               RUN SEKTIM.P.
               flexkvst = nytid.
               IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:
                  flexkvsl = flexkvslspar.
                  /*IF personaltemp.PERSONALKOD = "LMN" THEN DO:
                     flexkvsl = flexkvslspar.
                  END.
                  IF personaltemp.PERSONALKOD = "SEMGN" THEN DO:
                     flexkvsl = flexkvslspar.
                  END.
                  IF personaltemp.PERSONALKOD = "SEABM" THEN DO:
                     flexkvsl = flexkvslspar.
                  END.*/
               END.
            END.
            sok1 = personaltemp.ANSTALLNING.
            IF Guru.Konstanter:appcon THEN DO: 
               RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT 5,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
               INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
            END.
            ELSE DO:
               RUN FLEXTIDH.P 
               (INPUT 5,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
               INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
            END.   
            IF sok2 = 1 THEN sok2 = sok2.
            ELSE DO:
               sekunder = sok5.
               RUN SEKTIM.P.               
               IF nytid < regstart THEN DO:           
                  flexmostspar = flexmost.
                  nytid = flexmosl.
                  RUN TIMSEK.P.
                  seku = sekunder.
                  nytid = regstart.
                  RUN TIMSEK.P.
                  sekunder = seku - sok5 + sekunder.
                  RUN SEKTIM.P.
                  flexmosl = nytid.
                  nytid = flexmost.
                  RUN TIMSEK.P.
                  seku = sekunder.
                  nytid = regstart.
                  RUN TIMSEK.P.
                  sekunder = seku - sok5 + sekunder.
                  RUN SEKTIM.P.
                  flexmost = nytid.
                  IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:               
                     IF personaltemp.PERSONALKOD = "SEMGN" THEN DO:
                        flexmost = flexmostspar.
                     END.
                  END.
               END.               
            END.
         END.        
      END.
      ASSIGN
      sok1 = ansttemp.KOD
      sok4 = STRING(regdatum).
      IF Guru.Konstanter:appcon THEN DO: 
         RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 7,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.
      ELSE DO:
         RUN FLEXTIDH.P 
         (INPUT 7,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.
      IF sok2 = 1 THEN DO:
         flexkvst = regslut.
      END.    
   END.    
   MESSAGE "1" VIEW-AS ALERT-BOX.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN ANDUTOVA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT vart,INPUT personaltemp.PERSONALKOD,INPUT regdatum,INPUT CMB_KNAPP,
      INPUT sok1,INPUT sok2,INPUT FILL-IN_TID,INPUT FILL-IN_ORSAK,
      INPUT flexkvst,INPUT flexkvsl,INPUT Guru.Konstanter:globanv,INPUT regstart,INPUT regslut,
      INPUT regdagnamn, INPUT regvnr,INPUT FILL-IN-OVER ,OUTPUT TABLE felmeddtemp,INPUT-OUTPUT TABLE extratemp).
   END.
   ELSE DO:
      RUN ANDUTOVA.P  
      (INPUT vart,INPUT personaltemp.PERSONALKOD,INPUT regdatum,INPUT CMB_KNAPP,
      INPUT sok1,INPUT sok2,INPUT FILL-IN_TID,INPUT FILL-IN_ORSAK,
      INPUT flexkvst,INPUT flexkvsl,INPUT Guru.Konstanter:globanv,INPUT regstart,INPUT regslut,
      INPUT regdagnamn, INPUT regvnr ,INPUT FILL-IN-OVER ,OUTPUT TABLE felmeddtemp,INPUT-OUTPUT TABLE extratemp).
   END.   
   status-mus2 = SESSION:SET-WAIT-STATE("").
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.      
   {musarrow.i}
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
   IF VALID-HANDLE(otbeordapph) THEN DO:
       RUN borthandle_UI IN otbeordapph.
       DELETE PROCEDURE otbeordapph NO-ERROR.
       otbeordapph = ?.
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


&Scoped-define SELF-NAME CMB_KNAPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_KNAPP DIALOG-1
ON VALUE-CHANGED OF CMB_KNAPP IN FRAME DIALOG-1 /* Knapp */
DO:
  CMB_KNAPP = INPUT CMB_KNAPP.
  /*IF CMB_KNAPP = "FLEX UT"  OR CMB_KNAPP = "LUNCH UT" THEN DO:
     ASSIGN
     FILL-IN_AONR:HIDDEN = TRUE
     FILL-IN_DELNR:HIDDEN = TRUE
     BRW_AONR:HIDDEN = TRUE
     RAD_FAST:HIDDEN = TRUE   
     FILL-IN_AONRS:HIDDEN = TRUE 
     FILL-IN_ORTS:HIDDEN = TRUE.
  END.
  ELSE DO:
     ASSIGN
     FILL-IN_AONR:HIDDEN = FALSE
     FILL-IN_DELNR:HIDDEN = FALSE
     BRW_AONR:HIDDEN = FALSE
     RAD_FAST:HIDDEN = FALSE  
     FILL-IN_AONRS:HIDDEN = FALSE 
     FILL-IN_ORTS:HIDDEN = FALSE.
  END.*/
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


&Scoped-define SELF-NAME CMB_OVERUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OVERUT DIALOG-1
ON ENTRY OF CMB_OVERUT IN FRAME DIALOG-1 /* Övertiduttag */
DO:
   /*DISABLE FILL-IN-PRIS CMB_PRISTYP.
   ASSIGN
   FILL-IN-PRIS:HIDDEN = TRUE   
   CMB_PRISTYP:HIDDEN = TRUE.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OVERUT DIALOG-1
ON VALUE-CHANGED OF CMB_OVERUT IN FRAME DIALOG-1 /* Övertiduttag */
DO:
   CMB_OVERUT = INPUT CMB_OVERUT.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_PRISTYP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_PRISTYP DIALOG-1
ON ENTRY OF CMB_PRISTYP IN FRAME DIALOG-1 /* Debitering */
DO:
   
   DISABLE CMB_OVERUT.
   CMB_OVERUT:HIDDEN = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_PRISTYP DIALOG-1
ON VALUE-CHANGED OF CMB_PRISTYP IN FRAME DIALOG-1 /* Debitering */
DO:
   /*ASSIGN
   CMB_PRISTYP = INPUT CMB_PRISTYP  
   FILL-IN-PRISTYP = CMB_PRISTYP. 
   sok1 = ?. 
   IF Guru.Konstanter:appcon THEN DO: 
      RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 3,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT FILL-IN-PRISTYP,
      INPUT-OUTPUT personaltemp.PERSONALKOD,INPUT-OUTPUT tpris).            
   END.
   ELSE DO:
      RUN FLEXTIDH.P 
      (INPUT 3,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT FILL-IN-PRISTYP,
      INPUT-OUTPUT personaltemp.PERSONALKOD,INPUT-OUTPUT tpris).            
   END.
   ASSIGN
   FILL-IN-PRIS = tpris.  
   DISPLAY FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-OVER DIALOG-1
ON LEAVE OF FILL-IN-OVER IN FRAME DIALOG-1 /* Övertiduttag */
DO:
  FILL-IN-OVER = INPUT FILL-IN-OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PRIS DIALOG-1
ON LEAVE OF FILL-IN-PRIS IN FRAME DIALOG-1 /* Pris */
DO:
   FILL-IN-PRIS = INPUT FILL-IN-PRIS.
   DISABLE FILL-IN-PRIS CMB_PRISTYP.
   ASSIGN
   FILL-IN-PRIS:HIDDEN = TRUE   
   CMB_PRISTYP:HIDDEN = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONR DIALOG-1
ON ENTRY OF FILL-IN_AONR IN FRAME DIALOG-1 /* Aonr */
DO:
   RUN entryaonr_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONR DIALOG-1
ON LEAVE OF FILL-IN_AONR IN FRAME DIALOG-1 /* Aonr */
DO:
  IF FILL-IN_AONR NE INPUT FILL-IN_AONR THEN DO:
      FILL-IN_AONR = INPUT FILL-IN_AONR.
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN_AONR AND 
      utsokaonr.DELNR = FILL-IN_DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
      IF AVAILABLE utsokaonr THEN DO:                  
         DISPLAY FILL-IN_AONR FILL-IN_DELNR WITH FRAME {&FRAME-NAME}.
      END.
   END.
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
ON ENTRY OF FILL-IN_AONRS IN FRAME DIALOG-1 /* Aonr */
DO:
   SESSION:DATA-ENTRY-RETURN = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONRS DIALOG-1
ON LEAVE OF FILL-IN_AONRS IN FRAME DIALOG-1 /* Aonr */
DO:
   FILL-IN_AONRS = INPUT FILL-IN_AONRS.
   SESSION:DATA-ENTRY-RETURN = TRUE.
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
ON ENTRY OF FILL-IN_ORTS IN FRAME DIALOG-1 /* Benämning */
DO:
   SESSION:DATA-ENTRY-RETURN = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORTS DIALOG-1
ON LEAVE OF FILL-IN_ORTS IN FRAME DIALOG-1 /* Benämning */
DO:
   FILL-IN_ORTS = INPUT FILL-IN_ORTS.
   SESSION:DATA-ENTRY-RETURN = TRUE.
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


&Scoped-define SELF-NAME FILL-IN_TID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_TID DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN_TID IN FRAME DIALOG-1 /* Övertidens slut */
DO:
   klocka = INPUT FILL-IN_TID.
   
   RUN KLOCKAN.W.
   
   IF klocka = 00.00 THEN klocka = 24.00.
   FILL-IN_TID = klocka.
   DISPLAY FILL-IN_TID WITH FRAME {&FRAME-NAME}.

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
   IF Guru.Konstanter:globforetag = "cELPA" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
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
   FIND FIRST extratemp NO-ERROR.
   pkod = extratemp.PERSONALKOD.
   IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN DO:   
      RUN othmtbolag_UI IN otbeordapph (INPUT pkod,OUTPUT TABLE otidbeordtemp).
   END.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN ANVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 3,INPUT pkod,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   ELSE DO:
      RUN ANVSKAP.P 
      (INPUT 3,INPUT pkod,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN FLEXTAB.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 4,INPUT "",INPUT-OUTPUT TABLE ansttemp,INPUT-OUTPUT TABLE flexregtemp,
      INPUT-OUTPUT TABLE flexavttemp,INPUT-OUTPUT TABLE flexsaldotemp,INPUT-OUTPUT TABLE utryckningtemp).         END.
   ELSE DO:
      RUN FLEXTAB.P 
      (INPUT 4,INPUT "",INPUT-OUTPUT TABLE ansttemp,INPUT-OUTPUT TABLE flexregtemp,
      INPUT-OUTPUT TABLE flexavttemp,INPUT-OUTPUT TABLE flexsaldotemp,INPUT-OUTPUT TABLE utryckningtemp).      
   END.
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.    
   FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
   NO-LOCK NO-ERROR.   
   FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
   FIND FIRST anvandartemp WHERE anvandartemp.PERSONALKOD = personaltemp.PERSONALKOD 
   NO-LOCK NO-ERROR.
   FILL-IN-OVER = personaltemp.OVERTIDUTTAG.             
   regdatum = extratemp.DATUM.  
   bdatum = extratemp.DATUM.  
   RUN grundtid_UI.    
   RUN enable_UI. 
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
      OPEN QUERY BRW_OTBRD FOR EACH otidbeordtemp WHERE otidbeordtemp.AKTIV = TRUE NO-LOCK.   
      ENABLE FILL-IN-OTBRD BRW_OTBRD WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-OTBRD WITH FRAME {&FRAME-NAME}.      
   END.
   ASSIGN
   CMB_AVD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   CMB_OMR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_AONR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   RAD_FAST:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN_AONR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN_DELNR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN-TEXT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN_AONRS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN_ORTS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN-SKP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   RECT-22:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.

   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:                     
      IF personaltemp.OVERTIDUTTAG = "I" THEN DO:
         ASSIGN FILL-IN-OTBRD = "".         
         FILL-IN-OTBRD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         BRW_OTBRD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.         
      END.
      ELSE DO:
         FILL-IN-OTBRD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         BRW_OTBRD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.                  
      END.
   END.   
   {FRMSIZED.I}
   CMB_KNAPP = "Övertid ut".
   DISPLAY CMB_KNAPP WITH FRAME {&FRAME-NAME}.
   IF VART = "AND" THEN DISABLE CMB_KNAPP WITH FRAME {&FRAME-NAME}.  
   IF FILL-IN-OVER = "K" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "Komp".
   IF FILL-IN-OVER = "Ö" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "Över".
   IF FILL-IN-OVER = "F" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "Flex".     
   IF FILL-IN-OVER = "I" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "Ejöv".
   
   /*IF Guru.Konstanter:globforetag = "elpa" THEN vart = vart.
   ELSE IF vart = "nya" then do:
      status-ok = CMB_KNAPP:DELETE("ÖVERTID IN") IN FRAME {&FRAME-NAME}.
      status-ok = CMB_KNAPP:DELETE("ÖVERTID UT") IN FRAME {&FRAME-NAME}.
   END.       
   IF Guru.Konstanter:globforetag = "SUFL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      status-ok = CMB_KNAPP:DELETE("ÖVERTID IN") IN FRAME {&FRAME-NAME}.
      status-ok = CMB_KNAPP:DELETE("ÖVERTID UT") IN FRAME {&FRAME-NAME}.
      status-ok = CMB_KNAPP:DELETE("Lunch in") IN FRAME {&FRAME-NAME}.
      status-ok = CMB_KNAPP:DELETE("Lunch ut") IN FRAME {&FRAME-NAME}.
   END.
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:
      status-ok = CMB_KNAPP:DELETE("Lunch in") IN FRAME {&FRAME-NAME}.
      status-ok = CMB_KNAPP:DELETE("Lunch ut") IN FRAME {&FRAME-NAME}.
   END.*/
   
   /*FILL-IN_AONR:LABEL = Guru.Konstanter:gaok.*/
   CMB_AVD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   
   /*RUN entryaonr_UI.*/
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
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_AONR:HANDLE IN FRAME {&FRAME-NAME}).       
   RUN sattindex_UI IN brwproc[1] (INPUT "OMRADE").
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_OTBRD:HANDLE IN FRAME {&FRAME-NAME}).
   IF Guru.Konstanter:appcon THEN DO:      
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph.
   END.   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN OVERTIDBRDAPP.P PERSISTENT SET otbeordapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN OVERTIDBRDAPP.P PERSISTENT SET otbeordapph.
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
  DISPLAY FILL-IN_TID CMB_KNAPP FILL-IN_AONR FILL-IN_DELNR FILL-IN_ORSAK 
          FILL-IN-TEXT CMB_AVD CMB_OMR FILL-IN_PERSONALKOD FILL-IN_DATUM 
          FILL-IN-SKP 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN_TID CMB_KNAPP FILL-IN_AONR FILL-IN_DELNR FILL-IN_ORSAK BTN_REG 
         BTN_AVB CMB_AVD CMB_OMR CMB_OVERUT RECT-22 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE entryaonr_UI DIALOG-1 
PROCEDURE entryaonr_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/ 
   ASSIGN
   musz = FALSE
   FILL-IN-SKP = "Sök på:"
   CMB_OMR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
   CMB_AVD:HIDDEN = FALSE
   FILL-IN-TEXT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN_AONR AND utsokaonr.DELNR = FILL-IN_DELNR AND
   utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
   IF AVAILABLE utsokaonr THEN DO:
      IF Guru.Konstanter:globallao = FALSE AND utsokaonr.FASTAAONR = TRUE AND utsokaonr.OMRADE = " " THEN DO:
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = globomr 
         USE-INDEX OMR NO-LOCK NO-ERROR.           
         aonrrec = RECID(utsokaonr).
         RAD_FAST = utsokaonr.FASTAAONR.
         ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN NO-ERROR.                        
         IF CMB_OMR:SCREEN-VALUE = ? THEN DO:
            CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
         END.
      END.
      ELSE IF Guru.Konstanter:globallao = FALSE AND utsokaonr.FASTAAONR = FALSE AND utsokaonr.OMRADE = " " THEN DO:
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = globomr 
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
            IF Guru.Konstanter:globomr = "" OR Guru.Konstanter:globallao = TRUE THEN DO:
                sparomrade = Guru.Konstanter:gomrk + " : alla".
            END.
            ELSE DO:
               IF sparomrade = "" THEN DO:
                  FIND FIRST omrtemp WHERE omrtemp.OMRADE = globomr 
                  USE-INDEX OMR NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE omrtemp THEN DO:
                     FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
                  END.
                  sparomrade = omrtemp.NAMN.
               END.
            END.
            ASSIGN 
            CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
            CMB_OMR = INPUT CMB_OMR.
         END.
         ELSE DO:
            IF sparomrade = "" THEN DO:
               FIND FIRST omrtemp WHERE omrtemp.OMRADE = globomr 
               USE-INDEX OMR NO-LOCK NO-ERROR.
               IF NOT AVAILABLE omrtemp THEN DO:
                  FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
               END.
               sparomrade = omrtemp.NAMN.
            END.
            ELSE sparomrade = omrtemp.NAMN.
            FIND FIRST omrtemp WHERE omrtemp.OMRADE = utsokaonr.OMRADE 
            USE-INDEX OMR NO-LOCK NO-ERROR.
            ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN NO-ERROR.
            IF CMB_OMR:SCREEN-VALUE = ? THEN DO:
               CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
            END.
         END.
      END.
      aonrrec = RECID(utsokaonr).
      RAD_FAST = utsokaonr.FASTAAONR.
   END.
   ELSE DO:
      IF Guru.Konstanter:globomr = "" OR Guru.Konstanter:globallao = TRUE THEN DO:
         sparomrade = Guru.Konstanter:gomrk + " : alla".
         ASSIGN CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
         CMB_OMR = INPUT CMB_OMR.
         DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = globomr 
         USE-INDEX OMR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE omrtemp THEN DO:
            FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
         END.
         sparomrade = omrtemp.NAMN.
         CMB_OMR:SCREEN-VALUE = omrtemp.NAMN NO-ERROR.
         IF CMB_OMR:SCREEN-VALUE = ? THEN DO:
            CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
         END.
         CMB_OMR = INPUT CMB_OMR.
         DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
      END.
      aonrrec = 0.
      RAD_FAST = FALSE.
   END.
   DISPLAY RAD_FAST FILL-IN-SKP WITH FRAME {&FRAME-NAME}.
   ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.
   ENABLE FILL-IN_AONRS FILL-IN_ORTS RAD_FAST WITH FRAME {&FRAME-NAME}.
   ASSIGN
   BRW_AONR:HIDDEN = FALSE    
   RAD_FAST:HIDDEN = FALSE   
   FILL-IN_AONRS:HIDDEN = FALSE 
   FILL-IN_ORTS:HIDDEN = FALSE.
   RUN nycolsortprep_UI (INPUT 1).
   RUN openbdynspec_UI IN brwproc[1].
   IF FILL-IN_AONR NE "" THEN DO:
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN_AONR AND utsokaonr.DELNR = FILL-IN_DELNR AND
      utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
      IF AVAILABLE utsokaonr THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(utsokaonr)).
         RUN lastselectdyn_UI IN brwproc[1].
      END.
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
      FILL-IN_AONR = utsokaonr.AONR
      FILL-IN_DELNR = utsokaonr.DELNR.
      DISPLAY FILL-IN_AONR FILL-IN_DELNR WITH FRAME {&FRAME-NAME}. 
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
   FILL-IN_DATUM = bdatum. 
   /*IF extratemp.AUTO = "AUTO" THEN DO: 
      ASSIGN
      FILL-IN_AONR = extratemp.AONR
      FILL-IN_DELNR = extratemp.DELNR.
   END.*/
   /*IF vart = "NYA" THEN DO:
      ASSIGN
      bdatum = extratemp.DATUM
      FILL-IN_TID = 00.00
      CMB_KNAPP = "IN".
   END.   
   ELSE DO:*/
      ASSIGN
      bdatum = extratemp.DATUM
      FILL-IN_TID = extratemp.TID
      /*FILL-IN_AONR = extratemp.AONR
      FILL-IN_DELNR = extratemp.DELNR*/
      FILL-IN_ORSAK = extratemp.ORSAK.
      ASSIGN
      CMB_KNAPP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = extratemp.KNAPP. 
   /*END.      */
   FILL-IN_PERSONALKOD = extratemp.PERSONALKOD.  
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION klock100 DIALOG-1 
FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION klock60 DIALOG-1 
FUNCTION klock60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN  TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) * 60 / 100 ).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

