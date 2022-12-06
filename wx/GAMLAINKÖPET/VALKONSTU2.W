&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
/*          This .W file was created with the Progress AppBuilder.      */
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
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/
{BERIDTEMP.I}
{BERMTRL.I}
{BBENAMNTEMP.I}
{BERUPPTEMP.I}
&Scoped-define SHARED SHARED
{KONVALTEMP.I}
&Scoped-define NEW NEW
{KONVAL2TEMP.I}
{BEREDNINGTEMP.I}
DEFINE NEW SHARED VARIABLE avbvar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kalkrow2 AS ROWID NO-UNDO.
DEFINE NEW SHARED VARIABLE valaonr2 AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE valdelnr2 AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE valort2 AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE valomrade2 AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE katvar2 AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE kalkvar2 AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrrow2 AS ROWID NO-UNDO.
DEFINE SHARED VARIABLE klockansek AS INTEGER NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE kalkrow AS ROWID NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE spec_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE ordning AS INTEGER NO-UNDO.
DEFINE VARIABLE kombnr AS INTEGER NO-UNDO.
DEFINE VARIABLE valkonstapph AS HANDLE NO-UNDO.
DEFINE VARIABLE ktypkodvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE bernumvar AS INTEGER NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE datvar99 AS DATE NO-UNDO.
DEFINE VARIABLE valet AS INTEGER NO-UNDO.
DEFINE TEMP-TABLE ot NO-UNDO
      FIELD ORD AS INTEGER
      FIELD OROW AS ROWID
      INDEX ORD ORD
      INDEX NORD ORD DESCENDING.
      
DEFINE BUFFER idbuff FOR beridtemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_BER

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES beredningtemp kon_val berupptemp kon_val2

/* Definitions for BROWSE BRW_BER                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_BER beredningtemp.OMRADE ~
beredningtemp.BERNR beredningtemp.BENAMNING beredningtemp.AONR ~
beredningtemp.DELNR beredningtemp.AKTIV 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BER 
&Scoped-define QUERY-STRING-BRW_BER FOR EACH beredningtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BER OPEN QUERY BRW_BER FOR EACH beredningtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BER beredningtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BER beredningtemp


/* Definitions for BROWSE BRW_MTRL2                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL2 kon_val.F1 kon_val.ID2 ~
kon_val.EXTRA kon_val.UPPLAG 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL2 
&Scoped-define QUERY-STRING-BRW_MTRL2 FOR EACH kon_val NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL2 OPEN QUERY BRW_MTRL2 FOR EACH kon_val NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL2 kon_val
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL2 kon_val


/* Definitions for BROWSE BRW_UPP                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_UPP berupptemp.UPPLAG berupptemp.ADRESS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UPP 
&Scoped-define QUERY-STRING-BRW_UPP FOR EACH berupptemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UPP OPEN QUERY BRW_UPP FOR EACH berupptemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UPP berupptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UPP berupptemp


/* Definitions for BROWSE BRW_VAL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VAL kon_val2.F1 kon_val2.ID2 ~
kon_val2.EXTRA kon_val2.UPPLAG 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAL 
&Scoped-define QUERY-STRING-BRW_VAL FOR EACH kon_val2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VAL OPEN QUERY BRW_VAL FOR EACH kon_val2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAL kon_val2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAL kon_val2


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_BER CMB_VAL BTN_ORD BRW_VAL BRW_UPP ~
BRW_MTRL2 btn_over BTN_ORDUPP BTN_ORDNER btn_back BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_VAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON btn_back 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75.

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ORD 
     LABEL "Omvänd ordning på de markerade" 
     SIZE 30 BY 1 TOOLTIP "Byt ordning så att den sista kommer först av de markerade.".

DEFINE BUTTON BTN_ORDNER 
     IMAGE-UP FILE "BILDER\pilner":U
     LABEL "Ner" 
     SIZE 4 BY 1.5 TOOLTIP "Flytta ner".

DEFINE BUTTON BTN_ORDUPP 
     IMAGE-UP FILE "BILDER\pilupp":U
     LABEL "" 
     SIZE 4 BY 1.5 TOOLTIP "Flytta upp".

DEFINE BUTTON btn_over 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75.

DEFINE VARIABLE CMB_VAL AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     VIEW-AS COMBO-BOX INNER-LINES 2
     DROP-DOWN-LIST
     SIZE 25.5 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BER FOR 
      beredningtemp SCROLLING.

DEFINE QUERY BRW_MTRL2 FOR 
      kon_val SCROLLING.

DEFINE QUERY BRW_UPP FOR 
      berupptemp SCROLLING.

DEFINE QUERY BRW_VAL FOR 
      kon_val2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BER C-Win _STRUCTURED
  QUERY BRW_BER NO-LOCK DISPLAY
      beredningtemp.OMRADE FORMAT "x(6)":U
      beredningtemp.BERNR COLUMN-LABEL "Ber.nr" FORMAT "->>>>>>9":U
      beredningtemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(30)":U
            WIDTH 40.5
      beredningtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      beredningtemp.DELNR COLUMN-LABEL "Delnr" FORMAT "999":U
      beredningtemp.AKTIV COLUMN-LABEL "Aktiv/Inaktiv" FORMAT "Ja/Nej":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 86 BY 4.04 ROW-HEIGHT-CHARS .67 EXPANDABLE.

DEFINE BROWSE BRW_MTRL2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL2 C-Win _STRUCTURED
  QUERY BRW_MTRL2 NO-LOCK DISPLAY
      kon_val.F1 COLUMN-LABEL "F1" FORMAT "X(10)":U WIDTH 13
      kon_val.ID2 FORMAT "X(10)":U WIDTH 13.38
      kon_val.EXTRA COLUMN-LABEL "Fri id" FORMAT "X(15)":U WIDTH 13.38
      kon_val.UPPLAG COLUMN-LABEL "Upplag" FORMAT ">>>":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 50.5 BY 14.5
         TITLE "Valda byggnummer".

DEFINE BROWSE BRW_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UPP C-Win _STRUCTURED
  QUERY BRW_UPP NO-LOCK DISPLAY
      berupptemp.UPPLAG COLUMN-LABEL "Upplag" FORMAT ">>":U
      berupptemp.ADRESS COLUMN-LABEL "Adress" FORMAT "X(40)":U
            WIDTH 41
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 50.5 BY 14.5
         TITLE "Upplag".

DEFINE BROWSE BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAL C-Win _STRUCTURED
  QUERY BRW_VAL NO-LOCK DISPLAY
      kon_val2.F1 COLUMN-LABEL "F1" FORMAT "X(10)":U WIDTH 13
      kon_val2.ID2 FORMAT "X(10)":U WIDTH 13.38
      kon_val2.EXTRA COLUMN-LABEL "Fri id" FORMAT "X(15)":U WIDTH 13.38
      kon_val2.UPPLAG COLUMN-LABEL "Upplag" FORMAT ">>>":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 50.5 BY 14.5
         TITLE "Byggnummer".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_BER AT ROW 1.5 COL 13.25
     CMB_VAL AT ROW 6 COL 3 NO-LABEL
     BTN_ORD AT ROW 6 COL 65.5
     BRW_VAL AT ROW 7.5 COL 1.5
     BRW_UPP AT ROW 7.5 COL 1.5
     BRW_MTRL2 AT ROW 7.5 COL 59.88
     btn_over AT ROW 12.5 COL 53
     BTN_ORDUPP AT ROW 13 COL 113.5
     BTN_ORDNER AT ROW 15.92 COL 113.5
     btn_back AT ROW 16 COL 53
     BTN_OK AT ROW 22.5 COL 90.88
     BTN_AVB AT ROW 22.5 COL 105.88
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 120 BY 23.54.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: beredningtemp T "?" NO-UNDO temp-db beredningtemp
      TABLE: berupptemp T "?" NO-UNDO temp-db berupptemp
      TABLE: kon_val T "?" NO-UNDO temp-db kon_val
      TABLE: kon_val2 T "?" NO-UNDO temp-db kon_val2
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Val av byggnummer"
         HEIGHT             = 23.54
         WIDTH              = 120
         MAX-HEIGHT         = 26.42
         MAX-WIDTH          = 120.13
         VIRTUAL-HEIGHT     = 26.42
         VIRTUAL-WIDTH      = 120.13
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
/* BROWSE-TAB BRW_BER 1 DEFAULT-FRAME */
/* BROWSE-TAB BRW_VAL BTN_ORD DEFAULT-FRAME */
/* BROWSE-TAB BRW_UPP BRW_VAL DEFAULT-FRAME */
/* BROWSE-TAB BRW_MTRL2 BRW_UPP DEFAULT-FRAME */
ASSIGN 
       BRW_BER:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BRW_BER:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       BRW_MTRL2:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_MTRL2:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 10000
       BRW_MTRL2:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BRW_MTRL2:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       BRW_UPP:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 300
       BRW_UPP:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BRW_UPP:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       BRW_VAL:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 300
       BRW_VAL:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BRW_VAL:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       BTN_ORDNER:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_VAL IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BER
/* Query rebuild information for BROWSE BRW_BER
     _TblList          = "Temp-Tables.beredningtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.beredningtemp.OMRADE
     _FldNameList[2]   > Temp-Tables.beredningtemp.BERNR
"beredningtemp.BERNR" "Ber.nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.beredningtemp.BENAMNING
"beredningtemp.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no "40.5" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.beredningtemp.AONR
"beredningtemp.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.beredningtemp.DELNR
"beredningtemp.DELNR" "Delnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.beredningtemp.AKTIV
"beredningtemp.AKTIV" "Aktiv/Inaktiv" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BER */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL2
/* Query rebuild information for BROWSE BRW_MTRL2
     _TblList          = "Temp-Tables.kon_val"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kon_val.F1
"kon_val.F1" "F1" ? "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kon_val.ID2
"kon_val.ID2" ? "X(10)" "character" ? ? ? ? ? ? no ? no no "13.38" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.kon_val.EXTRA
"kon_val.EXTRA" "Fri id" "X(15)" "character" ? ? ? ? ? ? no ? no no "13.38" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.kon_val.UPPLAG
"kon_val.UPPLAG" "Upplag" ">>>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UPP
/* Query rebuild information for BROWSE BRW_UPP
     _TblList          = "Temp-Tables.berupptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.berupptemp.UPPLAG
"berupptemp.UPPLAG" "Upplag" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.berupptemp.ADRESS
"berupptemp.ADRESS" "Adress" ? "character" ? ? ? ? ? ? no ? no no "41" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_UPP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAL
/* Query rebuild information for BROWSE BRW_VAL
     _TblList          = "Temp-Tables.kon_val2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kon_val2.F1
"kon_val2.F1" "F1" ? "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kon_val2.ID2
"kon_val2.ID2" ? "X(10)" "character" ? ? ? ? ? ? no ? no no "13.38" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.kon_val2.EXTRA
"kon_val2.EXTRA" "Fri id" "X(15)" "character" ? ? ? ? ? ? no ? no no "13.38" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.kon_val2.UPPLAG
"kon_val2.UPPLAG" "Upplag" ">>>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VAL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Val av byggnummer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Val av byggnummer */
DO:
  /* This event will close the window and terminate the procedure.  */
  musz = TRUE.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BER
&Scoped-define SELF-NAME BRW_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BER C-Win
ON VALUE-CHANGED OF BRW_BER IN FRAME DEFAULT-FRAME
DO:
   {muswait.i}
   RUN vcbrwber_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL2
&Scoped-define SELF-NAME BRW_MTRL2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL2 C-Win
ON VALUE-CHANGED OF BRW_MTRL2 IN FRAME DEFAULT-FRAME /* Valda byggnummer */
DO:
   {muswait.i}
   RUN vcbrwval_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UPP
&Scoped-define SELF-NAME BRW_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UPP C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_UPP IN FRAME DEFAULT-FRAME /* Upplag */
DO:
    APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UPP C-Win
ON VALUE-CHANGED OF BRW_UPP IN FRAME DEFAULT-FRAME /* Upplag */
DO:
   {muswait.i}
   RUN vcbrwval2_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VAL
&Scoped-define SELF-NAME BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAL C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_VAL IN FRAME DEFAULT-FRAME /* Byggnummer */
DO:
    APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAL C-Win
ON VALUE-CHANGED OF BRW_VAL IN FRAME DEFAULT-FRAME /* Byggnummer */
DO:
   {muswait.i}
   RUN vcbrwval2_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:    
   musz = TRUE.
   RETURN.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_back C-Win
ON CHOOSE OF btn_back IN FRAME DEFAULT-FRAME
DO:   
   antal_valda = BRW_MTRL2:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_MTRL2:FETCH-SELECTED-ROW(antal_raknare). 
      IF AVAILABLE kon_val THEN DO:
         IF kon_val.EXTRA BEGINS "UPPLAG:" THEN DO:
            MESSAGE "Du kan inte ta bort Linor och kablar! Välj återställ av inköpsprotokollet."  VIEW-AS ALERT-BOX.
         END.
         ELSE DO:
   
            RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(kon_val)). 
            DELETE kon_val.         
            IF antal_raknare = antal_valda THEN RUN selnextprevrow_UI IN brwproc[{&RIGHT-BROWSE}].
         END.
      END.
      antal_raknare = antal_raknare + 1.   
   END.
   RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].  
   RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
   /*
   RUN refreshbrw_UI IN brwproc[2].
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME DEFAULT-FRAME /* Ok */
DO:
   {muswait.i} 
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ORD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ORD C-Win
ON CHOOSE OF BTN_ORD IN FRAME DEFAULT-FRAME /* Omvänd ordning på de markerade */
DO:
   RUN nerupp_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ORDNER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ORDNER C-Win
ON CHOOSE OF BTN_ORDNER IN FRAME DEFAULT-FRAME /* Ner */
DO:   
   RUN btnordnerupp_UI (INPUT FALSE).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ORDUPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ORDUPP C-Win
ON CHOOSE OF BTN_ORDUPP IN FRAME DEFAULT-FRAME
DO:
   RUN btnordnerupp_UI (INPUT TRUE).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_over
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_over C-Win
ON CHOOSE OF btn_over IN FRAME DEFAULT-FRAME
DO:     
   IF valet = 1 THEN DO:   
      antal_valda = BRW_VAL:NUM-SELECTED-ROWS.         
      antal_raknare = 1.
      IF antal_valda > 0 THEN DO:
         DO WHILE antal_raknare LE antal_valda:                                   
            status-ok = BRW_VAL:FETCH-SELECTED-ROW(antal_raknare).
            IF AVAILABLE kon_val2 THEN DO:
               FIND FIRST kon_val WHERE kon_val.BERAONR = kon_val2.BERAONR AND 
               kon_val.OMRADE = kon_val2.OMRADE AND kon_val.NUM = kon_val2.NUM
               NO-LOCK NO-ERROR.
               IF NOT AVAILABLE kon_val THEN DO:
                  RUN konval_UI.
                  RUN setcolindex_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "ORD").
                  RUN setcolsortvar_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").            
                  RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].
               END.
               ELSE DO:
                  spec_rowid = ROWID(kon_val).
               END.
            END.
            antal_raknare = antal_raknare + 1.
         END.
         FIND FIRST kon_val WHERE ROWID(kon_val) = spec_rowid NO-LOCK NO-ERROR.
         IF AVAILABLE kon_val THEN DO:
            RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(kon_val)).
            RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
         END.
         RUN vcbrwval_UI.

         RUN selnextprevrow_UI IN brwproc[{&LEFT-BROWSE}].
         /*
         DELETE kon_val2.       
         */
         
         RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].           
         RUN lastselectdyn_UI IN brwproc[{&LEFT-BROWSE}].
         /*
         status-ok = BRW_VAL:DESELECT-ROWS().
         APPLY "HOME" TO BRW_VAL IN FRAME {&FRAME-NAME}.
         */
      END.                 
   END.
   ELSE DO:
      antal_valda = BRW_UPP:NUM-SELECTED-ROWS.         
      antal_raknare = 1.
      IF antal_valda > 0 THEN DO:
         DO WHILE antal_raknare LE antal_valda:                                   
            status-ok = BRW_UPP:FETCH-SELECTED-ROW(antal_raknare).
            IF AVAILABLE berupptemp THEN DO:
               FOR EACH kon_val2 WHERE kon_val2.UPPLAG = berupptemp.UPPLAG:            
                  FIND FIRST kon_val WHERE kon_val.BERAONR = kon_val2.BERAONR AND 
                  kon_val.OMRADE = kon_val2.OMRADE AND kon_val.NUM = kon_val2.NUM
                  NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE kon_val THEN DO:
                     RUN konval_UI.
                     spec_rowid = ROWID(kon_val).                  
                  END.               
               END.
            END.
            antal_raknare = antal_raknare + 1.
         END.
         RUN setcolindex_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "ORD").
         RUN setcolsortvar_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").            
         RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].
         FIND FIRST kon_val WHERE ROWID(kon_val) = spec_rowid NO-LOCK NO-ERROR.
         IF AVAILABLE kon_val THEN DO:
            RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(kon_val)).
            RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
         END.
         RUN vcbrwval_UI.
         /*
         status-ok = BRW_UPP:DESELECT-ROWS().
         APPLY "HOME" TO BRW_UPP IN FRAME {&FRAME-NAME}.
         */
         RUN selnextprevrow_UI IN brwproc[4].
         RUN openbdynspec_UI IN brwproc[4].   
         RUN lastselectdyn_UI IN brwproc[4].
      END.                 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VAL C-Win
ON VALUE-CHANGED OF CMB_VAL IN FRAME DEFAULT-FRAME
DO:  
   CMB_VAL = INPUT CMB_VAL.
   IF CMB_VAL = "Konstruktioner" OR CMB_VAL = "Objekt" OR CMB_VAL = "Byggnummer" THEN DO:
      ASSIGN
      valet = 1
      BRW_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_VAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE DO:
      ASSIGN
      valet = 2
      BRW_UPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BRW_VAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BER
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(valkonstapph) THEN DELETE PROCEDURE valkonstapph.
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch.
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
   EMPTY TEMP-TABLE beredningtemp NO-ERROR.    
   RUN ladda_UI IN valkonstapph (INPUT valaonr,INPUT valomrade, INPUT Guru.Konstanter:globforetag, OUTPUT TABLE beredningtemp).
   
   FIND FIRST beredningtemp WHERE beredningtemp.BERNR = INTEGER(valaonr) AND
   beredningtemp.OMRADE = valomrade USE-INDEX BERNR NO-LOCK NO-ERROR.
   ASSIGN      
   valomrade2 = beredningtemp.OMRADE
   ordning = 0
   kombnr = 0
   aonrrow2 = ROWID(beredningtemp)
   valet = 1.      
   EMPTY TEMP-TABLE kon_val2 NO-ERROR.    
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN VALBER3.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT valaonr, INPUT valomrade, INPUT Guru.Konstanter:globforetag, OUTPUT TABLE kon_val2).
   END.
   ELSE DO:
      RUN VALBER3.P
      (INPUT valaonr, INPUT valomrade, INPUT Guru.Konstanter:globforetag, OUTPUT TABLE kon_val2).
   END.  
   FIND FIRST kon_val NO-LOCK NO-ERROR.
   IF AVAILABLE kon_val THEN DO: 
      FOR EACH kon_val:        
         FIND FIRST kon_val2 WHERE kon_val2.BERAONR = kon_val.BERAONR AND
         kon_val2.OMRADE = kon_val.OMRADE AND kon_val2.NUM = kon_val.NUM NO-LOCK NO-ERROR.
         kon_val2.ANMARK = "X".
      END.
   END.
   EMPTY TEMP-TABLE kon_val NO-ERROR.    
   FOR EACH kon_val2:      
      IF kon_val2.ANMARK = "X" THEN DO:
         FIND FIRST kon_val WHERE kon_val.BERAONR = kon_val2.BERAONR AND 
         kon_val.OMRADE = kon_val2.OMRADE AND kon_val.NUM = kon_val2.NUM
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE kon_val THEN DO:
            RUN konval_UI.
            DELETE kon_val2.
         END.
         ELSE DO:
            DELETE kon_val2.
         END.
      END.
      ELSE DO:         
         IF kon_val2.F6 NE "" THEN DO:      
            DELETE kon_val2.      
         END.           
      END.      
   END.
   RUN upplhmt_UI IN valkonstapph (INPUT valaonr,INPUT valomrade, OUTPUT TABLE berupptemp).
   OPEN QUERY BRW_BER FOR EACH beredningtemp NO-LOCK.
   &Scoped-define BROWSE-NAME BRW_BER
   {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
   REPOSITION {&BROWSE-NAME} TO ROWID aonrrow2 NO-ERROR.

   OPEN QUERY BRW_UPP FOR EACH berupptemp NO-LOCK.

   RUN setcolindex_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "ORD").
   RUN setcolsortvar_UI IN brwproc[{&LEFT-BROWSE}] (INPUT " WHERE BERAONR = '" + STRING(beredningtemp.BERAONR) + "' AND
   OMRADE = '" + STRING(beredningtemp.OMRADE) + "'").            
   RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].   
   

   RUN setcolindex_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "ORD").
   RUN setcolsortvar_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").            
   RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].
 
   RUN enable_UI.  
   ASSIGN
   status-ok = CMB_VAL:DELETE("0").
   IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      ASSIGN
      BRW_MTRL2:TITLE = "Valda byggnummer" 
      BRW_VAL:TITLE = "Byggnummer"
      C-Win:TITLE   = "Val av byggnummer"
      status-ok = CMB_VAL:ADD-LAST("Byggnummer")IN FRAME {&FRAME-NAME}            
      status-ok = CMB_VAL:ADD-LAST("Upplag")IN FRAME {&FRAME-NAME}
      CMB_VAL:SCREEN-VALUE = "Byggnummer".            
   END.
   ELSE IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      ASSIGN
      BRW_MTRL2:TITLE = "Valda objekt för beställning" 
      BRW_VAL:TITLE = "Objekt"
      C-Win:TITLE   = "Val av objekt"
      status-ok = CMB_VAL:ADD-LAST("Objekt")IN FRAME {&FRAME-NAME}            
      status-ok = CMB_VAL:ADD-LAST("Upplag")IN FRAME {&FRAME-NAME}
      CMB_VAL:SCREEN-VALUE = "Objekt".            
   END.
   ELSE DO:
      ASSIGN
      BRW_MTRL2:TITLE = "Valda konstruktioner för beställning" 
      BRW_VAL:TITLE = "Konstruktioner"
      C-Win:TITLE   = "Val av konstruktioner"
      status-ok = CMB_VAL:ADD-LAST("Konstruktioner")IN FRAME {&FRAME-NAME}            
      status-ok = CMB_VAL:ADD-LAST("Upplag")IN FRAME {&FRAME-NAME}
      CMB_VAL:SCREEN-VALUE = "Konstruktioner".            
   END.
   {FRMSIZE.I}     
      
   APPLY "VALUE-CHANGED" TO CMB_VAL IN FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO BRW_BER IN FRAME {&FRAME-NAME}.
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
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}] 
      (INPUT BRW_VAL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}] 
      (INPUT BRW_MTRL2:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_BER:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[4] 
      (INPUT BRW_UPP:HANDLE IN FRAME {&FRAME-NAME}).

   IF Guru.Konstanter:appcon THEN DO:
      RUN VALKONSTAPP2.P PERSISTENT SET valkonstapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN VALKONSTAPP2.P PERSISTENT SET valkonstapph.
   END. 
   tthandle = TEMP-TABLE bbenamntemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BBENAMNING", INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BBENAMNING", INPUT "").
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnordnerupp_UI C-Win 
PROCEDURE btnordnerupp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER upp AS LOGICAL NO-UNDO.
   DEFINE VARIABLE frannr AS INTEGER NO-UNDO.
   DEFINE VARIABLE tillnr AS INTEGER NO-UNDO.
   DEFINE VARIABLE till_rowid AS ROWID NO-UNDO.
   status-ok = BRW_MTRL2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE kon_val THEN DO:
      ASSIGN
      frannr = kon_val.ORD
      till_rowid = ROWID(kon_val).      
      IF upp = FALSE THEN status-ok = BRW_MTRL2:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.  
      ELSE status-ok = BRW_MTRL2:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.  
      IF status-ok = TRUE THEN DO:
         ASSIGN
         tillnr = kon_val.ORD
         kon_val.ORD = frannr.   
         FIND kon_val WHERE ROWID(kon_val) = till_rowid.
         kon_val.ORD = tillnr.
         RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].
         FIND kon_val WHERE ROWID(kon_val) = till_rowid NO-LOCK NO-ERROR.
         IF AVAILABLE kon_val THEN DO:
            RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(kon_val)).
            RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
         END.
      END.   
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
  DISPLAY CMB_VAL 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BRW_BER CMB_VAL BTN_ORD BRW_VAL BRW_UPP BRW_MTRL2 btn_over BTN_ORDUPP 
         BTN_ORDNER btn_back BTN_OK BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE konval_UI C-Win 
PROCEDURE konval_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   
   ordning = ordning + 1.
   CREATE kon_val. 
   ASSIGN
   kon_val.BERAONR = kon_val2.BERAONR
   kon_val.OMRADE = kon_val2.OMRADE
   kon_val.F1 = kon_val2.F1
   kon_val.F2 = kon_val2.F2
   kon_val.ID2 = kon_val2.ID2
   kon_val.EXTRA = kon_val2.EXTRA
   kon_val.NUM = kon_val2.NUM
   kon_val.GRUPP = kon_val2.GRUPP
   kon_val.ORD = ordning
   SUBSTRING(kon_val.ANMARK,1,15) = STRING(TODAY)
   SUBSTRING(kon_val.ANMARK,20,15) = STRING(klockansek)
   kon_val.UPPLAG = kon_val2.UPPLAG.
   spec_rowid = ROWID(kon_val).
   ASSIGN
   kon_val.EXTRA1 = SUBSTRING(kon_val.EXTRA,3)
   kon_val.EXTRA2 = SUBSTRING(kon_val.EXTRA,1,1).
   RUN laddaberid_UI IN valkonstapph (INPUT kon_val2.BERAONR,INPUT kon_val2.OMRADE,
                                      OUTPUT TABLE beridtemp).

   FIND FIRST beridtemp WHERE beridtemp.AONR = kon_val2.BERAONR AND 
   beridtemp.OMRADE = kon_val2.OMRADE AND beridtemp.NUM = kon_val2.NUM NO-LOCK NO-ERROR.
   IF AVAILABLE beridtemp THEN DO:
      IF beridtemp.XKORD NE ? THEN DO:
         ASSIGN
         kombnr = kombnr + 1
         kon_val.SKAPNUM = kombnr.
         OPEN QUERY idq FOR EACH idbuff WHERE idbuff.AONR = kon_val2.BERAONR AND 
         idbuff.OMRADE = kon_val2.OMRADE AND idbuff.XKORD = beridtemp.XKORD NO-LOCK.
         GET FIRST idq NO-LOCK.
         DO WHILE AVAILABLE(idbuff):
            IF idbuff.NUM = beridtemp.NUM THEN musz = musz.
            ELSE DO:
               RUN laddaberval_UI IN valkonstapph (INPUT kon_val2.BERAONR,INPUT kon_val2.OMRADE,
                                                   INPUT idbuff.NUM,OUTPUT ktypkodvar,OUTPUT bernumvar).
               ordning = ordning + 1.
               CREATE kon_val. 
               ASSIGN
               kon_val.BERAONR = kon_val2.BERAONR
               kon_val.OMRADE = kon_val2.OMRADE
               kon_val.F1 = ktypkodvar                        
               kon_val.NUM = bernumvar
               kon_val.ORD = ordning
               kon_val.SKAPNUM = kombnr
               kon_val.UPPLAG = kon_val2.UPPLAG.
               IF idbuff.FRI2 = ? THEN kon_val.ID2 = idbuff.NATNR.
               ELSE kon_val.ID2 = STRING(idbuff.FRI2).
               kon_val.EXTRA = idbuff.FRI3.
               ASSIGN
               kon_val.EXTRA1 = SUBSTRING(kon_val.EXTRA,3)
               kon_val.EXTRA2 = SUBSTRING(kon_val.EXTRA,1,1)
               SUBSTRING(kon_val.ANMARK,1,15) = STRING(TODAY)
               SUBSTRING(kon_val.ANMARK,20,15) = STRING(klockansek).               
            END.
            GET NEXT idq NO-LOCK.
         END.
         CLOSE QUERY idq.
      END.
      ELSE DO:
         kon_val.SKAPNUM = ?.
      END.
   END.
      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nerupp_UI C-Win 
PROCEDURE nerupp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE ot NO-ERROR. 
   antal_valda = BRW_MTRL2:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}. 
   antal_raknare = 1.
   IF antal_valda > 0 THEN DO:
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_MTRL2:FETCH-SELECTED-ROW(antal_raknare).
         IF AVAILABLE kon_val THEN DO:
            CREATE ot.
            ASSIGN
            ot.OROW = ROWID(kon_val)
            ot.ORD = kon_val.ORD.                            
         END.
         antal_raknare = antal_raknare + 1.
      END.
      FIND FIRST ot USE-INDEX ORD NO-LOCK NO-ERROR.
      antal_raknare = ot.ORD.
      FOR EACH ot USE-INDEX NORD:
         FIND kon_val WHERE ROWID(kon_val) = ot.OROW NO-ERROR.
         IF AVAILABLE kon_val THEN kon_val.ORD = antal_raknare.
         antal_raknare = antal_raknare + 1.
      END.     
      RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].                  
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vcbrwber_UI C-Win 
PROCEDURE vcbrwber_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}     
   /*IF Guru.Konstanter:appcon THEN DO:                           
      RUN VALBER3.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT beredningtemp.BERAONR, INPUT beredningtemp.OMRADE, OUTPUT TABLE kon_val2).
   END.
   ELSE DO:
      RUN VALBER3.P
      (INPUT beredningtemp.BERAONR, INPUT beredningtemp.OMRADE, OUTPUT TABLE kon_val2).
   END.
   */      
   RUN setcolsortvar_UI IN brwproc[{&LEFT-BROWSE}] (INPUT " WHERE BERAONR = '" + STRING(beredningtemp.BERAONR) + "' AND
   OMRADE = '" + STRING(beredningtemp.OMRADE) + "'").            
   RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].      
   APPLY "VALUE-CHANGED" TO BRW_VAL IN FRAME {&FRAME-NAME}.   
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vcbrwval2_UI C-Win 
PROCEDURE vcbrwval2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     
     /*
      RUN setlastrowid_UI IN brwproc[{&LEFT-BROWSE}] (INPUT ROWID(kon_val2)).
      RUN lastselectdyn_UI IN brwproc[{&LEFT-BROWSE}].      
      */
   IF AVAILABLE kon_val2 THEN DO:
      status-ok = BRW_VAL:SELECT-FOCUSED-ROW() IN FRAME DEFAULT-FRAME NO-ERROR.    
      FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = kon_val2.GRUPP
      USE-INDEX KOD NO-LOCK NO-ERROR.
      IF AVAILABLE bbenamntemp THEN DO:
         IF INDEX(kon_val2.F1:LABEL IN BROWSE BRW_VAL," ^") = 0 AND INDEX(kon_val2.F1:LABEL," v") = 0 THEN kon_val2.F1:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B1,1,20).      
         ELSE IF INDEX(kon_val2.F1:LABEL," ^") NE 0 THEN kon_val2.F1:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B1,1,20) + " ^".      
         ELSE IF INDEX(kon_val2.F1:LABEL," v") NE 0 THEN kon_val2.F1:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B1,1,20) + " v".      
      END.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
      END.
      ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr".
      END.
      ELSE DO:
         kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Id".
      END.
      
   END.
   ELSE DO:
      /*
      ASSIGN      
      kon_val2.F1:LABEL IN BROWSE BRW_VAL = "".     
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
      END.
      ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr".
      END.
      ELSE DO:
         kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Id".
      END.
      */
      status-ok = BRW_UPP:SELECT-FOCUSED-ROW() IN FRAME DEFAULT-FRAME NO-ERROR.
      RETURN.
   END.           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vcbrwval_UI C-Win 
PROCEDURE vcbrwval_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF AVAILABLE kon_val THEN DO:
      RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(kon_val)).
      RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
      FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = kon_val.GRUPP
      USE-INDEX KOD NO-LOCK NO-ERROR.      
      IF AVAILABLE bbenamntemp THEN DO:
         IF INDEX(kon_val.F1:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(kon_val.F1:LABEL," v") = 0 THEN kon_val.F1:LABEL IN BROWSE BRW_MTRL2 = SUBSTRING(bbenamntemp.B1,1,20).      
         ELSE IF INDEX(kon_val.F1:LABEL," ^") NE 0 THEN kon_val.F1:LABEL IN BROWSE BRW_MTRL2 = SUBSTRING(bbenamntemp.B1,1,20) + " ^".      
         ELSE IF INDEX(kon_val.F1:LABEL," v") NE 0 THEN kon_val.F1:LABEL IN BROWSE BRW_MTRL2 = SUBSTRING(bbenamntemp.B1,1,20) + " v".               
      END.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  THEN DO:
         kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr".
      END.
      ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr".
      END.
      ELSE DO:
         kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id".
      END.      
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}        
   END.
   ELSE DO:
      /*
      ASSIGN      
      kon_val.F1:LABEL IN BROWSE BRW_MTRL2 = "".     
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr".
      END.
      ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr".
      END.
      ELSE DO:
         kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id".
      END.
      */
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
      RETURN.
   END.              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

