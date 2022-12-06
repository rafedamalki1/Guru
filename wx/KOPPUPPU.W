&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 04/16/97 -  2:17 pm

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
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/
&Scoped-define SHARED SHARED   
{KONVALTEMP.I}
{BBENAMNTEMP.I}
{ANNAMN.I}   
{KONID.I} 
      
{KOPPLINA.I}
&Scoped-define NEW NEW
{BERUPPTEMP.I}
DEFINE NEW SHARED VARIABLE kon_rowid AS ROWID NO-UNDO.
DEFINE NEW SHARED VARIABLE komb_rowid AS ROWID NO-UNDO.
DEFINE NEW SHARED VARIABLE varupp AS INTEGER NO-UNDO.   
DEFINE NEW SHARED VARIABLE varadr AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE varanm AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE ny AS LOGICAL NO-UNDO.       
DEFINE NEW SHARED VARIABLE typval AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.   
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.  
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO. 
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE counter AS INTEGER NO-UNDO.  
DEFINE VARIABLE counter2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sparupp AS INTEGER NO-UNDO. 
DEFINE VARIABLE koppuppapph AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE kombtemp NO-UNDO
   FIELD NUM AS INTEGER
   FIELD F1 AS CHARACTER
   FIELD GRUPP AS INTEGER
   FIELD FORNR AS CHARACTER
   FIELD LINNR AS CHARACTER
   FIELD NATNR AS CHARACTER
   FIELD FRI1 AS INTEGER
   FIELD FRI2 AS INTEGER 
   FIELD XKORD AS INTEGER
   FIELD YKORD AS INTEGER
   FIELD ID AS LOGICAL
   FIELD ORD AS INTEGER
   FIELD ID2 AS CHARACTER FORMAT "X(10)"
   INDEX NUM IS PRIMARY NUM ASCENDING
   INDEX X XKORD ASCENDING
   INDEX ORD ORD ASCENDING.    
   
DEFINE NEW SHARED TEMP-TABLE kombtemp2 NO-UNDO
   FIELD NUM AS INTEGER
   FIELD F1 AS CHARACTER
   FIELD GRUPP AS INTEGER
   FIELD FORNR AS CHARACTER
   FIELD LINNR AS CHARACTER
   FIELD NATNR AS CHARACTER
   FIELD FRI1 AS INTEGER
   FIELD FRI2 AS INTEGER 
   FIELD XKORD AS INTEGER
   FIELD YKORD AS INTEGER
   FIELD ID AS LOGICAL
   FIELD ORD AS INTEGER    
   FIELD ID2 AS CHARACTER FORMAT "X(10)"
   INDEX NUM IS PRIMARY NUM ASCENDING
   INDEX X XKORD ASCENDING
   INDEX ORD ORD ASCENDING.  

DEFINE TEMP-TABLE kollkomb NO-UNDO
   FIELD XKORD AS INTEGER.

DEFINE BUFFER idbuff FOR kon_id.
DEFINE BUFFER kombbuff FOR kombtemp2.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_KOMB

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES kombtemp kombtemp2 berupptemp

/* Definitions for BROWSE BRW_KOMB                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KOMB kombtemp.F1 kombtemp.FORNR ~
kombtemp.LINNR kombtemp.NATNR kombtemp.FRI1 kombtemp.FRI2 kombtemp.ID ~
kombtemp.ID2 kombtemp.XKORD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KOMB 
&Scoped-define QUERY-STRING-BRW_KOMB FOR EACH kombtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KOMB OPEN QUERY BRW_KOMB FOR EACH kombtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KOMB kombtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KOMB kombtemp


/* Definitions for BROWSE BRW_KOMB2                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_KOMB2 kombtemp2.F1 kombtemp2.FORNR ~
kombtemp2.LINNR kombtemp2.NATNR kombtemp2.FRI1 kombtemp2.FRI2 kombtemp2.ID ~
kombtemp2.ID2 kombtemp2.XKORD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KOMB2 
&Scoped-define QUERY-STRING-BRW_KOMB2 FOR EACH kombtemp2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KOMB2 OPEN QUERY BRW_KOMB2 FOR EACH kombtemp2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KOMB2 kombtemp2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KOMB2 kombtemp2


/* Definitions for BROWSE BRW_UPP                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_UPP berupptemp.UPPLAG berupptemp.ADRESS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UPP 
&Scoped-define QUERY-STRING-BRW_UPP FOR EACH berupptemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UPP OPEN QUERY BRW_UPP FOR EACH berupptemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UPP berupptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UPP berupptemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-57 FBTN_MARK FBTN_KOMB FBTN_BKOMB ~
FBTN_UPP FBTN_VISA FBTN_SKRIV BTN_AVB RAD_VAL BRW_KOMB BRW_KOMB2 BRW_UPP ~
BTN_NY BTN_ANDRA BTN_BORT 
&Scoped-Define DISPLAYED-OBJECTS RAD_VAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BRW_KOMB 
       MENU-ITEM m_Konstruktioner LABEL "Konstruktioner".

DEFINE MENU POPUP-MENU-BRW_KOMB2 
       MENU-ITEM m_Kombinera_konstruktioner LABEL "Kombinera konstruktioner".


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ANDRA 
     LABEL "Ändra" 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1.5.

DEFINE BUTTON FBTN_BKOMB 
     LABEL "Ta bort komb." 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_KOMB 
     LABEL "Skapa komb." 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_MARK 
     LABEL "Markera" 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_UPP 
     LABEL "Koppla uppl." 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 12 BY 1.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Upplag", 1,
"Kombinera", 2
     SIZE 26.63 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 92 BY .5
     BGCOLOR 1 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_KOMB FOR 
      kombtemp SCROLLING.

DEFINE QUERY BRW_KOMB2 FOR 
      kombtemp2 SCROLLING.

DEFINE QUERY BRW_UPP FOR 
      berupptemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_KOMB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KOMB WINDOW-1 _STRUCTURED
  QUERY BRW_KOMB NO-LOCK DISPLAY
      kombtemp.F1 COLUMN-LABEL "F1" FORMAT "X(10)":U
      kombtemp.FORNR FORMAT "X(10)":U
      kombtemp.LINNR FORMAT "X(10)":U
      kombtemp.NATNR FORMAT "X(10)":U
      kombtemp.FRI1 FORMAT ">>>>>>>>>>":U
      kombtemp.FRI2 FORMAT ">>>>>>>>>>":U
      kombtemp.ID FORMAT "Ja/Nej":U
      kombtemp.ID2 COLUMN-LABEL "Fri id" FORMAT "X(10)":U
      kombtemp.XKORD FORMAT ">>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SEPARATORS MULTIPLE SIZE 90.5 BY 15.58
         TITLE "Konstruktioner".

DEFINE BROWSE BRW_KOMB2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KOMB2 WINDOW-1 _STRUCTURED
  QUERY BRW_KOMB2 NO-LOCK DISPLAY
      kombtemp2.F1 COLUMN-LABEL "F1" FORMAT "X(10)":U
      kombtemp2.FORNR FORMAT "X(10)":U
      kombtemp2.LINNR FORMAT "X(10)":U
      kombtemp2.NATNR FORMAT "X(10)":U
      kombtemp2.FRI1 FORMAT ">>>>>>>>>>":U
      kombtemp2.FRI2 FORMAT ">>>>>>>>>>":U
      kombtemp2.ID FORMAT "Ja/Nej":U
      kombtemp2.ID2 COLUMN-LABEL "Fri id" FORMAT "X(10)":U
      kombtemp2.XKORD FORMAT ">>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SEPARATORS MULTIPLE SIZE 90.5 BY 10.63
         TITLE "Kombinerbara konstruktioner".

DEFINE BROWSE BRW_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UPP WINDOW-1 _STRUCTURED
  QUERY BRW_UPP NO-LOCK DISPLAY
      berupptemp.UPPLAG COLUMN-LABEL "Upplag" FORMAT ">>":U
      berupptemp.ADRESS COLUMN-LABEL "Adress" FORMAT "X(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 50 BY 7.38
         TITLE "Upplagsregister".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     FBTN_MARK AT ROW 1.08 COL 16
     FBTN_KOMB AT ROW 1.08 COL 28
     FBTN_BKOMB AT ROW 1.08 COL 40
     FBTN_UPP AT ROW 1.08 COL 40
     FBTN_VISA AT ROW 1.08 COL 52
     FBTN_SKRIV AT ROW 1.08 COL 64
     BTN_AVB AT ROW 1.08 COL 79.75
     RAD_VAL AT ROW 2.96 COL 3.38 NO-LABEL
     BRW_KOMB AT ROW 3.92 COL 2
     BRW_KOMB2 AT ROW 3.92 COL 2.5
     BRW_UPP AT ROW 19.92 COL 22.25
     BTN_NY AT ROW 27.42 COL 28.25
     BTN_ANDRA AT ROW 27.42 COL 41.25
     BTN_BORT AT ROW 27.42 COL 54.25
     "  Funktioner:" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 1.08 COL 1.25
          FGCOLOR 1 FONT 17
     RECT-57 AT ROW 2.08 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 92.38 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: berupptemp T "?" NO-UNDO temp-db berupptemp
      TABLE: ? T "?" NO-UNDO temp-db kombtemp
      TABLE: ? T "?" NO-UNDO temp-db kombtemp2
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Koppla upplag / Skapa kombinationer"
         HEIGHT             = 28.42
         WIDTH              = 92.38
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 92.38
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 92.38
         RESIZE             = yes
         SCROLL-BARS        = yes
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
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* BROWSE-TAB BRW_KOMB RAD_VAL FRAME-A */
/* BROWSE-TAB BRW_KOMB2 BRW_KOMB FRAME-A */
/* BROWSE-TAB BRW_UPP BRW_KOMB2 FRAME-A */
ASSIGN 
       BRW_KOMB:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_KOMB:POPUP-MENU IN FRAME FRAME-A             = MENU POPUP-MENU-BRW_KOMB:HANDLE
       BRW_KOMB:MAX-DATA-GUESS IN FRAME FRAME-A         = 1000.

ASSIGN 
       BRW_KOMB2:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_KOMB2:POPUP-MENU IN FRAME FRAME-A             = MENU POPUP-MENU-BRW_KOMB2:HANDLE
       BRW_KOMB2:MAX-DATA-GUESS IN FRAME FRAME-A         = 1000.

ASSIGN 
       BRW_UPP:HIDDEN  IN FRAME FRAME-A                = TRUE.

ASSIGN 
       BTN_ANDRA:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       BTN_BORT:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       BTN_NY:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FBTN_BKOMB:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FBTN_KOMB:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FBTN_UPP:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KOMB
/* Query rebuild information for BROWSE BRW_KOMB
     _TblList          = "Temp-Tables.kombtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kombtemp.F1
"kombtemp.F1" "F1" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   = Temp-Tables.kombtemp.FORNR
     _FldNameList[3]   = Temp-Tables.kombtemp.LINNR
     _FldNameList[4]   = Temp-Tables.kombtemp.NATNR
     _FldNameList[5]   > Temp-Tables.kombtemp.FRI1
"kombtemp.FRI1" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.kombtemp.FRI2
"kombtemp.FRI2" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   = Temp-Tables.kombtemp.ID
     _FldNameList[8]   > Temp-Tables.kombtemp.ID2
"kombtemp.ID2" "Fri id" "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   = Temp-Tables.kombtemp.XKORD
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KOMB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KOMB2
/* Query rebuild information for BROWSE BRW_KOMB2
     _TblList          = "Temp-Tables.kombtemp2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kombtemp2.F1
"kombtemp2.F1" "F1" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   = Temp-Tables.kombtemp2.FORNR
     _FldNameList[3]   = Temp-Tables.kombtemp2.LINNR
     _FldNameList[4]   = Temp-Tables.kombtemp2.NATNR
     _FldNameList[5]   > Temp-Tables.kombtemp2.FRI1
"kombtemp2.FRI1" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.kombtemp2.FRI2
"kombtemp2.FRI2" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   = Temp-Tables.kombtemp2.ID
     _FldNameList[8]   > Temp-Tables.kombtemp2.ID2
"kombtemp2.ID2" "Fri id" "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   = Temp-Tables.kombtemp2.XKORD
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KOMB2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UPP
/* Query rebuild information for BROWSE BRW_UPP
     _TblList          = "Temp-Tables.berupptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.berupptemp.UPPLAG
"berupptemp.UPPLAG" "Upplag" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.berupptemp.ADRESS
"berupptemp.ADRESS" "Adress" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_UPP */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_KOMB
&Scoped-define SELF-NAME BRW_KOMB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KOMB WINDOW-1
ON VALUE-CHANGED OF BRW_KOMB IN FRAME FRAME-A /* Konstruktioner */
DO:  
   antal_valda = BRW_KOMB:NUM-SELECTED-ROWS NO-ERROR.  
   IF antal_valda = 0 THEN DO:  
      ASSIGN
      kombtemp.F1:LABEL IN BROWSE BRW_KOMB = "" 
      kombtemp.FORNR:LABEL IN BROWSE BRW_KOMB = ""  
      kombtemp.LINNR:LABEL IN BROWSE BRW_KOMB = ""
      kombtemp.NATNR:LABEL IN BROWSE BRW_KOMB = ""      
      kombtemp.FRI1:LABEL IN BROWSE BRW_KOMB = ""      
      kombtemp.FRI2:LABEL IN BROWSE BRW_KOMB = ""
      kombtemp.ID:LABEL IN BROWSE BRW_KOMB = "Id"
      kombtemp.XKORD:LABEL IN BROWSE BRW_KOMB = "Upplag".    
   END.   
   ELSE DO:   
      FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = kombtemp.GRUPP
      USE-INDEX KOD NO-LOCK NO-ERROR.
/*       {UPLABEL.I} */
      ASSIGN
      kombtemp.F1:LABEL IN BROWSE BRW_KOMB = SUBSTRING(bbenamntemp.B1,1,10) 
      kombtemp.FORNR:LABEL IN BROWSE BRW_KOMB = SUBSTRING(annamntemp.TAB1,1,10)  
      kombtemp.LINNR:LABEL IN BROWSE BRW_KOMB = SUBSTRING(annamntemp.TAB2,1,10)
      kombtemp.NATNR:LABEL IN BROWSE BRW_KOMB = SUBSTRING(annamntemp.TAB3,1,10)         
      kombtemp.ID:LABEL IN BROWSE BRW_KOMB = "Id"
      kombtemp.XKORD:LABEL IN BROWSE BRW_KOMB = "Upplag".
      IF Guru.Konstanter:globforetag = "SUND" THEN DO:
         ASSIGN
         kombtemp.FRI1:LABEL IN BROWSE BRW_KOMB = ""
         kombtemp.FRI2:LABEL IN BROWSE BRW_KOMB = "Objektnr".
      END.   
      ELSE DO:
         ASSIGN
         kombtemp.FRI1:LABEL IN BROWSE BRW_KOMB = SUBSTRING(bbenamntemp.ID1,1,8)
         kombtemp.FRI2:LABEL IN BROWSE BRW_KOMB = SUBSTRING(bbenamntemp.ID2,1,8).
      END.    
   END.       
   {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KOMB2
&Scoped-define SELF-NAME BRW_KOMB2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KOMB2 WINDOW-1
ON VALUE-CHANGED OF BRW_KOMB2 IN FRAME FRAME-A /* Kombinerbara konstruktioner */
DO:  
   antal_valda = BRW_KOMB2:NUM-SELECTED-ROWS NO-ERROR.  
   IF antal_valda = 0 THEN DO:  
      ASSIGN
      kombtemp2.F1:LABEL IN BROWSE BRW_KOMB2 = "" 
      kombtemp2.FORNR:LABEL IN BROWSE BRW_KOMB2 = ""  
      kombtemp2.LINNR:LABEL IN BROWSE BRW_KOMB2 = ""
      kombtemp2.NATNR:LABEL IN BROWSE BRW_KOMB2 = ""      
      kombtemp2.FRI1:LABEL IN BROWSE BRW_KOMB2 = ""      
      kombtemp2.FRI2:LABEL IN BROWSE BRW_KOMB2 = ""
      kombtemp2.ID:LABEL IN BROWSE BRW_KOMB2 = "Id"
      kombtemp2.XKORD:LABEL IN BROWSE BRW_KOMB2 = "Kombinera".    
   END.   
   ELSE DO:   
      FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = kombtemp2.GRUPP
      USE-INDEX KOD NO-LOCK NO-ERROR.
/*       {UPLABEL2.I} */
      ASSIGN
      kombtemp2.F1:LABEL IN BROWSE BRW_KOMB2 = SUBSTRING(bbenamntemp.B1,1,10) 
      kombtemp2.FORNR:LABEL IN BROWSE BRW_KOMB2 = SUBSTRING(annamntemp.TAB1,1,10)  
      kombtemp2.LINNR:LABEL IN BROWSE BRW_KOMB2 = SUBSTRING(annamntemp.TAB2,1,10)
      kombtemp2.NATNR:LABEL IN BROWSE BRW_KOMB2 = SUBSTRING(annamntemp.TAB3,1,10)         
      kombtemp2.ID:LABEL IN BROWSE BRW_KOMB2 = "Id"
      kombtemp2.XKORD:LABEL IN BROWSE BRW_KOMB2 = "Kombinera".
      IF Guru.Konstanter:globforetag = "SUND" THEN DO:
         ASSIGN
         kombtemp2.FRI1:LABEL IN BROWSE BRW_KOMB2 = ""
         kombtemp2.FRI2:LABEL IN BROWSE BRW_KOMB2 = "Objektnr".
      END.
      ELSE DO:
         ASSIGN
         kombtemp2.FRI1:LABEL IN BROWSE BRW_KOMB2 = SUBSTRING(bbenamntemp.ID1,1,8)
         kombtemp2.FRI2:LABEL IN BROWSE BRW_KOMB2 = SUBSTRING(bbenamntemp.ID2,1,8).
      END.
   END.       
   {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UPP
&Scoped-define SELF-NAME BRW_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UPP WINDOW-1
ON VALUE-CHANGED OF BRW_UPP IN FRAME FRAME-A /* Upplagsregister */
DO:
   ASSIGN
   status-ok = BRW_UPP:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ANDRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDRA WINDOW-1
ON CHOOSE OF BTN_ANDRA IN FRAME FRAME-A /* Ändra */
DO:
   {muswait.i} 
   FIND FIRST berupptemp WHERE berupptemp.AONR = valaonr AND 
   berupptemp.OMRADE = valomrade USE-INDEX OMR NO-LOCK NO-ERROR.
   IF AVAILABLE berupptemp THEN DO:             
      status-ok = BRW_UPP:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.     
      ASSIGN
      kon_rowid = berupptemp.BERROW
      ny = FALSE.
      {AVBGOM.I}
      RUN UPPLAGU.W.
      {AVBFRAM.I}
      IF musz = FALSE THEN DO:
         RUN openbdynspec_UI IN brwproc[3].      
         FIND berupptemp WHERE berupptemp.BERROW = kon_rowid NO-ERROR.
         IF AVAILABLE berupptemp THEN DO:
            RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(berupptemp)).
            RUN lastselectdyn_UI IN brwproc[3].
         END.
      END.
      musz = FALSE.
   END.
   ELSE DO:
      MESSAGE "Det finns inga upplag att ändra." VIEW-AS ALERT-BOX TITLE "MEDDELANDE".
   END.      
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta bort */
DO:   
   FIND FIRST berupptemp WHERE berupptemp.AONR = valaonr AND 
   berupptemp.OMRADE = valomrade USE-INDEX OMR NO-LOCK NO-ERROR.
   IF AVAILABLE berupptemp THEN DO:
      status-ok = BRW_UPP:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      ASSIGN                                   
         
      kon_rowid = berupptemp.BERROW.                           
      MESSAGE "Vill du ta bort upplag nr: " + STRING(berupptemp.UPPLAG) VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.         
      IF svar THEN DO:
         FIND FIRST kon_val WHERE kon_val.UPPLAG = berupptemp.UPPLAG NO-LOCK NO-ERROR.
         IF AVAILABLE kon_val THEN DO:
            MESSAGE "Det finns konstruktioner kopplade till upplaget. Borttag ej möjligt."
            VIEW-AS ALERT-BOX TITLE "Meddelande".
         END.
         ELSE DO:
            FIND FIRST kopp_lina WHERE kopp_lina.UPPLAG = berupptemp.UPPLAG NO-LOCK NO-ERROR.
            IF AVAILABLE kopp_lina THEN DO:
               MESSAGE "Det finns linor/kablar kopplade till upplaget. Borttag ej möjligt."
               VIEW-AS ALERT-BOX TITLE "Meddelande".
            END.
            ELSE DO:
               RUN bort_UI IN koppuppapph (INPUT kon_rowid).
               RUN selnextprevrow_UI IN brwproc[3].
               DELETE berupptemp.
               RUN setcolsortvar_UI IN brwproc[3] (INPUT "").  
               RUN refreshbrw_UI IN brwproc[3].
               RUN lastselectdyn_UI IN brwproc[3].               
            END.
         END.
      END.
   END.   
   ELSE DO:
      MESSAGE "Det finns inga upplag att ta bort." VIEW-AS ALERT-BOX TITLE "MEDDELANDE".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:
   {muswait.i} 
   ASSIGN
   musz = FALSE
   ny = TRUE.
   {AVBGOM.I}
   RUN UPPLAGU.W.
   {AVBFRAM.I} 
   IF musz = FALSE THEN DO:
      RUN refreshbrw_UI IN brwproc[3].
      FIND FIRST berupptemp WHERE berupptemp.BERROW = kon_rowid NO-LOCK NO-ERROR.
      IF AVAILABLE berupptemp THEN DO:
         RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(berupptemp)).
         RUN lastselectdyn_UI IN brwproc[3].
      END.     
   END.
   musz = FALSE.
   ny = FALSE.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_BKOMB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_BKOMB WINDOW-1
ON CHOOSE OF FBTN_BKOMB IN FRAME FRAME-A /* Ta bort komb. */
DO: 
   EMPTY TEMP-TABLE kollkomb  NO-ERROR. 
   antal_valda = BRW_KOMB2:NUM-SELECTED-ROWS NO-ERROR.  
   ASSIGN
   musz = FALSE
   antal_raknare = 1.   
   DO WHILE antal_raknare LE antal_valda:                                   
      ASSIGN
      status-ok = BRW_KOMB2:FETCH-SELECTED-ROW(antal_raknare).          
      IF kombtemp2.XKORD NE ? THEN DO:
         FIND FIRST kollkomb WHERE kollkomb.XKORD = kombtemp2.XKORD
         NO-LOCK NO-ERROR.
         IF AVAILABLE kollkomb THEN musz = musz.
         ELSE DO:         
            MESSAGE "Vill du ta bort kombination nr: " + STRING(kombtemp2.XKORD) 
            VIEW-AS ALERT-BOX 
            QUESTION BUTTONS YES-NO TITLE "Ta bort kombination?" UPDATE svar AS LOGICAL.         
            IF svar THEN DO: 
               CREATE kollkomb.
               ASSIGN
               kollkomb.XKORD = kombtemp2.XKORD.
               FOR EACH kombbuff WHERE kombbuff.XKORD = kombtemp2.XKORD:
                  FIND FIRST kon_id WHERE kon_id.NUM = kombbuff.NUM EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAILABLE kon_id THEN DO:
                     IF kon_id.ENDKOMB = TRUE THEN DELETE kon_id.
                     ELSE DO:
                        ASSIGN
                        kon_id.XKORD = ?.
                           /*
                        kon_id.YKORD = ?.
                        */
                     END.
                  END.
                  FIND FIRST kon_val WHERE kon_val.NUM = kombbuff.NUM AND
                  kon_val.KSKAP = FALSE EXCLUSIVE-LOCK NO-ERROR.
                  ASSIGN            
                  kon_val.ANDRAD = "AND".
                  kombbuff.XKORD = ?.
               END.
            END.
         END.
      END.  
      antal_raknare = antal_raknare + 1.   
   END.     
   RUN refreshbrw_UI IN brwproc[2].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_KOMB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_KOMB WINDOW-1
ON CHOOSE OF FBTN_KOMB IN FRAME FRAME-A /* Skapa komb. */
DO: 
   antal_valda = BRW_KOMB2:NUM-SELECTED-ROWS  NO-ERROR.
   ASSIGN
   musz = FALSE
   antal_raknare = 1.   
   DO WHILE antal_raknare LE antal_valda:                                   
      ASSIGN
      status-ok = BRW_KOMB2:FETCH-SELECTED-ROW(antal_raknare).          
      FIND FIRST kon_id WHERE kon_id.NUM = kombtemp2.NUM AND kon_id.ENDKOMB = FALSE
      NO-LOCK NO-ERROR.
      IF AVAILABLE kon_id THEN musz = TRUE.
      antal_raknare = antal_raknare + 1.   
   END.                      
   IF musz = TRUE THEN DO:    
      ASSIGN 
      status-ok = BRW_KOMB2:FETCH-SELECTED-ROW(1).
      FIND FIRST kon_val WHERE kon_val.NUM = kombtemp2.NUM AND
      kon_val.KSKAP = FALSE USE-INDEX NUM 
      NO-LOCK NO-ERROR.
      antal_valda = BRW_KOMB2:NUM-SELECTED-ROWS  NO-ERROR.
      ASSIGN
      sparupp = kon_val.UPPLAG
      musz = FALSE 
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                   
         ASSIGN
         status-ok = BRW_KOMB2:FETCH-SELECTED-ROW(antal_raknare).         
         FIND FIRST kon_val WHERE kon_val.NUM = kombtemp2.NUM AND
         kon_val.KSKAP = FALSE USE-INDEX NUM 
         NO-LOCK NO-ERROR.
         IF kon_val.UPPLAG NE sparupp THEN musz = TRUE.
         antal_raknare = antal_raknare + 1.   
      END.
      IF musz = FALSE THEN DO:
         FIND LAST kon_id WHERE kon_id.XKORD NE ? USE-INDEX X NO-LOCK NO-ERROR. 
         IF AVAILABLE kon_id THEN DO: 
            counter2 = kon_id.XKORD + 1. 
         END.
         ELSE DO:      
            counter2 = counter2 + 1. 
         END. 
         antal_valda = BRW_KOMB2:NUM-SELECTED-ROWS NO-ERROR.
         antal_raknare = 1.
         DO WHILE antal_raknare LE antal_valda:                                   
            ASSIGN
            status-ok = BRW_KOMB2:FETCH-SELECTED-ROW(antal_raknare)
            komb_rowid = ROWID(kombtemp2).
            FIND FIRST kombtemp2 WHERE ROWID(kombtemp2) = komb_rowid EXCLUSIVE-LOCK NO-ERROR.
            kombtemp2.XKORD = counter2.
            FIND FIRST kon_id WHERE kon_id.NUM = kombtemp2.NUM EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE kon_id THEN DO:
               ASSIGN
               kon_id.XKORD = counter2.
               /*
               kon_id.YKORD = counter2.      
               */
            END.
            ELSE DO:
               CREATE kon_id.
               ASSIGN                              
               kon_id.NUM = kombtemp2.NUM 
               kon_id.GRUPP = kombtemp2.GRUPP
               kon_id.FRI2 = ?
               kon_id.XKORD = counter2 
                  /*
               kon_id.YKORD = counter2
               */
               kon_id.ENDKOMB = TRUE. 
            END.   
            FIND FIRST kon_val WHERE kon_val.NUM = kombtemp2.NUM AND
            kon_val.KSKAP = FALSE EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN            
            kon_val.ANDRAD = "AND".
            antal_raknare = antal_raknare + 1.   
         END.
         RUN refreshbrw_UI IN brwproc[2].
      END.
      ELSE DO:
         MESSAGE "De valda konstruktionerna har olika upplag. Kombination ej möjlig."
         VIEW-AS ALERT-BOX. 
         musz = FALSE.  
      END.   
   END.
   ELSE DO:
      MESSAGE "Ingen av de valda konstruktionerna är identifierad. Kombination ej möjlig."
      VIEW-AS ALERT-BOX.
      musz = FALSE.
   END.           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_MARK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MARK WINDOW-1
ON CHOOSE OF FBTN_MARK IN FRAME FRAME-A /* Markera */
DO: 
   {muswait.i}
   IF RAD_VAL = 1 THEN DO:        
      ASSIGN
      status-ok = BRW_KOMB:DESELECT-ROWS() NO-ERROR.  
      GET FIRST BRW_KOMB NO-LOCK.   
      DO WHILE AVAILABLE(kombtemp):      
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kombtemp)).
         RUN lastselectdyn_UI IN brwproc[1].                       
         GET NEXT BRW_KOMB NO-LOCK.
      END.   
   END.
   ELSE DO:
      ASSIGN
      status-ok = BRW_KOMB2:DESELECT-ROWS() NO-ERROR.
      GET FIRST BRW_KOMB2 NO-LOCK.   
      DO WHILE AVAILABLE(kombtemp2): 
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(kombtemp2)).
         RUN lastselectdyn_UI IN brwproc[2].   
         GET NEXT BRW_KOMB2 NO-LOCK.
      END.
   END.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-1
ON CHOOSE OF FBTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO:
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:
      {muswait.i} 
      ASSIGN
      RAD_VAL = INPUT RAD_VAL  
      typval = RAD_VAL.                
      skrivut = TRUE.
      {AVBGOM.I}
      RUN VISAUPPLU.W.
      {AVBFRAM.I}
   END.     
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_UPP WINDOW-1
ON CHOOSE OF FBTN_UPP IN FRAME FRAME-A /* Koppla uppl. */
DO:
   FIND FIRST berupptemp WHERE berupptemp.AONR = valaonr AND 
   berupptemp.OMRADE = valomrade USE-INDEX OMR NO-LOCK NO-ERROR.
   IF AVAILABLE berupptemp THEN DO:  
      status-ok = BRW_UPP:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      ASSIGN                                   
          
      kon_rowid = berupptemp.BERROW
      counter = berupptemp.UPPLAG.                     
      antal_valda = BRW_KOMB:NUM-SELECTED-ROWS NO-ERROR.
      antal_raknare = 1. 
      DO WHILE antal_raknare LE antal_valda:                                   
         ASSIGN
         status-ok = BRW_KOMB:FETCH-SELECTED-ROW(antal_raknare)
         komb_rowid = ROWID(kombtemp).
         IF kombtemp.XKORD = counter THEN DO:
            musz = musz. 
         END.
         ELSE DO:   
            FIND FIRST kombtemp WHERE ROWID(kombtemp) = komb_rowid EXCLUSIVE-LOCK NO-ERROR.
            FIND FIRST kon_id WHERE kon_id.NUM = kombtemp.NUM USE-INDEX NUM NO-LOCK NO-ERROR.
            IF AVAILABLE kon_id THEN DO:
               IF kon_id.XKORD NE ? THEN DO:
                  MESSAGE "Konstruktionen " + kombtemp.F1 + " är kombinerad. Hela kombinationen kommer att kopplas till upplaget."
                  VIEW-AS ALERT-BOX TITLE "Meddelande".
                  FOR EACH idbuff WHERE idbuff.XKORD = kon_id.XKORD AND
                  ROWID(idbuff) NE ROWID(kon_id):
                     FIND FIRST kombtemp WHERE kombtemp.NUM = idbuff.NUM USE-INDEX NUM
                     NO-LOCK NO-ERROR.
                     kombtemp.XKORD = counter.
                     FIND FIRST kon_val WHERE kon_val.NUM = idbuff.NUM AND
                     kon_val.KSKAP = FALSE EXCLUSIVE-LOCK NO-ERROR.
                     ASSIGN
                     kon_val.UPPLAG = counter
                     kon_val.ANDRAD = "AND".
                  END.
                  FIND FIRST kombtemp WHERE ROWID(kombtemp) = komb_rowid 
                  EXCLUSIVE-LOCK NO-ERROR. 
                  kombtemp.XKORD = counter.
                  FIND FIRST kon_val WHERE kon_val.NUM = kombtemp.NUM AND
                  kon_val.KSKAP = FALSE EXCLUSIVE-LOCK NO-ERROR.
                  ASSIGN
                  kon_val.UPPLAG = counter
                  kon_val.ANDRAD = "AND".  
               END.
               ELSE DO:
                  kombtemp.XKORD = counter.
                  FIND FIRST kon_val WHERE kon_val.NUM = kombtemp.NUM AND
                  kon_val.KSKAP = FALSE EXCLUSIVE-LOCK NO-ERROR.
                  ASSIGN
                  kon_val.UPPLAG = counter
                  kon_val.ANDRAD = "AND".
               END.
            END.    
            ELSE DO:
               kombtemp.XKORD = counter.
               FIND FIRST kon_val WHERE kon_val.NUM = kombtemp.NUM AND
               kon_val.KSKAP = FALSE EXCLUSIVE-LOCK NO-ERROR.
               ASSIGN
               kon_val.UPPLAG = counter
               kon_val.ANDRAD = "AND".
            END.               
         END.   
         antal_raknare = antal_raknare + 1.   
      END.
      RUN refreshbrw_UI IN brwproc[1].
   END. 
   ELSE DO:
      MESSAGE "Det finns inget upplag att koppla till!" VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-1
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-A /* Visa */
DO: 
   {muswait.i}        
   ASSIGN
   RAD_VAL = INPUT RAD_VAL  
   typval = RAD_VAL.
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE.   
   {AVBGOM.I}
   RUN VISAUPPLU.W.
   {AVBFRAM.I}   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Kombinera_konstruktioner
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Kombinera_konstruktioner WINDOW-1
ON CHOOSE OF MENU-ITEM m_Kombinera_konstruktioner /* Kombinera konstruktioner */
DO:
   ASSIGN
   antal_valda = BRW_KOMB2:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} NO-ERROR.
   antal_raknare = 1.   
   DO WHILE antal_raknare LE antal_valda:                                   
      ASSIGN
      status-ok = BRW_KOMB2:FETCH-SELECTED-ROW(antal_raknare).          
      FIND FIRST kon_val WHERE kon_val.NUM = kombtemp2.NUM AND
      kon_val.KSKAP = FALSE USE-INDEX NUM NO-LOCK NO-ERROR.
      {muswait.i}   
      {AVBGOM.I}     
      RUN BERINFOU2.W (INPUT ROWID(kon_val)).    
      {AVBFRAM.I}
      {musarrow.i}
      antal_raknare = antal_raknare + 1.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Konstruktioner
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Konstruktioner WINDOW-1
ON CHOOSE OF MENU-ITEM m_Konstruktioner /* Konstruktioner */
DO:
   ASSIGN
   antal_valda = BRW_KOMB:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} NO-ERROR.
   antal_raknare = 1.   
   DO WHILE antal_raknare LE antal_valda:                                   
      ASSIGN
      status-ok = BRW_KOMB:FETCH-SELECTED-ROW(antal_raknare).          
      FIND FIRST kon_val WHERE kon_val.NUM = kombtemp.NUM AND
      kon_val.KSKAP = FALSE USE-INDEX NUM NO-LOCK NO-ERROR.
      {muswait.i}
      {AVBGOM.I}             
      RUN BERINFOU2.W (INPUT ROWID(kon_val)).     
      {AVBFRAM.I}
      {musarrow.i}
      antal_raknare = antal_raknare + 1.   
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL WINDOW-1
ON VALUE-CHANGED OF RAD_VAL IN FRAME FRAME-A
DO:
   RAD_VAL = INPUT RAD_VAL.
   IF RAD_VAL = 1 THEN DO:  
      ASSIGN
      FBTN_KOMB:HIDDEN = TRUE
      FBTN_BKOMB:HIDDEN = TRUE
      BRW_KOMB2:HIDDEN = TRUE
      BRW_KOMB:HIDDEN = FALSE
      FBTN_UPP:HIDDEN = FALSE
      BRW_UPP:HIDDEN = FALSE
      BTN_NY:HIDDEN = FALSE
      BTN_BORT:HIDDEN = FALSE
      BTN_ANDRA:HIDDEN = FALSE
      FBTN_MARK:HIDDEN = FALSE.          
      RUN openbdynspec_UI IN brwproc[1].
      RUN openbdynspec_UI IN brwproc[3].
   END.
   ELSE DO:   
      ASSIGN
      FBTN_KOMB:HIDDEN = FALSE
      FBTN_BKOMB:HIDDEN = FALSE
      BRW_KOMB2:HIDDEN = FALSE
      BRW_KOMB:HIDDEN = TRUE
      FBTN_UPP:HIDDEN = TRUE
      BRW_UPP:HIDDEN = TRUE
      BTN_NY:HIDDEN = TRUE
      BTN_BORT:HIDDEN = TRUE
      BTN_ANDRA:HIDDEN = TRUE
      FBTN_MARK:HIDDEN = TRUE.           
      RUN openbdynspec_UI IN brwproc[2].
   END.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KOMB
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(koppuppapph) THEN DELETE PROCEDURE koppuppapph.
   RUN disable_UI.
END.
   

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
   RUN ladda_UI IN koppuppapph (INPUT valaonr,INPUT valomrade,OUTPUT TABLE berupptemp).
   {BERTITLE.I}
   ASSIGN
   ny = FALSE
   counter = 0
   counter2 = 0.      
   FOR EACH kon_val WHERE kon_val.KSKAP = FALSE.
      FIND FIRST kon_id WHERE kon_id.NUM = kon_val.NUM USE-INDEX NUM NO-LOCK NO-ERROR.
      CREATE kombtemp.
      ASSIGN
      kombtemp.F1 = kon_val.F1 
      kombtemp.NUM = kon_val.NUM 
      kombtemp.ID = kon_val.ID 
      kombtemp.GRUPP = kon_val.GRUPP
      kombtemp.XKORD = kon_val.UPPLAG
      kombtemp.ORD = kon_val.ORD.
      IF AVAILABLE kon_id THEN DO:
         ASSIGN
         kombtemp.FORNR = kon_id.FORNR   
         kombtemp.NATNR = kon_id.NATNR
         kombtemp.LINNR = kon_id.LINNR
         kombtemp.FRI1 = kon_id.FRI1
         kombtemp.FRI2 = kon_id.FRI2
         kombtemp.ID2 = kon_id.FRI3.
      END.
   END.  
   EMPTY TEMP-TABLE kombtemp2  NO-ERROR. 
   FOR EACH kon_val WHERE kon_val.KSKAP = FALSE.
      FIND FIRST kon_id WHERE kon_id.NUM = kon_val.NUM USE-INDEX NUM NO-LOCK NO-ERROR.
      CREATE kombtemp2.
      ASSIGN
      kombtemp2.F1 = kon_val.F1 
      kombtemp2.NUM = kon_val.NUM 
      kombtemp2.ID = kon_val.ID
      kombtemp2.GRUPP = kon_val.GRUPP
      kombtemp2.ORD = kon_val.ORD.      
      IF AVAILABLE kon_id THEN DO:
         ASSIGN
         kombtemp2.FORNR = kon_id.FORNR   
         kombtemp2.NATNR = kon_id.NATNR
         kombtemp2.LINNR = kon_id.LINNR
         kombtemp2.FRI1 = kon_id.FRI1
         kombtemp2.FRI2 = kon_id.FRI2
         kombtemp2.ID2 = kon_id.FRI3
         kombtemp2.XKORD = kon_id.XKORD.
      END.
   END.                
   FIND FIRST annamntemp NO-LOCK NO-ERROR.   
   ASSIGN
   kombtemp.F1:LABEL IN BROWSE BRW_KOMB = "          " 
   kombtemp.FORNR:LABEL IN BROWSE BRW_KOMB = "          "  
   kombtemp.LINNR:LABEL IN BROWSE BRW_KOMB = "          "
   kombtemp.NATNR:LABEL IN BROWSE BRW_KOMB = "          "      
   kombtemp.FRI1:LABEL IN BROWSE BRW_KOMB = "        "      
   kombtemp.FRI2:LABEL IN BROWSE BRW_KOMB = "        "
   kombtemp.ID:LABEL IN BROWSE BRW_KOMB = "Id"
   kombtemp.XKORD:LABEL IN BROWSE BRW_KOMB = "Upplag".      
   ASSIGN
   kombtemp2.F1:LABEL IN BROWSE BRW_KOMB2 = "          " 
   kombtemp2.FORNR:LABEL IN BROWSE BRW_KOMB2 = "          "  
   kombtemp2.LINNR:LABEL IN BROWSE BRW_KOMB2 = "          "
   kombtemp2.NATNR:LABEL IN BROWSE BRW_KOMB2 = "          "      
   kombtemp2.FRI1:LABEL IN BROWSE BRW_KOMB2 = "        "      
   kombtemp2.FRI2:LABEL IN BROWSE BRW_KOMB2 = "        "
   kombtemp2.ID:LABEL IN BROWSE BRW_KOMB2 = "Id"
   kombtemp2.XKORD:LABEL IN BROWSE BRW_KOMB2 = "Kombinera".
   RAD_VAL = 1.
   RUN enable_UI.   
   {FRMSIZE.I}   
   ASSIGN
   FBTN_KOMB:HIDDEN = TRUE
   FBTN_BKOMB:HIDDEN = TRUE
   BRW_KOMB2:HIDDEN = TRUE. 
   RUN openbdynspec_UI IN brwproc[1].
   RUN openbdynspec_UI IN brwproc[3].
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/ 
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_KOMB:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_KOMB2:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_UPP:HANDLE IN FRAME {&FRAME-NAME}).
   RUN setcolindex_UI IN brwproc[1] (INPUT "ORD"). 
   RUN setcolindex_UI IN brwproc[2] (INPUT "ORD"). 
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
   RUN setcolsortvar_UI IN brwproc[2] (INPUT "").
   RUN setcolsortvar_UI IN brwproc[3] (INPUT " WHERE AONR = '" + valaonr + "' AND OMRADE = '" + valomrade + "' USE-INDEX OMR").
   RUN brwsetupstop_UI IN brwproc[1] (INPUT 1).
   RUN brwsetupstop_UI IN brwproc[2] (INPUT 1).
   IF Guru.Konstanter:appcon THEN DO:
      RUN KOPPUPPAPP.P PERSISTENT SET koppuppapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN KOPPUPPAPP.P PERSISTENT SET koppuppapph.
   END. 
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
  THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1  _DEFAULT-ENABLE
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
  DISPLAY RAD_VAL 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-57 FBTN_MARK FBTN_KOMB FBTN_BKOMB FBTN_UPP FBTN_VISA FBTN_SKRIV 
         BTN_AVB RAD_VAL BRW_KOMB BRW_KOMB2 BRW_UPP BTN_NY BTN_ANDRA BTN_BORT 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

