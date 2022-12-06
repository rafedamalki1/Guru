&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1


/* Temp-Table and Buffer definitions                                    */




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:  ao

  Created: 95/05/02 - 12:41 pm

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
           
{DEFSOK.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{SOKDEF.I}
{BERSTOLP.I}
{MTRLTEMP.I}
{HUVLEVTEMP.I}
{LEVTEMP.I}
DEFINE SHARED VARIABLE stolpe AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE huvudlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE mtrl_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE lev AS CHARACTER NO-UNDO.  
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE mtrlhmtapph AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE berstolpapph AS HANDLE NO-UNDO.
DEFINE VARIABLE berenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE stolpvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE satsvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE nettoh AS HANDLE NO-UNDO.
DEFINE VARIABLE entryepost AS LOGICAL NO-UNDO.
DEFINE TEMP-TABLE multiseltemp
   FIELD NUMSEL AS INTEGER
   FIELD SELROW AS ROWID
   INDEX NUM AS PRIMARY NUMSEL.
DEFINE VARIABLE brwid AS ROWID NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_HLEV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mtrltemp berstolptemp

/* Definitions for BROWSE BRW_HLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.NPRIS mtrltemp.LEVKOD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV 
&Scoped-define QUERY-STRING-BRW_HLEV FOR EACH mtrltemp NO-LOCK ~
    BY mtrltemp.Enr
&Scoped-define OPEN-QUERY-BRW_HLEV OPEN QUERY BRW_HLEV FOR EACH mtrltemp NO-LOCK ~
    BY mtrltemp.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV mtrltemp


/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL berstolptemp.ENR ~
berstolptemp.BENAMNING berstolptemp.ENHET berstolptemp.PRIS ~
berstolptemp.LEVKOD berstolptemp.ANTAL 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL berstolptemp.ANTAL 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MTRL berstolptemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MTRL berstolptemp
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH berstolptemp NO-LOCK ~
    BY berstolptemp.ENR
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH berstolptemp NO-LOCK ~
    BY berstolptemp.ENR.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL berstolptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL berstolptemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 IMAGE-9 BRW_HLEV BRW_MTRL FBTN_VISA ~
btn_over FBTN_SKRIV FBTN_EX btn_back CMB_LEV FILL-IN-ENR FILL-IN-BEN ~
RAD_SOK BTN_AVB FILL-IN-SOKALT 
&Scoped-Define DISPLAYED-OBJECTS CMB_LEV FILL-IN-ENR FILL-IN-BEN RAD_SOK ~
FILL-IN-SOKALT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON btn_back 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Ta bort markerade".

DEFINE BUTTON btn_over 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Lägg till markerade".

DEFINE BUTTON FBTN_EX 
     LABEL "Visa I EXCEL" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(15)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 30.25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKALT AS CHARACTER FORMAT "X(256)":U INITIAL "Sökalternativ:" 
      VIEW-AS TEXT 
     SIZE 14 BY .83 NO-UNDO.

DEFINE IMAGE IMAGE-9
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE VARIABLE RAD_SOK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 32.25 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50.5 BY 3.25
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_HLEV FOR 
      mtrltemp SCROLLING.

DEFINE QUERY BRW_MTRL FOR 
      berstolptemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV WINDOW-1 _STRUCTURED
  QUERY BRW_HLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(40)":U WIDTH 20
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U WIDTH 8
      mtrltemp.LEVKOD COLUMN-LABEL "Lev" FORMAT "X(4)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 48.5 BY 22.5
         TITLE "Materiellista huvudleverantör.".

DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-1 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      berstolptemp.ENR FORMAT "X(11)":U
      berstolptemp.BENAMNING FORMAT "x(40)":U WIDTH 25
      berstolptemp.ENHET FORMAT "x(5)":U
      berstolptemp.PRIS FORMAT ">>>>99.99":U
      berstolptemp.LEVKOD COLUMN-LABEL "Lev" FORMAT "x(4)":U
      berstolptemp.ANTAL COLUMN-LABEL "Ordning" FORMAT ">>>>>9":U
  ENABLE
      berstolptemp.ANTAL
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 56.25 BY 22.5
         TITLE "Upplagda materiel".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_HLEV AT ROW 1.58 COL 1.5
     BRW_MTRL AT ROW 1.58 COL 54.75
     FBTN_VISA AT ROW 8 COL 111.38
     btn_over AT ROW 8.08 COL 50.38
     FBTN_SKRIV AT ROW 9.08 COL 111.38
     FBTN_EX AT ROW 10.21 COL 111.38
     btn_back AT ROW 11.58 COL 50.38
     CMB_LEV AT ROW 24.42 COL 18.25 COLON-ALIGNED
     FILL-IN-ENR AT ROW 25.83 COL 18.25 COLON-ALIGNED
     FILL-IN-BEN AT ROW 26.83 COL 18.25 COLON-ALIGNED
     RAD_SOK AT ROW 27.71 COL 18.25 NO-LABEL
     BTN_AVB AT ROW 28.08 COL 111.38
     FILL-IN-SOKALT AT ROW 27.71 COL 1.63 COLON-ALIGNED NO-LABEL
     RECT-6 AT ROW 25.5 COL 1.5
     IMAGE-9 AT ROW 25.83 COL 2.25
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: berstolptemp T "?" NO-UNDO temp-db berstolptemp
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Administration av stolpar"
         HEIGHT             = 28.42
         WIDTH              = 125
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_HLEV IMAGE-9 FRAME-A */
/* BROWSE-TAB BRW_MTRL BRW_HLEV FRAME-A */
ASSIGN 
       BRW_HLEV:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_HLEV:MAX-DATA-GUESS IN FRAME FRAME-A         = 10000
       BRW_HLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_HLEV:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BRW_MTRL:MAX-DATA-GUESS IN FRAME FRAME-A         = 10000
       BRW_MTRL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_MTRL:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       CMB_LEV:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-BEN:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-ENR:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-SOKALT:READ-ONLY IN FRAME FRAME-A        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEV
/* Query rebuild information for BROWSE BRW_HLEV
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.mtrltemp.Enr|yes"
     _FldNameList[1]   = Temp-Tables.mtrltemp.Enr
     _FldNameList[2]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? ? "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.mtrltemp.Enhet
     _FldNameList[4]   > Temp-Tables.mtrltemp.NPRIS
"mtrltemp.NPRIS" ? ? "decimal" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.mtrltemp.LEVKOD
"mtrltemp.LEVKOD" "Lev" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.berstolptemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.berstolptemp.ENR|yes"
     _FldNameList[1]   = Temp-Tables.berstolptemp.ENR
     _FldNameList[2]   > Temp-Tables.berstolptemp.BENAMNING
"berstolptemp.BENAMNING" ? ? "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.berstolptemp.ENHET
     _FldNameList[4]   = Temp-Tables.berstolptemp.PRIS
     _FldNameList[5]   > Temp-Tables.berstolptemp.LEVKOD
"berstolptemp.LEVKOD" "Lev" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.berstolptemp.ANTAL
"berstolptemp.ANTAL" "Ordning" ">>>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_HLEV
&Scoped-define SELF-NAME BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_HLEV IN FRAME FRAME-A /* Materiellista huvudleverantör. */
DO:
    APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON START-SEARCH OF BRW_HLEV IN FRAME FRAME-A /* Materiellista huvudleverantör. */
DO:
   APPLY "END-SEARCH" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL
&Scoped-define SELF-NAME BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL WINDOW-1
ON ROW-LEAVE OF BRW_MTRL IN FRAME FRAME-A /* Upplagda materiel */
DO:
   IF entryepost = TRUE THEN DO:
      APPLY "LEAVE" TO berstolptemp.ANTAL IN BROWSE BRW_MTRL.
   END.
   /*
   status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE berstolptemp THEN DO:
      DISPLAY berstolptemp.ANTAL WITH BROWSE BRW_MTRL.
      berstolptemp.ANTAL   = INPUT BROWSE BRW_MTRL berstolptemp.ANTAL   .
      DISPLAY  berstolptemp.ANTAL WITH BROWSE BRW_MTRL.
   END.   
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL WINDOW-1
ON VALUE-CHANGED OF BRW_MTRL IN FRAME FRAME-A /* Upplagda materiel */
DO:
   status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME berstolptemp.ANTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL berstolptemp.ANTAL BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON ENTRY OF berstolptemp.ANTAL IN BROWSE BRW_MTRL /* Ordning */
DO:
   entryepost = TRUE.
   IF AVAILABLE berstolptemp THEN DISPLAY berstolptemp.ANTAL WITH BROWSE BRW_MTRL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL berstolptemp.ANTAL BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON LEAVE OF berstolptemp.ANTAL IN BROWSE BRW_MTRL /* Ordning */
DO:
   IF entryepost = TRUE THEN DO:
      IF AVAILABLE berstolptemp THEN DO:
         IF berstolptemp.ANTAL NE INPUT BROWSE BRW_MTRL berstolptemp.ANTAL THEN DO:
            berstolptemp.ANTAL = INPUT BROWSE BRW_MTRL berstolptemp.ANTAL.
            DISPLAY  berstolptemp.ANTAL WITH BROWSE BRW_MTRL.         
            EMPTY TEMP-TABLE eberstolptemp NO-ERROR. 
            CREATE eberstolptemp.
            BUFFER-COPY berstolptemp TO eberstolptemp.
            RUN skapa_UI IN berstolpapph (INPUT TABLE eberstolptemp).
            EMPTY TEMP-TABLE eberstolptemp NO-ERROR. 
          
            RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(berstolptemp)).        
          /*
            RUN openbdynspec_UI IN brwproc[2].
            RUN lastselectdyn_UI IN brwproc[2].
            */
         END.
         ELSE /*kontaktperstemp.NAMN:READ-ONLY IN BROWSE BRW_KONTAKT = TRUE.*/
      END.   
   END.
   entryepost = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL berstolptemp.ANTAL BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON MOUSE-SELECT-CLICK OF berstolptemp.ANTAL IN BROWSE BRW_MTRL /* Ordning */
DO:
   APPLY "ENTRY" TO berstolptemp.ANTAL IN BROWSE BRW_MTRL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:    
   RETURN.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_back WINDOW-1
ON CHOOSE OF btn_back IN FRAME FRAME-A
DO:   
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS.
   IF antal_valda = 0 THEN RETURN.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda: 
      status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).
      IF AVAILABLE berstolptemp THEN DO:
         berenr = berstolptemp.ENR.
         stolpvar = berstolptemp.STOLPE.
         DELETE berstolptemp.
         RUN bort_UI IN berstolpapph (INPUT berenr,INPUT stolpvar).
         FIND FIRST eberstolptemp WHERE eberstolptemp.ENR = berenr AND 
         eberstolptemp.STOLPE = stolpvar NO-LOCK NO-ERROR.
         IF AVAILABLE eberstolptemp THEN DO:
            DELETE eberstolptemp.
         END.
      END.
      IF antal_raknare = antal_valda THEN RUN selnextprevrow_UI IN brwproc[2].
      antal_raknare = antal_raknare + 1.   
   END.     
   IF stolpe = TRUE THEN DO:
      RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE berstolptemp.STOLPE = TRUE ").       
   END.
   ELSE DO:
      RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE berstolptemp.STOLPE = FALSE ").       
   END.        
   RUN openbdynspec_UI IN brwproc[2]. 
   RUN lastselectdyn_UI IN brwproc[2].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_over
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_over WINDOW-1
ON CHOOSE OF btn_over IN FRAME FRAME-A
DO:
   EMPTY TEMP-TABLE multiseltemp NO-ERROR. 
   antal_valda = BRW_HLEV:NUM-SELECTED-ROWS.         
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_HLEV:FETCH-SELECTED-ROW(antal_raknare).
      RUN over_UI.
   
   /*   antal_raknare = antal_raknare + 1.         */
   END.          
   IF status-ok THEN BRW_HLEV:DESELECT-ROWS() NO-ERROR.
   IF stolpe = TRUE THEN DO:
      RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE berstolptemp.STOLPE = TRUE ").  
      RUN openbdynspec_UI IN brwproc[2].  
   END.
   ELSE DO:
      RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE berstolptemp.STOLPE = FALSE ").  
      RUN openbdynspec_UI IN brwproc[2].  
   END. 
   FOR EACH multiseltemp BY NUMSEL DESCENDING:
      RUN setlastrowid_UI IN brwproc[2] (INPUT multiseltemp.SELROW).
      RUN lastselectdyn_UI IN brwproc[2].
   END.

   RUN setlastrowid_UI IN brwproc[2] (INPUT brwid).
   RUN lastselectdyn_UI IN brwproc[2].
   IF stolpe = FALSE THEN DO:

   END.
   EMPTY TEMP-TABLE multiseltemp NO-ERROR. 
   /* lena
   IF stolpe = TRUE THEN DO:
      IF posok NE " " THEN DO:
         APPLY "ENTRY" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
      ELSE DO:
         APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.                                              
   END.
   ELSE DO:*/
      APPLY "ENTRY" TO berstolptemp.ANTAL IN BROWSE BRW_MTRL.
   /*END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV WINDOW-1
ON VALUE-CHANGED OF CMB_LEV IN FRAME FRAME-A /* Leverantörer */
DO:                                 
   lev = INPUT CMB_LEV.
   IF lev NE huvudlev THEN DO:
      RUN setorgtitle_UI IN brwproc[1] (INPUT "Materiellista " + lev).
      FIND FIRST levtemp WHERE levtemp.LEVNAMN = lev 
      USE-INDEX LEV NO-LOCK NO-ERROR.
      IF AVAILABLE levtemp THEN vald_lev = levtemp.LEVKOD.       
      ELSE vald_lev = "Alla leverantörer".
   END.
   ELSE DO:
      RUN setorgtitle_UI IN brwproc[1] (INPUT "Materiellista huvudleverantör " + lev).         
      vald_lev = vald_kundlev.
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_EX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EX WINDOW-1
ON CHOOSE OF FBTN_EX IN FRAME FRAME-A /* Visa I EXCEL */
DO:
   {muswait.i} 

   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE.   
   /*
   MESSAGE "Vill du se listan i Excel klicka 'Ja' annars 'Nej'."
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Excel?" UPDATE svar AS LOGICAL.         
   IF svar THEN DO:
      RUN EXCSTOLPU.P.
   END.
   ELSE IF NOT svar THEN DO:
      {AVBGOM.I}
      RUN VISSTOLPU.W. 
      {AVBFRAM.I}
   END.   
   */
    RUN EXCSTOLPU.P.
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
      skrivut = TRUE.
      {AVBGOM.I}
      RUN VISSTOLPU.W.
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


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-1
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-A /* Visa */
DO:
   {muswait.i} 
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE.   
   /*
   MESSAGE "Vill du se listan i Excel klicka 'Ja' annars 'Nej'."
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Excel?" UPDATE svar AS LOGICAL.         
   IF svar THEN DO:
      RUN EXCSTOLPU.P.
   END.
   ELSE IF NOT svar THEN DO:
      {AVBGOM.I}
      RUN VISSTOLPU.W. 
      {AVBFRAM.I}
   END.   
   */
   {AVBGOM.I}
   RUN VISSTOLPU.W. 
   {AVBFRAM.I}
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON ANY-KEY OF FILL-IN-BEN IN FRAME FRAME-A /* Benämning */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN IN FRAME FRAME-A /* Benämning */
DO: 
   {muswait.i} 
   {BENHMT2.I}   
   RUN initsok_UI (INPUT 1,INPUT aosok).
   {musarrow.i}                                 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR WINDOW-1
ON ANY-KEY OF FILL-IN-ENR IN FRAME FRAME-A /* Enr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-ENR IN FRAME FRAME-A /* Enr */
DO: 
   {muswait.i}
   {ENRHMT2.I}
   RUN initsok_UI (INPUT 2,INPUT posok).
   {musarrow.i}              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_HLEV
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
   IF VALID-HANDLE(mtrlhmtapph) THEN DELETE PROCEDURE mtrlhmtapph.
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch.
   IF VALID-HANDLE(berstolpapph) THEN DELETE PROCEDURE berstolpapph.
   IF VALID-HANDLE(nettoh) THEN DELETE PROCEDURE nettoh NO-ERROR.      
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
   EMPTY TEMP-TABLE eberstolptemp NO-ERROR. 
   EMPTY TEMP-TABLE berstolptemp NO-ERROR. 
   RUN laddastolp_UI IN berstolpapph (OUTPUT TABLE berstolptemp).
   IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:        
      /*nettopris beredning adm*/                
      EMPTY TEMP-TABLE ikmtrltemp NO-ERROR. 
      RUN stpnettomark_UI IN nettoh (INPUT TABLE berstolptemp, OUTPUT TABLE ikmtrltemp APPEND).                  
   END.
   IF stolpe = TRUE THEN DO:
      {&WINDOW-NAME}:TITLE = "Administration av stolpar". 
   END.
   ELSE DO: 
      IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         {&WINDOW-NAME}:TITLE = "Administration av kablar". 
      END.
      ELSE DO:
         {&WINDOW-NAME}:TITLE = "Administration av transformatorer". 
      END.      
   END.            
   FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR.
   IF AVAILABLE huvlevtemp THEN DO:  
      vald_kundlev = huvlevtemp.LEVKOD.
   END.                              
   ELSE DO:
      FIND FIRST levtemp WHERE levtemp.LEVKOD NE "0" 
      AND levtemp.BORTTAG = FALSE NO-LOCK NO-ERROR.
      vald_kundlev = levtemp.LEVKOD.
   END. 
   FILL-IN-ENR:LABEL = Guru.Konstanter:genk.
   mtrltemp.ENR:LABEL IN BROWSE BRW_HLEV = Guru.Konstanter:genk. 
   berstolptemp.ENR:LABEL IN BROWSE BRW_MTRL = Guru.Konstanter:genk. 
   status-ok = CMB_LEV:DELETE("0"). 
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_kundlev
   USE-INDEX LEV NO-LOCK NO-ERROR. 
   ASSIGN
   status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
   CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN.
   ASSIGN
   Guru.SharedVariable:ValdmtrlLeverantor = levtemp.LEVKOD
   Guru.SharedVariable:ValdmtrlLeverantorName = levtemp.LEVNAMN. 
   FOR EACH levtemp WHERE levtemp.LEVKOD NE vald_kundlev AND 
   levtemp.LEVKOD NE "0" AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:      
      status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.              
   END.   
   status-ok = CMB_LEV:ADD-LAST("Alla leverantörer").            
   CMB_LEV = INPUT CMB_LEV.
   ASSIGN 
   satsvar = FALSE
   vald_lev = vald_kundlev.   
   RUN setorgtitle_UI IN brwproc[1] (INPUT "Materiellista huvudleverantör " + huvudlev).         
   RUN setorgtitle_UI IN brwproc[2] (INPUT "Upplagda materiel").         
   RUN enable_UI.   
   IF stolpe = TRUE THEN DO:
      RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE berstolptemp.STOLPE = TRUE ").  
      RUN openbdynspec_UI IN brwproc[2].  
      /*lena
      berstolptemp.ANTAL:VISIBLE IN BROWSE BRW_MTRL = FALSE.*/
   END.
   ELSE DO:
      RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE berstolptemp.STOLPE = FALSE ").  
      RUN openbdynspec_UI IN brwproc[2].  
   END.        
   {FRMSIZE.I} 
   {musarrow.i}
   APPLY "VALUE-CHANGED" TO BRW_MTRL.
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
      (INPUT BRW_HLEV:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN StartSokEnrLev_UI IN brwproc[1] (INPUT THIS-PROCEDURE).   
   RUN dynprogextra IN brwproc[1] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).   
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN StartSokEnrLev_UI IN brwproc[2] (INPUT THIS-PROCEDURE).   
   RUN dynprogextra IN brwproc[2] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[2] (INPUT TRUE).   
   RUN setcolindex_UI IN brwproc[2] (INPUT "STOLPE BY ANTAL").          /*vsab lagrar trafostorlek i antal*/
   /*
   RUN brwsetupstop_UI IN brwproc[1] (INPUT 1). 
   RUN brwsetupstop_UI IN brwproc[2] (INPUT 1). 
   */
   RUN settitlenum_UI IN brwproc[1] (INPUT TRUE).
   RUN settitlenum_UI IN brwproc[2] (INPUT TRUE).
   IF Guru.Konstanter:appcon THEN DO:   
      RUN BERSTOLPAPP.P PERSISTENT SET berstolpapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN BERSTOLPAPP.P PERSISTENT SET berstolpapph.
   END.  

   IF Guru.Konstanter:appcon THEN DO:   
      RUN MTRLHMT.P PERSISTENT SET mtrlhmtapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLHMT.P PERSISTENT SET mtrlhmtapph.
   END.  
   IF Guru.Konstanter:appcon THEN DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh.
   END. 
   tthandle = TEMP-TABLE levtemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "LEVERANTOR", INPUT " WHERE BORTTAG = FALSE ").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "LEVERANTOR", INPUT " WHERE BORTTAG = FALSE ").
   END.
   tthandle = TEMP-TABLE huvlevtemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HUVUDLEV", INPUT "").
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
  DISPLAY CMB_LEV FILL-IN-ENR FILL-IN-BEN RAD_SOK FILL-IN-SOKALT 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-6 IMAGE-9 BRW_HLEV BRW_MTRL FBTN_VISA btn_over FBTN_SKRIV FBTN_EX 
         btn_back CMB_LEV FILL-IN-ENR FILL-IN-BEN RAD_SOK BTN_AVB 
         FILL-IN-SOKALT 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE infoES_UI WINDOW-1 
PROCEDURE infoES_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   DEFINE INPUT  PARAMETER valenr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER brwname AS CHARACTER NO-UNDO.
   DEFINE VARIABLE valbrw AS INTEGER NO-UNDO.  
   DEFINE VARIABLE levnamnvar AS CHARACTER NO-UNDO.
   IF brwname = "BRW_HLEV" THEN valbrw = 1.  
   IF brwname = "BRW_MTRL" THEN valbrw = 2. 
   IF valbrw = 1 THEN DO:
      status-ok = BRW_HLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
      IF status-ok THEN DO:
         valenr = mtrltemp.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = mtrltemp.LEVKOD
         NO-LOCK NO-ERROR.      
      END.
   END.
   IF valbrw = 2 THEN DO:
      status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
      IF status-ok THEN DO:
         valenr = berstolptemp.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = berstolptemp.LEVKOD
         NO-LOCK NO-ERROR.      
      END.
   END.
   IF status-ok THEN DO:
      levnamnvar = levtemp.LEVNAMN.
      {LEVLANK.I}      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initsok_UI WINDOW-1 
PROCEDURE initsok_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/           
   DEFINE INPUT  PARAMETER vad AS INTEGER    NO-UNDO.
   DEFINE INPUT PARAMETER sokpa AS CHARACTER NO-UNDO.
   DEFINE VARIABLE orgfraga AS CHARACTER NO-UNDO.
   IF vad = 1 THEN DO:
      IF vald_lev = "Alla leverantörer" THEN DO:
         orgfraga = " WHERE KALKNR = " + STRING(0).
      END.
      ELSE  orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX LEV".
      tth = TEMP-TABLE mtrltemp:HANDLE.
      EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
      CREATE valsoktemp.
      ASSIGN 
      valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/
      valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
      valsoktemp.SOKCHAR[3] = "BENAMNING" /*sökfält*/
      valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
      valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
      RUN sokhmt_UI IN  brwproc[1] (INPUT TABLE valsoktemp).  
   END.
   IF vad = 2 THEN DO:
      IF vald_lev = "Alla leverantörer" THEN DO:
         orgfraga = " WHERE KALKNR = " + STRING(0).
      END.
      ELSE orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX LEV".
      tth = TEMP-TABLE mtrltemp:HANDLE.
      EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
      CREATE valsoktemp.
      ASSIGN 
      valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/
      valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
      valsoktemp.SOKCHAR[3] = "ENR" /*sökfält*/
      valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
      valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
      RUN sokhmt_UI IN  brwproc[1] (INPUT TABLE valsoktemp).  
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE over_UI WINDOW-1 
PROCEDURE over_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   IF stolpe = TRUE THEN DO:
      FIND FIRST berstolptemp WHERE berstolptemp.ENR = mtrltemp.ENR AND 
      berstolptemp.LEVKOD = mtrltemp.LEVKOD AND berstolptemp.STOLPE = TRUE USE-INDEX STOLPE
      NO-LOCK NO-ERROR. 
      IF AVAILABLE berstolptemp THEN DO:
         musz = musz.
      END.      
      ELSE DO:
         CREATE berstolptemp. 
         ASSIGN
         berstolptemp.ENR = mtrltemp.ENR
         berstolptemp.BENAMNING = mtrltemp.BENAMNING
         berstolptemp.ENHET = mtrltemp.ENHET         
         berstolptemp.PRIS = mtrltemp.NPRIS          
         berstolptemp.LEVKOD = mtrltemp.LEVKOD
         berstolptemp.STOLPE = TRUE. 
         CREATE eberstolptemp.
         BUFFER-COPY berstolptemp TO eberstolptemp.
         RUN skapa_UI IN berstolpapph (INPUT TABLE eberstolptemp).
         FIND FIRST eberstolptemp NO-LOCK NO-ERROR.
         IF AVAILABLE eberstolptemp THEN DELETE eberstolptemp.
         IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
            /*nettopris beredning */
            FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = mtrltemp.LEVKOD AND ikmtrltemp.ENR = mtrltemp.ENR AND ikmtrltemp.KALKNR = 0
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ikmtrltemp THEN DO:                        
               CREATE ikmtrltemp.
               BUFFER-COPY mtrltemp TO ikmtrltemp.                  
            END.
         END.
      END. 
   END.
   ELSE DO: 
      FIND FIRST berstolptemp WHERE berstolptemp.ENR = mtrltemp.ENR AND 
      berstolptemp.LEVKOD = mtrltemp.LEVKOD AND berstolptemp.STOLPE = FALSE USE-INDEX STOLPE
      NO-LOCK NO-ERROR. 
      IF AVAILABLE berstolptemp THEN DO:
         musz = musz.
      END.      
      ELSE DO:
         CREATE berstolptemp. 
         ASSIGN
         berstolptemp.ENR = mtrltemp.ENR
         berstolptemp.BENAMNING = mtrltemp.BENAMNING
         berstolptemp.ENHET = mtrltemp.ENHET         
         berstolptemp.PRIS = mtrltemp.NPRIS          
         berstolptemp.LEVKOD = mtrltemp.LEVKOD
         berstolptemp.STOLPE = FALSE.  
         CREATE eberstolptemp.
         BUFFER-COPY berstolptemp TO eberstolptemp.
         RUN skapa_UI IN berstolpapph (INPUT TABLE eberstolptemp).
         FIND FIRST eberstolptemp NO-LOCK NO-ERROR.
         IF AVAILABLE eberstolptemp THEN DELETE eberstolptemp.
         IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
            /*nettopris beredning */
            FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = mtrltemp.LEVKOD AND ikmtrltemp.ENR = mtrltemp.ENR AND ikmtrltemp.KALKNR = 0
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ikmtrltemp THEN DO:                        
               CREATE ikmtrltemp.
               BUFFER-COPY mtrltemp TO ikmtrltemp.                  
            END.
         END.
      END.       
   END.   
   CREATE multiseltemp.
   ASSIGN
   multiseltemp.NUMSEL = antal_raknare   /* Sparar undan markeringsordningen */
   multiseltemp.SELROW = ROWID(berstolptemp).  /* Sparar Rowid för posten */
   IF antal_raknare = 1 THEN DO:
      brwid = ROWID(berstolptemp).
   END.
   antal_raknare = antal_raknare + 1.                     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispextra_UI WINDOW-1 
PROCEDURE rowdispextra_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER TABLE FOR coltemp.
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.   
   IF brwh:NAME = "BRW_HLEV" THEN RUN rowdispmtrl_UI (INPUT 1).    
   IF brwh:NAME = "BRW_MTRL" THEN RUN rowdispmtrl_UI (INPUT 2).    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispmtrl_UI WINDOW-1 
PROCEDURE rowdispmtrl_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   
   IF vad = 1 THEN DO:
      IF AVAILABLE mtrltemp THEN DO:
          IF mtrltemp.KUND = TRUE THEN DO: 
            ASSIGN
            mtrltemp.ENR:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[28]
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[28].          
         END.         
         ELSE IF mtrltemp.KUND = ? THEN DO: 
            ASSIGN
            mtrltemp.ENR:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[38]
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[38].          
         END.         
      END.
   END.   
   IF vad = 2 THEN DO:       
      IF AVAILABLE berstolptemp THEN DO:         
         FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = berstolptemp.LEVKOD AND ikmtrltemp.ENR = berstolptemp.ENR AND ikmtrltemp.KALKNR = 0
         AND ikmtrltemp.KUND = TRUE  NO-LOCK NO-ERROR.
         IF AVAILABLE ikmtrltemp THEN DO:                     
            ASSIGN
            berstolptemp.ENR:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[28]
            berstolptemp.PRIS:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[28].                   
         END.
         ELSE DO:
            FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = berstolptemp.LEVKOD AND ikmtrltemp.ENR = berstolptemp.ENR AND ikmtrltemp.KALKNR = 0
            AND ikmtrltemp.KUND = ?  NO-LOCK NO-ERROR.
            IF AVAILABLE ikmtrltemp THEN DO:                     
               ASSIGN
               berstolptemp.ENR:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[38]
               berstolptemp.PRIS:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[38].                   
            END.
         END.
      END.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

