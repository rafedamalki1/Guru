&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */



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

DEFINE VARIABLE FILL-IN-AR AS INTEGER NO-UNDO.     
DEFINE VARIABLE brwvar AS INTEGER NO-UNDO.     
DEFINE VARIABLE brwopen AS LOGICAL EXTENT 10 NO-UNDO.
DEFINE VARIABLE brwsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE multibrwsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE multitid AS INTEGER NO-UNDO.
DEFINE VARIABLE brwwh AS WIDGET-HANDLE.
DEFINE VARIABLE brwsortvar AS INTEGER NO-UNDO.
DEFINE VARIABLE otbeordapph AS HANDLE NO-UNDO.
{OTIDBEORD.I}

DEFINE TEMP-TABLE vispers
   FIELD PERSONALKOD AS CHARACTER
   FIELD FORNAMN AS CHARACTER
   FIELD EFTERNAMN AS CHARACTER
   INDEX PKOD IS PRIMARY PERSONALKOD.   
DEFINE TEMP-TABLE valtemp
   FIELD VALDLISTA AS CHARACTER
   FIELD BAVAL AS INTEGER
   FIELD ALLTID AS LOGICAL
   FIELD STARTDATUM AS DATE
   FIELD SLUTDATUM AS DATE.

DEFINE INPUT-OUTPUT PARAMETER vallista AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR valtemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR otidbeordtemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR vispers.


/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}


DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO. 
DEFINE SHARED VARIABLE SEL_UPP AS CHARACTER NO-UNDO. 
DEFINE VARIABLE uppar AS INTEGER NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(8)" NO-UNDO. 
DEFINE VARIABLE ortssok AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_OTBRD

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES otidbeordtemp vispers

/* Definitions for BROWSE BRW_OTBRD                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_OTBRD otidbeordtemp.PERSONALKOD ~
otidbeordtemp.FORNAMN otidbeordtemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OTBRD 
&Scoped-define QUERY-STRING-BRW_OTBRD FOR EACH otidbeordtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_OTBRD OPEN QUERY BRW_OTBRD FOR EACH otidbeordtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_OTBRD otidbeordtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OTBRD otidbeordtemp


/* Definitions for BROWSE BRW_VPERS                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VPERS vispers.PERSONALKOD ~
vispers.FORNAMN vispers.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VPERS 
&Scoped-define QUERY-STRING-BRW_VPERS FOR EACH vispers NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VPERS OPEN QUERY BRW_VPERS FOR EACH vispers NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VPERS vispers
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VPERS vispers


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_OTBRD}~
    ~{&OPEN-QUERY-BRW_VPERS}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-22 RECT-49 BTN_NVE BTN_NVE-2 ~
FILL-IN-STARTDAT FILL-IN-STOPPDAT TOG_ALLTID BTN_FVE BTN_FVE-2 TOG_EJOGOD ~
FBTN_VISA BRW_OTBRD BRW_VPERS BTN_GODKAND BTN_ALLOVER BTN_OVER BTN_BACK ~
BTN_ALLBACK FILL-IN_SPERS FILL-IN_EPERS BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-TXT1 FILL-IN-TXT2 FILL-IN-STARTDAT ~
FILL-IN-STOPPDAT TOG_ALLTID TOG_EJOGOD FILL-IN_SPERS FILL-IN_EPERS ~
FILL-IN-AOTEXT FILL-IN-SOK FILL-IN-VAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ALLBACK 
     IMAGE-UP FILE "BILDER\rewind-u":U
     LABEL "Alla personer i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla valda personer tas bort"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER 
     IMAGE-UP FILE "BILDER\forwrd-u":U
     LABEL "Alla personer i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla personer väljs"
     FONT 11.

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort".

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-2 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_GODKAND 
     LABEL "Godkänn" 
     SIZE 14 BY 1.13.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-2 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-AOTEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Gör urval :" 
      VIEW-AS TEXT 
     SIZE 37.13 BY 1.04
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK AS CHARACTER FORMAT "X(256)":U INITIAL "Sök på:" 
      VIEW-AS TEXT 
     SIZE 9.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Från" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-STOPPDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Till" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-TXT1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 81 BY 1
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-TXT2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 81.13 BY 1
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-VAL AS CHARACTER FORMAT "X(256)":U INITIAL "Sök enstaka:" 
      VIEW-AS TEXT 
     SIZE 14 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_EPERS AS CHARACTER FORMAT "X(5)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .83.

DEFINE VARIABLE FILL-IN_SPERS AS CHARACTER FORMAT "X(5)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 1.21
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 1.21
     BGCOLOR 8 .

DEFINE VARIABLE TOG_ALLTID AS LOGICAL INITIAL no 
     LABEL "Visa all övertid" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.5 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_EJOGOD AS LOGICAL INITIAL yes 
     LABEL "Enbart ej ordinarie tidsedelgodkännare" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_OTBRD FOR 
      otidbeordtemp SCROLLING.

DEFINE QUERY BRW_VPERS FOR 
      vispers SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_OTBRD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OTBRD C-Win _STRUCTURED
  QUERY BRW_OTBRD NO-LOCK DISPLAY
      otidbeordtemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(7)":U
      otidbeordtemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 10
      otidbeordtemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 27.25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 49 BY 13.5
         TITLE "Övertidsbeordrare" FIT-LAST-COLUMN.

DEFINE BROWSE BRW_VPERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VPERS C-Win _STRUCTURED
  QUERY BRW_VPERS NO-LOCK DISPLAY
      vispers.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "X(7)":U
      vispers.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "X(256)":U
            WIDTH 15
      vispers.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "X(256)":U
            WIDTH 30
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 55 BY 13.5
         TITLE "Valda övertidsbeordrare".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-TXT1 AT ROW 1.5 COL 1.5 COLON-ALIGNED WIDGET-ID 4
     FILL-IN-TXT2 AT ROW 2.92 COL 1.38 COLON-ALIGNED WIDGET-ID 6
     BTN_NVE AT ROW 4.67 COL 32.63
     BTN_NVE-2 AT ROW 4.67 COL 52.63
     FILL-IN-STARTDAT AT ROW 5.08 COL 20.63 COLON-ALIGNED
     FILL-IN-STOPPDAT AT ROW 5.08 COL 40.63 COLON-ALIGNED
     TOG_ALLTID AT ROW 5.13 COL 1.5
     BTN_FVE AT ROW 5.54 COL 32.63
     BTN_FVE-2 AT ROW 5.54 COL 52.63
     TOG_EJOGOD AT ROW 6.67 COL 1.5
     FBTN_VISA AT ROW 8 COL 111.5
     BRW_OTBRD AT ROW 8.13 COL 1.5
     BRW_VPERS AT ROW 8.13 COL 55.75
     BTN_GODKAND AT ROW 9.75 COL 111.5 WIDGET-ID 2
     BTN_ALLOVER AT ROW 11.92 COL 51.5
     BTN_OVER AT ROW 14.13 COL 51.5
     BTN_BACK AT ROW 16.29 COL 51.5
     BTN_ALLBACK AT ROW 18.54 COL 51.5
     FILL-IN_SPERS AT ROW 21.83 COL 22.25 COLON-ALIGNED
     FILL-IN_EPERS AT ROW 21.83 COL 94.75 RIGHT-ALIGNED
     BTN_AVB AT ROW 21.88 COL 111.5
     FILL-IN-AOTEXT AT ROW 4.08 COL 1.5 NO-LABEL
     FILL-IN-SOK AT ROW 21.88 COL 2.25 NO-LABEL
     FILL-IN-VAL AT ROW 21.92 COL 55.13 COLON-ALIGNED NO-LABEL
     RECT-22 AT ROW 21.67 COL 1.5
     RECT-49 AT ROW 21.67 COL 55.75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.88 BY 22.63.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: otidbeordtemp T "?" NO-UNDO temp-db otidbeordtemp
      TABLE: vispers T "?" NO-UNDO temp-db vispers
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 22.67
         WIDTH              = 125
         MAX-HEIGHT         = 22.67
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 22.67
         VIRTUAL-WIDTH      = 125
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_OTBRD FBTN_VISA DEFAULT-FRAME */
/* BROWSE-TAB BRW_VPERS BRW_OTBRD DEFAULT-FRAME */
ASSIGN 
       BRW_VPERS:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 10000
       BRW_VPERS:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       BTN_FVE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_FVE-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-AOTEXT IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-SOK IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-STOPPDAT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TXT1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-TXT2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-VAL IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_EPERS IN FRAME DEFAULT-FRAME
   ALIGN-R                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OTBRD
/* Query rebuild information for BROWSE BRW_OTBRD
     _TblList          = "Temp-Tables.otidbeordtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.otidbeordtemp.PERSONALKOD
"otidbeordtemp.PERSONALKOD" "Enhet/!Sign" "x(7)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.otidbeordtemp.FORNAMN
"otidbeordtemp.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.otidbeordtemp.EFTERNAMN
"otidbeordtemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "27.25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_OTBRD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VPERS
/* Query rebuild information for BROWSE BRW_VPERS
     _TblList          = "Temp-Tables.vispers"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.vispers.PERSONALKOD
"vispers.PERSONALKOD" "Enhet/!Sign" "X(7)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.vispers.FORNAMN
"vispers.FORNAMN" "Förnamn" "X(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.vispers.EFTERNAMN
"vispers.EFTERNAMN" "Efternamn" "X(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_VPERS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_OTBRD
&Scoped-define SELF-NAME BRW_OTBRD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_OTBRD C-Win
ON VALUE-CHANGED OF BRW_OTBRD IN FRAME DEFAULT-FRAME /* Övertidsbeordrare */
DO:  
  
   /*IF AVAILABLE otidbeordtemp THEN DO:
      ASSIGN FILL-IN-OTBRD = otidbeordtemp.PERSONALKOD.
      DISPLAY FILL-IN-OTBRD WITH FRAME {&FRAME-NAME}.
   END.     */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   IF VALID-HANDLE(otbeordapph) THEN DO:
       RUN borthandle_UI IN otbeordapph.
       DELETE PROCEDURE otbeordapph NO-ERROR.
       otbeordapph = ?.
   END.
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE C-Win
ON CHOOSE OF BTN_FVE IN FRAME DEFAULT-FRAME /* - */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT - 1.      
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-2 C-Win
ON CHOOSE OF BTN_FVE-2 IN FRAME DEFAULT-FRAME /* - */
DO: 
   ASSIGN
   FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.   
   FILL-IN-STOPPDAT = FILL-IN-STOPPDAT - 1.      
   DISPLAY FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_GODKAND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_GODKAND C-Win
ON CHOOSE OF BTN_GODKAND IN FRAME DEFAULT-FRAME /* Godkänn */
DO:
   FIND FIRST vispers NO-LOCK NO-ERROR.
   RUN utvald_UI.
   IF NOT AVAILABLE vispers THEN DO:
      MESSAGE "Du måste välja någon person!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.           

   APPLY "GO" TO BTN_GODKAND IN FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_GODKAND C-Win
ON GO OF BTN_GODKAND IN FRAME DEFAULT-FRAME /* Godkänn */
DO:
  {muswait.i}
   {AVBGOM.I}
   FIND FIRST valtemp NO-ERROR.
   valtemp.VALDLISTA = "Godkänn övertid per övertidbeordrare".
   RUN BAUTFUG.W (INPUT vallista,INPUT TABLE valtemp,INPUT TABLE vispers).          
   {AVBFRAM.I}
   {musarrow.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE C-Win
ON CHOOSE OF BTN_NVE IN FRAME DEFAULT-FRAME /* + */
DO:   
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT + 1.        
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-2 C-Win
ON CHOOSE OF BTN_NVE-2 IN FRAME DEFAULT-FRAME /* + */
DO:   
   ASSIGN
   FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.   
   FILL-IN-STOPPDAT = FILL-IN-STOPPDAT + 1.        
   DISPLAY FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON CHOOSE OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO: 
   FIND FIRST vispers NO-LOCK NO-ERROR.
   RUN utvald_UI.
   IF NOT AVAILABLE vispers THEN DO:
      MESSAGE "Du måste välja någon person!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.           

   APPLY "GO" TO FBTN_VISA IN FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON GO OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO:
   {muswait.i}
   {AVBGOM.I}
   FIND FIRST valtemp NO-ERROR.
   valtemp.VALDLISTA = "Övertid per övertidbeordrare som ej är överläst till ekonomi och lönesystem.".
   RUN BAUTFU.W (INPUT vallista,INPUT TABLE valtemp,INPUT TABLE vispers).          
   {AVBFRAM.I}
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT C-Win
ON LEAVE OF FILL-IN-STARTDAT IN FRAME DEFAULT-FRAME /* Från */
DO:
  FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME DEFAULT-FRAME /* Från */
DO:
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STARTDAT.
   RUN AlmanBtn.w.
   FILL-IN-STARTDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STOPPDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STOPPDAT C-Win
ON LEAVE OF FILL-IN-STOPPDAT IN FRAME DEFAULT-FRAME /* Till */
DO:
  FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STOPPDAT C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-STOPPDAT IN FRAME DEFAULT-FRAME /* Till */
DO: 
   ASSIGN
   FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STOPPDAT.
   RUN AlmanBtn.w.
   FILL-IN-STOPPDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_EPERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_EPERS C-Win
ON ANY-KEY OF FILL-IN_EPERS IN FRAME DEFAULT-FRAME /* Enhet/Sign */
DO:
   {TRYCKS.I}
    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_EPERS IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_EPERS C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_EPERS IN FRAME DEFAULT-FRAME /* Enhet/Sign */
DO:
  {muswait.i}
   ASSIGN
   FILL-IN_EPERS = INPUT FILL-IN_EPERS.   
   FIND FIRST otidbeordtemp WHERE otidbeordtemp.PERSONALKOD = FILL-IN_EPERS    
   NO-LOCK NO-ERROR.     
   IF NOT AVAILABLE otidbeordtemp THEN DO:
      MESSAGE "Personen finns inte!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   FIND FIRST vispers WHERE vispers.PERSONALKOD = otidbeordtemp.PERSONALKOD 
   NO-LOCK NO-ERROR.   
   IF AVAILABLE vispers THEN DO:
      RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(vispers)).
      RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].  
   END.   
   ELSE DO:      
      CREATE vispers.
      ASSIGN           
      vispers.PERSONALKOD = otidbeordtemp.PERSONALKOD
      vispers.FORNAMN = otidbeordtemp.FORNAMN
      vispers.EFTERNAMN = otidbeordtemp.EFTERNAMN.            
   END. 
   RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").
   FIND FIRST vispers WHERE vispers.PERSONALKOD = otidbeordtemp.PERSONALKOD 
   NO-LOCK NO-ERROR.   
   IF AVAILABLE vispers THEN DO:      
      RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(vispers)).
      RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].      
   END.
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SPERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERS C-Win
ON ANY-KEY OF FILL-IN_SPERS IN FRAME DEFAULT-FRAME /* Enhet/Sign */
DO:
   {TRYCKS.I}
    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SPERS IN FRAME {&FRAME-NAME}.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERS C-Win
ON LEAVE OF FILL-IN_SPERS IN FRAME DEFAULT-FRAME /* Enhet/Sign */
DO:
   FILL-IN_SPERS = INPUT FILL-IN_SPERS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERS C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SPERS IN FRAME DEFAULT-FRAME /* Enhet/Sign */
DO:   
   FILL-IN_SPERS = INPUT FILL-IN_SPERS.
   aonrrec = RECID(otidbeordtemp).
   IF FILL-IN_SPERS = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SPERS IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   aosok = '*' + FILL-IN_SPERS + '*'.
   FIND otidbeordtemp WHERE RECID(otidbeordtemp) = aonrrec NO-LOCK NO-ERROR.
   FIND NEXT otidbeordtemp WHERE otidbeordtemp.PERSONALKOD MATCHES aosok 
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE otidbeordtemp THEN DO:
      FIND FIRST otidbeordtemp WHERE otidbeordtemp.PERSONALKOD MATCHES aosok 
      NO-LOCK NO-ERROR.
   END.
   IF NOT AVAILABLE otidbeordtemp THEN DO:
      MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SPERS IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.     
   END.
   IF AVAILABLE otidbeordtemp THEN DO:
      RUN setlastrowid_UI IN brwproc[{&LEFT-BROWSE}] (INPUT ROWID(otidbeordtemp)).
      RUN lastselectdyn_UI IN brwproc[{&LEFT-BROWSE}].      
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ALLTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ALLTID C-Win
ON VALUE-CHANGED OF TOG_ALLTID IN FRAME DEFAULT-FRAME /* All övertid */
DO:
   TOG_ALLTID = INPUT TOG_ALLTID.  
   FIND FIRST valtemp NO-ERROR.
   valtemp.ALLTID = TOG_ALLTID.   
   RUN goma_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_EJOGOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_EJOGOD C-Win
ON VALUE-CHANGED OF TOG_EJOGOD IN FRAME DEFAULT-FRAME /* Enbart ej ordinarie tidsedelgodkännare */
DO:
  TOG_EJOGOD = INPUT TOG_EJOGOD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   IF VALID-HANDLE(otbeordapph) THEN DO:
       RUN borthandle_UI IN otbeordapph.
       DELETE PROCEDURE otbeordapph NO-ERROR.
       otbeordapph = ?.
   END.
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
   EMPTY TEMP-TABLE vispers  NO-ERROR. 
   FIND FIRST valtemp NO-ERROR.
   IF AVAILABLE valtemp THEN DO:      
      ASSIGN
      FILL-IN-STARTDAT = valtemp.STARTDATUM
      FILL-IN-STOPPDAT = valtemp.SLUTDATUM
      
      TOG_ALLTID = valtemp.ALLTID.      
   END.   
   musz = FALSE.     
   RUN enable_UI.       
   {FRMSIZE.I}      
   RUN goma_UI.
   IF valtemp.VALDLISTA =  "U290- Övertid per Övertidbeordrare" THEN DO:
      BTN_GODKAND:HIDDEN = TRUE.
      ASSIGN
      FILL-IN-TXT1 = "Visar övertidsregistreringar som ej är överlästa till ekonomi och lönesystem eller"
      FILL-IN-TXT2 = "all övertid inom angiven period".
      DISPLAY FILL-IN-TXT1 FILL-IN-TXT2 WITH FRAME {&FRAME-NAME}.
      
   END.   
   
   ELSE IF valtemp.VALDLISTA =  "Godkänn övertid per övertidbeordrare" THEN DO:
      ASSIGN       
      BTN_GODKAND:HIDDEN = FALSE
      TOG_ALLTID = FALSE
      /*TOG_ALLTID:HIDDEN = TRUE*/
      TOG_EJOGOD = FALSE
      TOG_EJOGOD:HIDDEN = TRUE
      FILL-IN-AOTEXT:HIDDEN = TRUE.
      ASSIGN
      FILL-IN-TXT1 = "Visa-      övertidsregistreringar som ej är överlästa till ekonomi och lönesystem "
      FILL-IN-TXT2 = "Godkänn -  övertidsregistreringar som ej är färdigrapporterade.".
      
      DISPLAY FILL-IN-TXT1 FILL-IN-TXT2 WITH FRAME {&FRAME-NAME}.
   END.      
   {musarrow.i}   
   FIND FIRST valtemp NO-ERROR.
   ASSIGN C-win:TITLE = valtemp.VALDLISTA.
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
   RUN DYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}] (INPUT BRW_OTBRD:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}] (INPUT BRW_VPERS:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNARROW.P PERSISTENT SET brwproc[{&ARROWS}] (INPUT BRW_OTBRD:HANDLE, INPUT BRW_VPERS:HANDLE,
       INPUT BTN_OVER:HANDLE, INPUT BTN_ALLOVER:HANDLE,INPUT BTN_ALLBACK:HANDLE, INPUT BTN_BACK:HANDLE).
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN OVERTIDBRDAPP.P PERSISTENT SET otbeordapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN OVERTIDBRDAPP.P PERSISTENT SET otbeordapph.
   END. 
   RUN anvglob_UI IN otbeordapph (INPUT Guru.Konstanter:globanv).
   RUN othmtsek_UI IN otbeordapph (OUTPUT TABLE otidbeordtemp).
   RUN kollbolag_UI IN otbeordapph (INPUT-OUTPUT TABLE otidbeordtemp).
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
  DISPLAY FILL-IN-TXT1 FILL-IN-TXT2 FILL-IN-STARTDAT FILL-IN-STOPPDAT TOG_ALLTID 
          TOG_EJOGOD FILL-IN_SPERS FILL-IN_EPERS FILL-IN-AOTEXT FILL-IN-SOK 
          FILL-IN-VAL 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-22 RECT-49 BTN_NVE BTN_NVE-2 FILL-IN-STARTDAT FILL-IN-STOPPDAT 
         TOG_ALLTID BTN_FVE BTN_FVE-2 TOG_EJOGOD FBTN_VISA BRW_OTBRD BRW_VPERS 
         BTN_GODKAND BTN_ALLOVER BTN_OVER BTN_BACK BTN_ALLBACK FILL-IN_SPERS 
         FILL-IN_EPERS BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goma_UI C-Win 
PROCEDURE goma_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   GET FIRST BRW_OTBRD NO-LOCK.
   /*FIND FIRST  otidbeordtemp NO-LOCK NO-ERROR.*/
   IF AVAILABLE otidbeordtemp THEN DO:
      ASSIGN                      
      BTN_ALLBACK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_ALLOVER:HIDDEN = FALSE 
      BTN_OVER:HIDDEN = FALSE
      BTN_BACK:HIDDEN = FALSE
      BRW_OTBRD:HIDDEN = FALSE  
      FILL-IN-SOK:HIDDEN = FALSE      
      FILL-IN_SPERS:HIDDEN = FALSE.
   END.    
   ELSE DO:
      ASSIGN                  
      BTN_ALLBACK:HIDDEN = TRUE 
      BTN_ALLOVER:HIDDEN = TRUE 
      BTN_OVER:HIDDEN = TRUE 
      BTN_BACK:HIDDEN = TRUE   
      BRW_OTBRD:HIDDEN = TRUE  
      FILL-IN-SOK:HIDDEN = TRUE      
      FILL-IN_SPERS:HIDDEN = TRUE.
   END.
   IF TOG_ALLTID = FALSE THEN DO:
      ASSIGN         
      BTN_FVE:HIDDEN = TRUE 
      BTN_FVE-2:HIDDEN = TRUE 
      BTN_NVE:HIDDEN = TRUE 
      BTN_NVE-2:HIDDEN = TRUE         
      FILL-IN-STARTDAT:HIDDEN = TRUE 
      FILL-IN-STOPPDAT:HIDDEN = TRUE.
   END.
   ELSE DO:
      ASSIGN
      BTN_FVE:HIDDEN = FALSE 
      BTN_FVE-2:HIDDEN = FALSE 
      BTN_NVE:HIDDEN = FALSE 
      BTN_NVE-2:HIDDEN = FALSE
      FILL-IN-STARTDAT:HIDDEN = FALSE 
      FILL-IN-STOPPDAT:HIDDEN = FALSE.
   END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ladda_UI C-Win 
PROCEDURE ladda_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/        
   RUN goma_UI.                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE utvald_UI C-Win 
PROCEDURE utvald_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   FIND FIRST valtemp NO-ERROR.            
   IF TOG_EJOGOD = TRUE THEN ASSIGN valtemp.BAVAL = 2.
   ELSE ASSIGN valtemp.BAVAL = 1.
   IF TOG_ALLTID = FALSE THEN RETURN.
   FIND FIRST valtemp NO-ERROR.         
   ASSIGN            
   valtemp.STARTDATUM = FILL-IN-STARTDAT
   valtemp.SLUTDATUM  = FILL-IN-STOPPDAT.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valda_UI C-Win 
PROCEDURE valda_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   antal_valda = BRW_OTBRD:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF antal_valda = 0 THEN DO:      
      MESSAGE "Ingen person är vald!"  VIEW-AS ALERT-BOX.          
      RETURN.                
   END.
   ELSE DO:   
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda :
         status-ok = BRW_OTBRD:FETCH-SELECTED-ROW(antal_raknare).
         FIND FIRST vispers WHERE vispers.PERSONALKOD = otidbeordtemp.PERSONALKOD 
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE vispers THEN DO:
            CREATE vispers.
            ASSIGN           
            vispers.PERSONALKOD = otidbeordtemp.PERSONALKOD
            vispers.FORNAMN = otidbeordtemp.FORNAMN
            vispers.EFTERNAMN = otidbeordtemp.EFTERNAMN.            
         END.              
         antal_raknare = antal_raknare + 1.
      END.
   END.      
   status-ok = BRW_OTBRD:DESELECT-ROWS().      
   OPEN QUERY BRW_VPERS FOR EACH vispers NO-LOCK.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

