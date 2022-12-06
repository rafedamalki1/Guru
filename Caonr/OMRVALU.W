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
&SCOPED-DEFINE NEW
{OMRTEMPW.I}
{JURPERST.I}
{AVDTEMP.I}
DEFINE SHARED TEMP-TABLE omrtemp2 NO-UNDO LIKE omrtemp.
{SOKDEF.I}
DEFINE TEMP-TABLE omrtemp3 NO-UNDO LIKE omrtemp.
DEFINE INPUT-OUTPUT PARAMETER vallista AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR valsoktemp.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}

DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE SHARED VARIABLE omr AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE SEL_UPP AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE utomr AS CHARACTER NO-UNDO.
DEFINE VARIABLE aonrapph AS HANDLE NO-UNDO.
DEFINE VARIABLE nyttaoapph2 AS HANDLE NO-UNDO.                      /*NYTTAOAPP.P*/
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
&Scoped-define NEW
{VISUPPTMP.I}          
   

&Scoped-define LEFT-BROWSE 1
&Scoped-define RIGHT-BROWSE 2
&Scoped-define ARROWS 3

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_UROMR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES omrtemp3 omrtemp2

/* Definitions for BROWSE BRW_UROMR                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_UROMR omrtemp3.OMRADE omrtemp3.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UROMR 
&Scoped-define QUERY-STRING-BRW_UROMR FOR EACH omrtemp3 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UROMR OPEN QUERY BRW_UROMR FOR EACH omrtemp3 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UROMR omrtemp3
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UROMR omrtemp3


/* Definitions for BROWSE BRW_VOMR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VOMR omrtemp2.OMRADE omrtemp2.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VOMR 
&Scoped-define QUERY-STRING-BRW_VOMR FOR EACH omrtemp2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VOMR OPEN QUERY BRW_VOMR FOR EACH omrtemp2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VOMR omrtemp2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VOMR omrtemp2


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_UROMR}~
    ~{&OPEN-QUERY-BRW_VOMR}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_UPP RAD_PERIOD CMB_ARTAL BTN_NVE ~
BTN_NVE-2 FILL-IN-STARTDAT FILL-IN-STOPPDAT BTN_FVE BTN_FVE-2 RAD_TYP ~
FILL-IN-AR FBTN_VISA FBTN_EX BRW_UROMR BRW_VOMR BTN_ALLOVER BTN_OVER ~
BTN_BACK BTN_ALLBACK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_UPP RAD_PERIOD CMB_ARTAL ~
FILL-IN-STARTDAT FILL-IN-STOPPDAT RAD_TYP FILL-IN-AR FILL-IN-TEXT 

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
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla valda aonr tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER 
     IMAGE-UP FILE "BILDER\forwrd-u":U
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla aonr väljs"
     FONT 11.

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-2 
     LABEL "-" 
     SIZE 2.5 BY .75.

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

DEFINE BUTTON FBTN_EX 
     LABEL "Visa I EXCEL" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_ARTAL AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Årtal" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 12.25 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_UPP AS CHARACTER FORMAT "X(256)":U 
     LABEL "Val av lista" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AR AS INTEGER FORMAT "9999":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Från" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-STOPPDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Till" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Område:" 
      VIEW-AS TEXT 
     SIZE 20.5 BY 1.5
     FONT 17 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_PERIOD AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Visning per år", 1,
"Visning per period", 2
     SIZE 21.5 BY 2.75 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_TYP AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Intäkter", 1,
"Kostnader", 2
     SIZE 52 BY 1.29 NO-UNDO.

DEFINE VARIABLE TOG_ALLA AS LOGICAL INITIAL no 
     LABEL "Alla" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_UROMR FOR 
      omrtemp3 SCROLLING.

DEFINE QUERY BRW_VOMR FOR 
      omrtemp2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UROMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UROMR C-Win _STRUCTURED
  QUERY BRW_UROMR NO-LOCK DISPLAY
      omrtemp3.OMRADE COLUMN-LABEL "Område" FORMAT "X(6)":U
      omrtemp3.NAMN COLUMN-LABEL "Benämning" FORMAT "X(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 35 BY 14
         TITLE "Urvalsresultat".

DEFINE BROWSE BRW_VOMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VOMR C-Win _STRUCTURED
  QUERY BRW_VOMR NO-LOCK DISPLAY
      omrtemp2.OMRADE COLUMN-LABEL "Område" FORMAT "X(6)":U WIDTH 7.5
      omrtemp2.NAMN COLUMN-LABEL "Benämning" FORMAT "X(20)":U WIDTH 24
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 35 BY 14
         TITLE "Arbeta vidare med".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CMB_UPP AT ROW 1.25 COL 13 COLON-ALIGNED
     RAD_PERIOD AT ROW 3.58 COL 1.5 NO-LABEL
     CMB_ARTAL AT ROW 3.75 COL 33.13
     BTN_NVE AT ROW 4.83 COL 41.13
     BTN_NVE-2 AT ROW 4.83 COL 59.88
     FILL-IN-STARTDAT AT ROW 5.13 COL 29.5 COLON-ALIGNED
     FILL-IN-STOPPDAT AT ROW 5.13 COL 48.25 COLON-ALIGNED
     BTN_FVE AT ROW 5.67 COL 41.13
     BTN_FVE-2 AT ROW 5.67 COL 59.88
     RAD_TYP AT ROW 6.75 COL 1.5 NO-LABEL
     FILL-IN-AR AT ROW 7.96 COL 65 COLON-ALIGNED NO-LABEL
     FBTN_VISA AT ROW 8 COL 77.13
     TOG_ALLA AT ROW 8.13 COL 23
     FBTN_EX AT ROW 9.08 COL 77.13
     BRW_UROMR AT ROW 9.5 COL 1.5
     BRW_VOMR AT ROW 9.5 COL 41.38
     BTN_ALLOVER AT ROW 12.83 COL 37
     BTN_OVER AT ROW 15 COL 37
     BTN_BACK AT ROW 17.21 COL 37
     BTN_ALLBACK AT ROW 19.42 COL 37
     BTN_AVB AT ROW 22.5 COL 77.38
     FILL-IN-TEXT AT ROW 7.75 COL 1.5 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 91.13 BY 22.96.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: omrtemp T "?" NO-UNDO temp-db omrtemp
      TABLE: omrtemp2 T "?" NO-UNDO temp-db omrtemp2
      TABLE: omrtemp3 T "?" NO-UNDO temp-db omrtemp3
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Område"
         HEIGHT             = 23.17
         WIDTH              = 91.5
         MAX-HEIGHT         = 30.04
         MAX-WIDTH          = 128
         VIRTUAL-HEIGHT     = 30.04
         VIRTUAL-WIDTH      = 128
         RESIZE             = yes
         SCROLL-BARS        = yes
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
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BRW_UROMR FBTN_EX DEFAULT-FRAME */
/* BROWSE-TAB BRW_VOMR BRW_UROMR DEFAULT-FRAME */
ASSIGN 
       BRW_UROMR:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       BRW_VOMR:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       BTN_FVE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_FVE-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_ARTAL IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-AR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-STARTDAT IN FRAME DEFAULT-FRAME
   SHARED                                                               */
ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-STOPPDAT IN FRAME DEFAULT-FRAME
   SHARED                                                               */
ASSIGN 
       FILL-IN-STOPPDAT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TEXT IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR RADIO-SET RAD_PERIOD IN FRAME DEFAULT-FRAME
   SHARED                                                               */
/* SETTINGS FOR RADIO-SET RAD_TYP IN FRAME DEFAULT-FRAME
   SHARED                                                               */
ASSIGN 
       RAD_TYP:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_ALLA IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOG_ALLA:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UROMR
/* Query rebuild information for BROWSE BRW_UROMR
     _TblList          = "Temp-Tables.omrtemp3"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.omrtemp3.OMRADE
"omrtemp3.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.omrtemp3.NAMN
"omrtemp3.NAMN" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_UROMR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VOMR
/* Query rebuild information for BROWSE BRW_VOMR
     _TblList          = "Temp-Tables.omrtemp2"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.omrtemp2.OMRADE
"omrtemp2.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no "7.5" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.omrtemp2.NAMN
"omrtemp2.NAMN" "Benämning" ? "character" ? ? ? ? ? ? no ? no no "24" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_VOMR */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Område */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Område */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UROMR
&Scoped-define SELF-NAME BRW_UROMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UROMR C-Win
ON VALUE-CHANGED OF BRW_UROMR IN FRAME DEFAULT-FRAME /* Urvalsresultat */
DO:
   /*Inaktivera ctrl-a i browsern*/
   IF vallista = 10 OR vallista = 11 OR vallista = 13 OR vallista = 14 THEN DO:
      RUN ejtmultibr_UI IN brwproc[{&LEFT-BROWSE}] (INPUT ROWID(omrtemp3)).  
   END.
   ELSE RUN fetchselrowid_UI IN brwproc[{&LEFT-BROWSE}].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VOMR
&Scoped-define SELF-NAME BRW_VOMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VOMR C-Win
ON VALUE-CHANGED OF BRW_VOMR IN FRAME DEFAULT-FRAME /* Arbeta vidare med */
DO:
   /*Inaktivera ctrl-a i browsern*/
   IF vallista = 10 OR vallista = 11 OR vallista = 13 OR vallista = 14 THEN DO:
      RUN ejtmultibr_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(omrtemp2)).  
   END.
   ELSE RUN fetchselrowid_UI IN brwproc[{&RIGHT-BROWSE}].

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   musz = TRUE.
   RETURN.
END.


&Scoped-define SELF-NAME BTN_BACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BACK C-Win
ON CHOOSE OF BTN_BACK IN FRAME DEFAULT-FRAME
DO:
   RUN sel_back_UI IN brwproc[{&ARROWS}].
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


&Scoped-define SELF-NAME CMB_ARTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ARTAL C-Win
ON LEAVE OF CMB_ARTAL IN FRAME DEFAULT-FRAME /* Årtal */
DO:                            
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL .  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ARTAL C-Win
ON VALUE-CHANGED OF CMB_ARTAL IN FRAME DEFAULT-FRAME /* Årtal */
DO:                           
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_UPP C-Win
ON VALUE-CHANGED OF CMB_UPP IN FRAME DEFAULT-FRAME /* Val av lista */
DO:
   CMB_UPP = INPUT CMB_UPP.
   FIND FIRST visaupp WHERE visaupp.UT = CMB_UPP NO-ERROR.
   vallista = visaupp.UPPFOLJVAL.
   EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
   CREATE valsoktemp.
   ASSIGN 
   valsoktemp.SOKVAL = 0
   valsoktemp.SOKCHAR[1] = Guru.Konstanter:globanv.
   
   
   /*inlagt 20180117 lena för at man inte ska få persoanl fråga på andra listor än 12*/
   IF vallista = 12 THEN DO:             
      valsoktemp.SOKVAL = 1.
   END.
   ELSE IF vallista = 13 THEN DO: 
      omr = TRUE. 
   END.       
   IF valsoktemp.SOKDATE[1] = ? THEN DO:
      ASSIGN
      valsoktemp.SOKINT[2] = YEAR(TODAY)
      valsoktemp.SOKDATE[1] = DATE(01,01,YEAR(TODAY)).
      valsoktemp.SOKDATE[2] = TODAY.
   END.
   IF vallista = 10 OR vallista = 11 OR vallista = 13 OR vallista = 14 THEN DO:
      /*Sätt vänster browser till single select*/
      RUN setmultvar_UI IN brwproc[{&LEFT-BROWSE}] (INPUT FALSE).
      /*Sätt höger browser till single select*/
      RUN setmultvar_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT FALSE).  
      IF omr = FALSE THEN DO:
         FIND FIRST omrtemp3 NO-LOCK NO-ERROR.
         status-ok = BRW_UROMR:SELECT-FOCUSED-ROW() NO-ERROR.     
      END.
      ELSE DO:
         FIND FIRST omrtemp3 WHERE omrtemp3.OMRADE = utomr NO-LOCK NO-ERROR.
         status-ok = BRW_UROMR:SELECT-FOCUSED-ROW() NO-ERROR.   
         omr = omr.
      END.   
   END.
   IF RAD_PERIOD = 1 THEN DO:
      ASSIGN
      FILL-IN-STARTDAT = valsoktemp.SOKDATE[1]
      FILL-IN-STOPPDAT = valsoktemp.SOKDATE[2].
   END.
   FILL-IN-TEXT = Guru.Konstanter:gomrl + ":".
   APPLY "VALUE-CHANGED" TO RAD_PERIOD.
   FILL-IN-AR:HIDDEN = TRUE.
   RUN hide_UI.
   RUN openbdyn_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "").
   ASSIGN C-win:TITLE = visaupp.UT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_EX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EX C-Win
ON CHOOSE OF FBTN_EX IN FRAME DEFAULT-FRAME /* Visa I EXCEL */
DO: 
   visvalvar = 2.
   RUN btnok_UI.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON CHOOSE OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO: 
   visvalvar = 1.
   RUN btnok_UI.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON GO OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO:
   {muswait.i}
   musz = FALSE.
   IF vallista = 13 THEN DO:
      omr = FALSE.
   END.
   {AVBGOM.I} 
   RUN OMRLISTUTFU.W (INPUT-OUTPUT vallista,INPUT-OUTPUT TABLE valsoktemp).
   {AVBFRAM.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AR C-Win
ON LEAVE OF FILL-IN-AR IN FRAME DEFAULT-FRAME
DO:
   FILL-IN-AR = INPUT FILL-IN-AR.
  
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


&Scoped-define SELF-NAME RAD_PERIOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_PERIOD C-Win
ON VALUE-CHANGED OF RAD_PERIOD IN FRAME DEFAULT-FRAME
DO:
   RAD_PERIOD = INPUT RAD_PERIOD.
   RUN hide_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_TYP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_TYP C-Win
ON VALUE-CHANGED OF RAD_TYP IN FRAME DEFAULT-FRAME
DO:
   RAD_TYP = INPUT RAD_TYP.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ALLA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ALLA C-Win
ON VALUE-CHANGED OF TOG_ALLA IN FRAME DEFAULT-FRAME /* Alla */
DO:
   TOG_ALLA = INPUT TOG_ALLA.
   RUN hide_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UROMR
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
   IF VALID-HANDLE(aonrapph) THEN DELETE PROCEDURE aonrapph NO-ERROR.         
   IF VALID-HANDLE(nyttaoapph2) THEN DO:
      RUN borthandle_UI IN nyttaoapph2.
      DELETE PROCEDURE nyttaoapph2 NO-ERROR.
      nyttaoapph2 = ?.
   END.  
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
   FOR EACH visaupp WHERE visaupp.UPPFOLJVAL GT 9 AND visaupp.UPPFOLJVAL LT 15 USE-INDEX ORDNING:
      CMB_UPP:ADD-LAST(visaupp.UT).
   END. 
   FIND FIRST visaupp WHERE visaupp.UPPFOLJVAL = vallista NO-LOCK NO-ERROR.
   EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
   CREATE valsoktemp.
   ASSIGN 
   valsoktemp.SOKVAL = 0
   valsoktemp.SOKCHAR[1] = Guru.Konstanter:globanv.   
   IF vallista = 12 THEN DO:             
      valsoktemp.SOKVAL = 1.
   END.
   ELSE IF vallista = 13 THEN DO: 
      omr = TRUE. 
   END.
   ASSIGN 
   {&WINDOW-NAME}:TITLE = visaupp.UT
   CMB_UPP:SCREEN-VALUE = visaupp.UT. 
   FIND FIRST valsoktemp NO-ERROR.   
   IF valsoktemp.SOKDATE[1] = ? THEN DO:
      ASSIGN
      valsoktemp.SOKINT[2] = YEAR(TODAY)
      valsoktemp.SOKDATE[1] = DATE(01,01,YEAR(TODAY)).
      valsoktemp.SOKDATE[2] = TODAY.
   END.
   FIND FIRST omrtemp NO-ERROR.
   IF NOT AVAILABLE omrtemp THEN DO:
      {OMRHMT.I}.
   END.
   {JURPAVDSTART4.I}   

   IF vallista = 10 OR vallista = 11 OR vallista = 13 OR vallista = 14 THEN DO:
      /*Sätt vänster browser till single select*/
      RUN setmultvar_UI IN brwproc[{&LEFT-BROWSE}] (INPUT FALSE).
      /*Sätt höger browser till single select*/
      RUN setmultvar_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT FALSE).  
      IF omr = FALSE THEN DO:
         FIND FIRST omrtemp3 NO-LOCK NO-ERROR.
         status-ok = BRW_UROMR:SELECT-FOCUSED-ROW() NO-ERROR.     
      END.
      ELSE DO:
         FIND FIRST omrtemp3 WHERE omrtemp3.OMRADE = utomr NO-LOCK NO-ERROR.
         status-ok = BRW_UROMR:SELECT-FOCUSED-ROW() NO-ERROR.   
         omr = omr.
      END.   
   END.
   IF RAD_PERIOD = 1 THEN DO:
      ASSIGN
      FILL-IN-STARTDAT = valsoktemp.SOKDATE[1]
      FILL-IN-STOPPDAT = valsoktemp.SOKDATE[2].
   END.
   FILL-IN-TEXT = Guru.Konstanter:gomrl + ":".
   RUN enable_UI.
   {FRMSIZE.I}  
   RUN hide_UI.
   FILL-IN-AR:HIDDEN = TRUE.
   RUN openbdyn_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "").
   {musarrow.i}
   {ARTALBORT.I}
   
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
   RUN DYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}] (INPUT BRW_UROMR:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}] (INPUT BRW_VOMR:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNARROW.P PERSISTENT SET brwproc[{&ARROWS}] (INPUT BRW_UROMR:HANDLE, INPUT BRW_VOMR:HANDLE,
       INPUT BTN_OVER:HANDLE, INPUT BTN_ALLOVER:HANDLE,INPUT BTN_ALLBACK:HANDLE, INPUT BTN_BACK:HANDLE).
   IF Guru.Konstanter:appcon THEN DO:      
      RUN MAONRAPP.P PERSISTENT SET aonrapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.       
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph2 ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:      
      RUN MAONRAPP.P PERSISTENT SET aonrapph.      
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph2. 
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnok_UI C-Win 
PROCEDURE btnok_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST omrtemp2 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE omrtemp2 THEN DO:
      IF TOG_ALLA = FALSE THEN DO:
         MESSAGE "Du måste välja något " LC(Guru.Konstanter:gomrk).
         RETURN NO-APPLY.
      END.
   END.
   IF TOG_ALLA = FALSE THEN DO:
      status-ok = BRW_VOMR:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   END.
   IF vallista = 10 OR vallista = 11 OR vallista = 13 OR vallista = 14 THEN DO:
      IF TOG_ALLA = TRUE THEN utomr = "ALLA".
      ELSE utomr = omrtemp2.OMRADE.
      IF RAD_PERIOD = 1 THEN
         bdatum = DATE(01,01,FILL-IN-AR).
      ELSE DO:
         ASSIGN
         bdatum = FILL-IN-STARTDAT
         avdatum = FILL-IN-STOPPDAT.
      END.    
      skrivut = FALSE.  
   END.
   RUN utvald_UI.
   APPLY "GO" TO FBTN_VISA IN FRAME {&FRAME-NAME}. 
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
  DISPLAY CMB_UPP RAD_PERIOD CMB_ARTAL FILL-IN-STARTDAT FILL-IN-STOPPDAT RAD_TYP 
          FILL-IN-AR FILL-IN-TEXT 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE CMB_UPP RAD_PERIOD CMB_ARTAL BTN_NVE BTN_NVE-2 FILL-IN-STARTDAT 
         FILL-IN-STOPPDAT BTN_FVE BTN_FVE-2 RAD_TYP FILL-IN-AR FBTN_VISA 
         FBTN_EX BRW_UROMR BRW_VOMR BTN_ALLOVER BTN_OVER BTN_BACK BTN_ALLBACK 
         BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hide_UI C-Win 
PROCEDURE hide_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   FIND FIRST visaupp WHERE visaupp.UPPFOLJVAL = vallista NO-LOCK NO-ERROR.
   IF visaupp.UPPFOLJVAL = 13 THEN DO:
      ASSIGN RAD_TYP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE DO:
      ASSIGN RAD_TYP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   IF RAD_PERIOD = 1 THEN DO:            
      ASSIGN
      FILL-IN-STARTDAT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-STOPPDAT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_FVE:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_FVE-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_NVE:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_NVE-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE      
      CMB_ARTAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.     
   END.
   ELSE DO:                                             
      ASSIGN
      FILL-IN-STARTDAT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-STOPPDAT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_FVE:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_FVE-2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_NVE:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_NVE-2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      CMB_ARTAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. 
   END. 
   IF TOG_ALLA = TRUE THEN DO:
      ASSIGN
      BRW_UROMR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_VOMR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_ALLBACK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
      BTN_ALLOVER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
      BTN_BACK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
      BTN_OVER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   ELSE DO:
      ASSIGN
      BRW_UROMR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BRW_VOMR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_ALLBACK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_ALLOVER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_BACK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_OVER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.

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
    
   FIND FIRST valsoktemp NO-ERROR.      
   ASSIGN
   RAD_PERIOD = INPUT FRAME {&FRAME-NAME} RAD_PERIOD
   CMB_ARTAL = INPUT FRAME {&FRAME-NAME} CMB_ARTAL
   FILL-IN-STARTDAT = INPUT FRAME {&FRAME-NAME} FILL-IN-STARTDAT 
   FILL-IN-STOPPDAT = INPUT FRAME {&FRAME-NAME} FILL-IN-STOPPDAT.      
   IF valsoktemp.SOKVAL = 1 THEN DO:
      MESSAGE "Vill du visa personal ?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Koppling" UPDATE svarkopp AS LOGICAL.         
      CASE svarkopp:
         WHEN TRUE THEN DO:
            valsoktemp.SOKLOG[1] = TRUE.
         END.
         WHEN FALSE THEN DO:
            valsoktemp.SOKLOG[1] = FALSE.
         END.
      END CASE.
      ASSIGN  
      valsoktemp.SOKINT[1] = RAD_PERIOD
      valsoktemp.SOKINT[2] = CMB_ARTAL
      valsoktemp.SOKDATE[1] = FILL-IN-STARTDAT
      valsoktemp.SOKDATE[2]  = FILL-IN-STOPPDAT.  
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

