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

  Author: 

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
DEFINE INPUT  PARAMETER bomr AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER bbnr AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER valdatum AS DATE NO-UNDO.
{BERIN.I}
{DIRDEF.I}
/* Local Variable Definitions ---                                       */  
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW
{OMRTEMPW.I}

{SOKDEF.I}
&Scoped-define NEW 
{KONVALTEMP.I} 
&Scoped-define SHARED SHARED
{AVDTEMP.I}
{AVDELNINGTEMP.I}

{BERTEMPTAB.I}
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}
DEFINE SHARED TEMP-TABLE list_mtrl2 
   {LISTMTRLTT.I}
{KONVAL2TEMP.I}

DEFINE NEW SHARED VARIABLE ejanv AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE avbvar AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE kalkrow2 AS ROWID NO-UNDO.
DEFINE SHARED VARIABLE valaonr2 AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr2 AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valort2 AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade2 AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE katvar2 AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE kalkvar2 AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrow2 AS ROWID NO-UNDO.
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE berstopp AS LOGICAL NO-UNDO.
DEFINE VARIABLE datvar2 AS DATE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE aonrapph AS HANDLE NO-UNDO.
DEFINE VARIABLE valberapph AS HANDLE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE berapph AS HANDLE NO-UNDO.
DEFINE VARIABLE leftbrwakt AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE nyttaoapph2 AS HANDLE NO-UNDO.                      /*NYTTAOAPP.P*/
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE valdutf AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_VBER

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES eurberedningtemp

/* Definitions for BROWSE BRW_VBER                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VBER eurberedningtemp.OMRADE ~
eurberedningtemp.BERNR eurberedningtemp.BENAMNING eurberedningtemp.AONR ~
eurberedningtemp.DELNR eurberedningtemp.AKTIV eurberedningtemp.ANVANDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VBER 
&Scoped-define QUERY-STRING-BRW_VBER FOR EACH eurberedningtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VBER OPEN QUERY BRW_VBER FOR EACH eurberedningtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VBER eurberedningtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VBER eurberedningtemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-22 TOG_AKT TOG_INAKT CMB_JURP CMB_AVD ~
CMB_OMR CMB_UTF BTN_HAMT BRW_VBER FILL-IN-BERNR FILL-IN-HBERNR FILL-IN_AONR ~
FILL-IN_EAONR FILL-IN_DELNR FILL-IN-BEN BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS TOG_AKT TOG_INAKT CMB_JURP CMB_AVD CMB_OMR ~
CMB_UTF FILL-IN-BERNR FILL-IN-HBERNR FILL-IN_AONR FILL-IN_EAONR ~
FILL-IN_DELNR FILL-IN-BEN FILL-IN-AOTEXT FILL-IN-SOKTEXT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD brwval WINDOW-1 
FUNCTION brwval RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_HAMT 
     LABEL "Hämta och visa urval" 
     SIZE 22 BY 1 TOOLTIP "Dina val ovan avgör vilka aonr du får i listan nedan.".

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Avdelning" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_JURP AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Juridisp" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Område" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_UTF AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utfärdare" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY .96 NO-UNDO.

DEFINE VARIABLE FILL-IN-AOTEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Gör urval av beredning:" 
      VIEW-AS TEXT 
     SIZE 26.5 BY 1
     FGCOLOR 5 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 29.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BERNR AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Beredningsnr" 
     VIEW-AS FILL-IN 
     SIZE 11.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-HBERNR AS INTEGER FORMAT "->>>>>>>>9" INITIAL 0 
     LABEL "Hämta beredning" 
     VIEW-AS FILL-IN 
     SIZE 10.5 BY 1 TOOLTIP "Tryck RETURN här för välja enstaka beredningar.".

DEFINE VARIABLE FILL-IN-SOKTEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Sök i urval:" 
      VIEW-AS TEXT 
     SIZE 12.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(6)" INITIAL "0" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 11.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT ">99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 TOOLTIP "Tryck RETURN här för välja enstaka aonr.".

DEFINE VARIABLE FILL-IN_EAONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 TOOLTIP "Tryck RETURN här för välja enstaka aonr.".

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90.5 BY 4
     BGCOLOR 8 .

DEFINE VARIABLE TOG_AKT AS LOGICAL INITIAL yes 
     LABEL "Aktiva":L 
     VIEW-AS TOGGLE-BOX
     SIZE 10.75 BY .71 NO-UNDO.

DEFINE VARIABLE TOG_INAKT AS LOGICAL INITIAL no 
     LABEL "Inaktiva":L 
     VIEW-AS TOGGLE-BOX
     SIZE 11.63 BY .71 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_VBER FOR 
      eurberedningtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_VBER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VBER WINDOW-1 _STRUCTURED
  QUERY BRW_VBER NO-LOCK DISPLAY
      eurberedningtemp.OMRADE FORMAT "x(6)":U
      eurberedningtemp.BERNR COLUMN-LABEL "Ber.nr" FORMAT "->,>>>,>>9":U
            WIDTH 7 COLUMN-FGCOLOR 5 LABEL-FGCOLOR 5
      eurberedningtemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 43
      eurberedningtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      eurberedningtemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      eurberedningtemp.AKTIV COLUMN-LABEL "Aktiv/!Inaktiv" FORMAT "Aktiv/Inaktiv":U
      eurberedningtemp.ANVANDARE COLUMN-LABEL "Utfärdare" FORMAT "x(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 90 BY 14.42
         TITLE "Arbeta vidare med".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     TOG_AKT AT ROW 2.13 COL 36.25
     TOG_INAKT AT ROW 2.13 COL 47.88
     CMB_JURP AT ROW 3 COL 26.25
     CMB_AVD AT ROW 4.08 COL 25.25
     CMB_OMR AT ROW 5.17 COL 34.25 COLON-ALIGNED
     CMB_UTF AT ROW 6.25 COL 34.25 COLON-ALIGNED
     BTN_HAMT AT ROW 7.75 COL 36.25
     BRW_VBER AT ROW 8.83 COL 1.5
     FILL-IN-BERNR AT ROW 23.58 COL 25.75 COLON-ALIGNED
     FILL-IN-HBERNR AT ROW 23.58 COL 74 COLON-ALIGNED AUTO-RETURN 
     FILL-IN_AONR AT ROW 24.79 COL 25.63 COLON-ALIGNED
     FILL-IN_EAONR AT ROW 24.79 COL 74.38 COLON-ALIGNED AUTO-RETURN 
     FILL-IN_DELNR AT ROW 24.79 COL 84 COLON-ALIGNED NO-LABEL
     FILL-IN-BEN AT ROW 26 COL 25.63 COLON-ALIGNED
     BTN_OK AT ROW 27.54 COL 77.5
     BTN_AVB AT ROW 27.54 COL 92.5
     FILL-IN-AOTEXT AT ROW 1.08 COL 1.5 NO-LABEL
     FILL-IN-SOKTEXT AT ROW 23.63 COL 1.75 NO-LABEL
     RECT-22 AT ROW 23.46 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.13 BY 27.88.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: eurberedningtemp T "?" NO-UNDO temp-db eurberedningtemp
      TABLE: urberedningtemp T "?" NO-UNDO temp-db urberedningtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Beredning"
         HEIGHT             = 27.88
         WIDTH              = 106.25
         MAX-HEIGHT         = 30.33
         MAX-WIDTH          = 128
         VIRTUAL-HEIGHT     = 30.33
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
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_VBER BTN_HAMT FRAME-A */
ASSIGN 
       BRW_VBER:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_VBER:MAX-DATA-GUESS IN FRAME FRAME-A         = 1000
       BRW_VBER:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_VBER:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_AVD IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_JURP IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-AOTEXT IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-SOKTEXT IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VBER
/* Query rebuild information for BROWSE BRW_VBER
     _TblList          = "Temp-Tables.eurberedningtemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   = Temp-Tables.eurberedningtemp.OMRADE
     _FldNameList[2]   > Temp-Tables.eurberedningtemp.BERNR
"eurberedningtemp.BERNR" "Ber.nr" ? "integer" ? 5 ? ? 5 ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.eurberedningtemp.BENAMNING
"eurberedningtemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.eurberedningtemp.AONR
"eurberedningtemp.AONR" "Aonr" ? "character" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.eurberedningtemp.DELNR
"eurberedningtemp.DELNR" "Del!nr" ">99" "integer" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.eurberedningtemp.AKTIV
"eurberedningtemp.AKTIV" "Aktiv/!Inaktiv" "Aktiv/Inaktiv" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.eurberedningtemp.ANVANDARE
"eurberedningtemp.ANVANDARE" "Utfärdare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VBER */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_VBER
&Scoped-define SELF-NAME BRW_VBER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VBER WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_VBER IN FRAME FRAME-A /* Arbeta vidare med */
DO:
  APPLY "CHOOSE" TO BTN_OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VBER WINDOW-1
ON VALUE-CHANGED OF BRW_VBER IN FRAME FRAME-A /* Arbeta vidare med */
DO:
   status-ok = BRW_VBER:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avbryt */
DO:
    avbvar = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_HAMT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_HAMT WINDOW-1
ON CHOOSE OF BTN_HAMT IN FRAME FRAME-A /* Hämta och visa urval */
DO:
   BTN_HAMT:LOAD-IMAGE("BILDER\xbtn_hamt_over.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   RUN hamta_UI (INPUT 1).
   BTN_HAMT:LOAD-IMAGE("BILDER\xbtn_hamt.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
/*    RUN storapp_UI. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK WINDOW-1
ON CHOOSE OF BTN_OK IN FRAME FRAME-A /* Ok */
DO: 
   RUN kalk_UI.
   avbvar = FALSE.
   IF musz = TRUE THEN musz = FALSE.
   ELSE APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AVD WINDOW-1
ON VALUE-CHANGED OF CMB_AVD IN FRAME FRAME-A /* Avdelning */
DO:
     
   CMB_AVD = INPUT CMB_AVD.      
   
   {CMB_AVDN1.I}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_JURP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_JURP WINDOW-1
ON VALUE-CHANGED OF CMB_JURP IN FRAME FRAME-A /* Juridisp */
DO:
     
   CMB_JURP = INPUT CMB_JURP.      
   
   {CMB_JURP.I}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR WINDOW-1
ON VALUE-CHANGED OF CMB_OMR IN FRAME FRAME-A /* Område */
DO:
   CMB_OMR = INPUT CMB_OMR.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-HBERNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-HBERNR WINDOW-1
ON ANY-KEY OF FILL-IN-HBERNR IN FRAME FRAME-A /* Hämta beredning */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-HBERNR IN FRAME {&FRAME-NAME}.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-HBERNR WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-HBERNR IN FRAME FRAME-A /* Hämta beredning */
DO:
   RUN hamta_UI (INPUT 2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_DELNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DELNR WINDOW-1
ON ANY-KEY OF FILL-IN_DELNR IN FRAME FRAME-A
DO:
   {TRYCKS.I}
  /* MESSAGE KEYLABEL(LASTKEY). */
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_DELNR IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DELNR WINDOW-1
ON ENTRY OF FILL-IN_DELNR IN FRAME FRAME-A
DO:
   /*BTN_EXTRA:DEFAULT = TRUE. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DELNR WINDOW-1
ON LEAVE OF FILL-IN_DELNR IN FRAME FRAME-A
DO:
    /*
   BTN_EXTRA:DEFAULT = FALSE. 
   BTN_VISA:DEFAULT = TRUE.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DELNR WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_DELNR IN FRAME FRAME-A
DO:
   {muswait.i}      
   ASSIGN
   FILL-IN_EAONR = INPUT FRAME {&FRAME-NAME} FILL-IN_EAONR
   FILL-IN_DELNR = INPUT FILL-IN_DELNR.
   RUN hamta_UI (INPUT 3).
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_EAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_EAONR WINDOW-1
ON ANY-KEY OF FILL-IN_EAONR IN FRAME FRAME-A /* Aonr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_EAONR IN FRAME {&FRAME-NAME}.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_EAONR WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_EAONR IN FRAME FRAME-A /* Aonr */
DO:
   FILL-IN_EAONR = INPUT FILL-IN_EAONR.
   APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_DELNR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_AKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_AKT WINDOW-1
ON VALUE-CHANGED OF TOG_AKT IN FRAME FRAME-A /* Aktiva */
DO:
   TOG_AKT = INPUT TOG_AKT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_INAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_INAKT WINDOW-1
ON VALUE-CHANGED OF TOG_INAKT IN FRAME FRAME-A /* Inaktiva */
DO:
   TOG_INAKT = INPUT TOG_INAKT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   IF VALID-HANDLE(berapph) THEN DELETE PROCEDURE berapph NO-ERROR.
   IF VALID-HANDLE(valberapph) THEN DELETE PROCEDURE valberapph.
   IF VALID-HANDLE(aonrapph) THEN DELETE PROCEDURE aonrapph NO-ERROR. 
   IF VALID-HANDLE(nyttaoapph2) THEN DO:
      RUN borthandle_UI IN nyttaoapph2.
      DELETE PROCEDURE nyttaoapph2.
      nyttaoapph2 = ?.
   END.    
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph NO-ERROR.
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
   {JURPAVDSTART2.I}
   RUN huvud_UI.
   RUN enable_UI.
   IF juidraknare < 2 THEN CMB_JURP:HIDDEN = TRUE.
   IF avdraknare < 2 THEN CMB_AVD:HIDDEN = TRUE.
   EMPTY TEMP-TABLE eurberedningtemp NO-ERROR. 
   FOR EACH valberedningtemp:
      CREATE eurberedningtemp.
      BUFFER-COPY valberedningtemp TO eurberedningtemp.
   END.
   RUN openbdynspec_UI IN brwproc[1].
   {FRMSIZE.I}
  /*
   RUN PlaceraKnapp_UI.
   */
   {musarrow.i} 
   {WIN_M_SLUT.I}
   FIND FIRST jurperstemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE jurperstemp THEN CMB_JURP:HIDDEN = TRUE.
   /*
   IF jid NE "" THEN CMB_JURP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF Guru.Konstanter:globforetag = "lule" OR Guru.Konstanter:globforetag = "celpa" THEN CMB_JURP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   */
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
      (INPUT BRW_VBER:HANDLE IN FRAME {&FRAME-NAME}).
   IF Guru.Konstanter:appcon THEN DO:
      RUN MAONRAPP.P PERSISTENT SET aonrapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.      
      RUN MENYBERAPP.P PERSISTENT SET berapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph2 ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MAONRAPP.P PERSISTENT SET aonrapph.      
      RUN MENYBERAPP.P PERSISTENT SET berapph.
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph2.
   END.
   RUN anvglob_UI IN berapph (INPUT Guru.Konstanter:globanv).
   IF Guru.Konstanter:appcon THEN DO:
      RUN VALBERAPP.P PERSISTENT SET valberapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN VALBERAPP.P PERSISTENT SET valberapph.
   END. 
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.                  
   
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN-BEN:HANDLE IN FRAME {&FRAME-NAME}, INPUT "BENAMNING").
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN-BERNR:HANDLE IN FRAME {&FRAME-NAME}, INPUT "BERNR"). 
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN_AONR:HANDLE IN FRAME {&FRAME-NAME}, INPUT "AONR").   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anvkoll_UI WINDOW-1 
PROCEDURE anvkoll_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   ASSIGN
   felmedd = "".
   RUN anvkoll_UI IN valberapph (INPUT valaonr2,INPUT valomrade2,OUTPUT felmedd,
                                 INPUT-OUTPUT berstopp,INPUT-OUTPUT ejanv).
   IF felmedd NE "" THEN DO:
      MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE berkoll_UI WINDOW-1 
PROCEDURE berkoll_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   RUN anvkoll_UI.
   IF berstopp = FALSE THEN DO:  
      RUN berdathmt_UI IN valberapph (INPUT valaonr2,INPUT valomrade2,
                                       OUTPUT datvar2).
      valdatum = datvar2.
      EMPTY TEMP-TABLE kon_val2 NO-ERROR. 
      EMPTY TEMP-TABLE list_mtrl2 NO-ERROR. 
      
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN VALBER2U.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT valaonr2, INPUT valomrade2, INPUT datvar2, OUTPUT TABLE kon_val2,
         OUTPUT TABLE list_mtrl2).
      END.
      ELSE DO:
         RUN VALBER2U.P
         (INPUT valaonr2, INPUT valomrade2, INPUT datvar2, OUTPUT TABLE kon_val2,
         OUTPUT TABLE list_mtrl2).
      END.      
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
  DISPLAY TOG_AKT TOG_INAKT CMB_JURP CMB_AVD CMB_OMR CMB_UTF FILL-IN-BERNR 
          FILL-IN-HBERNR FILL-IN_AONR FILL-IN_EAONR FILL-IN_DELNR FILL-IN-BEN 
          FILL-IN-AOTEXT FILL-IN-SOKTEXT 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-22 TOG_AKT TOG_INAKT CMB_JURP CMB_AVD CMB_OMR CMB_UTF BTN_HAMT 
         BRW_VBER FILL-IN-BERNR FILL-IN-HBERNR FILL-IN_AONR FILL-IN_EAONR 
         FILL-IN_DELNR FILL-IN-BEN BTN_OK BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hamta_UI WINDOW-1 
PROCEDURE hamta_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER hamtval AS INTEGER NO-UNDO.
   {muswait.i} 
   IF hamtval = 1 THEN DO:
      ASSIGN
      CMB_OMR = INPUT FRAME {&FRAME-NAME} CMB_OMR
      CMB_UTF = INPUT CMB_UTF      
      CMB_AVD = INPUT FRAME {&FRAME-NAME} CMB_AVD 
      TOG_AKT = TOG_AKT
      TOG_INAKT = INPUT TOG_INAKT.
      IF jid = "" THEN CMB_JURP = INPUT FRAME {&FRAME-NAME} CMB_JURP. 
      IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "celpa" THEN CMB_JURP = INPUT FRAME {&FRAME-NAME} CMB_JURP. 
      FIND FIRST valsoktemp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE valsoktemp THEN CREATE valsoktemp.
      FIND FIRST avdtemp WHERE avdtemp.AVDELNINGNAMN = CMB_AVD NO-LOCK NO-ERROR.   
      IF AVAILABLE avdtemp THEN valsoktemp.SOKCHAR[4] = STRING(avdtemp.AVDELNINGNR).
      ELSE valsoktemp.SOKCHAR[4] = "ALLA".
      FIND FIRST jurperstemp WHERE jurperstemp.NAMN = CMB_JURP NO-LOCK NO-ERROR.        
      IF AVAILABLE jurperstemp THEN valsoktemp.SOKCHAR[3] = jurperstemp.JUDID.
      ELSE valsoktemp.SOKCHAR[3] = "ALLA".
      ASSIGN
      valsoktemp.SOKVAL = hamtval   
      valsoktemp.SOKCHAR[2] = CMB_UTF
      valsoktemp.SOKLOG[1] = TOG_AKT
      valsoktemp.SOKLOG[2] = TOG_INAKT.
      FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE omrtemp THEN DO:
         valsoktemp.SOKCHAR[1] = "ALLA".
      END.
      ELSE DO:
         valsoktemp.SOKCHAR[1] = omrtemp.OMRADE.
      END.
      
      EMPTY TEMP-TABLE eurberedningtemp NO-ERROR. 
      RUN urberhmt_UI IN berapph (INPUT TABLE valsoktemp,OUTPUT TABLE eurberedningtemp).
      RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
      RUN openbdynspec_UI IN brwproc[1].
      RUN fetchselrowid_UI IN brwproc[1].
   END.
   ELSE IF hamtval = 2 THEN DO:
      FILL-IN-HBERNR = INPUT FILL-IN-HBERNR.
      CMB_OMR = INPUT CMB_OMR.
      FIND FIRST valsoktemp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE valsoktemp THEN CREATE valsoktemp.
      ASSIGN
      valsoktemp.SOKVAL = hamtval 
      valsoktemp.SOKINT[1] = FILL-IN-HBERNR.
      valsoktemp.SOKCHAR[1] = CMB_OMR.
      IF CMB_OMR NE "Alla" THEN DO:
         FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-LOCK NO-ERROR.
         IF AVAILABLE omrtemp THEN DO:
            FIND FIRST eurberedningtemp WHERE eurberedningtemp.BERNR = FILL-IN-HBERNR AND 
            eurberedningtemp.OMRADE = omrtemp.OMRADE NO-LOCK NO-ERROR.
            IF NOT AVAILABLE eurberedningtemp THEN DO:
               RUN valberhmten_UI IN berapph (INPUT TABLE valsoktemp,OUTPUT TABLE eurberedningtemp APPEND).
            END.
            FIND FIRST eurberedningtemp WHERE eurberedningtemp.BERNR = FILL-IN-HBERNR AND 
            eurberedningtemp.OMRADE = omrtemp.OMRADE NO-LOCK NO-ERROR.
            IF NOT AVAILABLE eurberedningtemp THEN DO:
                MESSAGE "Hittar ingen beredning med beredningsnummer:" FILL-IN-HBERNR 
                  "för område:" CMB_OMR VIEW-AS ALERT-BOX TITLE "Meddelande!".
            END.

            IF AVAILABLE eurberedningtemp THEN DO:
               RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(eurberedningtemp)).
               RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
               RUN openbdynspec_UI IN brwproc[1].
               RUN lastselectdyn_UI IN brwproc[1].
            END.          
         END.
      END.      
      ELSE DO:
         /*
         FIND FIRST eurberedningtemp WHERE eurberedningtemp.BERNR = FILL-IN-HBERNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE eurberedningtemp THEN DO:
         */
         FOR EACH eurberedningtemp WHERE eurberedningtemp.BERNR = FILL-IN-HBERNR:
            DELETE eurberedningtemp.
         END.
         RUN valberhmten_UI IN berapph (INPUT TABLE valsoktemp,OUTPUT TABLE eurberedningtemp APPEND).         
         FIND FIRST eurberedningtemp WHERE eurberedningtemp.BERNR = FILL-IN-HBERNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE eurberedningtemp THEN DO:
             MESSAGE "Hittar ingen beredning med beredningsnummer:" FILL-IN-HBERNR 
             VIEW-AS ALERT-BOX TITLE "Meddelande!".
         END.
         IF AVAILABLE eurberedningtemp THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(eurberedningtemp)).
            RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
            RUN openbdynspec_UI IN brwproc[1].
            RUN setdefaultcol_UI IN brwproc[1] (INPUT 2).
            RUN colsortdynbrw_UI IN brwproc[1] (INPUT "").
            RUN lastselectdyn_UI IN brwproc[1].
         END.                
      END.
   END.   
   ELSE IF hamtval = 3 THEN DO:
      FOR EACH eurberedningtemp WHERE eurberedningtemp.AONR = FILL-IN_EAONR  AND eurberedningtemp.DELNR = FILL-IN_DELNR:
         DELETE eurberedningtemp.
      END. 
      RUN hmtberaonr_UI IN berapph (INPUT FILL-IN_EAONR,INPUT FILL-IN_DELNR, OUTPUT TABLE eurberedningtemp APPEND).
      FIND FIRST eurberedningtemp WHERE eurberedningtemp.AONR = FILL-IN_EAONR  AND eurberedningtemp.DELNR = FILL-IN_DELNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE eurberedningtemp THEN DO:
         MESSAGE "Hittar ingen beredning med beredningsnummer:" FILL-IN_EAONR FILL-IN_DELNR 
             VIEW-AS ALERT-BOX TITLE "Meddelande!".
      END.
      IF AVAILABLE eurberedningtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(eurberedningtemp)).
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
         RUN openbdynspec_UI IN brwproc[1].
         RUN setdefaultcol_UI IN brwproc[1] (INPUT 4).
         RUN colsortdynbrw_UI IN brwproc[1] (INPUT "").
         RUN lastselectdyn_UI IN brwproc[1].
      END.                      
   END.   
   {musarrow.i} 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI WINDOW-1 
PROCEDURE huvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   valdutf = 0.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "FAVO"                   
   inextradatatemp.HUVUDCH = Guru.Konstanter:globanv              
   inextradatatemp.HUVUDINT =  ?.   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO:
      valdutf = extradatatemp.SOKINT[2].      
   END.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   status-ok = CMB_UTF:ADD-FIRST("Alla") IN FRAME {&FRAME-NAME}.
   status-ok = CMB_UTF:ADD-LAST(Guru.Konstanter:globanv).
   IF valdutf = 0 THEN ASSIGN CMB_UTF:SCREEN-VALUE = "Alla".   
   ELSE ASSIGN CMB_UTF:SCREEN-VALUE = Guru.Konstanter:globanv.
   status-ok = CMB_OMR:ADD-FIRST("Alla").   
   {OMRHMT.I}
   /*FOR EACH omrtemp:
      status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).
   END.*/
   
   FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE omrtemp THEN DO:
      FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
   END.
   ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.       
   IF Guru.Konstanter:globomr = "" OR Guru.Konstanter:globallao = TRUE THEN DO:
      ASSIGN CMB_OMR:SCREEN-VALUE = "Alla".
      CMB_OMR = INPUT CMB_OMR.
      DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
   END.                  
   
   
   ASSIGN
   CMB_JURP:LABEL = Guru.Konstanter:gjuk
   CMB_AVD:LABEL= Guru.Konstanter:gavdk
   FILL-IN_EAONR:LABEL = Guru.Konstanter:gaok
   FILL-IN_AONR:LABEL = Guru.Konstanter:gaok. 
   CMB_OMR:LABEL = Guru.Konstanter:gomrk.
   &Scoped-define FORMATNAMN FILL-IN_AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN_EAONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN_DELNR   
   {DELNRFORMAT.I}
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kalk_UI WINDOW-1 
PROCEDURE kalk_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   status-ok = BRW_VBER:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF bomr = eurberedningtemp.OMRADE AND bbnr = eurberedningtemp.BERNR THEN DO:
      MESSAGE "Du kan inte välja samma beredning som du jobbar med just nu!"
      VIEW-AS ALERT-BOX.
      musz = TRUE.
   END.   
   ELSE DO: 
      RUN val_UI.     
      RUN berkoll_UI.     
          
      musz = FALSE.
   END.
   {musarrow.i}   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PlaceraKnapp_UI WINDOW-1 
PROCEDURE PlaceraKnapp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   Guru.Konstanter:LabelFlytt(FILL-IN-HBERNR:HANDLE IN FRAME FRAME-A).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val_UI WINDOW-1 
PROCEDURE val_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   ASSIGN
   valaonr2 = eurberedningtemp.BERAO
   valdelnr2 = 0
   valomrade2 = eurberedningtemp.OMRADE
   valort2 = eurberedningtemp.BENAMNING
   kalkvar2 = eurberedningtemp.KALKYL
   katvar2 = eurberedningtemp.KATAR.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION brwval WINDOW-1 
FUNCTION brwval RETURNS LOGICAL
  (  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   RUN urvalaktiv_UI IN brwproc[{&ARROWS}] (OUTPUT leftbrwakt).
   antal_valda = BRW_VBER:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}. 
   IF antal_valda = 0 THEN DO:      
      MESSAGE "Ingen beredning är vald!" VIEW-AS ALERT-BOX.
      RETURN TRUE.                
   END.
   BRW_VBER:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}. 
   RETURN FALSE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

