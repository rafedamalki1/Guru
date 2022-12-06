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

  Created: 02/14/97 -  3:20 pm

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
{GLOBVAR2DEL1.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{SMTRL.I}
{MTRLTEMP.I}
&Scoped-define NEW
&Scoped-define SHARED SHARED
{LEVTEMP.I}
DEFINE TEMP-TABLE byt_mtrl NO-UNDO LIKE spec_mtrl.


DEFINE NEW SHARED VARIABLE satstrue AS LOGICAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE satsinn AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE satsmtrl AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE mtrl_recid AS RECID NO-UNDO.    

DEFINE VARIABLE dxtapph AS HANDLE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO. 
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE satsvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE satsvar2 AS LOGICAL NO-UNDO.

/*MATERIEL DEFINITIONER*/

DEFINE SHARED VARIABLE sok AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE sokant AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE bestant AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE huvudlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valkalknr AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE kalkmtrl AS LOGICAL NO-UNDO.   
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.   

DEFINE VARIABLE vald_lev2 AS CHARACTER NO-UNDO.  
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE lvrant AS CHARACTER NO-UNDO.  
DEFINE VARIABLE val AS LOGICAL NO-UNDO. 
DEFINE VARIABLE svar AS LOGICAL NO-UNDO. 
DEFINE VARIABLE tempbufh AS HANDLE NO-UNDO.
DEFINE VARIABLE sokstring AS CHARACTER FORMAT "x(40)" EXTENT 10 NO-UNDO.
DEFINE VARIABLE nyaquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE orgbufh AS HANDLE NO-UNDO.
DEFINE VARIABLE appqueh AS HANDLE NO-UNDO.
DEFINE VARIABLE valdkommandoquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempbufmerah AS HANDLE NO-UNDO.
/*{EGENBEN.I}*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_HLEV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mtrltemp satstemp

/* Definitions for BROWSE BRW_HLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV 
&Scoped-define QUERY-STRING-BRW_HLEV FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = vald_kundlev ~
 AND mtrltemp.KALKNR = 0 NO-LOCK ~
    BY mtrltemp.Enr INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_HLEV OPEN QUERY BRW_HLEV FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = vald_kundlev ~
 AND mtrltemp.KALKNR = 0 NO-LOCK ~
    BY mtrltemp.Enr INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV mtrltemp


/* Definitions for BROWSE BRW_HLEV2                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV2 mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV2 
&Scoped-define QUERY-STRING-BRW_HLEV2 FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = vald_kundlev ~
 AND mtrltemp.KALKNR = 0 NO-LOCK ~
    BY mtrltemp.Enr INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_HLEV2 OPEN QUERY BRW_HLEV2 FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = vald_kundlev ~
 AND mtrltemp.KALKNR = 0 NO-LOCK ~
    BY mtrltemp.Enr INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV2 mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV2 mtrltemp


/* Definitions for BROWSE BRW_SATS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_SATS satstemp.KOD satstemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SATS 
&Scoped-define QUERY-STRING-BRW_SATS FOR EACH satstemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_SATS OPEN QUERY BRW_SATS FOR EACH satstemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_SATS satstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SATS satstemp


/* Definitions for BROWSE BRW_SATS2                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_SATS2 satstemp.KOD satstemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SATS2 
&Scoped-define QUERY-STRING-BRW_SATS2 FOR EACH satstemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_SATS2 OPEN QUERY BRW_SATS2 FOR EACH satstemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_SATS2 satstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SATS2 satstemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 RECT-4 BRW_HLEV BRW_SATS BRW_SATS2 ~
BRW_HLEV2 BTN_BYT CMB_LEV CMB_LEV2 btn_uppant BTN_MOD BTN_LIN FILL-IN-ENR ~
FILL-IN-KOD FILL-IN-ENR2 FILL-IN-KOD2 FILL-IN-BEN FILL-IN-BEN2 BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_LEV CMB_LEV2 FILL-IN-ENR FILL-IN-KOD ~
FILL-IN-ENR2 FILL-IN-KOD2 FILL-IN-BEN FILL-IN-BEN2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY DEFAULT 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BYT 
     LABEL "Byt mtrl" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_LIN 
     LABEL "Lin/Kab":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_MOD 
     LABEL "Moduler":L 
     SIZE 12 BY 1.

DEFINE BUTTON btn_uppant 
     LABEL "Antal":L 
     SIZE 12 BY 1.

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(40)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_LEV2 AS CHARACTER FORMAT "X(40)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(40)":U 
     LABEL "Ben" 
     VIEW-AS FILL-IN 
     SIZE 27 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN2 AS CHARACTER FORMAT "X(40)":U 
     LABEL "Ben" 
     VIEW-AS FILL-IN 
     SIZE 27 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR2 AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "X(11)":U 
     LABEL "Kod" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOD2 AS CHARACTER FORMAT "X(11)":U 
     LABEL "Kod" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34.5 BY 3.04
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34.5 BY 3.04
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_HLEV FOR 
      mtrltemp SCROLLING.

DEFINE QUERY BRW_HLEV2 FOR 
      mtrltemp SCROLLING.

DEFINE QUERY BRW_SATS FOR 
      satstemp SCROLLING.

DEFINE QUERY BRW_SATS2 FOR 
      satstemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV WINDOW-1 _STRUCTURED
  QUERY BRW_HLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(40)":U
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 34.5 BY 16.63
         FONT 19
         TITLE "Från".

DEFINE BROWSE BRW_HLEV2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV2 WINDOW-1 _STRUCTURED
  QUERY BRW_HLEV2 NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(40)":U
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 34.5 BY 16.63
         FONT 19
         TITLE "Till".

DEFINE BROWSE BRW_SATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SATS WINDOW-1 _STRUCTURED
  QUERY BRW_SATS NO-LOCK DISPLAY
      satstemp.KOD FORMAT "X(11)":U
      satstemp.BENAMNING FORMAT "x(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 34.5 BY 16.63
         FONT 19
         TITLE "Från".

DEFINE BROWSE BRW_SATS2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SATS2 WINDOW-1 _STRUCTURED
  QUERY BRW_SATS2 NO-LOCK DISPLAY
      satstemp.KOD FORMAT "X(11)":U
      satstemp.BENAMNING FORMAT "x(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 38.5 BY 16.63
         FONT 19
         TITLE "Till".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_HLEV AT ROW 1.75 COL 1.5
     BRW_SATS AT ROW 1.75 COL 1.5
     BRW_SATS2 AT ROW 1.75 COL 39.5
     BRW_HLEV2 AT ROW 1.75 COL 43.5
     BTN_BYT AT ROW 8 COL 80
     CMB_LEV AT ROW 18.88 COL 14.88 COLON-ALIGNED
     CMB_LEV2 AT ROW 18.88 COL 56.38 COLON-ALIGNED
     btn_uppant AT ROW 20.13 COL 40.38
     BTN_MOD AT ROW 20.13 COL 53.13
     BTN_LIN AT ROW 20.13 COL 65.88
     FILL-IN-ENR AT ROW 22.63 COL 5.5 COLON-ALIGNED
     FILL-IN-KOD AT ROW 22.63 COL 5.5 COLON-ALIGNED
     FILL-IN-ENR2 AT ROW 22.63 COL 48 COLON-ALIGNED
     FILL-IN-KOD2 AT ROW 22.63 COL 48 COLON-ALIGNED
     FILL-IN-BEN AT ROW 23.88 COL 5.5 COLON-ALIGNED
     FILL-IN-BEN2 AT ROW 23.88 COL 48.13 COLON-ALIGNED
     BTN_AVB AT ROW 24.17 COL 80
     RECT-3 AT ROW 22.13 COL 1.5
     RECT-4 AT ROW 22.13 COL 43.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 94.13 BY 24.63
         CANCEL-BUTTON BTN_AVB.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
      TABLE: satstemp T "?" NO-UNDO temp-db satstemp
      TABLE: ? T "?" NO-UNDO temp-db spec_mtrl
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Window 1"
         HEIGHT             = 24.67
         WIDTH              = 94.25
         MAX-HEIGHT         = 26.33
         MAX-WIDTH          = 109.88
         VIRTUAL-HEIGHT     = 26.33
         VIRTUAL-WIDTH      = 109.88
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_HLEV RECT-4 FRAME-A */
/* BROWSE-TAB BRW_SATS BRW_HLEV FRAME-A */
/* BROWSE-TAB BRW_SATS2 BRW_SATS FRAME-A */
/* BROWSE-TAB BRW_HLEV2 BRW_SATS2 FRAME-A */
ASSIGN 
       BRW_HLEV:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_HLEV:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_HLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_HLEV:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BRW_HLEV2:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_HLEV2:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_HLEV2:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_HLEV2:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BRW_SATS:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_SATS:MAX-DATA-GUESS IN FRAME FRAME-A         = 50000
       BRW_SATS:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_SATS:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BRW_SATS2:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_SATS2:MAX-DATA-GUESS IN FRAME FRAME-A         = 50000
       BRW_SATS2:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_SATS2:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BTN_MOD:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       CMB_LEV:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       CMB_LEV2:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-BEN:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-BEN2:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-ENR:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-ENR2:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-KOD:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-KOD2:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEV
/* Query rebuild information for BROWSE BRW_HLEV
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.mtrltemp.Enr|yes"
     _Where[1]         = "Temp-Tables.mtrltemp.LEVKOD = vald_kundlev
 AND Temp-Tables.mtrltemp.KALKNR = 0"
     _FldNameList[1]   = Temp-Tables.mtrltemp.Enr
     _FldNameList[2]   = Temp-Tables.mtrltemp.Benamning
     _FldNameList[3]   = Temp-Tables.mtrltemp.Enhet
     _FldNameList[4]   = Temp-Tables.mtrltemp.NPRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEV2
/* Query rebuild information for BROWSE BRW_HLEV2
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.mtrltemp.Enr|yes"
     _Where[1]         = "Temp-Tables.mtrltemp.LEVKOD = vald_kundlev
 AND Temp-Tables.mtrltemp.KALKNR = 0"
     _FldNameList[1]   = Temp-Tables.mtrltemp.Enr
     _FldNameList[2]   = Temp-Tables.mtrltemp.Benamning
     _FldNameList[3]   = Temp-Tables.mtrltemp.Enhet
     _FldNameList[4]   = Temp-Tables.mtrltemp.NPRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEV2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SATS
/* Query rebuild information for BROWSE BRW_SATS
     _TblList          = "Temp-Tables.satstemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.satstemp.KOD
     _FldNameList[2]   = Temp-Tables.satstemp.BENAMNING
     _Query            is NOT OPENED
*/  /* BROWSE BRW_SATS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SATS2
/* Query rebuild information for BROWSE BRW_SATS2
     _TblList          = "Temp-Tables.satstemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.satstemp.KOD
     _FldNameList[2]   = Temp-Tables.satstemp.BENAMNING
     _Query            is NOT OPENED
*/  /* BROWSE BRW_SATS2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_HLEV
&Scoped-define SELF-NAME BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON START-SEARCH OF BRW_HLEV IN FRAME FRAME-A /* Från */
DO:
   APPLY "END-SEARCH" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_HLEV2
&Scoped-define SELF-NAME BRW_HLEV2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV2 WINDOW-1
ON START-SEARCH OF BRW_HLEV2 IN FRAME FRAME-A /* Till */
DO:
   APPLY "END-SEARCH" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_SATS
&Scoped-define SELF-NAME BRW_SATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_SATS WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_SATS IN FRAME FRAME-A /* Från */
DO:
   {muswait.i}
   
   satstrue = TRUE.   
   RUN SATSINNEV.W (INPUT ROWID(satstemp)).
   satstrue = FALSE.
   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_SATS WINDOW-1
ON START-SEARCH OF BRW_SATS IN FRAME FRAME-A /* Från */
DO:
   APPLY "END-SEARCH" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_SATS2
&Scoped-define SELF-NAME BRW_SATS2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_SATS2 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_SATS2 IN FRAME FRAME-A /* Till */
DO:
   {muswait.i}
   
   satstrue = TRUE.
   
   RUN SATSINNEV.W (INPUT ROWID(satstemp)).   
   satstrue = FALSE.
   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_SATS2 WINDOW-1
ON START-SEARCH OF BRW_SATS2 IN FRAME FRAME-A /* Till */
DO:
   APPLY "END-SEARCH" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BYT WINDOW-1
ON CHOOSE OF BTN_BYT IN FRAME FRAME-A /* Byt mtrl */
DO:   
   {muswait.i}
   IF satsvar = FALSE THEN DO: 
      status-ok = BRW_HLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      RUN kollmtrl_UI.                      
   END.
   ELSE DO:
      status-ok = BRW_SATS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      RUN kollsats_UI.           
   END.
   EMPTY TEMP-TABLE spec_mtrl NO-ERROR.   
   CREATE spec_mtrl.
   ASSIGN
   spec_mtrl.ENR = INPUT FILL-IN-ENR
   spec_mtrl.LEVKOD = vald_lev.
   IF satsvar2 = FALSE THEN DO: 
      status-ok = BRW_HLEV2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      RUN kollmtrl2_UI.                      
   END.
   ELSE DO:
      status-ok = BRW_SATS2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      RUN kollsats2_UI.           
   END.   
   MESSAGE "Är det korrekt att Ni vill ersätta " + LC(Guru.Konstanter:genk) + ":" + spec_mtrl.ENR + " med " + LC(Guru.Konstanter:genk) + ":" +
   byt_mtrl.ENR + " i alla depåer" + "?" VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
   IF svar THEN DO:
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN DBYTXTYV.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT TABLE spec_mtrl, INPUT TABLE byt_mtrl).
      END.
      ELSE DO:
         RUN DBYTXTYV.P
         (INPUT TABLE spec_mtrl, INPUT TABLE byt_mtrl).
      END.
      MESSAGE "Byte av artikel klart!"
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.
   ELSE DO:
      RETURN NO-APPLY.
   END.  
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_LIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_LIN WINDOW-1
ON CHOOSE OF BTN_LIN IN FRAME FRAME-A /* Lin/Kab */
DO:
   IF satsvar2 = FALSE THEN DO: 
      status-ok = BRW_HLEV2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      RUN kollmtrl_UI.                      
   END.
   ELSE DO:
      status-ok = BRW_SATS2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      RUN kollsats_UI.           
   END.   
   IF spec_mtrl.LINKAB = FALSE THEN DO: 
      musz = FALSE.
      {AVBGOM.I}
      RUN TRUELINA.W (INPUT RECID(spec_mtrl)).                                
      {AVBFRAM.I}
   END.
   ELSE DO:
      ASSIGN
      spec_mtrl.LINKAB = FALSE
      spec_mtrl.DIAMETER = 0.  
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MOD WINDOW-1
ON CHOOSE OF BTN_MOD IN FRAME FRAME-A /* Moduler */
DO:
   IF satsvar2 = FALSE THEN DO: 
      status-ok = BRW_HLEV2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      RUN kollmtrl_UI.                      
   END.
   ELSE DO:
      status-ok = BRW_SATS2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      RUN kollsats_UI.           
   END.  
   {AVBGOM.I}
   RUN MODANTAL.W (INPUT RECID(spec_mtrl)).
   {AVBFRAM.I}
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_uppant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_uppant WINDOW-1
ON CHOOSE OF btn_uppant IN FRAME FRAME-A /* Antal */
DO:          
   IF satsvar2 = FALSE THEN DO: 
      status-ok = BRW_HLEV2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      RUN kollmtrl_UI.                      
   END.
   ELSE DO:
      status-ok = BRW_SATS2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      RUN kollsats_UI.           
   END.                              
   ASSIGN  
   musz = FALSE
   sokant = FALSE
   bestant = FALSE.       
   {AVBGOM.I}
   RUN BERANTAL.W (INPUT RECID(spec_mtrl)).
   {AVBFRAM.I}
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV WINDOW-1
ON VALUE-CHANGED OF CMB_LEV IN FRAME FRAME-A /* Leverantörer */
DO:                                 
   ASSIGN           
   lvrant = INPUT CMB_LEV.   
   IF SUBSTRING(lvrant,1,5) = "Sats " THEN DO:
      ASSIGN                      
      BRW_HLEV:HIDDEN = TRUE               
      FILL-IN-ENR:HIDDEN = TRUE      
      BRW_SATS:HIDDEN = FALSE         
      FILL-IN-KOD:HIDDEN = FALSE 
      satsvar = TRUE
      val = TRUE.

      FIND FIRST levtemp WHERE levtemp.LEVNAMN = SUBSTRING(lvrant,6,20) 
      USE-INDEX LEV NO-LOCK NO-ERROR.
      vald_lev = levtemp.LEVKOD.   

      FIND FIRST satstemp WHERE satstemp.LEVKOD = vald_lev NO-ERROR.
      IF NOT AVAILABLE satstemp THEN DO:
         RUN satslehmt_UI IN dxtapph (INPUT vald_lev ,OUTPUT TABLE satstemp).             
      END.            
      RUN setcolsortvar_UI IN brwproc[3] (INPUT " WHERE satstemp.LEVKOD = '" +   STRING(vald_lev) + "'  AND satstemp.SATS = TRUE").
      RUN openbdynspec_UI IN brwproc[3].

      /*FIND FIRST levtemp WHERE levtemp.LEVNAMN = SUBSTRING(lvrant,6,20) 
      USE-INDEX LEV NO-LOCK NO-ERROR.
      vald_lev = levtemp.LEVKOD.
      OPEN QUERY BRW_SATS FOR EACH satstemp WHERE satstemp.LEVKOD = vald_lev AND
      satstemp.SATS = TRUE NO-LOCK
      USE-INDEX KOD INDEXED-REPOSITION.         */
   END.
   ELSE DO:
      ASSIGN                      
      BRW_HLEV:HIDDEN = FALSE           
      BRW_SATS:HIDDEN = TRUE
      FILL-IN-KOD:HIDDEN = TRUE           
      val = TRUE
      satsvar = FALSE.

      FIND FIRST levtemp WHERE levtemp.LEVNAMN = lvrant 
      USE-INDEX LEV NO-LOCK NO-ERROR.
      vald_lev = levtemp.LEVKOD.   

      FIND FIRST mtrltemp WHERE mtrltemp.LEVKOD = vald_lev NO-ERROR.
      IF NOT AVAILABLE mtrltemp THEN DO:
         RUN mtrllehmt_UI IN dxtapph (INPUT vald_lev ,OUTPUT TABLE mtrltemp).             
      END.            
      RUN setcolsortvar_UI IN brwproc[1] (INPUT " WHERE mtrltemp.KALKNR = 0 AND mtrltemp.LEVKOD = '" +   STRING(vald_lev) + "'").
      RUN openbdynspec_UI IN brwproc[1].

      /*FIND FIRST levtemp WHERE levtemp.LEVNAMN = lvrant 
      USE-INDEX LEV NO-LOCK NO-ERROR.
      vald_lev = levtemp.LEVKOD.        
      OPEN QUERY BRW_HLEV FOR EACH mtrltemp WHERE mtrltemp.LEVKOD = vald_lev AND
      mtrltemp.KALKNR = 0 NO-LOCK
      USE-INDEX LEV INDEXED-REPOSITION.*/
   END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV2 WINDOW-1
ON VALUE-CHANGED OF CMB_LEV2 IN FRAME FRAME-A /* Leverantörer */
DO:                                 
   ASSIGN           
   lvrant = INPUT CMB_LEV2.   
   IF SUBSTRING(lvrant,1,5) = "Sats " THEN DO:
      ASSIGN                      
      BRW_HLEV2:HIDDEN = TRUE               
      FILL-IN-ENR2:HIDDEN = TRUE      
      BRW_SATS2:HIDDEN = FALSE         
      FILL-IN-KOD2:HIDDEN = FALSE 
      satsvar2 = TRUE
      val = TRUE.

      FIND FIRST levtemp WHERE levtemp.LEVNAMN = SUBSTRING(lvrant,6,20) 
      USE-INDEX LEV NO-LOCK NO-ERROR.
      vald_lev = levtemp.LEVKOD.   

      FIND FIRST satstemp WHERE satstemp.LEVKOD = vald_lev NO-ERROR.
      IF NOT AVAILABLE satstemp THEN DO:
         RUN satslehmt_UI IN dxtapph (INPUT vald_lev ,OUTPUT TABLE satstemp).             
      END.            
      RUN setcolsortvar_UI IN brwproc[4] (INPUT " WHERE satstemp.LEVKOD = '" +   STRING(vald_lev) + "'  AND satstemp.SATS = TRUE").
      RUN openbdynspec_UI IN brwproc[4].


      /*FIND FIRST levtemp WHERE levtemp.LEVNAMN = SUBSTRING(lvrant,6,20) 
      USE-INDEX LEV NO-LOCK NO-ERROR.
      vald_lev2 = levtemp.LEVKOD.
      OPEN QUERY BRW_SATS2 FOR EACH satstemp WHERE satstemp.LEVKOD = vald_lev2 AND
      satstemp.SATS = TRUE NO-LOCK
      USE-INDEX KOD INDEXED-REPOSITION.         */
   END.
   ELSE DO:
      ASSIGN                      
      BRW_HLEV2:HIDDEN = FALSE           
      BRW_SATS2:HIDDEN = TRUE
      FILL-IN-KOD2:HIDDEN = TRUE           
      val = TRUE
      satsvar2 = FALSE.

      FIND FIRST levtemp WHERE levtemp.LEVNAMN = lvrant 
      USE-INDEX LEV NO-LOCK NO-ERROR.
      vald_lev2 = levtemp.LEVKOD.   

      FIND FIRST mtrltemp WHERE mtrltemp.LEVKOD = vald_lev2 NO-ERROR.
      IF NOT AVAILABLE mtrltemp THEN DO:
         RUN mtrllehmt_UI IN dxtapph (INPUT vald_lev2 ,OUTPUT TABLE mtrltemp).             
      END.            
      RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE mtrltemp.KALKNR = 0 AND mtrltemp.LEVKOD = '" +   STRING(vald_lev2) + "'").
      RUN openbdynspec_UI IN brwproc[2].

      /*FIND FIRST levtemp WHERE levtemp.LEVNAMN = lvrant 
      USE-INDEX LEV NO-LOCK NO-ERROR.
      vald_lev2 = levtemp.LEVKOD.        
      OPEN QUERY BRW_HLEV2 FOR EACH mtrltemp WHERE mtrltemp.LEVKOD = vald_lev2 AND
      mtrltemp.KALKNR = 0 NO-LOCK
      USE-INDEX LEV INDEXED-REPOSITION.*/
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON ANY-KEY OF FILL-IN-BEN IN FRAME FRAME-A /* Ben */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN IN FRAME FRAME-A /* Ben */
DO: 
   {muswait.i}
   ASSIGN
   sok = FALSE                   
   FILL-IN-BEN = INPUT FILL-IN-BEN. 
   IF satsvar = FALSE THEN DO:
      IF AVAILABLE mtrltemp THEN mtrl_recid = RECID(mtrltemp).
      IF FILL-IN-BEN = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      aosok = '*' + FILL-IN-BEN + '*'. 
      status-ok = BRW_HLEV:DESELECT-ROWS().       
      FIND mtrltemp WHERE RECID(mtrltemp) = mtrl_recid NO-LOCK NO-ERROR.
      FIND NEXT mtrltemp WHERE mtrltemp.BENAMNING MATCHES aosok AND mtrltemp.LEVKOD = vald_lev
      AND mtrltemp.KALKNR = 0 USE-INDEX BEN NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE mtrltemp THEN DO:      
         FIND FIRST mtrltemp WHERE mtrltemp.BENAMNING MATCHES aosok AND mtrltemp.LEVKOD = vald_lev
         AND mtrltemp.KALKNR = 0 USE-INDEX BEN NO-LOCK NO-ERROR.   
         IF NOT AVAILABLE mtrltemp THEN DO:
            MESSAGE "Det finns inget på sökbegreppet hos vald leverantör."
            VIEW-AS ALERT-BOX
            TITLE "Sökning".      
            APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.               
         END. 
      END.     
      IF AVAILABLE mtrltemp AND sok = FALSE THEN DO:      
         RUN repo_UI (INPUT 1, INPUT RECID(mtrltemp)).
         status-ok = BRW_HLEV:SELECT-FOCUSED-ROW().
      END.
   END.
   ELSE DO:       
      IF AVAILABLE satstemp THEN mtrl_recid = RECID(satstemp).
      IF FILL-IN-BEN = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      ASSIGN
      posok = '*' + FILL-IN-BEN + '*'  
      status-ok = BRW_SATS:DESELECT-ROWS().      
      FIND satstemp WHERE RECID(satstemp) = mtrl_recid NO-LOCK NO-ERROR.
      FIND NEXT satstemp WHERE satstemp.BENAMNING MATCHES posok AND satstemp.LEVKOD = vald_lev
      AND satstemp.SATS = TRUE USE-INDEX KOD NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE satstemp THEN DO:      
         FIND FIRST satstemp WHERE satstemp.BENAMNING MATCHES posok AND satstemp.LEVKOD = vald_lev
         AND satstemp.SATS = TRUE USE-INDEX KOD NO-LOCK NO-ERROR.   
         IF NOT AVAILABLE satstemp THEN DO:
            MESSAGE "Det finns ingen på sökbegreppet."
            VIEW-AS ALERT-BOX
            TITLE "Sökning".                       
            APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.                          
         END.
      END.              
      IF AVAILABLE satstemp THEN DO:               
         RUN repo_UI (INPUT 3, INPUT RECID(satstemp)).
         status-ok = BRW_SATS:SELECT-FOCUSED-ROW().
      END.     
   END.   
   {musarrow.i}                                 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN2 WINDOW-1
ON ANY-KEY OF FILL-IN-BEN2 IN FRAME FRAME-A /* Ben */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN2 IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN2 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN2 IN FRAME FRAME-A /* Ben */
DO: 
   {muswait.i}
   ASSIGN
   sok = FALSE                   
   FILL-IN-BEN2 = INPUT FILL-IN-BEN2. 
   IF satsvar2 = FALSE THEN DO:
      IF AVAILABLE mtrltemp THEN mtrl_recid = RECID(mtrltemp).
      IF FILL-IN-BEN2 = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-BEN2 IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      aosok = '*' + FILL-IN-BEN2 + '*'. 
      status-ok = BRW_HLEV2:DESELECT-ROWS().       
      FIND mtrltemp WHERE RECID(mtrltemp) = mtrl_recid NO-LOCK NO-ERROR.
      FIND NEXT mtrltemp WHERE mtrltemp.BENAMNING MATCHES aosok AND mtrltemp.LEVKOD = vald_lev2
      AND mtrltemp.KALKNR = 0 USE-INDEX BEN NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE mtrltemp THEN DO:      
         FIND FIRST mtrltemp WHERE mtrltemp.BENAMNING MATCHES aosok AND mtrltemp.LEVKOD = vald_lev2
         AND mtrltemp.KALKNR = 0 USE-INDEX BEN NO-LOCK NO-ERROR.   
         IF NOT AVAILABLE mtrltemp THEN DO:
            MESSAGE "Det finns inget på sökbegreppet hos vald leverantör."
            VIEW-AS ALERT-BOX
            TITLE "Sökning".      
            APPLY "ENTRY" TO FILL-IN-BEN2 IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.               
         END. 
      END.     
      IF AVAILABLE mtrltemp AND sok = FALSE THEN DO:               
         RUN repo_UI (INPUT 2, INPUT RECID(mtrltemp)).
         status-ok = BRW_HLEV2:SELECT-FOCUSED-ROW().
      END.
   END.
   ELSE DO:       
      IF AVAILABLE satstemp THEN mtrl_recid = RECID(satstemp).
      IF FILL-IN-BEN2 = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-BEN2 IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      ASSIGN
      posok = '*' + FILL-IN-BEN2 + '*'  
      status-ok = BRW_SATS2:DESELECT-ROWS().      
      FIND satstemp WHERE RECID(satstemp) = mtrl_recid NO-LOCK NO-ERROR.
      FIND NEXT satstemp WHERE satstemp.BENAMNING MATCHES posok AND satstemp.LEVKOD = vald_lev2
      AND satstemp.SATS = TRUE USE-INDEX KOD NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE satstemp THEN DO:      
         FIND FIRST satstemp WHERE satstemp.BENAMNING MATCHES posok AND satstemp.LEVKOD = vald_lev2
         AND satstemp.SATS = TRUE USE-INDEX KOD NO-LOCK NO-ERROR.   
         IF NOT AVAILABLE satstemp THEN DO:
            MESSAGE "Det finns ingen på sökbegreppet."
            VIEW-AS ALERT-BOX
            TITLE "Sökning".                       
            APPLY "ENTRY" TO FILL-IN-BEN2 IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.                          
         END.
      END.              
      IF AVAILABLE satstemp THEN DO:               
         RUN repo_UI (INPUT 4, INPUT RECID(satstemp)).
         status-ok = BRW_SATS2:SELECT-FOCUSED-ROW().
      END.     
   END.   
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
   ASSIGN
   sok = FALSE  
   FILL-IN-ENR = INPUT FILL-IN-ENR.  
   IF AVAILABLE mtrltemp THEN mtrl_recid = RECID(mtrltemp).
   IF FILL-IN-ENR = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX TITLE "Meddelande".
      APPLY "ENTRY" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   posok = '*' + FILL-IN-ENR + '*'. 
   status-ok = BRW_HLEV:DESELECT-ROWS().       
   FIND mtrltemp WHERE RECID(mtrltemp) = mtrl_recid NO-LOCK NO-ERROR.
   FIND NEXT mtrltemp WHERE mtrltemp.ENR MATCHES posok AND mtrltemp.LEVKOD = vald_lev
   AND mtrltemp.KALKNR = 0 USE-INDEX LEV NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE mtrltemp THEN DO:      
      FIND FIRST mtrltemp WHERE mtrltemp.ENR MATCHES posok AND mtrltemp.LEVKOD = vald_lev
      AND mtrltemp.KALKNR = 0 USE-INDEX LEV NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE mtrltemp THEN DO:
         MESSAGE "Det finns inget på sökbegreppet hos vald leverantör."
         VIEW-AS ALERT-BOX
         TITLE "Sökning".                  
      END.
   END.              
   IF AVAILABLE mtrltemp AND sok = FALSE THEN DO:               
      RUN repo_UI (INPUT 1, INPUT RECID(mtrltemp)).
      status-ok = BRW_HLEV:SELECT-FOCUSED-ROW().
   END.   
   {musarrow.i}                   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENR2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR2 WINDOW-1
ON ANY-KEY OF FILL-IN-ENR2 IN FRAME FRAME-A /* Enr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-ENR2 IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR2 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-ENR2 IN FRAME FRAME-A /* Enr */
DO: 
   {muswait.i}
   ASSIGN
   sok = FALSE  
   FILL-IN-ENR2 = INPUT FILL-IN-ENR2.  
   IF AVAILABLE mtrltemp THEN mtrl_recid = RECID(mtrltemp).
   IF FILL-IN-ENR2 = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX TITLE "Meddelande".
      APPLY "ENTRY" TO FILL-IN-ENR2 IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   posok = '*' + FILL-IN-ENR2 + '*'. 
   status-ok = BRW_HLEV2:DESELECT-ROWS().       
   FIND mtrltemp WHERE RECID(mtrltemp) = mtrl_recid NO-LOCK NO-ERROR.
   FIND NEXT mtrltemp WHERE mtrltemp.ENR MATCHES posok AND mtrltemp.LEVKOD = vald_lev2
   AND mtrltemp.KALKNR = 0 USE-INDEX LEV NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE mtrltemp THEN DO:      
      FIND FIRST mtrltemp WHERE mtrltemp.ENR MATCHES posok AND mtrltemp.LEVKOD = vald_lev2
      AND mtrltemp.KALKNR = 0 USE-INDEX LEV NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE mtrltemp THEN DO:
         MESSAGE "Det finns inget på sökbegreppet hos vald leverantör."
         VIEW-AS ALERT-BOX
         TITLE "Sökning".                  
      END.
   END.              
   IF AVAILABLE mtrltemp AND sok = FALSE THEN DO:               
      RUN repo_UI (INPUT 2, INPUT RECID(mtrltemp)).
      status-ok = BRW_HLEV2:SELECT-FOCUSED-ROW().
   END.       
   {musarrow.i}            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD WINDOW-1
ON ANY-KEY OF FILL-IN-KOD IN FRAME FRAME-A /* Kod */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-KOD IN FRAME FRAME-A /* Kod */
DO: 
   {muswait.i}
   ASSIGN
   sok = FALSE  
   FILL-IN-KOD = INPUT FILL-IN-KOD.     
   RUN sokkod_UI.             
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KOD2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD2 WINDOW-1
ON ANY-KEY OF FILL-IN-KOD2 IN FRAME FRAME-A /* Kod */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD2 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-KOD2 IN FRAME FRAME-A /* Kod */
DO: 
   {muswait.i}
   ASSIGN
   sok = FALSE  
   FILL-IN-KOD2 = INPUT FILL-IN-KOD2.     
   RUN sokkod2_UI.             
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
ON CLOSE OF THIS-PROCEDURE DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(dxtapph) THEN DELETE PROCEDURE dxtapph.
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
   

   {&WINDOW-NAME}:TITLE = "Byte av artikel i alla depåer".      

   EMPTY TEMP-TABLE spec_mtrl NO-ERROR.
   
   /*FIND FIRST HUVUDLEV WHERE HUVUDLEV.DEP-NR = 999 NO-LOCK NO-ERROR.
   IF AVAILABLE HUVUDLEV THEN DO:  
      vald_kundlev = HUVUDLEV.LEVKOD.
   END.                              
   ELSE DO:
      FIND FIRST levtemp WHERE levtemp.LEVKOD NE "0" 
      AND levtemp.BORTTAG = FALSE NO-LOCK NO-ERROR.
      vald_kundlev = levtemp.LEVKOD.
   END.*/
   mtrltemp.ENR:LABEL IN BROWSE BRW_HLEV = Guru.Konstanter:genk. 
   mtrltemp.ENR:LABEL IN BROWSE BRW_HLEV2 = Guru.Konstanter:genk.  
   FIND FIRST mtrltemp NO-ERROR.
   IF NOT AVAILABLE mtrltemp THEN DO:
      RUN mtrlhmt_UI IN dxtapph (INPUT TABLE levtemp,OUTPUT TABLE mtrltemp,OUTPUT TABLE satstemp ,OUTPUT vald_kundlev).             
   END.

   ASSIGN
   status-ok = CMB_LEV:DELETE("0")
   status-ok = CMB_LEV2:DELETE("0"). 
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_kundlev
   USE-INDEX LEV NO-LOCK NO-ERROR. 
   ASSIGN
   status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
   CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN
   status-ok = CMB_LEV2:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
   CMB_LEV2:SCREEN-VALUE = levtemp.LEVNAMN
   huvudlev = levtemp.LEVNAMN. 
   FOR EACH levtemp WHERE levtemp.LEVKOD NE vald_kundlev AND 
   levtemp.LEVKOD NE "0" AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:      
      ASSIGN
      status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
      status-ok = CMB_LEV2:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.              
   END.        
   FOR EACH levtemp WHERE  
   levtemp.LEVKOD NE "0" AND levtemp.LEVKOD NE "99" AND
   levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:      
      ASSIGN
      status-ok = CMB_LEV:ADD-LAST("Sats " + levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
      status-ok = CMB_LEV2:ADD-LAST("Sats " + levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.              
   END.        
   ASSIGN 
   CMB_LEV = INPUT CMB_LEV
   CMB_LEV = INPUT CMB_LEV
   vald_lev = vald_kundlev
   vald_lev2 = vald_kundlev
   bestant = FALSE
   val = FALSE
   sok = FALSE.         
   RUN setcolsortvar_UI IN brwproc[1] (INPUT " WHERE mtrltemp.KALKNR = 0 AND mtrltemp.LEVKOD = '" +   STRING(vald_lev) + "'").
   RUN openbdynspec_UI IN brwproc[1].      
   RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE mtrltemp.KALKNR = 0 AND mtrltemp.LEVKOD = '" +   STRING(vald_lev) + "'").
   RUN openbdynspec_UI IN brwproc[2].
   RUN setcolsortvar_UI IN brwproc[3] (INPUT " WHERE satstemp.LEVKOD = '" +   STRING(vald_lev) + "'").
   RUN openbdynspec_UI IN brwproc[3].
   RUN setcolsortvar_UI IN brwproc[4] (INPUT " WHERE satstemp.LEVKOD = '" +   STRING(vald_lev) + "'").
   RUN openbdynspec_UI IN brwproc[4].
   RUN enable_UI.   
   {FRMSIZE.I}      
   ASSIGN
   BTN_MOD:HIDDEN = TRUE
   BTN_UPPANT:HIDDEN = TRUE
   BTN_LIN:HIDDEN = TRUE.
   APPLY "VALUE-CHANGED" TO CMB_LEV.
   APPLY "VALUE-CHANGED" TO CMB_LEV2.   
   ASSIGN   
   BRW_SATS:HIDDEN = TRUE
   FILL-IN-KOD:HIDDEN = TRUE. 
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
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_HLEV:HANDLE IN FRAME {&FRAME-NAME}).         
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_HLEV2:HANDLE IN FRAME {&FRAME-NAME}).         
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_SATS:HANDLE IN FRAME {&FRAME-NAME}).         
   RUN DYNBRW.P PERSISTENT SET brwproc[4]
      (INPUT BRW_SATS2:HANDLE IN FRAME {&FRAME-NAME}).         
   IF Guru.Konstanter:appcon THEN DO:
      RUN DXTAPP.P PERSISTENT SET dxtapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN DXTAPP.P PERSISTENT SET dxtapph.
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
  DISPLAY CMB_LEV CMB_LEV2 FILL-IN-ENR FILL-IN-KOD FILL-IN-ENR2 FILL-IN-KOD2 
          FILL-IN-BEN FILL-IN-BEN2 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-3 RECT-4 BRW_HLEV BRW_SATS BRW_SATS2 BRW_HLEV2 BTN_BYT CMB_LEV 
         CMB_LEV2 btn_uppant BTN_MOD BTN_LIN FILL-IN-ENR FILL-IN-KOD 
         FILL-IN-ENR2 FILL-IN-KOD2 FILL-IN-BEN FILL-IN-BEN2 BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kollmtrl2_UI WINDOW-1 
PROCEDURE kollmtrl2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   EMPTY TEMP-TABLE byt_mtrl NO-ERROR.   
   CREATE byt_mtrl.
   ASSIGN
   byt_mtrl.ENR = mtrltemp.ENR
   byt_mtrl.BENAMNING = mtrltemp.BENAMNING
   byt_mtrl.ENHET = mtrltemp.ENHET
   byt_mtrl.NPRIS = mtrltemp.NPRIS
   byt_mtrl.LEVKOD = mtrltemp.LEVKOD
   byt_mtrl.BERKVANT = 1
   byt_mtrl.SATS = FALSE.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kollmtrl_UI WINDOW-1 
PROCEDURE kollmtrl_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
   EMPTY TEMP-TABLE spec_mtrl NO-ERROR.      
   CREATE spec_mtrl.
   ASSIGN
   spec_mtrl.ENR = mtrltemp.ENR
   spec_mtrl.BENAMNING = mtrltemp.BENAMNING
   spec_mtrl.ENHET = mtrltemp.ENHET
   spec_mtrl.NPRIS = mtrltemp.NPRIS
   spec_mtrl.LEVKOD = mtrltemp.LEVKOD
   spec_mtrl.BERKVANT = 1
   spec_mtrl.SATS = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kollsats2_UI WINDOW-1 
PROCEDURE kollsats2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   EMPTY TEMP-TABLE byt_mtrl NO-ERROR.         
   CREATE byt_mtrl.
   ASSIGN
   byt_mtrl.ENR = satstemp.ENR
   byt_mtrl.BENAMNING = satstemp.BENAMNING
   byt_mtrl.ENHET = satstemp.ENHET
   byt_mtrl.NPRIS = satstemp.PRIS
   byt_mtrl.LEVKOD = satstemp.LEVKOD. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kollsats_UI WINDOW-1 
PROCEDURE kollsats_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
   EMPTY TEMP-TABLE spec_mtrl NO-ERROR.            
   CREATE spec_mtrl.
   ASSIGN
   spec_mtrl.ENR = satstemp.ENR
   spec_mtrl.BENAMNING = satstemp.BENAMNING
   spec_mtrl.ENHET = satstemp.ENHET
   spec_mtrl.NPRIS = satstemp.PRIS
   spec_mtrl.LEVKOD = satstemp.LEVKOD
   spec_mtrl.SATS = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo_UI WINDOW-1 
PROCEDURE repo_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER brwvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER browrec AS RECID NO-UNDO.
   IF brwvar = 1 THEN DO:
      &Scoped-define BROWSE-NAME BRW_HLEV
      {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
      REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   END.
   IF brwvar = 2 THEN DO:
      &Scoped-define BROWSE-NAME BRW_HLEV2
      {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
      REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   END.
   IF brwvar = 3 THEN DO:
      &Scoped-define BROWSE-NAME BRW_SATS
      {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
      REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   END.
   IF brwvar = 4 THEN DO:
      &Scoped-define BROWSE-NAME BRW_SATS2
      {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
      REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   END.   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sokkod2_UI WINDOW-1 
PROCEDURE sokkod2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF AVAILABLE satstemp THEN mtrl_recid = RECID(satstemp).
   IF FILL-IN-KOD2 = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX TITLE "Meddelande".
      APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   ASSIGN
   posok = '*' + FILL-IN-KOD2 + '*'  
   status-ok = BRW_SATS2:DESELECT-ROWS().      
   FIND satstemp WHERE RECID(satstemp) = mtrl_recid NO-LOCK NO-ERROR.
   FIND NEXT satstemp WHERE satstemp.KOD MATCHES posok AND satstemp.LEVKOD = vald_lev2
   AND satstemp.SATS = TRUE USE-INDEX KOD NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE satstemp THEN DO:      
      FIND FIRST satstemp WHERE satstemp.KOD MATCHES posok AND satstemp.LEVKOD = vald_lev2
      AND satstemp.SATS = TRUE USE-INDEX KOD NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE satstemp THEN DO:
         MESSAGE "Det finns ingen på sökbegreppet."
         VIEW-AS ALERT-BOX
         TITLE "Sökning".                       
         APPLY "ENTRY" TO FILL-IN-KOD2 IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.                          
      END.
   END.              
   IF AVAILABLE satstemp THEN DO:               
      RUN repo_UI (INPUT 4, INPUT RECID(satstemp)).
      status-ok = BRW_SATS2:SELECT-FOCUSED-ROW().
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sokkod_UI WINDOW-1 
PROCEDURE sokkod_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF AVAILABLE satstemp THEN mtrl_recid = RECID(satstemp).
   IF FILL-IN-KOD = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX TITLE "Meddelande".
      APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   ASSIGN
   posok = '*' + FILL-IN-KOD + '*'  
   status-ok = BRW_SATS:DESELECT-ROWS().      
   FIND satstemp WHERE RECID(satstemp) = mtrl_recid NO-LOCK NO-ERROR.
   FIND NEXT satstemp WHERE satstemp.KOD MATCHES posok AND satstemp.LEVKOD = vald_lev
   AND satstemp.SATS = TRUE USE-INDEX KOD NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE satstemp THEN DO:      
      FIND FIRST satstemp WHERE satstemp.KOD MATCHES posok AND satstemp.LEVKOD = vald_lev
      AND satstemp.SATS = TRUE USE-INDEX KOD NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE satstemp THEN DO:
         MESSAGE "Det finns ingen på sökbegreppet."
         VIEW-AS ALERT-BOX
         TITLE "Sökning".                       
         APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.                          
      END.
   END.              
   IF AVAILABLE satstemp THEN DO:               
      RUN repo_UI (INPUT 3, INPUT RECID(satstemp)).
      status-ok = BRW_SATS:SELECT-FOCUSED-ROW().
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sokmera_UI WINDOW-1 
PROCEDURE sokmera_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
   DEFINE VARIABLE rkollvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE ttcopyh AS HANDLE NO-UNDO.
   DELETE WIDGET-POOL "dynpool" NO-ERROR.
   IF satsvar = FALSE THEN tempbufh = TEMP-TABLE mtrltemp:HANDLE:DEFAULT-BUFFER-HANDLE.
   ELSE tempbufh = TEMP-TABLE satstemp:HANDLE:DEFAULT-BUFFER-HANDLE.
   CREATE TEMP-TABLE ttcopyh. 
   ttcopyh:CREATE-LIKE(tempbufh).
   ttcopyh:TEMP-TABLE-PREPARE("ttkopia").
   tempbufmerah = ttcopyh:DEFAULT-BUFFER-HANDLE.
   nyaquery = "".
   rkollvar = 2.
   REPEAT: 
      
      orgbufh = tempbufh.
      IF sokstring[rkollvar] = "" THEN LEAVE.
      RUN starwars_UI (INPUT sokstring[rkollvar]).
      IF begvar = ? THEN LEAVE.
      RUN kommandoskap_UI (INPUT sokstring[rkollvar]).
      /* Skapar query och kopplar queryn till den skarpa tabellens buffer */
      CREATE QUERY appqueh.
      appqueh:SET-BUFFERS(orgbufh).
      appqueh:QUERY-PREPARE(valdkommandoquery).
      appqueh:QUERY-OPEN().      
      appqueh:GET-FIRST(NO-LOCK).
      IF appqueh:QUERY-OFF-END = TRUE THEN DO:
         orgbufh:EMPTY-TEMP-TABLE  NO-ERROR.
         felmedd = "Det finns inget på sökbegreppet.". 
         LEAVE.     
      END.
      ELSE DO:
         tempbufmerah:EMPTY-TEMP-TABLE  NO-ERROR.
         DO WHILE appqueh:QUERY-OFF-END = FALSE:
            tempbufmerah:BUFFER-CREATE().
            tempbufmerah:BUFFER-COPY(orgbufh) NO-ERROR.
            appqueh:GET-NEXT(NO-LOCK).
         END.         
      END.
      orgbufh:EMPTY-TEMP-TABLE  NO-ERROR.
      CREATE QUERY appqueh.
      appqueh:SET-BUFFERS(tempbufmerah).
      appqueh:QUERY-PREPARE("FOR EACH " + tempbufmerah:TABLE).
      appqueh:QUERY-OPEN().      
      appqueh:GET-FIRST(NO-LOCK).
      DO WHILE appqueh:QUERY-OFF-END = FALSE:
         orgbufh:BUFFER-CREATE().
         orgbufh:BUFFER-COPY(tempbufmerah) NO-ERROR.
         appqueh:GET-NEXT(NO-LOCK).             
      END.      
      tempbufmerah:EMPTY-TEMP-TABLE  NO-ERROR.
      rkollvar = rkollvar + 1.
      IF rkollvar > 10 THEN LEAVE.
   END.
   */  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sokoch_UI WINDOW-1 
PROCEDURE sokoch_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*
   DEFINE INPUT  PARAMETER sokstringorg AS CHARACTER NO-UNDO.
   DEFINE VARIABLE forsta AS CHARACTER NO-UNDO.
   DEFINE VARIABLE sista AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kollvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE rkollvar AS INTEGER NO-UNDO.
   IF INDEX(sokstringorg,"&&") = 0  THEN RETURN.
   IF SUBSTRING(sokstringorg,1,1) = "*" THEN  forsta = "*".
   IF SUBSTRING(sokstringorg,LENGTH(sokstringorg),1) = "*" THEN  sista = "*".
   sokstringorg = REPLACE(sokstringorg,"*","").
   rkollvar = 1.
   kollvar = 1.
   REPEAT: 
      IF kollvar >= LENGTH(sokstringorg) THEN LEAVE.
      IF rkollvar > 1 THEN DO:
         ASSIGN
         forsta = "*"
         sista = "*".
      END.   
      IF INDEX(sokstringorg,"&&",kollvar) = 0 THEN DO: 
         sokstring[rkollvar] = TRIM(forsta + SUBSTRING(sokstringorg,kollvar) + sista).
         LEAVE.
      END.
      sokstring[rkollvar] = TRIM(forsta + SUBSTRING(sokstringorg,kollvar,INDEX(sokstringorg,"&&",kollvar) - kollvar) + sista).
      kollvar = INDEX(sokstringorg,"&&",kollvar) + 2.
      rkollvar = rkollvar + 1.
      IF rkollvar > 10 THEN LEAVE.
   END.
   */  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

