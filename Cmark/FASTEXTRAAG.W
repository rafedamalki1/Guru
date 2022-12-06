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

  Created: 95/05/22 -  1:44 pm

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

&Scoped-define NEW 
&Scoped-define SHARED SHARED
{MARKVAL.I}                         

{FASTEXTRA.I}
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE spartidvar AS INTEGER NO-UNDO.
DEFINE VARIABLE spvf AS LOGICAL NO-UNDO.
DEFINE VARIABLE spvf2 AS LOGICAL NO-UNDO. 
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_FEXTRA

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES fastextra

/* Definitions for BROWSE BRW_FEXTRA                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_FEXTRA fastextra.beteckning ~
fastextra.marknr fastextra.MARKAGARE fastextra.ANDEL fastextra.DAT1 ~
fastextra.ch13 fastextra.LG4 fastextra.ch1 fastextra.ch2 fastextra.ch3 ~
fastextra.ch4 fastextra.ch11 fastextra.ch7 fastextra.ch8 fastextra.ch5 ~
fastextra.LG1 fastextra.LG2 fastextra.INT1 fastextra.INT2 fastextra.INT3 ~
fastextra.ch10 fastextra.LG3 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FEXTRA fastextra.DAT1 ~
fastextra.ch13 fastextra.LG4 fastextra.ch1 fastextra.ch2 fastextra.ch3 ~
fastextra.ch4 fastextra.ch11 fastextra.ch7 fastextra.ch8 fastextra.ch5 ~
fastextra.LG1 fastextra.LG2 fastextra.INT1 fastextra.INT2 fastextra.INT3 ~
fastextra.ch10 fastextra.LG3 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_FEXTRA fastextra
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_FEXTRA fastextra
&Scoped-define QUERY-STRING-BRW_FEXTRA FOR EACH fastextra NO-LOCK
&Scoped-define OPEN-QUERY-BRW_FEXTRA OPEN QUERY BRW_FEXTRA FOR EACH fastextra NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_FEXTRA fastextra
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FEXTRA fastextra


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_FEXTRA EDT_ANM FBTN_SNABB FBTN_OK ~
BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-VFASTIGHET EDT_ANM 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SNABB 
     LABEL "Snabbspara" 
     SIZE 14 BY 1.

DEFINE VARIABLE EDT_ANM AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 81 BY 4 NO-UNDO.

DEFINE VARIABLE FILL-IN-VFASTIGHET AS CHARACTER FORMAT "X(256)":U 
     LABEL "Anmärkning till markerad fastighet" 
     VIEW-AS FILL-IN 
     SIZE 31.75 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_FEXTRA FOR 
      fastextra SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_FEXTRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FEXTRA WINDOW-1 _STRUCTURED
  QUERY BRW_FEXTRA NO-LOCK DISPLAY
      fastextra.beteckning COLUMN-LABEL "Beteckning" FORMAT "X(256)":U
            WIDTH 20
      fastextra.marknr COLUMN-LABEL "Marknr" FORMAT ">>>>>>9":U
      fastextra.MARKAGARE COLUMN-LABEL "Markägare" FORMAT "x(256)":U
            WIDTH 15
      fastextra.ANDEL COLUMN-LABEL "Andel" FORMAT "x(6)":U
      fastextra.DAT1 COLUMN-LABEL "Bevakning" FORMAT "9999/99/99":U
      fastextra.ch13 COLUMN-LABEL "Vad bevakas?" FORMAT "x(256)":U
            WIDTH 30
      fastextra.LG4 COLUMN-LABEL "Ej!avl" FORMAT "Ja/Nej":U WIDTH 5
      fastextra.ch1 COLUMN-LABEL "Staknings-!tillstånd" FORMAT "X(256)":U
            WIDTH 10
      fastextra.ch2 COLUMN-LABEL "Röjning!&fällning" FORMAT "X(256)":U
            WIDTH 10
      fastextra.ch3 COLUMN-LABEL "Skickat" FORMAT "X(256)":U WIDTH 9
      fastextra.ch4 COLUMN-LABEL "Mottaget" FORMAT "X(256)":U WIDTH 9
      fastextra.ch11 COLUMN-LABEL "Ersätt!senast" FORMAT "x(256)":U
            WIDTH 15
      fastextra.ch7 COLUMN-LABEL "Bankkonto/!Bankgiro" FORMAT "X(256)":U
            WIDTH 10
      fastextra.ch8 COLUMN-LABEL "Banknamn" FORMAT "X(256)":U WIDTH 8
      fastextra.ch5 COLUMN-LABEL "Ersatt" FORMAT "X(256)":U WIDTH 9
      fastextra.LG1 COLUMN-LABEL "F" FORMAT "Ja/Nej":U
      fastextra.LG2 COLUMN-LABEL "T" FORMAT "Ja/Nej":U
      fastextra.INT1 COLUMN-LABEL "F-Rotnetto" FORMAT "->>>>>>9":U
      fastextra.INT2 COLUMN-LABEL "T-Expl på! rotnetto" FORMAT "->>>>>>9":U
      fastextra.INT3 COLUMN-LABEL "Vp!Skog" FORMAT "->>>>>>9":U
      fastextra.ch10 COLUMN-LABEL "Kommentar" FORMAT "X(256)":U
            WIDTH 25
      fastextra.LG3 COLUMN-LABEL "Moms!reg" FORMAT "Ja/Nej":U
  ENABLE
      fastextra.DAT1
      fastextra.ch13
      fastextra.LG4
      fastextra.ch1
      fastextra.ch2
      fastextra.ch3
      fastextra.ch4
      fastextra.ch11
      fastextra.ch7
      fastextra.ch8
      fastextra.ch5
      fastextra.LG1
      fastextra.LG2
      fastextra.INT1
      fastextra.INT2
      fastextra.INT3
      fastextra.ch10
      fastextra.LG3
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 124 BY 21
         TITLE "Extra uppgifter Värdering,Fastighet".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_FEXTRA AT ROW 1.63 COL 1.38
     FILL-IN-VFASTIGHET AT ROW 22.79 COL 36.25 COLON-ALIGNED
     EDT_ANM AT ROW 23.92 COL 2 NO-LABEL
     FBTN_SNABB AT ROW 25.75 COL 95.88 WIDGET-ID 2
     FBTN_OK AT ROW 27.08 COL 95.88
     BTN_AVB AT ROW 27.08 COL 110.88
     "Gulmarkerade fastigheter har en anmärkning" VIEW-AS TEXT
          SIZE 44.5 BY 1 AT ROW 22.79 COL 74 WIDGET-ID 4
          BGCOLOR 14 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 27.5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: fastextra T "?" NO-UNDO temp-db fastextra
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 27.5
         WIDTH              = 125
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125
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
/* BROWSE-TAB BRW_FEXTRA TEXT-1 FRAME-A */
ASSIGN 
       BRW_FEXTRA:MAX-DATA-GUESS IN FRAME FRAME-A         = 1000.

/* SETTINGS FOR FILL-IN FILL-IN-VFASTIGHET IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FEXTRA
/* Query rebuild information for BROWSE BRW_FEXTRA
     _TblList          = "Temp-Tables.fastextra"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.fastextra.beteckning
"beteckning" "Beteckning" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.fastextra.marknr
"marknr" "Marknr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.fastextra.MARKAGARE
"MARKAGARE" "Markägare" "x(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.fastextra.ANDEL
"ANDEL" "Andel" "x(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.fastextra.DAT1
"DAT1" "Bevakning" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.fastextra.ch13
"ch13" "Vad bevakas?" "x(256)" "character" ? ? ? ? ? ? yes ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.fastextra.LG4
"LG4" "Ej!avl" ? "logical" ? ? ? ? ? ? yes ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.fastextra.ch1
"ch1" "Staknings-!tillstånd" "X(256)" "character" ? ? ? ? ? ? yes ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.fastextra.ch2
"ch2" "Röjning!&fällning" "X(256)" "character" ? ? ? ? ? ? yes ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.fastextra.ch3
"ch3" "Skickat" "X(256)" "character" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.fastextra.ch4
"ch4" "Mottaget" "X(256)" "character" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.fastextra.ch11
"ch11" "Ersätt!senast" "x(256)" "character" ? ? ? ? ? ? yes ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-Tables.fastextra.ch7
"ch7" "Bankkonto/!Bankgiro" "X(256)" "character" ? ? ? ? ? ? yes ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Temp-Tables.fastextra.ch8
"ch8" "Banknamn" "X(256)" "character" ? ? ? ? ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Temp-Tables.fastextra.ch5
"ch5" "Ersatt" "X(256)" "character" ? ? ? ? ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > Temp-Tables.fastextra.LG1
"LG1" "F" ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > Temp-Tables.fastextra.LG2
"LG2" "T" ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > Temp-Tables.fastextra.INT1
"INT1" "F-Rotnetto" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > Temp-Tables.fastextra.INT2
"INT2" "T-Expl på! rotnetto" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > Temp-Tables.fastextra.INT3
"INT3" "Vp!Skog" ? "int64" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > Temp-Tables.fastextra.ch10
"ch10" "Kommentar" "X(256)" "character" ? ? ? ? ? ? yes ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > Temp-Tables.fastextra.LG3
"LG3" "Moms!reg" ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FEXTRA */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_FEXTRA
&Scoped-define SELF-NAME BRW_FEXTRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_FEXTRA WINDOW-1
ON ROW-LEAVE OF BRW_FEXTRA IN FRAME FRAME-A /* Extra uppgifter Värdering,Fastighet */
DO:
   RUN selfoc_UI.
   RUN brwfextrarowleav_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_FEXTRA WINDOW-1
ON VALUE-CHANGED OF BRW_FEXTRA IN FRAME FRAME-A /* Extra uppgifter Värdering,Fastighet */
DO:
  
  RUN selfoc_UI.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fastextra.DAT1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.DAT1 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON ENTRY OF fastextra.DAT1 IN BROWSE BRW_FEXTRA /* Bevakning */
DO:
  IF AVAILABLE fastextra  THEN DO:      
      ASSIGN fastextra.DAT1 = INPUT BROWSE BRW_FEXTRA fastextra.DAT1.      
      DISPLAY fastextra.DAT1 WITH BROWSE BRW_FEXTRA NO-ERROR.       
   END. 


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.DAT1 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON LEAVE OF fastextra.DAT1 IN BROWSE BRW_FEXTRA /* Bevakning */
DO:
  IF AVAILABLE fastextra THEN DO:      
      ASSIGN fastextra.DAT1 = INPUT BROWSE BRW_FEXTRA fastextra.DAT1.            
      DISPLAY fastextra.DAT1 WITH BROWSE BRW_FEXTRA NO-ERROR.
      IF fastextra.DAT1 > TODAY THEN DO:
         ASSIGN fastextra.LG4 = TRUE.
         DISPLAY fastextra.LG4 WITH BROWSE BRW_FEXTRA NO-ERROR.
      END.            
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fastextra.ch13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.ch13 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON LEAVE OF fastextra.ch13 IN BROWSE BRW_FEXTRA /* Vad bevakas? */
DO:
  fastextra.ch13:FORMAT IN BROWSE BRW_FEXTRA = "x(256)" .  
  IF AVAILABLE fastextra THEN DO:
      IF fastextra.ch13 NE INPUT BROWSE BRW_FEXTRA fastextra.ch13 THEN DO:                  
         fastextra.ch13 = INPUT BROWSE BRW_FEXTRA fastextra.ch13.           
         RUN selfoc_UI.            
         DISPLAY fastextra.ch13 WITH BROWSE BRW_FEXTRA.
         DISPLAY fastextra.ch13 WITH BROWSE BRW_FEXTRA.       
         APPLY "ENTRY" TO fastextra.ch13 IN BROWSE BRW_FEXTRA.
         RUN selfoc_UI.
      END.
   END.
   {musarrow.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fastextra.LG4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.LG4 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON ENTRY OF fastextra.LG4 IN BROWSE BRW_FEXTRA /* Ej!avl */
DO:
  IF AVAILABLE fastextra  THEN DO:      
      ASSIGN fastextra.LG4 = INPUT BROWSE BRW_FEXTRA fastextra.LG4.      
      DISPLAY fastextra.LG4 WITH BROWSE BRW_FEXTRA NO-ERROR.       
   END. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.LG4 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON LEAVE OF fastextra.LG4 IN BROWSE BRW_FEXTRA /* Ej!avl */
DO:
  IF AVAILABLE fastextra THEN DO:      
      ASSIGN fastextra.LG4 = INPUT BROWSE BRW_FEXTRA fastextra.LG4.            
      DISPLAY fastextra.LG4 WITH BROWSE BRW_FEXTRA NO-ERROR.       
   END.   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.LG4 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON MOUSE-SELECT-CLICK OF fastextra.LG4 IN BROWSE BRW_FEXTRA /* Ej!avl */
DO:
  DISPLAY fastextra.LG4 WITH BROWSE BRW_FEXTRA.
   IF INPUT BROWSE BRW_FEXTRA fastextra.LG4 = TRUE THEN fastextra.LG4 = FALSE.
   IF INPUT BROWSE BRW_FEXTRA fastextra.LG4 = FALSE THEN fastextra.LG4 = TRUE.   
   DISPLAY fastextra.LG4 WITH BROWSE BRW_FEXTRA NO-ERROR.   


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fastextra.ch1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.ch1 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON ENTRY OF fastextra.ch1 IN BROWSE BRW_FEXTRA /* Staknings-!tillstånd */
DO:
   
   RUN selfoc_UI.   
   IF AVAILABLE fastextra THEN DO:
      DISPLAY fastextra.ch1 WITH BROWSE BRW_FEXTRA.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.ch1 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON LEAVE OF fastextra.ch1 IN BROWSE BRW_FEXTRA /* Staknings-!tillstånd */
DO:
   IF AVAILABLE fastextra THEN DO:
      IF fastextra.ch1 NE INPUT BROWSE BRW_FEXTRA fastextra.ch1 THEN DO:         
         fastextra.ch1 = INPUT BROWSE BRW_FEXTRA fastextra.ch1.           
         RUN selfoc_UI.         
         DISPLAY fastextra.ch1 WITH BROWSE BRW_FEXTRA.
         APPLY "ENTRY" TO fastextra.ch1 IN BROWSE BRW_FEXTRA.
         RUN selfoc_UI.
         DISPLAY fastextra.ch1 WITH BROWSE BRW_FEXTRA.
      END.
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fastextra.ch2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.ch2 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON ENTRY OF fastextra.ch2 IN BROWSE BRW_FEXTRA /* Röjning!fällning */
DO:
   RUN selfoc_UI.   
   IF AVAILABLE fastextra THEN DO:
      DISPLAY fastextra.ch2 WITH BROWSE BRW_FEXTRA.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.ch2 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON LEAVE OF fastextra.ch2 IN BROWSE BRW_FEXTRA /* Röjning!fällning */
DO:
   IF AVAILABLE fastextra THEN DO:
      IF fastextra.ch2 NE INPUT BROWSE BRW_FEXTRA fastextra.ch2 THEN DO:         
         fastextra.ch2 = INPUT BROWSE BRW_FEXTRA fastextra.ch2.           
         RUN selfoc_UI.         
         DISPLAY fastextra.ch2 WITH BROWSE BRW_FEXTRA.      
         APPLY "ENTRY" TO fastextra.ch2 IN BROWSE BRW_FEXTRA.
         RUN selfoc_UI.

      END.
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fastextra.ch3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.ch3 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON ENTRY OF fastextra.ch3 IN BROWSE BRW_FEXTRA /* Skickat */
DO:   
   RUN selfoc_UI.
   IF AVAILABLE fastextra THEN DO:
      DISPLAY fastextra.ch3 WITH BROWSE BRW_FEXTRA.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.ch3 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON LEAVE OF fastextra.ch3 IN BROWSE BRW_FEXTRA /* Skickat */
DO:
  
  fastextra.ch3:FORMAT IN BROWSE BRW_FEXTRA = "x(256)" .  
  IF AVAILABLE fastextra THEN DO:
      IF fastextra.ch3 NE INPUT BROWSE BRW_FEXTRA fastextra.ch3 THEN DO:                  
         fastextra.ch3 = INPUT BROWSE BRW_FEXTRA fastextra.ch3.           
         RUN selfoc_UI.            
         DISPLAY fastextra.ch3 WITH BROWSE BRW_FEXTRA.
         DISPLAY fastextra.ch3 WITH BROWSE BRW_FEXTRA.       
         APPLY "ENTRY" TO fastextra.ch3 IN BROWSE BRW_FEXTRA.
         RUN selfoc_UI.
      END.
   END.
   IF INPUT BROWSE BRW_FEXTRA fastextra.ch3 NE "" THEN DO:
      IF Guru.Konstanter:globforetag = "tect" OR Guru.Konstanter:globforetag = "POWE" OR Guru.Konstanter:globforetag = "PFBR" OR Guru.Konstanter:globforetag = "wsp" OR Guru.Konstanter:globforetag = "SSEL" THEN fastextra.ch3:FORMAT IN BROWSE BRW_FEXTRA = "9999/99/99" .
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fastextra.ch4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.ch4 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON ENTRY OF fastextra.ch4 IN BROWSE BRW_FEXTRA /* Mottaget */
DO:  
   RUN selfoc_UI.
   IF AVAILABLE fastextra THEN DO:
      DISPLAY fastextra.ch4 WITH BROWSE BRW_FEXTRA.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.ch4 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON LEAVE OF fastextra.ch4 IN BROWSE BRW_FEXTRA /* Mottaget */
DO:
  fastextra.ch4:FORMAT IN BROWSE BRW_FEXTRA = "x(256)" .
  IF AVAILABLE fastextra THEN DO:
      IF fastextra.ch4 NE INPUT BROWSE BRW_FEXTRA fastextra.ch4 THEN DO:         
         fastextra.ch4 = INPUT BROWSE BRW_FEXTRA fastextra.ch4.                    
         RUN selfoc_UI.         
         DISPLAY fastextra.ch4 WITH BROWSE BRW_FEXTRA.
         APPLY "ENTRY" TO fastextra.ch4 IN BROWSE BRW_FEXTRA.
         RUN selfoc_UI.
         IF fastextra.ch4 NE "" THEN DO:
            fastextra.ch11 = STRING(DATE(fastextra.ch4) + 90,"99999999").
            DISPLAY fastextra.ch11 WITH BROWSE BRW_FEXTRA.
         END.  
         ELSE DO:
            fastextra.ch11 = "".
            DISPLAY fastextra.ch11 WITH BROWSE BRW_FEXTRA.
         END.               
      END.
   END.
   IF INPUT BROWSE BRW_FEXTRA fastextra.ch4 NE "" THEN DO:
      IF Guru.Konstanter:globforetag = "tect"  OR Guru.Konstanter:globforetag = "POWE" OR Guru.Konstanter:globforetag = "PFBR" OR Guru.Konstanter:globforetag = "wsp" OR Guru.Konstanter:globforetag = "SSEL" THEN DO:
         fastextra.ch4:FORMAT IN BROWSE BRW_FEXTRA = "9999/99/99" .
         fastextra.ch11:FORMAT IN BROWSE BRW_FEXTRA = "9999/99/99" .
      END.   
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fastextra.ch11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.ch11 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON ENTRY OF fastextra.ch11 IN BROWSE BRW_FEXTRA /* Ersätt!senast */
DO:
  RUN selfoc_UI.
   IF AVAILABLE fastextra THEN DO:
      DISPLAY fastextra.ch11 WITH BROWSE BRW_FEXTRA.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.ch11 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON LEAVE OF fastextra.ch11 IN BROWSE BRW_FEXTRA /* Ersätt!senast */
DO:
  fastextra.ch11:FORMAT IN BROWSE BRW_FEXTRA = "x(256)" .
  IF AVAILABLE fastextra THEN DO:
      IF fastextra.ch11 NE INPUT BROWSE BRW_FEXTRA fastextra.ch11 THEN DO:         
         fastextra.ch11 = INPUT BROWSE BRW_FEXTRA fastextra.ch11.              
         RUN selfoc_UI.         
         DISPLAY fastextra.ch11 WITH BROWSE BRW_FEXTRA.
         APPLY "ENTRY" TO fastextra.ch11 IN BROWSE BRW_FEXTRA.
         RUN selfoc_UI.
      END.
   END.
   IF INPUT BROWSE BRW_FEXTRA fastextra.ch11 NE "" THEN DO:
      IF Guru.Konstanter:globforetag = "tect"  OR Guru.Konstanter:globforetag = "POWE" OR Guru.Konstanter:globforetag = "PFBR" OR Guru.Konstanter:globforetag = "wsp" OR Guru.Konstanter:globforetag = "SSEL" THEN fastextra.ch11:FORMAT IN BROWSE BRW_FEXTRA = "9999/99/99" .
   END.
   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fastextra.ch7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.ch7 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON ENTRY OF fastextra.ch7 IN BROWSE BRW_FEXTRA /* Bankkonto/!Bankgiro */
DO:  
   RUN selfoc_UI.
   IF AVAILABLE fastextra THEN DO:
      DISPLAY fastextra.ch7 WITH BROWSE BRW_FEXTRA.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.ch7 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON LEAVE OF fastextra.ch7 IN BROWSE BRW_FEXTRA /* Bankkonto/!Bankgiro */
DO:
  IF AVAILABLE fastextra THEN DO:
      IF fastextra.ch7 NE INPUT BROWSE BRW_FEXTRA fastextra.ch7 THEN DO:         
         fastextra.ch7 = INPUT BROWSE BRW_FEXTRA fastextra.ch7.              
         RUN selfoc_UI.
         DISPLAY fastextra.ch7 WITH BROWSE BRW_FEXTRA.         
         APPLY "ENTRY" TO fastextra.ch7 IN BROWSE BRW_FEXTRA.
         RUN selfoc_UI.

      END.
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fastextra.ch8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.ch8 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON ENTRY OF fastextra.ch8 IN BROWSE BRW_FEXTRA /* Banknamn */
DO:  
   RUN selfoc_UI.
   IF AVAILABLE fastextra THEN DO:
      DISPLAY fastextra.ch8 WITH BROWSE BRW_FEXTRA.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.ch8 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON LEAVE OF fastextra.ch8 IN BROWSE BRW_FEXTRA /* Banknamn */
DO:
  IF AVAILABLE fastextra THEN DO:
      IF fastextra.ch8 NE INPUT BROWSE BRW_FEXTRA fastextra.ch8 THEN DO:         
         fastextra.ch8 = INPUT BROWSE BRW_FEXTRA fastextra.ch8.           
         /*status-ok = BRW_FEXTRA:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.*/
         RUN selfoc_UI.
         DISPLAY fastextra.ch8 WITH BROWSE BRW_FEXTRA.        
         APPLY "ENTRY" TO fastextra.ch8 IN BROWSE BRW_FEXTRA.
         RUN selfoc_UI.

      END.
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fastextra.ch5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.ch5 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON ENTRY OF fastextra.ch5 IN BROWSE BRW_FEXTRA /* Ersatt */
DO:  
   RUN selfoc_UI.
   
   IF AVAILABLE fastextra THEN DO:
      DISPLAY fastextra.ch5 WITH BROWSE BRW_FEXTRA.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.ch5 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON LEAVE OF fastextra.ch5 IN BROWSE BRW_FEXTRA /* Ersatt */
DO:
  fastextra.ch5:FORMAT IN BROWSE BRW_FEXTRA = "x(256)" . 
  IF AVAILABLE fastextra THEN DO:
      IF fastextra.ch5 NE INPUT BROWSE BRW_FEXTRA fastextra.ch5 THEN DO:         
         fastextra.ch5 = INPUT BROWSE BRW_FEXTRA fastextra.ch5.              
         RUN selfoc_UI.
         DISPLAY fastextra.ch5 WITH BROWSE BRW_FEXTRA.
         IF fastextra.CH5 NE "" THEN DO:                              
            /*färga grön om Ersatt ifylld*/
            fastextra.CH5:BGCOLOR IN BROWSE BRW_FEXTRA = 10.      
                         
         END.
         /*SÄTT TILLBAKA TILL TRASNSPARENT*/
         ELSE fastextra.CH5:BGCOLOR IN BROWSE BRW_FEXTRA = 15.
      
         APPLY "ENTRY" TO fastextra.ch5 IN BROWSE BRW_FEXTRA.
         RUN selfoc_UI.

      END.
   END.
   IF INPUT BROWSE BRW_FEXTRA fastextra.ch5 NE "" THEN DO:
      IF Guru.Konstanter:globforetag = "tect"  OR Guru.Konstanter:globforetag = "POWE" OR Guru.Konstanter:globforetag = "PFBR" OR Guru.Konstanter:globforetag = "wsp" OR Guru.Konstanter:globforetag = "SSEL" THEN fastextra.ch5:FORMAT IN BROWSE BRW_FEXTRA = "9999/99/99" .
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fastextra.LG1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.LG1 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON ENTRY OF fastextra.LG1 IN BROWSE BRW_FEXTRA /* F */
DO:
   IF AVAILABLE fastextra  THEN DO:      
      ASSIGN fastextra.LG1 = INPUT BROWSE BRW_FEXTRA fastextra.LG1.      
      DISPLAY fastextra.LG1 WITH BROWSE BRW_FEXTRA NO-ERROR.       
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.LG1 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON LEAVE OF fastextra.LG1 IN BROWSE BRW_FEXTRA /* F */
DO:
  IF AVAILABLE fastextra THEN DO:      
      ASSIGN fastextra.LG1 = INPUT BROWSE BRW_FEXTRA fastextra.LG1.            
      DISPLAY fastextra.LG1 WITH BROWSE BRW_FEXTRA NO-ERROR.       
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.LG1 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON MOUSE-SELECT-CLICK OF fastextra.LG1 IN BROWSE BRW_FEXTRA /* F */
DO:
   DISPLAY fastextra.LG1 WITH BROWSE BRW_FEXTRA.
   IF INPUT BROWSE BRW_FEXTRA fastextra.LG1 = TRUE THEN fastextra.LG1 = FALSE.
   IF INPUT BROWSE BRW_FEXTRA fastextra.LG1 = FALSE THEN fastextra.LG1 = TRUE.   
   DISPLAY fastextra.LG1 WITH BROWSE BRW_FEXTRA NO-ERROR.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fastextra.LG2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.LG2 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON ENTRY OF fastextra.LG2 IN BROWSE BRW_FEXTRA /* T */
DO:
  IF AVAILABLE fastextra  THEN DO:      
      ASSIGN fastextra.LG2 = INPUT BROWSE BRW_FEXTRA fastextra.LG2.      
      DISPLAY fastextra.LG2 WITH BROWSE BRW_FEXTRA NO-ERROR.       
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.LG2 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON LEAVE OF fastextra.LG2 IN BROWSE BRW_FEXTRA /* T */
DO:
  IF AVAILABLE fastextra THEN DO:      
      ASSIGN fastextra.LG2 = INPUT BROWSE BRW_FEXTRA fastextra.LG2.            
      DISPLAY fastextra.LG2 WITH BROWSE BRW_FEXTRA NO-ERROR.       
   END.   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.LG2 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON MOUSE-SELECT-CLICK OF fastextra.LG2 IN BROWSE BRW_FEXTRA /* T */
DO:
  DISPLAY fastextra.LG2 WITH BROWSE BRW_FEXTRA.
   IF INPUT BROWSE BRW_FEXTRA fastextra.LG2 = TRUE THEN fastextra.LG2 = FALSE.
   IF INPUT BROWSE BRW_FEXTRA fastextra.LG2 = FALSE THEN fastextra.LG2 = TRUE.   
   DISPLAY fastextra.LG2 WITH BROWSE BRW_FEXTRA NO-ERROR.   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fastextra.INT1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.INT1 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON ENTRY OF fastextra.INT1 IN BROWSE BRW_FEXTRA /* F-Rotnetto */
DO:
  RUN selfoc_UI.
   IF AVAILABLE fastextra THEN DO:
      DISPLAY fastextra.INT1 WITH BROWSE BRW_FEXTRA.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.INT1 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON LEAVE OF fastextra.INT1 IN BROWSE BRW_FEXTRA /* F-Rotnetto */
DO:
  IF AVAILABLE fastextra THEN DO:
      IF fastextra.INT1 NE INPUT BROWSE BRW_FEXTRA fastextra.INT1 THEN DO:         
         fastextra.INT1 = INPUT BROWSE BRW_FEXTRA fastextra.INT1.     
      END.
   END.
   {musarrow.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fastextra.INT2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.INT2 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON ENTRY OF fastextra.INT2 IN BROWSE BRW_FEXTRA /* T-Expl på! rotnetto */
DO:
  RUN selfoc_UI.
   IF AVAILABLE fastextra THEN DO:
      DISPLAY fastextra.INT2 WITH BROWSE BRW_FEXTRA.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.INT2 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON LEAVE OF fastextra.INT2 IN BROWSE BRW_FEXTRA /* T-Expl på! rotnetto */
DO:
  IF AVAILABLE fastextra THEN DO:
      IF fastextra.INT2 NE INPUT BROWSE BRW_FEXTRA fastextra.INT2 THEN DO:         
         fastextra.INT2 = INPUT BROWSE BRW_FEXTRA fastextra.INT2.                  
         RUN selfoc_UI.         
         DISPLAY fastextra.INT2 WITH BROWSE BRW_FEXTRA.
         APPLY "ENTRY" TO fastextra.INT2 IN BROWSE BRW_FEXTRA.
         RUN selfoc_UI.

      END.
   END.
   {musarrow.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fastextra.INT3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.INT3 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON ENTRY OF fastextra.INT3 IN BROWSE BRW_FEXTRA /* Vp!Skog */
DO:
  RUN selfoc_UI.
   IF AVAILABLE fastextra THEN DO:
      DISPLAY fastextra.INT3 WITH BROWSE BRW_FEXTRA.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.INT3 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON LEAVE OF fastextra.INT3 IN BROWSE BRW_FEXTRA /* Vp!Skog */
DO:
  IF AVAILABLE fastextra THEN DO:
      IF fastextra.INT3 NE INPUT BROWSE BRW_FEXTRA fastextra.INT3 THEN DO:         
         fastextra.INT3 = INPUT BROWSE BRW_FEXTRA fastextra.INT3.                  
         RUN selfoc_UI.         
         DISPLAY fastextra.INT3 WITH BROWSE BRW_FEXTRA.
         APPLY "ENTRY" TO fastextra.INT3 IN BROWSE BRW_FEXTRA.
         RUN selfoc_UI.

      END.
   END.
   {musarrow.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fastextra.ch10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.ch10 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON ENTRY OF fastextra.ch10 IN BROWSE BRW_FEXTRA /* Kommentar */
DO:  
   RUN selfoc_UI.
   IF AVAILABLE fastextra THEN DO:
      DISPLAY fastextra.ch10 WITH BROWSE BRW_FEXTRA.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.ch10 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON LEAVE OF fastextra.ch10 IN BROWSE BRW_FEXTRA /* Kommentar */
DO:
  IF AVAILABLE fastextra THEN DO:
      IF fastextra.ch10 NE INPUT BROWSE BRW_FEXTRA fastextra.ch10 THEN DO:         
         fastextra.ch10 = INPUT BROWSE BRW_FEXTRA fastextra.ch10.              
         RUN selfoc_UI.
         DISPLAY fastextra.ch10 WITH BROWSE BRW_FEXTRA.
         DISPLAY fastextra.ch10 WITH BROWSE BRW_FEXTRA.
         APPLY "ENTRY" TO fastextra.ch10 IN BROWSE BRW_FEXTRA.
         RUN selfoc_UI.

      END.
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fastextra.LG3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.LG3 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON ENTRY OF fastextra.LG3 IN BROWSE BRW_FEXTRA /* Moms!reg */
DO:
  IF AVAILABLE fastextra  THEN DO:      
      ASSIGN fastextra.LG3 = INPUT BROWSE BRW_FEXTRA fastextra.LG3.      
      DISPLAY fastextra.LG3 WITH BROWSE BRW_FEXTRA NO-ERROR.       
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.LG3 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON LEAVE OF fastextra.LG3 IN BROWSE BRW_FEXTRA /* Moms!reg */
DO:
  IF AVAILABLE fastextra THEN DO:      
      ASSIGN fastextra.LG3 = INPUT BROWSE BRW_FEXTRA fastextra.LG3.            
      DISPLAY fastextra.LG3 WITH BROWSE BRW_FEXTRA NO-ERROR.       
   END.   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fastextra.LG3 BRW_FEXTRA _BROWSE-COLUMN WINDOW-1
ON MOUSE-SELECT-CLICK OF fastextra.LG3 IN BROWSE BRW_FEXTRA /* Moms!reg */
DO:
  DISPLAY fastextra.LG3 WITH BROWSE BRW_FEXTRA.
   IF INPUT BROWSE BRW_FEXTRA fastextra.LG3 = TRUE THEN fastextra.LG3 = FALSE.
   IF INPUT BROWSE BRW_FEXTRA fastextra.LG3 = FALSE THEN fastextra.LG3 = TRUE.   
   DISPLAY fastextra.LG3 WITH BROWSE BRW_FEXTRA NO-ERROR.   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avbryt */
DO:   
   DEFINE VARIABLE svar AS LOGICAL NO-UNDO INITIAL TRUE.
   MESSAGE "OBS! Vill du spara dina ändringar?"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar .         
   IF svar THEN DO:
      APPLY "CHOOSE" TO FBTN_OK.         
   END.
   ELSE IF NOT svar THEN DO:    
      APPLY "CLOSE":U TO THIS-PROCEDURE. 
   END.                    
   ELSE DO:
      RETURN NO-APPLY.
   END. 
   
   
   /*APPLY "CLOSE":U TO THIS-PROCEDURE.*/      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EDT_ANM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDT_ANM WINDOW-1
ON LEAVE OF EDT_ANM IN FRAME FRAME-A
DO:
   
   EDT_ANM = INPUT EDT_ANM.
   IF AVAILABLE fastextra THEN DO:
      fastextra.CH9 = EDT_ANM.      
      IF fastextra.CH9 NE "" THEN DO:                              
         /*färga fastigheter som har kommentar gula*/
         fastextra.BETECKNING:BGCOLOR IN BROWSE BRW_FEXTRA = 14.                     
      END.
      /*SÄTT TILLBAKA TILL TRASNSPARENT*/
      ELSE fastextra.BETECKNING:BGCOLOR IN BROWSE BRW_FEXTRA = 15.
   
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK WINDOW-1
ON CHOOSE OF FBTN_OK IN FRAME FRAME-A /* Ok */
DO:
   {muswait.i}      
   FOR EACH fastextra NO-LOCK:
      IF fastextra.LG1 = TRUE AND fastextra.LG2 = TRUE THEN DO:
         MESSAGE "Du måste välja antingen R eller F. Fastighet: " + fastextra.BETECKNING  VIEW-AS ALERT-BOX.   
         RETURN NO-APPLY.
      END.
   END.      
   RUN fbtnsnabb_UI.
   
   {musarrow.i}
   APPLY "GO" TO FRAME {&FRAME-NAME}.              
   APPLY "CLOSE":U TO THIS-PROCEDURE.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SNABB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SNABB WINDOW-1
ON CHOOSE OF FBTN_SNABB IN FRAME FRAME-A /* Snabbspara */
DO:
   {muswait.i}
   FOR EACH fastextra NO-LOCK:
      IF fastextra.LG1 = TRUE AND fastextra.LG2 = TRUE THEN DO:
         MESSAGE "Du måste välja antingen R eller F. Fastighet: " + fastextra.BETECKNING  VIEW-AS ALERT-BOX.   
         RETURN NO-APPLY.
      END.
   END.      
   RUN fbtnsnabb_UI.
   
   {musarrow.i}   
                  
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
ON CLOSE OF THIS-PROCEDURE DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
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
  
   EMPTY TEMP-TABLE fastextra NO-ERROR. 
   FOR EACH markval NO-LOCK:
      FIND FIRST fastextra WHERE fastextra.BETECKNING = markval.BETECKNING AND fastextra.MARKNR = markval.MARKNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE fastextra THEN DO:
         CREATE fastextra.
         ASSIGN
         fastextra.VARDNR = markval.VARDNR
         fastextra.BETECKNING = markval.BETECKNING
         fastextra.MARKNR = markval.MARKNR
         fastextra.MARKAGARE = markval.MARKAGARE
         fastextra.ANDEL = markval.ANDEL.
      
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
         EMPTY TEMP-TABLE extradatatemp NO-ERROR.
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "VARDFASTAG"     
         inextradatatemp.HUVUDINT = markval.VARDNR
         inextradatatemp.HUVUDCH = markval.BETECKNING.         
         RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
         FIND FIRST extradatatemp WHERE extradatatemp.SOKINT[1] = markval.MARKNR NO-LOCK NO-ERROR.
         IF AVAILABLE extradatatemp THEN DO:      
            ASSIGN
            fastextra.CH1 = extradatatemp.SOKCH[1]
            fastextra.CH2 = extradatatemp.SOKCH[2]
            fastextra.CH3 = extradatatemp.SOKCH[3]
            fastextra.CH4 = extradatatemp.SOKCH[4]
            fastextra.CH5 = extradatatemp.SOKCH[5]
            fastextra.CH6 = extradatatemp.SOKCH[6]
            fastextra.CH7 = extradatatemp.SOKCH[7]
            fastextra.CH8 = extradatatemp.SOKCH[8]
            fastextra.CH9 = extradatatemp.SOKCH[9]
            fastextra.CH10 = extradatatemp.SOKCH[10].
         END.    
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
         EMPTY TEMP-TABLE extradatatemp NO-ERROR.
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "VARDFASTAG2"     
         inextradatatemp.HUVUDINT = markval.VARDNR
         inextradatatemp.HUVUDCH = markval.BETECKNING.
         RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
         FIND FIRST extradatatemp WHERE extradatatemp.SOKINT[4] = markval.MARKNR NO-LOCK NO-ERROR.
         IF AVAILABLE extradatatemp THEN DO:      
            ASSIGN
            fastextra.CH11 = extradatatemp.SOKCH[1]
            /*fastextra.CH12 = extradatatemp.SOKCH[2]*/
            fastextra.CH13 = extradatatemp.SOKCH[3]         
            fastextra.LG1 = extradatatemp.SOKLOG[1]        
            fastextra.LG2 = extradatatemp.SOKLOG[2]
            fastextra.LG3 = extradatatemp.SOKLOG[3]
            fastextra.LG4 = extradatatemp.SOKLOG[4]
            fastextra.INT1 = extradatatemp.SOKINT[1]        
            fastextra.INT2 = extradatatemp.SOKINT[2]
            fastextra.INT3 = extradatatemp.SOKINT[3].
            fastextra.DAT1 = extradatatemp.SOKDATE[1].
         END.
       END.
   END.
   RUN openbdynspec_UI IN brwproc[1].
   IF Guru.Konstanter:varforetypval[48] = 1 THEN DO:   
      ASSIGN fastextra.LG1:VISIBLE IN BROWSE BRW_FEXTRA = TRUE.
      ASSIGN fastextra.LG2:VISIBLE IN BROWSE BRW_FEXTRA = TRUE.      
      ASSIGN fastextra.INT1:VISIBLE IN BROWSE BRW_FEXTRA = TRUE.
      ASSIGN fastextra.INT2:VISIBLE IN BROWSE BRW_FEXTRA = TRUE.
      /*släck VP skog INT3-läggs in i värdering istället. Släck löpnr marägare CH11- tänkt för inläsning  */
      ASSIGN fastextra.INT3:VISIBLE IN BROWSE BRW_FEXTRA = FALSE.
      /*ch11 bytt från "löpnr markägare" till "ersätt senast"  = "mottagit + 90 dagar */
      ASSIGN fastextra.CH11:VISIBLE IN BROWSE BRW_FEXTRA = TRUE.      
      
      /*istället för Ersatt*/
      IF Guru.Konstanter:globforetag = "tect" THEN .
      ELSE fastextra.ch5:LABEL IN BROWSE BRW_FEXTRA = "Intrångsers.".
      IF Guru.Konstanter:globforetag = "tect" THEN fastextra.ch10:LABEL IN BROWSE BRW_FEXTRA = "Märke".
      /*SLÄCK 25% EXPL PÅ ROTNETTO AUTOMATISKT UTRÄKNAT Lena 20180307*/
      ASSIGN fastextra.INT2:READ-ONLY IN BROWSE BRW_FEXTRA = TRUE.
      ASSIGN fastextra.INT2:VISIBLE IN BROWSE BRW_FEXTRA = FALSE.
      IF Guru.Konstanter:globforetag = "tect" THEN ASSIGN fastextra.LG3:VISIBLE IN BROWSE BRW_FEXTRA = TRUE.     
      ELSE ASSIGN fastextra.LG3:VISIBLE IN BROWSE BRW_FEXTRA = false.
   END.
   ELSE DO:
      ASSIGN fastextra.LG1:VISIBLE IN BROWSE BRW_FEXTRA = FALSE.
      ASSIGN fastextra.LG2:VISIBLE IN BROWSE BRW_FEXTRA = FALSE.
      ASSIGN fastextra.LG3:VISIBLE IN BROWSE BRW_FEXTRA = false.
      ASSIGN fastextra.INT1:VISIBLE IN BROWSE BRW_FEXTRA = FALSE.
      ASSIGN fastextra.INT2:VISIBLE IN BROWSE BRW_FEXTRA = FALSE.
      ASSIGN fastextra.INT3:VISIBLE IN BROWSE BRW_FEXTRA = FALSE.
      ASSIGN fastextra.CH11:VISIBLE IN BROWSE BRW_FEXTRA = FALSE.
   END.
   IF Guru.Konstanter:globforetag = "umea" OR Guru.Konstanter:globforetag = "umbr" THEN DO:
      ASSIGN fastextra.DAT1:VISIBLE IN BROWSE BRW_FEXTRA = TRUE.
      ASSIGN fastextra.CH13:VISIBLE IN BROWSE BRW_FEXTRA = TRUE.
      ASSIGN fastextra.LG4:VISIBLE IN BROWSE BRW_FEXTRA = TRUE.
   END.
   ELSE DO:
      ASSIGN fastextra.DAT1:VISIBLE IN BROWSE BRW_FEXTRA = FALSE.
      ASSIGN fastextra.CH13:VISIBLE IN BROWSE BRW_FEXTRA = FALSE.
      ASSIGN fastextra.LG4:VISIBLE IN BROWSE BRW_FEXTRA = FALSE.
   END.   
        
   FIND FIRST fastextra WHERE NO-LOCK NO-ERROR.
   IF AVAILABLE fastextra THEN DO:
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(fastextra)).
      RUN lastselectdyn_UI IN brwproc[1].  
   END.   
   
   RUN enable_UI.   
   {FRMSIZE.I}           
   
   {musarrow.i}
   {WIN_M_SLUT.I}
   {KRYSSBORT.I}

   BRW_FEXTRA:SELECT-FOCUSED-ROW() NO-ERROR.
   APPLY "ENTRY" TO fastextra.ch1 IN BROWSE BRW_FEXTRA.
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
      (INPUT BRW_FEXTRA:HANDLE IN FRAME {&FRAME-NAME}).       
   RUN dynprogextra IN brwproc[1] (INPUT "rowdispkom_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).  
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.   
   RUN spartid_UI.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE brwfextrarowleav_UI WINDOW-1 
PROCEDURE brwfextrarowleav_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF AVAILABLE fastextra THEN DO:
      DISPLAY  fastextra.CH1       
               fastextra.CH2                                
               fastextra.CH7              
               fastextra.CH8       
               fastextra.CH10                  
         WITH BROWSE BRW_FEXTRA.
      
      ASSIGN
      fastextra.CH1    = INPUT BROWSE BRW_FEXTRA fastextra.CH1        
      fastextra.CH2    = INPUT BROWSE BRW_FEXTRA fastextra.CH2         
      fastextra.CH7    = INPUT BROWSE BRW_FEXTRA fastextra.CH7    
      fastextra.CH8    = INPUT BROWSE BRW_FEXTRA fastextra.CH8                     
      fastextra.CH10   = INPUT BROWSE BRW_FEXTRA fastextra.CH10.
      fastextra.CH9 = EDT_ANM.
      
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
  DISPLAY FILL-IN-VFASTIGHET EDT_ANM 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE BRW_FEXTRA EDT_ANM FBTN_SNABB FBTN_OK BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fbtnsnabb_UI WINDOW-1 
PROCEDURE fbtnsnabb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}
   RUN brwfextrarowleav_UI.
   FOR EACH fastextra NO-LOCK:
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "VARDFASTAG"                            
      inextradatatemp.HUVUDINT = fastextra.VARDNR
      inextradatatemp.HUVUDCH = fastextra.BETECKNING
      inextradatatemp.SOKINT[1] = fastextra.MARKNR         
      inextradatatemp.SOKCH[1]  =      fastextra.CH1 
      inextradatatemp.SOKCH[2]  =      fastextra.CH2 
      inextradatatemp.SOKCH[3]  =      fastextra.CH3 
      inextradatatemp.SOKCH[4]  =      fastextra.CH4 
      inextradatatemp.SOKCH[5]  =      fastextra.CH5 
      inextradatatemp.SOKCH[6]  =      fastextra.CH6 
      inextradatatemp.SOKCH[7]  =      fastextra.CH7 
      inextradatatemp.SOKCH[8]  =      fastextra.CH8 
      inextradatatemp.SOKCH[9]  =      fastextra.CH9 
      inextradatatemp.SOKCH[10] =    fastextra.CH10.
      RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).
       
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "VARDFASTAG2"                            
      inextradatatemp.HUVUDINT = fastextra.VARDNR
      inextradatatemp.HUVUDCH = fastextra.BETECKNING
      inextradatatemp.SOKINT[4] = fastextra.MARKNR         
      inextradatatemp.SOKCH[1]  =      fastextra.CH11
      /*inextradatatemp.SOKCH[2]  =      fastextra.CH12*/
      inextradatatemp.SOKCH[3]  =      fastextra.CH13          
      inextradatatemp.SOKLOG[1]  =      fastextra.LG1 
      inextradatatemp.SOKLOG[2] =    fastextra.LG2
      inextradatatemp.SOKLOG[3] =    fastextra.LG3
      inextradatatemp.SOKLOG[4] =    fastextra.LG4
      inextradatatemp.SOKINT[1]  =      fastextra.INT1 
      inextradatatemp.SOKINT[2] =    fastextra.INT2
      inextradatatemp.SOKINT[3] =    fastextra.INT3.
      inextradatatemp.SOKDATE[1] =    fastextra.DAT1.
      RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp). 
           
   END.
   
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispkom_UI WINDOW-1 
PROCEDURE rowdispkom_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER TABLE FOR coltemp.
DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.

   IF AVAILABLE fastextra THEN DO:
      IF fastextra.CH9 NE "" THEN DO:                              
         /*färga grön om Ersatt ifylld*/
         fastextra.BETECKNING:BGCOLOR IN BROWSE BRW_FEXTRA = 14.                     
      END.
      IF fastextra.CH5 NE "" THEN DO:                              
         /*färga fastigheter som har kommentar gula*/
         fastextra.CH5:BGCOLOR IN BROWSE BRW_FEXTRA = 10.                     
      END.
   END.

   IF Guru.Konstanter:globforetag = "tect"  OR Guru.Konstanter:globforetag = "POWE" OR Guru.Konstanter:globforetag = "PFBR" OR Guru.Konstanter:globforetag = "wsp" OR Guru.Konstanter:globforetag = "SSEL" THEN DO:
      fastextra.ch3:FORMAT IN BROWSE BRW_FEXTRA = "9999/99/99" .
      fastextra.ch4:FORMAT IN BROWSE BRW_FEXTRA = "9999/99/99" .
      fastextra.ch5:FORMAT IN BROWSE BRW_FEXTRA = "9999/99/99" .       
      fastextra.ch11:FORMAT IN BROWSE BRW_FEXTRA = "9999/99/99" .
       
  END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selfoc_UI WINDOW-1 
PROCEDURE selfoc_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
/* blir fel när du scrollar i fält!*/
 
   IF AVAILABLE fastextra THEN DO:
      ASSIGN
      EDT_ANM = fastextra.CH9
      FILL-IN-VFASTIGHET = fastextra.BETECKNING.
      DISPLAY EDT_ANM FILL-IN-VFASTIGHET WITH FRAME {&FRAME-NAME}.      
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spartid_UI WINDOW-1 
PROCEDURE spartid_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /*  EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "FAVO"                   
   inextradatatemp.HUVUDCH = Guru.Konstanter:globanv              
   inextradatatemp.HUVUDINT =  ?.   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO:
      spartidvar = extradatatemp.SOKINT[1].    
   END.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   spartidvar = spartidvar * 60 * 1000.*/ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

