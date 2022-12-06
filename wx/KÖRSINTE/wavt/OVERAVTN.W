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

  Created: 95/05/02 -  1:43 pm

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
&Scoped-define NEW NEW
{OAVTEMP.I}
{FORETEMP.I}


DEFINE NEW SHARED VARIABLE brec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE borec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE ovrec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE oavapph AS HANDLE NO-UNDO.
DEFINE BUFFER oavtab FOR OVERAVTAB.

{TIDUTTTNEW.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_OVERAV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES oavtemp

/* Definitions for BROWSE BRW_OVERAV                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_OVERAV oavtemp.KOD oavtemp.DATUM ~
oavtemp.OVERTIDTILL oavtemp.OVERTIDUTTAG oavtemp.START1 oavtemp.STOPP1 ~
oavtemp.START2 oavtemp.STOPP2 oavtemp.EQDAG 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OVERAV oavtemp.KOD oavtemp.DATUM ~
oavtemp.OVERTIDUTTAG oavtemp.START1 oavtemp.STOPP1 oavtemp.START2 ~
oavtemp.STOPP2 oavtemp.EQDAG 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_OVERAV oavtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_OVERAV oavtemp
&Scoped-define QUERY-STRING-BRW_OVERAV FOR EACH oavtemp WHERE TRUE /* Join to ANSTFORMTAB incomplete */ NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_OVERAV OPEN QUERY BRW_OVERAV FOR EACH oavtemp WHERE TRUE /* Join to ANSTFORMTAB incomplete */ NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_OVERAV oavtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OVERAV oavtemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_OVERAV BTN_NY BTN_AND BTN_BORT ~
FILL-IN_DATUM FILL-IN_DAT2 FILL-IN_DAT3 FILL-IN_DAT4 FILL-IN_DAT5 ~
FILL-IN-KOD BTN_AVSL FILL-IN-FRKOD FILL-IN_DAT-7 FILL-IN_DAT-8 ~
FILL-IN-TILLKOD 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_DATUM FILL-IN_DAT2 FILL-IN_DAT3 ~
FILL-IN_DAT4 FILL-IN_DAT5 FILL-IN-KOD FILL-IN-FRKOD FILL-IN_DAT-7 ~
FILL-IN_DAT-8 FILL-IN-TILLKOD 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra":L 
     SIZE 10 BY 1.17.

DEFINE BUTTON BTN_AVSL AUTO-END-KEY 
     LABEL "AVSLUTA":L 
     SIZE 10.5 BY 1.42.

DEFINE BUTTON BTN_BORT 
     LABEL "Bort":L 
     SIZE 10 BY 1.17.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 11 BY 1.17.

DEFINE VARIABLE FILL-IN-FRKOD AS CHARACTER FORMAT "x(2)" 
     LABEL "Från kod" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "x(2)" 
     LABEL "KOD" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-TILLKOD AS CHARACTER FORMAT "x(2)" 
     LABEL "Ny kod" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN_DAT-7 AS DATE FORMAT "99/99/99" 
     LABEL "DATUM" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY .92
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN_DAT-8 AS DATE FORMAT "99/99/99" 
     LABEL "DATUM" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY .92
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN_DAT2 AS DATE FORMAT "99/99/99" 
     LABEL "DATUM" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY .92
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN_DAT3 AS DATE FORMAT "99/99/99" 
     LABEL "DATUM" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY .92
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN_DAT4 AS DATE FORMAT "99/99/99" 
     LABEL "DATUM" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY .92
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN_DAT5 AS DATE FORMAT "99/99/99" 
     LABEL "DATUM" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY .92
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN_DATUM AS DATE FORMAT "99/99/99" 
     LABEL "DATUM" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY .92
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_OVERAV FOR 
      oavtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_OVERAV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OVERAV WINDOW-1 _STRUCTURED
  QUERY BRW_OVERAV NO-LOCK DISPLAY
      oavtemp.KOD FORMAT "x(8)":U
      oavtemp.DATUM FORMAT "99/99/99":U
      oavtemp.OVERTIDTILL FORMAT "x(8)":U
      oavtemp.OVERTIDUTTAG FORMAT "x(8)":U
      oavtemp.START1 FORMAT ">>>>>>9":U
      oavtemp.STOPP1 FORMAT "->>>>>>9":U
      oavtemp.START2 FORMAT "->>>>>>9":U
      oavtemp.STOPP2 FORMAT "->>>>>>9":U
      oavtemp.EQDAG FORMAT "9":U
  ENABLE
      oavtemp.KOD
      oavtemp.DATUM
      oavtemp.OVERTIDUTTAG
      oavtemp.START1
      oavtemp.STOPP1
      oavtemp.START2
      oavtemp.STOPP2
      oavtemp.EQDAG
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 97 BY 15
         TITLE "Avvikelsekalender övertid".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_OVERAV AT ROW 2 COL 4.5 WIDGET-ID 100
     BTN_NY AT ROW 17.38 COL 20
     BTN_AND AT ROW 17.38 COL 35
     BTN_BORT AT ROW 17.38 COL 49.5
     FILL-IN_DATUM AT ROW 20.29 COL 7 COLON-ALIGNED
     FILL-IN_DAT2 AT ROW 20.38 COL 32.5 COLON-ALIGNED
     FILL-IN_DAT3 AT ROW 20.42 COL 49.25 COLON-ALIGNED
     FILL-IN_DAT4 AT ROW 22.71 COL 32.13 COLON-ALIGNED
     FILL-IN_DAT5 AT ROW 22.71 COL 48.88 COLON-ALIGNED
     FILL-IN-KOD AT ROW 22.75 COL 20 COLON-ALIGNED
     BTN_AVSL AT ROW 23.5 COL 83
     FILL-IN-FRKOD AT ROW 24.63 COL 9.75 COLON-ALIGNED
     FILL-IN_DAT-7 AT ROW 24.63 COL 32.88 COLON-ALIGNED
     FILL-IN_DAT-8 AT ROW 24.63 COL 49.63 COLON-ALIGNED
     FILL-IN-TILLKOD AT ROW 24.67 COL 20.75 COLON-ALIGNED
     "Sök kopiera" VIEW-AS TEXT
          SIZE 13.25 BY .88 AT ROW 21.83 COL 4.13
     "Sök byt:" VIEW-AS TEXT
          SIZE 12 BY .67 AT ROW 19.46 COL 21.75
     "Overavtab" VIEW-AS TEXT
          SIZE 12 BY .75 AT ROW 17.5 COL 5
     "Sök på:" VIEW-AS TEXT
          SIZE 12 BY .67 AT ROW 19.42 COL 2.38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 103.13 BY 25.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: oavtemp T "?" NO-UNDO temp-db oavtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Overavtab"
         COLUMN             = 20.75
         ROW                = 7.38
         HEIGHT             = 25
         WIDTH              = 104.5
         MAX-HEIGHT         = 42
         MAX-WIDTH          = 124.38
         VIRTUAL-HEIGHT     = 42
         VIRTUAL-WIDTH      = 124.38
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
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_OVERAV TEXT-2 FRAME-A */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OVERAV
/* Query rebuild information for BROWSE BRW_OVERAV
     _TblList          = "Temp-Tables.oavtemp WHERE RT9.ANSTFORMTAB <external> ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.oavtemp.KOD
"oavtemp.KOD" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.oavtemp.DATUM
"oavtemp.DATUM" ? ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.oavtemp.OVERTIDTILL
     _FldNameList[4]   > Temp-Tables.oavtemp.OVERTIDUTTAG
"oavtemp.OVERTIDUTTAG" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.oavtemp.START1
"oavtemp.START1" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.oavtemp.STOPP1
"oavtemp.STOPP1" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.oavtemp.START2
"oavtemp.START2" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.oavtemp.STOPP2
"oavtemp.STOPP2" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.oavtemp.EQDAG
"oavtemp.EQDAG" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_OVERAV */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND WINDOW-1
ON CHOOSE OF BTN_AND IN FRAME FRAME-A /* Ändra */
DO:
  RUN andra.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSL WINDOW-1
ON CHOOSE OF BTN_AVSL IN FRAME FRAME-A /* AVSLUTA */
DO:
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Bort */
DO:
  RUN bort.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:
  RUN ny.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FRKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FRKOD WINDOW-1
ON LEAVE OF FILL-IN-FRKOD IN FRAME FRAME-A /* Från kod */
DO:
  FILL-IN-KOD = INPUT FILL-IN-KOD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD WINDOW-1
ON LEAVE OF FILL-IN-KOD IN FRAME FRAME-A /* KOD */
DO:
  FILL-IN-KOD = INPUT FILL-IN-KOD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TILLKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TILLKOD WINDOW-1
ON LEAVE OF FILL-IN-TILLKOD IN FRAME FRAME-A /* Ny kod */
DO:
  FILL-IN-KOD = INPUT FILL-IN-KOD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_DAT-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DAT-8 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_DAT-8 IN FRAME FRAME-A /* DATUM */
DO:
   DEFINE VARIABLE posok AS DATE NO-UNDO.   
   FILL-IN-FRKOD = INPUT FILL-IN-FRKOD.
   FILL-IN-TILLKOD = INPUT FILL-IN-TILLKOD.
   FILL-IN_DAT-7 = INPUT FILL-IN_DAT-7.
   FILL-IN_DAT-8 = INPUT FILL-IN_DAT-8.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   ovrec = RECID(OVERAVTAB).
     
   posok = FILL-IN_DAT-7.
  
  FOR EACH OVERAVTAB WHERE OVERAVTAB.DATUM = FILL-IN_DAT-7 
  AND OVERAVTAB.KOD =  FILL-IN-FRKOD USE-INDEX ODATUM EXCLUSIVE-LOCK:
    CREATE oavtab.
    ASSIGN   
     oavtab.KOD  = FILL-IN-TILLKOD
     oavtab.OVERTIDTILL  = OVERAVTAB.OVERTIDTILL
     oavtab.OVERTIDUTTAG  = OVERAVTAB.OVERTIDUTTAG
     oavtab.START1  = OVERAVTAB.START1
     oavtab.START2  = OVERAVTAB.START2
     oavtab.STOPP1  = OVERAVTAB.STOPP1
     oavtab.STOPP2  = OVERAVTAB.STOPP2
     oavtab.DAGEQ  = OVERAVTAB.DAGEQ
     oavtab.EQDAG  = OVERAVTAB.EQDAG
     oavtab.DATUM = FILL-IN_DAT-8.
  END.
   
/*   OPEN QUERY {&BROWSE-NAME} FOR EACH OVERAVTAB NO-LOCK
   BY OVERAVTAB.DATUM BY OVERAVTAB.KOD  BY OVERAVTAB.OVERTIDUTTAG.
   FIND FIRST OVERAVTAB WHERE RECID(OVERAVTAB) = brec NO-LOCK NO-ERROR. 
   IF AVAILABLE OVERAVTAB THEN DO:
     REPOSITION {&BROWSE-NAME} TO RECID brec.
   END.*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_DAT3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DAT3 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_DAT3 IN FRAME FRAME-A /* DATUM */
DO:
   DEFINE VARIABLE posok AS DATE NO-UNDO.   
   
   FILL-IN_DAT2 = INPUT FILL-IN_DAT2.
   FILL-IN_DAT3 = INPUT FILL-IN_DAT3.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   ovrec = RECID(OVERAVTAB).
     
   posok = FILL-IN_DAT2.
/*   FOR EACH OVERAVTAB WHERE OVERAVTAB.DATUM = FILL-IN_DAT2 USE-INDEX ODATUM EXCLUSIVE-LOCK:
     ASSIGN OVERAVTAB.DATUM = FILL-IN_DAT3.
   END.
   OPEN QUERY {&BROWSE-NAME} FOR EACH OVERAVTAB NO-LOCK
   BY OVERAVTAB.DATUM BY OVERAVTAB.KOD  BY OVERAVTAB.OVERTIDUTTAG.
   FIND FIRST OVERAVTAB WHERE RECID(OVERAVTAB) = brec NO-LOCK NO-ERROR. 
   IF AVAILABLE OVERAVTAB THEN DO:
     REPOSITION {&BROWSE-NAME} TO RECID brec.
   END.*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_DAT5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DAT5 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_DAT5 IN FRAME FRAME-A /* DATUM */
DO:
   DEFINE VARIABLE posok AS DATE NO-UNDO.   
   FILL-IN-KOD = INPUT FILL-IN-KOD.
   FILL-IN_DAT4 = INPUT FILL-IN_DAT4.
   FILL-IN_DAT5 = INPUT FILL-IN_DAT5.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   ovrec = RECID(OVERAVTAB).
     
   posok = FILL-IN_DAT4.
   IF FILL-IN-KOD NE "" THEN DO:
      FOR EACH OVERAVTAB WHERE OVERAVTAB.DATUM = FILL-IN_DAT4 
      AND OVERAVTAB.KOD =  FILL-IN-KOD USE-INDEX ODATUM EXCLUSIVE-LOCK:
        CREATE oavtab.
        ASSIGN   
         oavtab.KOD  = OVERAVTAB.KOD
         oavtab.OVERTIDTILL  = OVERAVTAB.OVERTIDTILL
         oavtab.OVERTIDUTTAG  = OVERAVTAB.OVERTIDUTTAG
         oavtab.START1  = OVERAVTAB.START1
         oavtab.START2  = OVERAVTAB.START2
         oavtab.STOPP1  = OVERAVTAB.STOPP1
         oavtab.STOPP2  = OVERAVTAB.STOPP2
         oavtab.DAGEQ  = OVERAVTAB.DAGEQ
         oavtab.EQDAG  = OVERAVTAB.EQDAG
         oavtab.DATUM = FILL-IN_DAT5.
      END.
   END.
   ELSE DO:
      FOR EACH OVERAVTAB WHERE OVERAVTAB.DATUM = FILL-IN_DAT4 
      USE-INDEX ODATUM EXCLUSIVE-LOCK:
        CREATE oavtab.
        ASSIGN   
         oavtab.KOD  = OVERAVTAB.KOD
         oavtab.OVERTIDTILL  = OVERAVTAB.OVERTIDTILL
         oavtab.OVERTIDUTTAG  = OVERAVTAB.OVERTIDUTTAG
         oavtab.START1  = OVERAVTAB.START1
         oavtab.START2  = OVERAVTAB.START2
         oavtab.STOPP1  = OVERAVTAB.STOPP1
         oavtab.STOPP2  = OVERAVTAB.STOPP2
         oavtab.DAGEQ  = OVERAVTAB.DAGEQ
         oavtab.EQDAG  = OVERAVTAB.EQDAG
         oavtab.DATUM = FILL-IN_DAT5.
      END.
   END.   
   /*OPEN QUERY {&BROWSE-NAME} FOR EACH OVERAVTAB NO-LOCK
   BY OVERAVTAB.DATUM BY OVERAVTAB.KOD  BY OVERAVTAB.OVERTIDUTTAG.
   FIND FIRST OVERAVTAB WHERE RECID(OVERAVTAB) = brec NO-LOCK NO-ERROR. 
   IF AVAILABLE OVERAVTAB THEN DO:
     REPOSITION {&BROWSE-NAME} TO RECID brec.
   END.*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_DATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DATUM WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_DATUM IN FRAME FRAME-A /* DATUM */
DO:
   DEFINE VARIABLE posok AS DATE NO-UNDO.   
   
   FILL-IN_DATUM = INPUT FILL-IN_DATUM.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   ovrec = RECID(OVERAVTAB).
/*   IF FILL-IN_DATUM = " THEN DO:
      MESSAGE "SÖKBEGREPPET KAN INTE VARA BLANKT" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_DATUM IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.*/      
   posok = FILL-IN_DATUM.

   FIND OVERAVTAB WHERE RECID(OVERAVTAB) = ovrec NO-LOCK NO-ERROR.
   FIND NEXT OVERAVTAB WHERE  OVERAVTAB.DATUM = posok       
   USE-INDEX ODATUM NO-LOCK NO-ERROR.
   IF NOT AVAILABLE OVERAVTAB THEN DO:
      MESSAGE "DET FINNS INGEN PÅ SÖKBEGREPPET" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_DATUM IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   IF AVAILABLE OVERAVTAB THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID RECID(OVERAVTAB).
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_OVERAV
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   IF VALID-HANDLE(oavapph) THEN DELETE PROCEDURE oavapph.
   {BORTBRWPROC.I}
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
   RUN ftghmt_UI IN oavapph (OUTPUT TABLE foretemp).
   
   RUN enable_UI.   
   {FRMSIZE.I}  
   RUN oavhmt_UI IN oavapph (OUTPUT TABLE oavtemp).             
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   {musarrow.i}
   {WIN_M_SLUT.I}
 
 
   
  /*RUN enable_UI.
  APPLY "HOME" TO {&BROWSE-NAME}.
  status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
  brec = RECID(OVERAVTAB).*/
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-1 
PROCEDURE allstartbrw_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_OVERAV:HANDLE IN FRAME {&FRAME-NAME}).      
   
   IF Guru.Konstanter:appcon THEN DO:      
      RUN AVTUPPG.P PERSISTENT SET oavapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN AVTUPPG.P PERSISTENT SET oavapph.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra WINDOW-1 
PROCEDURE andra :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  {muswait.i}
  RUN OAVTNY.W (INPUT RECID(OVERAVTAB)). 
  {musarrow.i}
  IF musz = FALSE THEN DO: 
   /* OPEN QUERY {&BROWSE-NAME} FOR EACH OVERAVTAB NO-LOCK
    BY OVERAVTAB.DATUM BY OVERAVTAB.KOD  BY OVERAVTAB.OVERTIDUTTAG.
    FIND FIRST OVERAVTAB WHERE RECID(OVERAVTAB) = brec NO-LOCK NO-ERROR. 
    IF AVAILABLE OVERAVTAB THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID brec.
    END.*/
  END.
  musz = FALSE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort WINDOW-1 
PROCEDURE bort :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
MESSAGE "VILL DU VERKLIGEN TA BORT DENNA OVERAVTAB?"
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE OVERAVTAB.OVERTIDTILL
  UPDATE answer AS LOGICAL.
  IF answer THEN DO TRANSACTION:
    {muswait.i}
/*    FIND OVERAVTAB WHERE RECID(OVERAVTAB) = brec NO-LOCK NO-ERROR.
    FIND NEXT OVERAVTAB NO-LOCK NO-ERROR.
    IF AVAILABLE OVERAVTAB THEN borec = RECID(OVERAVTAB).
    ELSE DO:
      FIND OVERAVTAB WHERE RECID(OVERAVTAB) = brec NO-LOCK NO-ERROR.
      FIND PREV OVERAVTAB NO-LOCK NO-ERROR.
      IF AVAILABLE OVERAVTAB THEN borec = RECID(OVERAVTAB).
    END.
    FIND OVERAVTAB WHERE RECID(OVERAVTAB) = brec EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE OVERAVTAB THEN DELETE OVERAVTAB.
    OPEN QUERY {&BROWSE-NAME} FOR EACH OVERAVTAB NO-LOCK
    BY OVERAVTAB.DATUM BY OVERAVTAB.KOD  BY OVERAVTAB.OVERTIDUTTAg. 
    brec = borec.
    FIND FIRST OVERAVTAB WHERE RECID(OVERAVTAB) = brec NO-LOCK NO-ERROR.  
    IF AVAILABLE OVERAVTAB THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID brec.
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}. 
    END.*/  
    {musarrow.i}   
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
  DISPLAY FILL-IN_DATUM FILL-IN_DAT2 FILL-IN_DAT3 FILL-IN_DAT4 FILL-IN_DAT5 
          FILL-IN-KOD FILL-IN-FRKOD FILL-IN_DAT-7 FILL-IN_DAT-8 FILL-IN-TILLKOD 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE BRW_OVERAV BTN_NY BTN_AND BTN_BORT FILL-IN_DATUM FILL-IN_DAT2 
         FILL-IN_DAT3 FILL-IN_DAT4 FILL-IN_DAT5 FILL-IN-KOD BTN_AVSL 
         FILL-IN-FRKOD FILL-IN_DAT-7 FILL-IN_DAT-8 FILL-IN-TILLKOD 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW WINDOW-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny WINDOW-1 
PROCEDURE ny :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  {muswait.i}
  RUN OAVTNY.W (INPUT ?).
  {musarrow.i}
   IF musz = FALSE THEN DO: 
/*    OPEN QUERY {&BROWSE-NAME} FOR EACH OVERAVTAB NO-LOCK
    BY OVERAVTAB.DATUM BY OVERAVTAB.KOD  BY OVERAVTAB.OVERTIDUTTAG.
    FIND FIRST OVERAVTAB WHERE RECID(OVERAVTAB) = brec NO-LOCK NO-ERROR. 
    IF AVAILABLE OVERAVTAB THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID brec.
    END.*/
  END.
  musz = FALSE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

