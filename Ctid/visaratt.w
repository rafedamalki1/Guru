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

  Created: 04/08/97 -  8:47 am

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
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{TIDALLT.I}
DEFINE TEMP-TABLE orginaltemp NO-UNDO LIKE extratidallt.
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER valet AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER tidalltrec AS RECID NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tidallt.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{TIDFALLT.I}
&Scoped-define NEW
&Scoped-define SHARED SHARED
{TIDPERS.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
{OMRTEMPW.I}
{FLEXTAB.I}
{PHMT.I}
DEFINE NEW SHARED VARIABLE brwbdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE brwavdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE varfabef AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE korda AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE debitering AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE datvar AS DATE NO-UNDO.
DEFINE SHARED VARIABLE startvar AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE slutvar AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE varlon AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE varlant AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE dirtidoveraknare AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE dirtid AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE dirtidrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE allatidvar AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE perssokrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vaxla AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE flexav AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vartgamla AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE tidsedrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE tidsedlog AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE bilforare AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE VARIABLE allatider AS INTEGER NO-UNDO.
DEFINE VARIABLE qhandle AS HANDLE NO-UNDO.

{TIDUTTTNEW.I}

DEFINE NEW SHARED VARIABLE RAD_TIDSVAL AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Alla tidsedlar för perioden", 1,
"Godkända", 2,
"Ej godkända", 3
     SIZE 31.5 BY 2
     BGCOLOR 8  NO-UNDO.
DEFINE NEW SHARED VARIABLE TOGGLE-MONTH AS LOGICAL INITIAL ? 
     LABEL "Två tidsedlar vid månadasskifte":L 
     VIEW-AS TOGGLE-BOX
     SIZE 34.5 BY 1 NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_FEL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidfeltemp

/* Definitions for BROWSE BRW_FEL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_FEL tidfeltemp.DATUM tidfeltemp.DAG ~
tidfeltemp.PERSONALKOD tidfeltemp.START tidfeltemp.SLUT tidfeltemp.VILART ~
tidfeltemp.LONTILLANTAL tidfeltemp.TRAKTANTAL tidfeltemp.BERANTAL ~
tidfeltemp.AONR tidfeltemp.DELNR tidfeltemp.BEREDSKAPSTART ~
tidfeltemp.BEREDSKAPSLUT tidfeltemp.TOTALT tidfeltemp.DEBET ~
tidfeltemp.FELDATUM tidfeltemp.FELKORD tidfeltemp.FELANVAND ~
tidfeltemp.VIBEFATTNING tidfeltemp.PRISTYP tidfeltemp.PRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FEL tidfeltemp.DATUM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_FEL tidfeltemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_FEL tidfeltemp
&Scoped-define QUERY-STRING-BRW_FEL FOR EACH tidfeltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_FEL OPEN QUERY BRW_FEL FOR EACH tidfeltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_FEL tidfeltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FEL tidfeltemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_FEL BTN_SKRIV BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-PKOD FILL-IN_FORNAMN-2 

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

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-MANAD AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_AR AS INTEGER FORMAT "9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN-2 AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_FEL FOR 
      tidfeltemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_FEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FEL WINDOW-1 _STRUCTURED
  QUERY BRW_FEL NO-LOCK DISPLAY
      tidfeltemp.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      tidfeltemp.DAG COLUMN-LABEL "Dag" FORMAT "x(3)":U
      tidfeltemp.PERSONALKOD COLUMN-LABEL "Enhet!/Sign" FORMAT "x(5)":U
            WIDTH 6
      tidfeltemp.START COLUMN-LABEL "Start!tid" FORMAT "99.99":U
      tidfeltemp.SLUT COLUMN-LABEL "Slut!tid" FORMAT "99.99":U
      tidfeltemp.VILART COLUMN-LABEL "Lart" FORMAT "X(4)":U
      tidfeltemp.LONTILLANTAL COLUMN-LABEL "Antal" FORMAT "->>>9.<<":U
      tidfeltemp.TRAKTANTAL COLUMN-LABEL "Antal" FORMAT "-99.9":U
      tidfeltemp.BERANTAL COLUMN-LABEL "Antal" FORMAT "-99.99":U
      tidfeltemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      tidfeltemp.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      tidfeltemp.BEREDSKAPSTART COLUMN-LABEL "Start" FORMAT "99.99":U
      tidfeltemp.BEREDSKAPSLUT COLUMN-LABEL "Slut" FORMAT "99.99":U
      tidfeltemp.TOTALT COLUMN-LABEL "Timmar!100" FORMAT "-99.99":U
      tidfeltemp.DEBET FORMAT "Ja/Nej":U
      tidfeltemp.FELDATUM COLUMN-LABEL "Rättad" FORMAT "99/99/99":U
      tidfeltemp.FELKORD COLUMN-LABEL "Körd" FORMAT "x(8)":U
      tidfeltemp.FELANVAND FORMAT "X(12)":U
      tidfeltemp.VIBEFATTNING COLUMN-LABEL "Befattning" FORMAT "X(15)":U
      tidfeltemp.PRISTYP COLUMN-LABEL "Pristyp" FORMAT "X(10)":U
      tidfeltemp.PRIS COLUMN-LABEL "Pris" FORMAT ">>>>9":U
  ENABLE
      tidfeltemp.DATUM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 81.5 BY 9.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     FILL-IN-PKOD AT ROW 1.67 COL 11.63 COLON-ALIGNED
     FILL-IN_FORNAMN-2 AT ROW 1.67 COL 20.63 COLON-ALIGNED NO-LABEL
     FILL-IN_AR AT ROW 3.13 COL 16.38 COLON-ALIGNED NO-LABEL
     FILL-IN-MANAD AT ROW 3.13 COL 25.75 COLON-ALIGNED NO-LABEL
     BRW_FEL AT ROW 4.92 COL 1.5
     BTN_SKRIV AT ROW 6.67 COL 84
     BTN_AVB AT ROW 14.67 COL 84
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98 BY 15.08.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: tidfeltemp T "?" NO-UNDO temp-db tidfeltemp
      TABLE: ? T "?" NO-UNDO temp-db overtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Visa rättning"
         HEIGHT             = 15.08
         WIDTH              = 98.5
         MAX-HEIGHT         = 23.58
         MAX-WIDTH          = 98.63
         VIRTUAL-HEIGHT     = 23.58
         VIRTUAL-WIDTH      = 98.63
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
  NOT-VISIBLE,                                                          */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_FEL FILL-IN-MANAD FRAME-A */
/* SETTINGS FOR FILL-IN FILL-IN-MANAD IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-PKOD IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_AR IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_AR:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN-2 IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FEL
/* Query rebuild information for BROWSE BRW_FEL
     _TblList          = "Temp-Tables.tidfeltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.tidfeltemp.DATUM
"DATUM" "Datum" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tidfeltemp.DAG
"DAG" "Dag" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tidfeltemp.PERSONALKOD
"PERSONALKOD" "Enhet!/Sign" ? "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.tidfeltemp.START
"START" "Start!tid" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.tidfeltemp.SLUT
"SLUT" "Slut!tid" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.tidfeltemp.VILART
"VILART" "Lart" "X(4)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.tidfeltemp.LONTILLANTAL
"LONTILLANTAL" "Antal" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.tidfeltemp.TRAKTANTAL
"TRAKTANTAL" "Antal" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.tidfeltemp.BERANTAL
"BERANTAL" "Antal" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.tidfeltemp.AONR
"AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.tidfeltemp.DELNR
"DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.tidfeltemp.BEREDSKAPSTART
"BEREDSKAPSTART" "Start" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-Tables.tidfeltemp.BEREDSKAPSLUT
"BEREDSKAPSLUT" "Slut" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Temp-Tables.tidfeltemp.TOTALT
"TOTALT" "Timmar!100" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   = Temp-Tables.tidfeltemp.DEBET
     _FldNameList[16]   > Temp-Tables.tidfeltemp.FELDATUM
"FELDATUM" "Rättad" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > Temp-Tables.tidfeltemp.FELKORD
"FELKORD" "Körd" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > Temp-Tables.tidfeltemp.FELANVAND
"FELANVAND" ? "X(12)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > Temp-Tables.tidfeltemp.VIBEFATTNING
"VIBEFATTNING" "Befattning" "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > Temp-Tables.tidfeltemp.PRISTYP
"PRISTYP" "Pristyp" "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > Temp-Tables.tidfeltemp.PRIS
"PRIS" "Pris" ">>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FEL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO: 
   ASSIGN
   flexav = FALSE  /*obs lena flex*/
   tidtabrec = ?
   tidtabrec2 = ?.    
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-1
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO:        
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:   
      {muswait.i}
      FIND FIRST tidallt WHERE tidallt.RECTIDVIS = tidalltrec NO-LOCK NO-ERROR.
      IF AVAILABLE tidallt THEN DO:
         FOR EACH orginaltemp:
            DELETE orginaltemp.
         END.
         CREATE orginaltemp.
         BUFFER-COPY tidallt TO orginaltemp.          
         FOR EACH extratidallt:
            DELETE extratidallt.
         END.
         qhandle = BRW_FEL:QUERY.
         qhandle:GET-FIRST(NO-LOCK).
         DO WHILE qhandle:QUERY-OFF-END = FALSE:
            CREATE extratidallt.
            BUFFER-COPY tidfeltemp TO extratidallt.
            extratidallt.RECTIDVIS = orginaltemp.RECTIDVIS.
            qhandle:GET-NEXT(NO-LOCK).
         END.
         skrivut = TRUE.
         RUN RATTVISA.W (INPUT pkod,INPUT allatider, INPUT TABLE extratidallt, INPUT TABLE orginaltemp).
      END.  
      {musarrow.i}
   END.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-1
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_FEL
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
   ASSIGN   
   korda = 0
   tidtabrec = 0
   bilforare = FALSE
   brwbdatum = bdatum
   allatider = valet.
   brwbdatum = DATE(MONTH(bdatum),01,YEAR(bdatum)).
   brwavdatum = avdatum.    
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN TIDFHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 1,INPUT pkod,INPUT datvar,INPUT brwavdatum, 
      INPUT-OUTPUT TABLE tidfeltemp).
   END.
   ELSE DO:
      RUN TIDFHMT.P  
      (INPUT 1,INPUT pkod,INPUT datvar,INPUT brwavdatum, 
      INPUT-OUTPUT TABLE tidfeltemp).
   END.
   FIND FIRST tidpers WHERE tidpers.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   persrec = tidpers.TIDPERSREC.
   FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod 
   NO-LOCK NO-ERROR.
   FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.       
   regmnr = MONTH(bdatum).
   RUN MANNAMN.P.
   ASSIGN
   vart = "AND"  
   FILL-IN_FORNAMN-2 = tidpers.FORNAMN + " " + tidpers.EFTERNAMN      
   FILL-IN-PKOD = tidpers.PERSONALKOD
   FILL-IN_AR = YEAR(bdatum)
   FILL-IN-MANAD = regmannamn.        
   RUN enable_UI.   
   {FRMSIZE.I}  
   RUN open_UI.
   DISPLAY FILL-IN_AR FILL-IN-MANAD WITH FRAME {&FRAME-NAME}. 
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
   tidfeltemp.DATUM:READ-ONLY IN BROWSE BRW_FEL = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_FEL:HANDLE IN FRAME {&FRAME-NAME}).      
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
  DISPLAY FILL-IN-PKOD FILL-IN_FORNAMN-2 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE BRW_FEL BTN_SKRIV BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open_UI WINDOW-1 
PROCEDURE open_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/     
   DEFINE VARIABLE tempvar AS CHARACTER NO-UNDO.
   ASSIGN
   tidfeltemp.START:VISIBLE IN BROWSE BRW_FEL = FALSE
   tidfeltemp.SLUT:VISIBLE IN BROWSE BRW_FEL = FALSE
   tidfeltemp.VILART:VISIBLE IN BROWSE BRW_FEL = FALSE
   tidfeltemp.LONTILLANTAL:VISIBLE IN BROWSE BRW_FEL = FALSE
   tidfeltemp.TRAKTANTAL:VISIBLE IN BROWSE BRW_FEL = FALSE
   tidfeltemp.BERANTAL:VISIBLE IN BROWSE BRW_FEL = FALSE
   tidfeltemp.BEREDSKAPSTART:VISIBLE IN BROWSE BRW_FEL = FALSE
   tidfeltemp.BEREDSKAPSLUT:VISIBLE IN BROWSE BRW_FEL = FALSE
   tidfeltemp.TOTALT:VISIBLE IN BROWSE BRW_FEL = FALSE.
   IF allatider = 1 THEN DO:   
      tempvar = "tidfeltemp.DATUM = " + STRING(datvar) + " AND tidfeltemp.START >= " + STRING(startvar) +
         " AND tidfeltemp.SLUT <= " + STRING(slutvar) + " AND tidfeltemp.TIDLOG = TRUE AND tidfeltemp.DEBET = TRUE".
      RUN setcolsortvar_UI IN brwproc[1] (INPUT tempvar).
      ASSIGN
      tidfeltemp.START:VISIBLE IN BROWSE BRW_FEL = TRUE
      tidfeltemp.SLUT:VISIBLE IN BROWSE BRW_FEL = TRUE
      tidfeltemp.TOTALT:VISIBLE IN BROWSE BRW_FEL = TRUE.   
   END.
   ELSE IF allatider = 2 THEN DO:   
      tempvar = "tidfeltemp.DATUM = " + STRING(datvar) + " AND tidfeltemp.LONTILLAGG >= STRING(" + STRING(varlon) + 
         ") AND STRING(tidfeltemp.LONTILLANTAL) <= STRING(" + STRING(varlant) + ") AND tidfeltemp.TIDLOG = FALSE AND tidfeltemp.DEBET = TRUE".
      RUN setcolsortvar_UI IN brwproc[1] (INPUT tempvar).
      ASSIGN
      tidfeltemp.VILART:VISIBLE IN BROWSE BRW_FEL = TRUE
      tidfeltemp.LONTILLANTAL:VISIBLE IN BROWSE BRW_FEL = TRUE.     
   END.
   ELSE IF allatider = 3 THEN DO:   
      tempvar = "tidfeltemp.DATUM = " + STRING(datvar) + " AND tidfeltemp.BEREDSKAP >= STRING(" + STRING(varlon) +
         ") AND tidfeltemp.BERANTAL <= STRING(" + STRING(varlant) + ") AND tidfeltemp.TIDLOG = FALSE AND tidfeltemp.DEBET = TRUE".
      RUN setcolsortvar_UI IN brwproc[1] (INPUT tempvar).
      ASSIGN
      tidfeltemp.BERANTAL:VISIBLE IN BROWSE BRW_FEL = TRUE
      tidfeltemp.BEREDSKAPSTART:VISIBLE IN BROWSE BRW_FEL = TRUE
      tidfeltemp.BEREDSKAPSLUT:VISIBLE IN BROWSE BRW_FEL = TRUE.     
   END.
   ELSE IF allatider = 4 THEN DO:   
      tempvar = "tidfeltemp.DATUM = " + STRING(datvar) + " AND tidfeltemp.TRAKTKOD >= STRING(" + STRING(varlon) +
         ") AND tidfeltemp.TRAKTANTAL <= STRING(" + STRING(varlant) + ") AND tidfeltemp.TIDLOG = FALSE AND tidfeltemp.DEBET = TRUE".
      RUN setcolsortvar_UI IN brwproc[1] (INPUT tempvar).
      ASSIGN
      tidfeltemp.VILART:VISIBLE IN BROWSE BRW_FEL = TRUE
      tidfeltemp.TRAKTANTAL:VISIBLE IN BROWSE BRW_FEL = TRUE.   
   END. 
   ENABLE BRW_FEL WITH FRAME {&FRAME-NAME}.      
   RUN openbdynspec_UI IN brwproc[1].
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

