&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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
{ALLDEF.I}
{DIRDEF.I}
DEFINE INPUT PARAMETER vallista AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR uppvaltemp.
DEFINE INPUT PARAMETER TABLE FOR valdaao.
&Scoped-define NEW  
{REGVAR.I}
{FAKTTEMP.I}
{GLOBVAR2DEL1.I}
{VISUPPTMP.I} 
{EXECLIN.I}
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE linkhj AS CHARACTER NO-UNDO.
DEFINE VARIABLE brwallth AS HANDLE NO-UNDO.
DEFINE VARIABLE brwgkosth AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_ALLT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES sumpers gfaktemp

/* Definitions for BROWSE BRW_ALLT                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_ALLT sumpers.VIBEFATTNING sumpers.AONR ~
sumpers.DELNR sumpers.TIMMAR sumpers.OTIMMAR sumpers.BELOPP 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ALLT 
&Scoped-define QUERY-STRING-BRW_ALLT FOR EACH sumpers NO-LOCK ~
    BY sumpers.AONR ~
       BY sumpers.DELNR ~
        BY sumpers.VIBEFATTNING
&Scoped-define OPEN-QUERY-BRW_ALLT OPEN QUERY BRW_ALLT FOR EACH sumpers NO-LOCK ~
    BY sumpers.AONR ~
       BY sumpers.DELNR ~
        BY sumpers.VIBEFATTNING.
&Scoped-define TABLES-IN-QUERY-BRW_ALLT sumpers
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ALLT sumpers


/* Definitions for BROWSE BRW_GKOST                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_GKOST gfaktemp.TYPTEXT gfaktemp.AONR ~
gfaktemp.DELNR gfaktemp.TOTALT gfaktemp.MTRL gfaktemp.OVRIG ~
gfaktemp.ARBKOST gfaktemp.OBELOPP gfaktemp.TRAKT gfaktemp.LONTILL ~
gfaktemp.RES gfaktemp.KBELOPP 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_GKOST 
&Scoped-define QUERY-STRING-BRW_GKOST FOR EACH gfaktemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_GKOST OPEN QUERY BRW_GKOST FOR EACH gfaktemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_GKOST gfaktemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_GKOST gfaktemp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_GKOST BRW_ALLT BTN_EXCEL BTN_SKRIV ~
BTN_AVS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 15 BY 1.

DEFINE BUTTON BTN_EXCEL 
     LABEL "excel" 
     SIZE 15 BY 1.

DEFINE BUTTON BTN_SKRIV 
     LABEL "SKRIV UT" 
     SIZE 15 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ALLT FOR 
      sumpers SCROLLING.

DEFINE QUERY BRW_GKOST FOR 
      gfaktemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ALLT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ALLT C-Win _STRUCTURED
  QUERY BRW_ALLT DISPLAY
      sumpers.VIBEFATTNING COLUMN-LABEL "Rubrik" FORMAT "x(30)":U
      sumpers.AONR COLUMN-LABEL "Aonr" FORMAT "X(7)":U
      sumpers.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      sumpers.TIMMAR COLUMN-LABEL "Antal" FORMAT "->>>>>>>9.99":U
      sumpers.OTIMMAR COLUMN-LABEL "A´pris" FORMAT "->>>>9":U
      sumpers.BELOPP COLUMN-LABEL "Summa" FORMAT "->>>>>>>>>>9":U
            COLUMN-FGCOLOR 9 LABEL-FGCOLOR 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 80 BY 15.75.

DEFINE BROWSE BRW_GKOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_GKOST C-Win _STRUCTURED
  QUERY BRW_GKOST DISPLAY
      gfaktemp.TYPTEXT FORMAT "X(256)":U WIDTH 25
      gfaktemp.AONR FORMAT "x(8)":U
      gfaktemp.DELNR FORMAT "->,>>>,>>9":U
      gfaktemp.TOTALT FORMAT "->>>>>>>9":U COLUMN-FGCOLOR 9 LABEL-FGCOLOR 9
      gfaktemp.MTRL COLUMN-LABEL "Materiel!kostnad" FORMAT "->>>>>>>9":U
      gfaktemp.OVRIG COLUMN-LABEL "Övrig!kostnad" FORMAT "->>>>>>9":U
      gfaktemp.ARBKOST COLUMN-LABEL "Arbets!kostnad" FORMAT "->>>>>>>9":U
      gfaktemp.OBELOPP COLUMN-LABEL "Övertid!kostnad" FORMAT "->>>>>>9":U
      gfaktemp.TRAKT COLUMN-LABEL "Trakt.!kostnad" FORMAT "->>>>>9":U
      gfaktemp.LONTILL COLUMN-LABEL "Lönetill.!kostnad" FORMAT "->>>>>>9":U
      gfaktemp.RES COLUMN-LABEL "Res!kostnad" FORMAT "->>>>>>9":U
      gfaktemp.KBELOPP COLUMN-LABEL "Fr.tjänst!kost." FORMAT "->>>>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 120 BY 7.88.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_GKOST AT ROW 2.38 COL 1.5 WIDGET-ID 200
     BRW_ALLT AT ROW 12 COL 1.5 WIDGET-ID 300
     BTN_EXCEL AT ROW 28.25 COL 80.38 WIDGET-ID 4
     BTN_SKRIV AT ROW 28.25 COL 95.63 WIDGET-ID 6
     BTN_AVS AT ROW 28.25 COL 110.63 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125.25 BY 28.54 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: gfaktemp T "?" NO-UNDO temp-db gfaktemp
      TABLE: sumpers T "?" NO-UNDO temp-db sumpers
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Uppföljning"
         HEIGHT             = 28.54
         WIDTH              = 125.25
         MAX-HEIGHT         = 42.42
         MAX-WIDTH          = 240
         VIRTUAL-HEIGHT     = 42.42
         VIRTUAL-WIDTH      = 240
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
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_GKOST 1 DEFAULT-FRAME */
/* BROWSE-TAB BRW_ALLT BRW_GKOST DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ALLT
/* Query rebuild information for BROWSE BRW_ALLT
     _TblList          = "Temp-Tables.sumpers"
     _OrdList          = "Temp-Tables.sumpers.AONR|yes,Temp-Tables.sumpers.DELNR|yes,Temp-Tables.sumpers.VIBEFATTNING|yes"
     _FldNameList[1]   > Temp-Tables.sumpers.VIBEFATTNING
"sumpers.VIBEFATTNING" "Rubrik" "x(30)" "character" ? ? ? ? ? ? no "" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.sumpers.AONR
"sumpers.AONR" "Aonr" "X(7)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.sumpers.DELNR
"sumpers.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.sumpers.TIMMAR
"sumpers.TIMMAR" "Antal" "->>>>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.sumpers.OTIMMAR
"sumpers.OTIMMAR" "A´pris" "->>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.sumpers.BELOPP
"sumpers.BELOPP" "Summa" "->>>>>>>>>>9" "decimal" ? 9 ? ? 9 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ALLT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_GKOST
/* Query rebuild information for BROWSE BRW_GKOST
     _TblList          = "Temp-Tables.gfaktemp"
     _FldNameList[1]   > Temp-Tables.gfaktemp.TYPTEXT
"gfaktemp.TYPTEXT" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = Temp-Tables.gfaktemp.AONR
     _FldNameList[3]   = Temp-Tables.gfaktemp.DELNR
     _FldNameList[4]   > Temp-Tables.gfaktemp.TOTALT
"gfaktemp.TOTALT" ? ? "integer" ? 9 ? ? 9 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.gfaktemp.MTRL
"gfaktemp.MTRL" "Materiel!kostnad" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.gfaktemp.OVRIG
"gfaktemp.OVRIG" "Övrig!kostnad" "->>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.gfaktemp.ARBKOST
"gfaktemp.ARBKOST" "Arbets!kostnad" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.gfaktemp.OBELOPP
"gfaktemp.OBELOPP" "Övertid!kostnad" "->>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.gfaktemp.TRAKT
"gfaktemp.TRAKT" "Trakt.!kostnad" "->>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.gfaktemp.LONTILL
"gfaktemp.LONTILL" "Lönetill.!kostnad" "->>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.gfaktemp.RES
"gfaktemp.RES" "Res!kostnad" "->>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.gfaktemp.KBELOPP
"gfaktemp.KBELOPP" "Fr.tjänst!kost." ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_GKOST */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Uppföljning */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Uppföljning */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ALLT
&Scoped-define SELF-NAME BRW_ALLT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_ALLT C-Win
ON VALUE-CHANGED OF BRW_ALLT IN FRAME DEFAULT-FRAME
DO:
   status-ok = BRW_ALLT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_GKOST
&Scoped-define SELF-NAME BRW_GKOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_GKOST C-Win
ON VALUE-CHANGED OF BRW_GKOST IN FRAME DEFAULT-FRAME
DO:
   status-ok = BRW_GKOST:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS C-Win
ON CHOOSE OF BTN_AVS IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXCEL C-Win
ON CHOOSE OF BTN_EXCEL IN FRAME DEFAULT-FRAME /* excel */
DO:
   RUN btnexcel_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV C-Win
ON CHOOSE OF BTN_SKRIV IN FRAME DEFAULT-FRAME /* SKRIV UT */
DO:
  RUN btnskriv_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ALLT
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {WIN_M_START.I}      
   {ALLSTARTDYN.I}   
    {FRMSIZE.I}
   FIND FIRST visaupp WHERE visaupp.UPPFOLJVAL = vallista NO-LOCK NO-ERROR.
   FIND FIRST uppvaltemp WHERE uppvaltemp.VALDLISTA = visaupp.UT NO-LOCK NO-ERROR.
   {&WINDOW-NAME}:TITLE = visaupp.UT.
   EMPTY TEMP-TABLE sumpers NO-ERROR. 
   EMPTY TEMP-TABLE gfaktemp NO-ERROR. 
  
   FOR EACH valdaao WHERE NO-LOCK:
      IF Guru.Konstanter:appcon THEN DO:
         RUN ProjKosttUppApp.p ON Guru.Konstanter:apphand TRANSACTION DISTINCT     
         (INPUT valdaao.AONR,INPUT valdaao.DELNR,INPUT TABLE uppvaltemp,OUTPUT TABLE sumpers APPEND,OUTPUT TABLE gfaktemp APPEND).
      END.
      ELSE DO:
         RUN ProjKosttUppApp.p
        (INPUT valdaao.AONR,INPUT valdaao.DELNR,INPUT TABLE uppvaltemp,OUTPUT TABLE sumpers APPEND,OUTPUT TABLE gfaktemp APPEND).
      END. 
   END.
 
   RUN enable_UI.
   RUN setcolindex_UI IN brwproc[1] (INPUT "sumpers.AONR BY sumpers.DELNR BY sumpers.ORDNING").
   RUN setcolindex_UI IN brwproc[2] (INPUT "gfaktemp.AONR BY gfaktemp.DELNR").
   RUN openbdynspec_UI IN brwproc[1].
   RUN openbdynspec_UI IN brwproc[2].
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN DYNBRW.P PERSISTENT SET brwproc[1] (INPUT BRW_ALLT:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] (INPUT BRW_GKOST:HANDLE IN FRAME {&FRAME-NAME}).
   brwgkosth = BRW_GKOST:HANDLE.
   brwallth = BRW_ALLT:HANDLE. 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnexcel_UI C-Win 
PROCEDURE btnexcel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   RUN visaexel_UI (TRUE).
   RUN excel_UI (INPUT FALSE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnskriv_UI C-Win 
PROCEDURE btnskriv_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN DO:
      musz = FALSE. 
      RETURN NO-APPLY.
   END.
   RUN visaexel_UI (FALSE).
   RUN excel_UI (INPUT TRUE).
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
  ENABLE BRW_GKOST BRW_ALLT BTN_EXCEL BTN_SKRIV BTN_AVS 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel_UI C-Win 
PROCEDURE excel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER saveas AS LOGICAL NO-UNDO.
   DEFINE VARIABLE selectwh AS COM-HANDLE NO-UNDO.
   DEFINE VARIABLE knapphj AS INTEGER NO-UNDO.
   DEFINE VARIABLE excelmax AS INTEGER NO-UNDO.    
   DEFINE VARIABLE q AS CHARACTER NO-UNDO.
   {AMERICANEUROPEAN.I}
   RUN allac_UI.   
   IF saveas = TRUE THEN RUN startexcelval_UI (INPUT FALSE).
   ELSE RUN startexcelval_UI (INPUT TRUE).
   
   /*FOREBILDER*/
   {LOGGOR.I}
   linkhj = link.
   RUN excelhead_UI (INPUT 1,{&WINDOW-NAME}:TITLE + " " + STRING(TODAY)).
   IF link NE ? THEN DO:
      RUN imageexcel_UI (INPUT link,INPUT "A",INPUT 1).  
      irad = irad + 5.
      cRange = allac[1] + STRING(irad).
      chWorkSheet:Range(cRange):FONT:Bold = YES NO-ERROR.   
      chWorkSheet:Range(cRange):RowHeight = 20 NO-ERROR.
      irad = irad + 2.
   END.
   ELSE irad = irad + 2.
   irad = irad + 2. 
   ASSIGN 
   bredd = 0.
   dubbelradhj = TRUE.
   RUN brwexcelrubriker_UI (INPUT brwgkosth,INPUT saveas).
   dynbrwexcelbuffh = TEMP-TABLE gfaktemp:DEFAULT-BUFFER-HANDLE. 
   q = "FOR EACH " + dynbrwexcelbuffh:TABLE + " by gfaktemp.AONR BY gfaktemp.DELNR".
   RUN brwexcelquery_UI (INPUT brwgkosth, INPUT q).  
   irad = irad + 1.   
   ASSIGN 
   bredd = 0.
   dubbelradhj = TRUE.   
   RUN brwexcelrubriker_UI (INPUT brwallth,INPUT saveas).
   dynbrwexcelbuffh = TEMP-TABLE sumpers:DEFAULT-BUFFER-HANDLE. 
   q = "FOR EACH " + dynbrwexcelbuffh:TABLE + " BY sumpers.AONR BY sumpers.DELNR BY sumpers.ORDNING".
                      
   RUN brwexcelquery_UI (INPUT brwallth, INPUT q).     
   selectwh = chWorkSheet.
   chWorkSheet = chExcelApplication:Sheets:ITEM(1) NO-ERROR.
   chWorkSheet:SELECT NO-ERROR.     
   RUN screenexcel_UI.
   estartnr = 0.
   RUN colbredd_UI.
    raknare = 1. 
   RUN kolumnexcel_UI.
   RUN sidbrytbredd_UI (INPUT 1).   
   IF saveas = TRUE THEN RUN slutmedprint_UI (INPUT 1).
   ELSE RUN slutexcel_UI.
   {EUROPEANAMERICAN.I}
   {musarrow.i}
   RETURN.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


