&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-2


/* Temp-Table and Buffer definitions                                    */




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/09/15 -  2:57 pm

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
{TIDPERS.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
{TILLATNODF.I}
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.   
DEFINE VARIABLE periodtot AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE pekodtot AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE pertot AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE pkodtot AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE difftot AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE VARIABLE excellista AS INTEGER NO-UNDO.
DEFINE VARIABLE dispens AS LOGICAL NO-UNDO.
DEFINE VARIABLE dispensm AS LOGICAL NO-UNDO.
DEFINE VARIABLE dispens48 AS LOGICAL NO-UNDO.
DEFINE VARIABLE dmandatum AS DATE  NO-UNDO.
DEFINE VARIABLE dmangodk AS CHARACTER NO-UNDO.
DEFINE VARIABLE dardatum AS DATE  NO-UNDO.
DEFINE VARIABLE dargodk AS CHARACTER NO-UNDO.
DEFINE VARIABLE d48datum AS DATE  NO-UNDO.
DEFINE VARIABLE d48godk AS CHARACTER NO-UNDO.
DEFINE VARIABLE ater50 AS LOGICAL NO-UNDO.   
DEFINE VARIABLE a50datum AS DATE  NO-UNDO.
DEFINE VARIABLE a50godk AS CHARACTER NO-UNDO.   
DEFINE VARIABLE a50tim AS INTEGER NO-UNDO.
DEFINE VARIABLE nofall AS LOGICAL NO-UNDO.
DEFINE VARIABLE nodatum AS DATE NO-UNDO.
DEFINE VARIABLE nogodk AS CHARACTER NO-UNDO.




DEFINE VARIABLE asfaktapph AS HANDLE NO-UNDO.                     /**/

DEFINE TEMP-TABLE invartemp   
   FIELD GA AS CHARACTER
   FIELD GM AS LOGICAL 
   FIELD SK AS LOGICAL 
   FIELD TI AS RECID 
   FIELD PER AS RECID 
   FIELD PER2 AS RECID 
   FIELD MU AS LOGICAL    
   FIELD REGST AS DECIMAL
   FIELD REGSU AS DECIMAL 
   FIELD RV AS INTEGER FORMAT "999" 
   FIELD RDAG AS CHARACTER FORMAT "X(3)"         
   FIELD RD AS DATE 
   FIELD RM AS INTEGER FORMAT "99" 
   FIELD RMN AS CHARACTER  
   FIELD REGA AS INTEGER FORMAT "99" 
   FIELD RT AS DECIMAL       
   FIELD BD AS DATE 
   FIELD AD AS DATE 
   FIELD NY AS DECIMAL 
   FIELD SEK AS INTEGER FORMAT "-9999999" 
   FIELD RSEK AS INTEGER 
   FIELD REGS AS INTEGER 
   FIELD GL AS CHARACTER.
   
DEFINE VARIABLE str AS CHARACTER FORMAT "X(90)" NO-UNDO.
/*{TIDUTTTNEW.I}*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-TIDS
&Scoped-define BROWSE-NAME BRW_TILLATNODF

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tillatnodf

/* Definitions for BROWSE BRW_TILLATNODF                                */
&Scoped-define FIELDS-IN-QUERY-BRW_TILLATNODF tillatnodf.PERSONALKOD ~
tillatnodf.FORNAMN tillatnodf.EFTERNAMN tillatnodf.TNODF ~
tillatnodf.NODDATUM tillatnodf.NODGODK 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TILLATNODF tillatnodf.TNODF 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_TILLATNODF tillatnodf
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_TILLATNODF tillatnodf
&Scoped-define QUERY-STRING-BRW_TILLATNODF FOR EACH tillatnodf NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_TILLATNODF OPEN QUERY BRW_TILLATNODF FOR EACH tillatnodf NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_TILLATNODF tillatnodf
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TILLATNODF tillatnodf


/* Definitions for FRAME FRAME-TIDS                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-TIDS ~
    ~{&OPEN-QUERY-BRW_TILLATNODF}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_TILLATNODF BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_TILLATNODF FOR 
      tillatnodf SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_TILLATNODF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TILLATNODF WINDOW-2 _STRUCTURED
  QUERY BRW_TILLATNODF NO-LOCK DISPLAY
      tillatnodf.PERSONALKOD COLUMN-LABEL "Enhet!/Sign" FORMAT "x(5)":U
      tillatnodf.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(15)":U
      tillatnodf.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(25)":U
      tillatnodf.TNODF COLUMN-LABEL "Tillåt!nödfallsövertid" FORMAT "Ja/Nej":U
            WIDTH 25
      tillatnodf.NODDATUM COLUMN-LABEL "Inlagt! datum" FORMAT "99/99/99":U
      tillatnodf.NODGODK COLUMN-LABEL "Inlagt! av" FORMAT "x(8)":U
  ENABLE
      tillatnodf.TNODF
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 121.5 BY 22.25
         TITLE "Tillåt nödfallsövertid" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-TIDS
     BRW_TILLATNODF AT ROW 3.25 COL 2.5 WIDGET-ID 100
     BTN_AVB AT ROW 28 COL 110.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 123.63 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: tillatnodf T "?" NO-UNDO temp-db tillatnodf
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Tillåt nödfallsövertid"
         HEIGHT             = 28.42
         WIDTH              = 124.5
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 124.5
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 124.5
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
/* SETTINGS FOR WINDOW WINDOW-2
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-TIDS
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_TILLATNODF 1 FRAME-TIDS */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TILLATNODF
/* Query rebuild information for BROWSE BRW_TILLATNODF
     _TblList          = "Temp-Tables.tillatnodf"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.tillatnodf.PERSONALKOD
"tillatnodf.PERSONALKOD" "Enhet!/Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tillatnodf.FORNAMN
"tillatnodf.FORNAMN" "Förnamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tillatnodf.EFTERNAMN
"tillatnodf.EFTERNAMN" "Efternamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.tillatnodf.TNODF
"tillatnodf.TNODF" "Tillåt!nödfallsövertid" ? "logical" ? ? ? ? ? ? yes ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.tillatnodf.NODDATUM
"tillatnodf.NODDATUM" "Inlagt! datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.tillatnodf.NODGODK
"tillatnodf.NODGODK" "Inlagt! av" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_TILLATNODF */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-TIDS
/* Query rebuild information for FRAME FRAME-TIDS
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-TIDS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_TILLATNODF
&Scoped-define SELF-NAME BRW_TILLATNODF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_TILLATNODF WINDOW-2
ON ROW-LEAVE OF BRW_TILLATNODF IN FRAME FRAME-TIDS /* Tillåt nödfallsövertid */
DO:
   RUN rowleave_UI.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tillatnodf.TNODF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tillatnodf.TNODF BRW_TILLATNODF _BROWSE-COLUMN WINDOW-2
ON ENTRY OF tillatnodf.TNODF IN BROWSE BRW_TILLATNODF /* Tillåt!nödfallsövertid */
DO:
  IF AVAILABLE tillatnodf THEN DO:
      DISPLAY tillatnodf.TNODF WITH BROWSE BRW_TILLATNODF.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tillatnodf.TNODF BRW_TILLATNODF _BROWSE-COLUMN WINDOW-2
ON LEAVE OF tillatnodf.TNODF IN BROWSE BRW_TILLATNODF /* Tillåt!nödfallsövertid */
DO:
  IF AVAILABLE tillatnodf THEN DO:
      tillatnodf.TNODF = INPUT BROWSE BRW_TILLATNODF tillatnodf.TNODF.           
      DISPLAY tillatnodf.TNODF WITH BROWSE BRW_TILLATNODF.             
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tillatnodf.TNODF BRW_TILLATNODF _BROWSE-COLUMN WINDOW-2
ON MOUSE-SELECT-CLICK OF tillatnodf.TNODF IN BROWSE BRW_TILLATNODF /* Tillåt!nödfallsövertid */
DO:
  IF AVAILABLE tillatnodf THEN DO:
      IF INPUT BROWSE BRW_TILLATNODF tillatnodf.TNODF = TRUE THEN tillatnodf.TNODF = FALSE.
      IF INPUT BROWSE BRW_TILLATNODF tillatnodf.TNODF = FALSE THEN tillatnodf.TNODF = TRUE.        
      DISPLAY tillatnodf.TNODF WITH BROWSE BRW_TILLATNODF.             
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-2
ON CHOOSE OF BTN_AVB IN FRAME FRAME-TIDS /* Avsluta */
DO:
   RUN rowleave_UI.
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

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
   {ALLSTARTDYN.I} 
/*   EMPTY TEMP-TABLE tidut NO-ERROR.*/ 
   ASSIGN   str=                                                                              
"======.===============================.===================.".   
         
   {muswait.i} 
   CREATE invartemp.
   ASSIGN
   invartemp.GA = Guru.Konstanter:globanv
   invartemp.GM = gvisatidpermanad
   invartemp.SK = skrivut 
   invartemp.TI = tidtabrec 
   invartemp.PER = persrec 
   invartemp.PER2 = persrec2 
   invartemp.MU = musz    
   invartemp.REGST = regstart  
   invartemp.REGSU = regslut 
   invartemp.RV = regvnr 
   invartemp.RDAG = regdagnamn         
   invartemp.RD = regdatum 
   invartemp.RM = regmnr 
   invartemp.RMN = regmannamn 
   invartemp.REGA = regar 
   invartemp.RT = regtotalt       
   invartemp.BD = bdatum 
   invartemp.AD = avdatum 
   invartemp.NY = nytid 
   invartemp.SEK = sekunder 
   invartemp.RSEK = regstartsek 
   invartemp.REGS = regslutsek 
   invartemp.GL = Guru.Konstanter:globforetag.
   EMPTY TEMP-TABLE tillatnodf  NO-ERROR. 
   FOR EACH tidpers:
      CREATE tillatnodf.
      BUFFER-COPY tidpers TO tillatnodf.
   END.   
   FOR EACH tillatnodf:
      RUN htillatnodf IN asfaktapph (INPUT tillatnodf.PERSONALKOD, OUTPUT nofall, OUTPUT nodatum, OUTPUT nogodk).            
      ASSIGN
      tillatnodf.TNODF = nofall
      tillatnodf.NODDATUM = nodatum
      tillatnodf.NODGODK = nogodk.      
   END.   
      
   RUN enable_UI.
         
   {FRMSIZE.I}     

   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_TILLATNODF:HANDLE IN FRAME {&FRAME-NAME}).
      IF Guru.Konstanter:appcon THEN DO:
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN ASFAKTAPP.P PERSISTENT SET asfaktapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:      
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN ASFAKTAPP.P PERSISTENT SET asfaktapph.
   END.
  
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-2  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
  THEN DELETE WIDGET WINDOW-2.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-2  _DEFAULT-ENABLE
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
  ENABLE BRW_TILLATNODF BTN_AVB 
      WITH FRAME FRAME-TIDS IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-TIDS}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowleave_UI WINDOW-2 
PROCEDURE rowleave_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAILABLE tillatnodf THEN DO:     
      DISPLAY tillatnodf.TNODF  WITH BROWSE BRW_TILLATNODF.
      tillatnodf.TNODF  = INPUT BROWSE BRW_TILLATNODF tillatnodf.TNODF .
      
          
      IF tillatnodf.TNODF = FALSE THEN RUN bortnodfall  IN asfaktapph (INPUT tillatnodf.PERSONALKOD).   
      ELSE DO:
          RUN spnodfall  IN asfaktapph (INPUT tillatnodf.PERSONALKOD, INPUT tillatnodf.TNODF, INPUT TODAY, INPUT Guru.Konstanter:globanv).
          IF tillatnodf.NODGODK = "" THEN DO:
             tillatnodf.NODDATUM = TODAY.
             tillatnodf.NODGODK = Guru.Konstanter:globanv.
          END.         
      END.    
      
      DISPLAY  tillatnodf.TNODF tillatnodf.NODGODK tillatnodf.NODDATUM  WITH BROWSE BRW_TILLATNODF.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

