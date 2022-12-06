&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ValdbGURU.w

  Description: 
     

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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

{ALLDEF2.I}
&Scoped-define NEW NEW 
{APPCONDEF.I}
{VALDBDEF.I}
 
/*Anders Olsson Elpool i Umeå AB  23 maj 2017 11:40:22 
flyttar cert 
*/
DEFINE VARIABLE brwproc AS HANDLE EXTENT 30 NO-UNDO.
DEFINE VARIABLE framesizeh AS HANDLE NO-UNDO.

/*
DEFINE VARIABLE dpis AS System.Drawing.SizeF NO-UNDO.
DEFINE VARIABLE dpim AS System.Windows.Forms.AutoScaleMode NO-UNDO.
dpis = NEW System.Drawing.SizeF(96, 96). 
dpim = System.Windows.Forms.AutoScaleMode:Dpi.
*/

RUN certs_UI (INPUT "fc5a8f99.0"). /* Rexels cert */

RUN certs_UI (INPUT "4f7fd3cf.0").

RUN certs_UI (INPUT "607986c7.0").

RUN certs_UI (INPUT "d5a26bee.0").

RUN certs_UI (INPUT "80ecc636.0").

RUN certs_UI (INPUT "e0708dc5.0").

RUN certs_UI (INPUT "10ca93dc.0").

RUN certs_UI (INPUT "968d05c4.0").

RUN Infrahmt.P (INPUT Guru.Konstanter:dlcvar, INPUT Guru.Konstanter:guruvar).




/*
"80ecc636.0"
*/
{NAMNDB.I}
 
{HKEYSTARTLOAD.I}



DEFINE INPUT  PARAMETER runvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR valdbtemp.
DEFINE VARIABLE condbnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE antaldb AS INTEGER NO-UNDO.
DEFINE VARIABLE dynbrwh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynqueh AS HANDLE NO-UNDO.
DEFINE VARIABLE nyheight AS DECIMAL NO-UNDO.
DEFINE VARIABLE kommandosortquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE computername  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE complength    AS INTEGER     NO-UNDO INITIAL 128.
DEFINE VARIABLE retvalue      AS INTEGER     NO-UNDO.


/*
DEFINE VARIABLE InfraInstalln  AS Helpers.InfraInstall              NO-UNDO.
*/

{Computer_LanIP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_VDB

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES valdbtemp

/* Definitions for BROWSE BRW_VDB                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VDB valdbtemp.FORETAG valdbtemp.GFORETAG ~
valdbtemp.VALDB valdbtemp.DBCACHE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VDB 
&Scoped-define QUERY-STRING-BRW_VDB FOR EACH valdbtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VDB OPEN QUERY BRW_VDB FOR EACH valdbtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VDB valdbtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VDB valdbtemp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOG_DB-2 TOG_DB BRW_VDB FILL-IN-APPSERVER ~
BTN_START BTN_BACKUP BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS TOG_DB-2 TOG_DB FILL-IN-APPSERVER 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACKUP AUTO-GO 
     LABEL "Kontroll backupp" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_START AUTO-GO 
     LABEL "Starta Guru" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-APPSERVER AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "APPSERVER" 
     VIEW-AS FILL-IN 
     SIZE 4.5 BY 1 NO-UNDO.

DEFINE VARIABLE TOG_DB AS LOGICAL INITIAL no 
     LABEL "Databaser på denna dator" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.5 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_DB-2 AS LOGICAL INITIAL no 
     LABEL "Ej våra databaser" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.5 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_VDB FOR 
      valdbtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_VDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VDB C-Win _STRUCTURED
  QUERY BRW_VDB DISPLAY
      valdbtemp.FORETAG FORMAT "X(5)":U
      valdbtemp.GFORETAG COLUMN-LABEL "GFöretag" FORMAT "X(30)":U
            WIDTH 6
      valdbtemp.VALDB FORMAT "X(256)":U WIDTH 20
      valdbtemp.DBCACHE COLUMN-LABEL "Stoppa Db" FORMAT "x(256)":U
            WIDTH 20
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 34 BY 6.83
         FONT 4
         TITLE "Databaser" NO-EMPTY-SPACE TOOLTIP "Välj databas".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     TOG_DB-2 AT ROW 1.5 COL 34 WIDGET-ID 32
     TOG_DB AT ROW 2.5 COL 34 WIDGET-ID 30
     BRW_VDB AT ROW 3.75 COL 1.5
     FILL-IN-APPSERVER AT ROW 3.75 COL 43.5 COLON-ALIGNED WIDGET-ID 2
     BTN_START AT ROW 5.5 COL 36.75
     BTN_BACKUP AT ROW 7 COL 36.5 WIDGET-ID 34
     BTN_AVB AT ROW 9.58 COL 36.75
     "Välj databas för start av Guru !" VIEW-AS TEXT
          SIZE 32 BY 2 AT ROW 1.5 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 51 BY 10.17
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db valdbtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Start av Guru"
         HEIGHT             = 10.08
         WIDTH              = 51
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
/* BROWSE-TAB BRW_VDB TOG_DB DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VDB
/* Query rebuild information for BROWSE BRW_VDB
     _TblList          = "Temp-Tables.valdbtemp"
     _FldNameList[1]   = Temp-Tables.valdbtemp.FORETAG
     _FldNameList[2]   > Temp-Tables.valdbtemp.GFORETAG
"GFORETAG" "GFöretag" "X(30)" "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.valdbtemp.VALDB
"VALDB" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.valdbtemp.DBCACHE
"DBCACHE" "Stoppa Db" "x(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VDB */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Start av Guru */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Start av Guru */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VDB
&Scoped-define SELF-NAME BRW_VDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VDB C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_VDB IN FRAME DEFAULT-FRAME /* Databaser */
DO:
   APPLY "CHOOSE" TO BTN_START.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VDB C-Win
ON VALUE-CHANGED OF BRW_VDB IN FRAME DEFAULT-FRAME /* Databaser */
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   {&BROWSE-NAME}:TOOLTIP = "Välj databas".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:       
  /* IF Guru.Konstanter:globforetag = "SOLE" THEN QUIT.*/
   SESSION:PRINTER-CONTROL-HANDLE = 0.
   RUN val_UI.
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN RETURN.
  
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON ENDKEY OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
  {BORTBRWPROC.I}   
  SESSION:PRINTER-CONTROL-HANDLE = 0.
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BACKUP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BACKUP C-Win
ON CHOOSE OF BTN_BACKUP IN FRAME DEFAULT-FRAME /* Kontroll backupp */
DO:
   FILL-IN-APPSERVER = INPUT FILL-IN-APPSERVER.
   FIND FIRST valdbtempbuff WHERE valdbtempbuff.GFORETAG = "INGEN" NO-LOCK NO-ERROR.
   IF AVAILABLE valdbtempbuff THEN DO:
      MESSAGE valdbtemp.APPCON
      VIEW-AS ALERT-BOX.
      RUN BACKUPSTATUSTART.p (INPUT FILL-IN-APPSERVER,INPUT valdbtemp.GFORETAG,INPUT valdbtemp.APPCON).
      
      
   END.
    musz = FALSE.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_START C-Win
ON CHOOSE OF BTN_START IN FRAME DEFAULT-FRAME /* Starta Guru */
DO:
   FILL-IN-APPSERVER = INPUT FILL-IN-APPSERVER.
   /*IF runvar = "FSNAT" OR runvar = "SUNDNAT" THEN RUN ByteGuruStart_UI.*/ 
   
   FIND FIRST valdbtempbuff WHERE valdbtempbuff.GFORETAG = "INGEN" OR valdbtempbuff.GFORETAG = "ALLADB" NO-LOCK NO-ERROR.
   IF AVAILABLE valdbtempbuff THEN DO:
      /*WTID*/
      RUN kuddbstart_UI.
   END.
   ELSE IF valdbtemp.APPCON = "" THEN RUN ejappcon_UI.  
   ELSE DO:
      RUN val_UI.
      IF musz = FALSE THEN DO:
         {&WINDOW-NAME}:HIDDEN = TRUE.
         DEFAULT-WINDOW:HIDDEN = TRUE.
         {&WINDOW-NAME}:ALWAYS-ON-TOP = FALSE.       
         {&WINDOW-NAME}:HIDDEN = TRUE.      
         DISABLE BRW_VDB BTN_START WITH FRAME {&FRAME-NAME}.    
         RUN Spring.p (TRUE, valdbtemp.GFORETAG ).
         IF PROVERSION BEGINS "11.2" AND SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
            MESSAGE "Du använder en gammal version av Progress! Kontakta Anders på Elpool så hjälper han till med en ominstallation av Guru!" skip
            "Tel 090-184544!" SKIP 
            "OBS! Efter den 2020-01-14 kommer du inte att kunna starta Guru!"
            VIEW-AS ALERT-BOX.
         END.    
         IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
         IF Guru.Konstanter:apphand NE ? THEN DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.
         
         ENABLE BRW_VDB BTN_START WITH FRAME {&FRAME-NAME}.
         BTN_AVB:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         {&WINDOW-NAME}:HIDDEN = FALSE.
         {&WINDOW-NAME}:HIDDEN = FALSE.
         {&WINDOW-NAME}:MOVE-TO-TOP ().
         {musarrow.i}
      END.
   END.
   musz = FALSE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_DB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_DB C-Win
ON VALUE-CHANGED OF TOG_DB IN FRAME DEFAULT-FRAME /* Databaser på denna dator */
DO:
   TOG_DB = INPUT TOG_DB. 
   IF TOG_DB = FALSE THEN DO:
      kommandosortquery = "for each valdbtemp".
   END.
   ELSE DO:
      FIND FIRST valdbtemp WHERE valdbtemp.DBCON MATCHES "*" + Computer_LanIP + "*" NO-LOCK NO-ERROR.
      IF NOT AVAILABLE valdbtemp THEN DO:
         computername = FILL(" ", complength).
         RUN GetComputerNameA (INPUT-OUTPUT computername, OUTPUT complength, OUTPUT retvalue).
         computername = RIGHT-TRIM(computername).
         Computer_LanIP = computername.
      END.
      kommandosortquery = "for each valdbtemp where valdbtemp.DBCON MATCHES '*" + Computer_LanIP + "*'".
      FILL-IN-APPSERVER = FALSE.
      DISPLAY FILL-IN-APPSERVER WITH FRAME {&FRAME-NAME} .
   END.  
   RUN openbdynspec_UI. 
   Computer_LanIP = Ipcheck:checkIp().     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_DB-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_DB-2 C-Win
ON VALUE-CHANGED OF TOG_DB-2 IN FRAME DEFAULT-FRAME /* Ej våra databaser */
DO:
   TOG_DB-2 = INPUT TOG_DB-2. 
   IF TOG_DB-2 = FALSE THEN DO:
      kommandosortquery = "for each valdbtemp".
   END.
   ELSE DO:
      kommandosortquery = 'for each valdbtemp where valdbtemp.WWWFTP = false AND valdbtemp.FORETAG ne "ELPA"'.
   END.  
   RUN openbdynspec_UI.     
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
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:   
   /*CMB_DB:SCREEN-VALUE = "Energiadministration". */
   FRAME {&FRAME-NAME}:FONT = 4. 
   FILL-IN-APPSERVER = TRUE.
   {ALLSTARTDYN.I}
   {VALDBMAING.I}
   IF PROVERSION BEGINS "11.2" AND SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
      IF TODAY > 01/14/2020 THEN DO:
         MESSAGE "Du använder en för gammal version av Progress! 
         Kontakta Anders på Elpool så hjälper han till med en ominstallation av Guru!" skip
         "Tel 090-184544!" SKIP 
         VIEW-AS ALERT-BOX.
         QUIT. 
      END.   
   END.   
   IF antaldb > 8 THEN DO: 
      nyheight = antaldb - 8. 
      C-Win:HEIGHT-CHARS = C-Win:HEIGHT-CHARS + nyheight.
      IF C-Win:HEIGHT-PIXELS >  SESSION:WORK-AREA-HEIGHT-PIXELS - 240 THEN DO:
          C-Win:HEIGHT-PIXELS = SESSION:WORK-AREA-HEIGHT-PIXELS - 240.
      END.   
      FRAME DEFAULT-FRAME:HEIGHT-CHARS = C-Win:HEIGHT-CHARS.
      BRW_VDB:HEIGHT-CHARS = C-Win:HEIGHT-CHARS - (BRW_VDB:ROW + 0.1).
   END.
   IF runvar = "BERGSUPPORT" THEN DO:
      BTN_BACKUP:HIDDEN = TRUE.
      TOG_DB-2:HIDDEN = TRUE.
      TOG_DB:HIDDEN = TRUE.
      FILL-IN-APPSERVER:HIDDEN = TRUE.
      BRW_VDB:LABELS = TRUE.
   END.
   ELSE DO:
      FIND FIRST valdbtempbuff WHERE valdbtempbuff.GFORETAG = "INGEN" NO-LOCK NO-ERROR.
      IF AVAILABLE valdbtempbuff THEN DO:
         {&WINDOW-NAME}:WIDTH = {&WINDOW-NAME}:WIDTH + 10.
         FRAME {&FRAME-NAME}:WIDTH =  FRAME {&FRAME-NAME}:WIDTH + 10.
         BTN_BACKUP:HIDDEN = FALSE.
         TOG_DB-2:HIDDEN = FALSE.
         TOG_DB:HIDDEN = FALSE.
         FILL-IN-APPSERVER:HIDDEN = FALSE.
         BRW_VDB:LABELS = TRUE.
         
         BTN_BACKUP:COLUMN =  BTN_BACKUP:COLUMN + 10.
         BTN_START:COLUMN = BTN_BACKUP:COLUMN.
         BTN_AVB:COLUMN = BTN_BACKUP:COLUMN.
         FILL-IN-APPSERVER:COLUMN = FILL-IN-APPSERVER:COLUMN + 10.
         Guru.Konstanter:LabelFlytt(FILL-IN-APPSERVER:HANDLE).
         BRW_VDB:WIDTH = BRW_VDB:WIDTH + 10.
      END.
      ELSE DO:
         ASSIGN 
         BRW_VDB:LABELS = FALSE 
         valdbtemp.FORETAG:VISIBLE IN BROWSE BRW_VDB = FALSE
         valdbtemp.GFORETAG:VISIBLE IN BROWSE BRW_VDB = FALSE
         valdbtemp.DBCACH:VISIBLE IN BROWSE BRW_VDB = FALSE
         BTN_BACKUP:HIDDEN = TRUE.
         TOG_DB:HIDDEN = TRUE.
         TOG_DB-2:HIDDEN = TRUE.
         FILL-IN-APPSERVER:HIDDEN = TRUE.
      END.     
   END.
   RUN PlaceraKnapp_UI.
      
   FIND FIRST valdbtempbuff  WHERE valdbtempbuff.GFORETAG = runvar NO-LOCK NO-ERROR.
   IF AVAILABLE valdbtempbuff THEN DO:
      RUN selectdyn_UI IN  brwproc[1] (INPUT ROWID(valdbtempbuff)).
   END.         
   {SLUTWIN.I}
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
   DEFINE VARIABLE dynok AS LOGICAL NO-UNDO.
   dynbrwh = BRW_VDB:HANDLE IN FRAME {&FRAME-NAME}.
   dynqueh = dynbrwh:QUERY.
   IF runvar = "SKOGSK" THEN dynok = dynqueh:QUERY-PREPARE("FOR EACH valdbtemp USE-INDEX ORDNING2").
   ELSE IF runvar = "REJLERS" THEN dynok = dynqueh:QUERY-PREPARE("FOR EACH valdbtemp by valdbtemp.VALDB").
   ELSE dynok = dynqueh:QUERY-PREPARE("FOR EACH valdbtemp").
   dynok = dynqueh:QUERY-OPEN() NO-ERROR.
   
   dynqueh:GET-LAST(NO-LOCK).
   antaldb = dynqueh:NUM-RESULTS.
   dynqueh:GET-FIRST(NO-LOCK).
   
   IF NOT VALID-HANDLE(brwproc[1]) THEN RUN DYNBRWLIGHT.P PERSISTENT SET brwproc[1] (INPUT BRW_VDB:HANDLE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByteGuruStart_UI C-Win 
PROCEDURE ByteGuruStart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*Anders Olsson Elpool i Umeå AB  4 sep 2016 18:05:54 
   Ej i drift ännu behövs oftas inte se annan teknik
   */
 /*
   IF runvar = "elpa" THEN RETURN.
   */
   
   DEFINE VARIABLE appnamn AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE companyname AS CHARACTER NO-UNDO.
   DEFINE VARIABLE urlvar AS CHARACTER NO-UNDO.
  /*
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN.
   ELSE RETURN.
  */
   IF PROVERSION BEGINS "11" THEN DO:
      IF valdbtemp.WWWSTART = "" THEN RETURN.
   END. 
   {HKEYCURRENTUSER.I}
   ASSIGN 
   companyname = "SOFTWARE\Elpool i Umeå AB\".
   
   IF PROVERSION BEGINS "10" THEN appnamn = "GuruOnWeb10\ProwcappLocator".
   IF PROVERSION BEGINS "11" THEN appnamn = "GuruOnWeb11\ProwcappLocator".
   IF valdbtemp.FORETAG = "SEKG" THEN DO:
      IF PROVERSION BEGINS "10" THEN appnamn = "EKGOnWeb10\ProwcappLocator".
      IF PROVERSION BEGINS "11" THEN appnamn = "EKGOnWeb116\ProwcappLocator".
   END.   

   LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
   LOAD companyname BASE-KEY hkeyvar NO-ERROR. 
   USE companyname NO-ERROR. 
   GET-KEY-VALUE SECTION appnamn KEY "URL" VALUE urlvar.
   
   IF PROVERSION BEGINS "11" THEN DO:
      IF urlvar NE  valdbtemp.WWWSTART THEN  PUT-KEY-VALUE SECTION appnamn KEY "URL" VALUE  valdbtemp.WWWSTART NO-ERROR.
   END.


   UNLOAD companyname + appnamn + "\"  NO-ERROR.
   UNLOAD companyname  NO-ERROR. 
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE certs_Ui C-Win 
PROCEDURE certs_Ui :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER certname AS CHARACTER NO-UNDO.
   IF SEARCH(gurucert + certname) = ? THEN DO:
      gurucert = REPLACE(gurucert,"\CINSTALL","\WTID").
   END.
   IF SEARCH(gurucert + certname) NE ? THEN DO:   
      IF SEARCH(dlccert + certname) = ? THEN DO:
         OS-COPY VALUE(gurucert + certname) VALUE(dlccert + certname).  
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE con_UI C-Win 
PROCEDURE con_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   IF NOT CONNECTED(valdbtemp.DBNAMN) THEN DO:
      MESSAGE valdbtemp.DBPLATS valdbtemp.DBNAMN 
      VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
      UPDATE valb AS LOGICAL.
      CASE valb: 
         WHEN TRUE THEN DO:
            RUN SETKUNDVALDB.P (INPUT valdbtemp.DBCON ).                 
         END.      
      END CASE.          
   END.
                     
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE discon_UI C-Win 
PROCEDURE discon_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
      IF LDBNAME(1) NE valdbtemp.DBNAMN THEN DO:
         IF LDBNAME(1) NE ? THEN DO:
            DISCONNECT VALUE(LDBNAME(1)) NO-ERROR.
             {DELALIAS.I}
         END.             
      END.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ejappcon_UI C-Win 
PROCEDURE ejappcon_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*
   DISCONNECT VALUE(LDBNAME(condbnamn)) NO-ERROR.
   DISCONNECT VALUE(LDBNAME(1)) NO-ERROR.
   DISCONNECT VALUE("RT9") NO-ERROR.
   {DELALIAS.I}
   */
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   DEFINE VARIABLE nyaprog AS LOGICAL NO-UNDO.
   BTN_AVB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   {&WINDOW-NAME}:HIDDEN = TRUE.
   DEFAULT-WINDOW:HIDDEN = TRUE.
   {&WINDOW-NAME}:ALWAYS-ON-TOP = FALSE.       
   {&WINDOW-NAME}:HIDDEN = TRUE.      
   DISABLE BRW_VDB BTN_START WITH FRAME {&FRAME-NAME}.      
   Guru.SharedVariable:singel = FALSE. 
   condbnamn = valdbtemp.DBNAMN.
   RUN Spring.p (FALSE, valdbtemp.GFORETAG ).
   DISCONNECT VALUE(LDBNAME(1)) NO-ERROR.
  
   ENABLE BRW_VDB BTN_START WITH FRAME {&FRAME-NAME}.
   BTN_AVB:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   {&WINDOW-NAME}:HIDDEN = FALSE.
   {&WINDOW-NAME}:HIDDEN = FALSE.
   {&WINDOW-NAME}:MOVE-TO-TOP ().
   {musarrow.i}
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
  DISPLAY TOG_DB-2 TOG_DB FILL-IN-APPSERVER 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE TOG_DB-2 TOG_DB BRW_VDB FILL-IN-APPSERVER BTN_START BTN_BACKUP BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE flexmitt_UI C-Win 
PROCEDURE flexmitt_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE flexsund_UI C-Win 
PROCEDURE flexsund_UI :
/*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
   /* VALDBMAING.I i VALDBGURU.W styr att man kör flexsund_ui SÄTTER appfel start MULTISTART STARTA FLEXTID.W*/
   DEFINE VARIABLE nyaprog AS LOGICAL NO-UNDO. 
   isweb = TRUE.
   RUN val_UI.
   IF musz = FALSE THEN DO:
      /*IF runvar = "FSNAT" OR runvar = "SUNDNAT" THEN RUN ByteGuruStart_UI.*/
      IF isweb = TRUE THEN DO:
         Guru.Konstanter:appfel = TRUE.
         RUN Spring.p (TRUE, valdbtemp.GFORETAG ).
         IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
         DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.
         QUIT.
      END.
     
      musz = FALSE.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kuddbstart_UI C-Win 
PROCEDURE kuddbstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   IF valdbtemp.GFORETAG = "INGEN" THEN DO:
     /* MESSAGE valdbtemp.VALDB
      VIEW-AS ALERT-BOX.
      */
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
        
         RUN wtid\VALDBSERVERMULTI.w.
      END.
      ELSE  RUN VALDBSERVERMULTI.w.
   END.   
   /*
   ELSE IF valdbtemp.GFORETAG = "ALLADB" THEN DO:
      RUN Register\AllaDatabaserStart.p.
   END.
  */   
   ELSE IF valdbtemp.VALDB BEGINS "USERS" THEN DO:
      MESSAGE valdbtemp.VALDB
      VIEW-AS ALERT-BOX.
      RUN ANTALUSERSSTART.p (INPUT valdbtemp.GFORETAG,INPUT valdbtemp.APPCON).
   END.
   ELSE IF FILL-IN-APPSERVER = TRUE THEN DO: 
      RUN val_UI.
      RUN discon_UI.
      IF musz = FALSE THEN DO:
         MESSAGE valdbtemp.APPCON
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
         {&WINDOW-NAME}:HIDDEN = TRUE.
         DEFAULT-WINDOW:HIDDEN = TRUE.
         {&WINDOW-NAME}:ALWAYS-ON-TOP = FALSE.       
         {&WINDOW-NAME}:HIDDEN = TRUE.       
         IF valdbtemp.GFORETAG = "dELPA" OR valdbtemp.GFORETAG = "classELPA" THEN Guru.SharedVariable:demokvar = TRUE.
         ELSE Guru.SharedVariable:demokvar = FALSE.
        
         RUN Spring.p (TRUE, valdbtemp.GFORETAG ).
         {&WINDOW-NAME}:HIDDEN = FALSE.
         {&WINDOW-NAME}:HIDDEN = FALSE.
         {&WINDOW-NAME}:MOVE-TO-TOP ().
          {musarrow.i}
         
      END.
   END.
   ELSE DO:
      MESSAGE valdbtemp.DBPLATS valdbtemp.DBNAMN "ejappcon"
       VIEW-AS ALERT-BOX.
      RUN ejappcon_UI.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE medd_UI C-Win 
PROCEDURE medd_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   MESSAGE 
   "Anslutningen till " valdbtemp.VALDB " misslyckades!" SKIP
   ERROR-STATUS:NUM-MESSAGES 
   " fel uppkom vid anslutningen." SKIP 
   "Vill du se dem ?" 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
   UPDATE view-errs AS LOGICAL.       
   IF view-errs THEN DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
      MESSAGE ERROR-STATUS:GET-NUMBER(i)
      ERROR-STATUS:GET-MESSAGE(i)
      VIEW-AS ALERT-BOX.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openbdynspec_UI C-Win 
PROCEDURE openbdynspec_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE dynok AS LOGICAL NO-UNDO.
   
   dynok = dynqueh:QUERY-PREPARE(kommandosortquery).
   IF dynok = TRUE THEN dynok = dynqueh:QUERY-OPEN() NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PlaceraKnapp_UI C-Win 
PROCEDURE PlaceraKnapp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   Guru.GlobalaVariabler:StartRadForKnappar = BTN_START:ROW IN FRAME {&FRAME-NAME}. 
   Guru.Konstanter:PlaceraKnapparLodratt(BTN_START:HANDLE).                   
   Guru.Konstanter:PlaceraKnapparLodratt(BTN_BACKUP:HANDLE).         
   Guru.Konstanter:PlaceraKnapparLodratt(BTN_AVB:HANDLE). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val_UI C-Win 
PROCEDURE val_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {musarrow.i}
   IF valdbtemp.GFORETAG = "dELPA" OR valdbtemp.GFORETAG = "classELPA" THEN Guru.SharedVariable:demokvar = TRUE.
   ELSE Guru.SharedVariable:demokvar = FALSE.
   
   
END PROCEDURE.

PROCEDURE GetComputerNameA EXTERNAL "kernel32":
   DEFINE INPUT-OUTPUT PARAMETER lpszName AS CHAR.
   DEFINE OUTPUT PARAMETER lpdwcBuffer AS LONG.
   DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

