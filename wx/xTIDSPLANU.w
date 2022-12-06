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

/* Local Variable Definitions ---                                       */
{EXTRATAB.I}  
{ALLDEF.I}
{SOKDEF.I}
{FAKTIN.I}
{KALKIN.I}
{BERIN.I}
{MARKVARDIN.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/
{FAKTTYPDEF.I}
{ANSPROJBER.I}
{AUTOMREGTEMP.I}
{OMRTEMPW.I}
{GATILL.I}
{HOPPSEK2W.I}
{AVTPLANTEMP.I}
{ANVPERS.I}
&Scoped-define SHARED SHARED 
{AVDTEMP.I}
{DIRDEF.I}
{AONRDEF.I}
{BESTKUNDALLT.I}
{JURPERST.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED 
{AVTAONRTEMP.I}
/*{PLANNRTEMP.I} */







DEFINE SHARED VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 24 BY 1 NO-UNDO.
DEFINE NEW SHARED VARIABLE grundmappvar AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE omrbildvar AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE kalkrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE regvnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE nyttsparatao AS LOGICAL NO-UNDO. 
DEFINE VARIABLE kalknrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE kalktypvar AS INTEGER NO-UNDO.
DEFINE VARIABLE tidbgamla AS CHARACTER NO-UNDO.
DEFINE VARIABLE tidagamla AS CHARACTER NO-UNDO.
DEFINE VARIABLE tidpgamla AS CHARACTER NO-UNDO.
DEFINE VARIABLE valnr AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE nrvalvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE aoomradenr AS LOGICAL NO-UNDO.
DEFINE VARIABLE nrserierec AS RECID NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE mappvarhj AS CHARACTER NO-UNDO.
DEFINE VARIABLE avtrec AS RECID NO-UNDO.
DEFINE VARIABLE sokmapp AS CHARACTER NO-UNDO.
DEFINE VARIABLE varfilnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE Okvald AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE openqkoll AS LOGICAL EXTENT 50 NO-UNDO.
DEFINE VARIABLE sldatum AS DATE NO-UNDO.
DEFINE VARIABLE stdatum AS DATE NO-UNDO.
DEFINE VARIABLE projrapp AS CHARACTER FORMAT "X(101)" NO-UNDO.  
DEFINE VARIABLE omradespar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE bestspar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE aonrrecsp AS RECID NO-UNDO.
DEFINE VARIABLE aonrrecsp2 AS RECID NO-UNDO.
DEFINE VARIABLE aonrrecdel AS RECID NO-UNDO.
DEFINE VARIABLE aonrrecdel2 AS RECID NO-UNDO.
DEFINE VARIABLE stat AS INTEGER NO-UNDO.
DEFINE VARIABLE bortaoapph AS HANDLE NO-UNDO.                     /*BORTAOAPP.P*/

DEFINE NEW SHARED VARIABLE nyttaoapph AS HANDLE NO-UNDO.                      /*NYTTAOAPP.P*/
DEFINE VARIABLE nyttaoejapph AS HANDLE NO-UNDO.                              /* dokhant.P */
DEFINE VARIABLE editanm AS CHARACTER FORMAT "x(68)" NO-UNDO.
DEFINE VARIABLE flagga AS INTEGER NO-UNDO.
DEFINE VARIABLE aokopprec AS RECID NO-UNDO.
DEFINE VARIABLE aotidrec AS RECID NO-UNDO.
DEFINE VARIABLE tabvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE omrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE bestvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE radfast AS LOGICAL NO-UNDO.
DEFINE VARIABLE autoregvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE TOG_BERKOP AS LOGICAL NO-UNDO.
DEFINE VARIABLE TOG_KALKOP AS LOGICAL NO-UNDO.
DEFINE VARIABLE TOG_KOPPKOP AS LOGICAL NO-UNDO.
DEFINE VARIABLE TOG_TIDLKOP AS LOGICAL NO-UNDO.
DEFINE VARIABLE reslog AS LOGICAL NO-UNDO.
DEFINE VARIABLE ordnvar AS INTEGER NO-UNDO.
DEFINE VARIABLE idvar AS INTEGER NO-UNDO.
DEFINE VARIABLE fbestapph AS HANDLE NO-UNDO.
DEFINE VARIABLE datornamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE refvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE extraomr AS CHARACTER NO-UNDO EXTENT 2.
DEFINE VARIABLE sparavdat AS DATE NO-UNDO.
DEFINE VARIABLE brwkoll AS INTEGER NO-UNDO EXTENT 10.
DEFINE VARIABLE gfastkh AS CHARACTER NO-UNDO.
DEFINE VARIABLE gtillkh AS CHARACTER NO-UNDO.
DEFINE VARIABLE radspar AS INTEGER NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE valford AS CHARACTER NO-UNDO.
DEFINE VARIABLE mainvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE arbbspar AS DECIMAL NO-UNDO.
DEFINE VARIABLE arbfspar AS DECIMAL NO-UNDO.

DEFINE VARIABLE dynbrwh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynfrmh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynbufh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynqueh AS HANDLE NO-UNDO.
DEFINE VARIABLE openquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmpcolh AS HANDLE NO-UNDO.
DEFINE VARIABLE fieldh AS HANDLE NO-UNDO.
DEFINE VARIABLE exbrwfields AS CHARACTER NO-UNDO.
DEFINE VARIABLE raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE antalveckor AS INTEGER NO-UNDO.
DEFINE VARIABLE colnum AS INTEGER NO-UNDO.
DEFINE VARIABLE tidsplandynh AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE tidsplantemp NO-UNDO 
   FIELD AONR           AS CHARACTER
   FIELD DELNR          AS INTEGER
   FIELD ARBART          AS CHARACTER
   FIELD AKTIVITET      AS CHARACTER FORMAT "X(256)" 
   FIELD TIDLAGE        AS CHARACTER
   FIELD IDTIDLAG       AS CHARACTER
   FIELD TIDSPERIOD     AS CHARACTER
   FIELD STARTDAT       AS DATE 
   FIELD SLUTDAT        AS DATE 
   FIELD PERSONAL       AS CHARACTER
   FIELD ANDRAD         AS LOGICAL
   FIELD MON            AS CHARACTER
   FIELD TIS            AS CHARACTER
   FIELD ONS            AS CHARACTER
   FIELD TOR            AS CHARACTER
   FIELD FRE            AS CHARACTER
   FIELD LOR            AS CHARACTER
   FIELD SON            AS CHARACTER
   INDEX AONR IS PRIMARY AONR DELNR STARTDAT
   INDEX DATUM STARTDAT SLUTDAT AONR DELNR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-57 RECT-3 BTN_VECKA BTN_BORT ~
BTN_AVS FILL-IN-AKT FILL-IN-TILLF FILL-IN-EXT FILL-IN-DELAD FILL-IN_PROJEKT ~
FILL-IN_DATUM 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-AKT FILL-IN-TILLF FILL-IN-EXT ~
FILL-IN-DELAD FILL-IN_PROJEKT FILL-IN_DATUM 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS 
     LABEL "Avsluta" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort vecka" 
     SIZE 16 BY 1.

DEFINE BUTTON BTN_VECKA 
     LABEL "Lägg till vecka" 
     SIZE 16 BY 1.

DEFINE VARIABLE FILL-IN-AKT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Planerad aktivitet" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 12  NO-UNDO.

DEFINE VARIABLE FILL-IN-DELAD AS CHARACTER FORMAT "X(256)":U 
     LABEL "Delad" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FILL-IN-EXT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Externa aktiviteter" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 2  NO-UNDO.

DEFINE VARIABLE FILL-IN-TILLF AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tillfällig aktivitet" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 9  NO-UNDO.

DEFINE VARIABLE FILL-IN_DATUM AS DATE FORMAT "9999/99/99":U 
     LABEL "Datum" 
      VIEW-AS TEXT 
     SIZE 15.88 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN_PROJEKT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Projekt" 
      VIEW-AS TEXT 
     SIZE 16 BY .63 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 26.5 BY 2.5.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 97.5 BY 2.5.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 124.5 BY .5
     BGCOLOR 1 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BTN_VECKA AT ROW 1.08 COL 16
     BTN_BORT AT ROW 1.08 COL 32
     BTN_AVS AT ROW 1.08 COL 112.75
     FILL-IN-AKT AT ROW 26.88 COL 48.5 COLON-ALIGNED
     FILL-IN-TILLF AT ROW 26.88 COL 81.75 COLON-ALIGNED
     FILL-IN-EXT AT ROW 27.96 COL 48.5 COLON-ALIGNED
     FILL-IN-DELAD AT ROW 27.96 COL 81.75 COLON-ALIGNED
     FILL-IN_PROJEKT AT ROW 27 COL 9 COLON-ALIGNED
     FILL-IN_DATUM AT ROW 27.96 COL 9 COLON-ALIGNED
     "  Funktioner:" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 1.08 COL 1.25
          FGCOLOR 1 FONT 17
     RECT-1 AT ROW 26.75 COL 1.5
     RECT-57 AT ROW 2.08 COL 1
     RECT-3 AT ROW 26.75 COL 28
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.75 BY 28.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: anlaggtemp T "?" NO-UNDO temp-db anlaggtemp
      TABLE: ansvaraotemp T "?" NO-UNDO temp-db ansvaraotemp
      TABLE: aonrkonttemp T "?" NO-UNDO temp-db aonrkonttemp
      TABLE: aonrtidperstemp T "?" NO-UNDO temp-db aonrtidperstemp
      TABLE: aotidslagtemp T "?" NO-UNDO temp-db aotidslagtemp
      TABLE: arbarttemp T "?" NO-UNDO temp-db arbarttemp
      TABLE: avtalaonrtemp T "?" NO-UNDO temp-db avtalaonrtemp
      TABLE: beratemp T "?" NO-UNDO temp-db beratemp
      TABLE: bestkundallt T "?" NO-UNDO temp-db bestkundallt
      TABLE: bestkundextra T "?" NO-UNDO temp-db bestkundextra
      TABLE: dagboktemp T "?" NO-UNDO temp-db dagboktemp
      TABLE: gatill T "?" NO-UNDO temp-db gatill
      TABLE: priotemp T "?" NO-UNDO temp-db priotemp
      TABLE: projtemp T "?" NO-UNDO temp-db projtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Guru-Projekthuvud"
         HEIGHT             = 28.42
         WIDTH              = 124.88
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 124.88
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 124.88
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
                                                                        */
ASSIGN 
       FILL-IN-AKT:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN-DELAD:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN-EXT:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN-TILLF:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN_DATUM:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN_PROJEKT:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Guru-Projekthuvud */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Guru-Projekthuvud */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS C-Win
ON CHOOSE OF BTN_AVS IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT C-Win
ON CHOOSE OF BTN_BORT IN FRAME DEFAULT-FRAME /* Ta bort vecka */
DO:
   RUN bortvecka_UI IN tidsplandynh.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VECKA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VECKA C-Win
ON CHOOSE OF BTN_VECKA IN FRAME DEFAULT-FRAME /* Lägg till vecka */
DO:
   RUN vecka_UI IN tidsplandynh (INPUT 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AKT C-Win
ON ENTRY OF FILL-IN-AKT IN FRAME DEFAULT-FRAME /* Planerad aktivitet */
DO:
  RUN colvalue_UI IN tidsplandynh (INPUT FILL-IN-AKT:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DELAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DELAD C-Win
ON ENTRY OF FILL-IN-DELAD IN FRAME DEFAULT-FRAME /* Delad */
DO:
  RUN colvalue_UI IN tidsplandynh (INPUT FILL-IN-DELAD:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-EXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-EXT C-Win
ON ENTRY OF FILL-IN-EXT IN FRAME DEFAULT-FRAME /* Externa aktiviteter */
DO:
  RUN colvalue_UI IN tidsplandynh (INPUT FILL-IN-EXT:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TILLF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TILLF C-Win
ON ENTRY OF FILL-IN-TILLF IN FRAME DEFAULT-FRAME /* Tillfällig aktivitet */
DO:
  RUN colvalue_UI IN tidsplandynh (INPUT FILL-IN-TILLF:HANDLE).
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
   IF VALID-HANDLE(nyttaoapph) THEN DELETE PROCEDURE nyttaoapph NO-ERROR.
   RUN releaseh_UI IN tidsplandynh.
   IF VALID-HANDLE(tidsplandynh) THEN DELETE PROCEDURE tidsplandynh NO-ERROR.
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
   ASSIGN
   FILL-IN_DATUM = TODAY
   FILL-IN_PROJEKT = globforetag.
   RUN arbartout_UI IN nyttaoapph (OUTPUT TABLE arbarttemp).
   FOR EACH arbarttemp NO-LOCK:
      FOR EACH valdaao WHERE valdaao.ARBARTKOD = arbarttemp.ARBARTKOD NO-LOCK:
         RUN laddaaotid IN nyttaoapph (INPUT valdaao.AONR,INPUT valdaao.DELNR,OUTPUT TABLE aotidslagtemp).
         FOR EACH aotidslagtemp WHERE aotidslagtemp.AONR = valdaao.AONR AND aotidslagtemp.DELNR = valdaao.DELNR
            AND aotidslagtemp.IDTLAG = "AONRAVSL"
            NO-LOCK:
            CREATE tidsplantemp.
            ASSIGN 
            tidsplantemp.AONR          = valdaao.AONR
            tidsplantemp.DELNR         = valdaao.DELNR
            tidsplantemp.ARBART        = arbarttemp.ARBBENAMNING
            tidsplantemp.AKTIVITET     = valdaao.ORT
            tidsplantemp.IDTIDLAG      = aotidslagtemp.IDTIDLAG
            tidsplantemp.TIDLAGE       = aotidslagtemp.TIDLAGE
            tidsplantemp.TIDSPERIOD    = ""
            tidsplantemp.STARTDAT      = aotidslagtemp.DAT1
            tidsplantemp.SLUTDAT       = aotidslagtemp.DAT2
            tidsplantemp.PERSONAL      = aotidslagtemp.ANVANDARE1
            tidsplantemp.ANDRAD        = FALSE.
         END.           
      END.
   END.
   RUN createbrw_UI IN tidsplandynh (INPUT TABLE tidsplantemp).
   RUN DYNBRW.P PERSISTENT SET brwproc[1] (INPUT dynbrwh).
   RUN dynprogextra IN brwproc[1] (INPUT tidsplandynh).
   RUN rowdispextrakor IN brwproc[1] (INPUT TRUE). 
   RUN addmenuitem2_UI IN brwproc[1] (INPUT dynbrwh, INPUT "Sätt startdatum", INPUT "setcol_UI").
   RUN addmenuitem2_UI IN brwproc[1] (INPUT dynbrwh, INPUT "Sätt slutdatum", INPUT "setcol_UI"). 
  
   RUN enable_UI.
   {FRMSIZE.I}  
   {WIN_M_SLUT.I}
   mainvar = FALSE.   
   IF NOT THIS-PROCEDURE:PERSISTENT THEN WAIT-FOR CLOSE OF THIS-PROCEDURE.  
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
   CREATE BROWSE dynbrwh.
   
   RUN xTIDSPLANDYN.P PERSISTENT SET tidsplandynh
      (INPUT dynbrwh,INPUT FRAME DEFAULT-FRAME:HANDLE).
  
   IF Guru.Konstanter:appcon THEN DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph.
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
  DISPLAY FILL-IN-AKT FILL-IN-TILLF FILL-IN-EXT FILL-IN-DELAD FILL-IN_PROJEKT 
          FILL-IN_DATUM 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-57 RECT-3 BTN_VECKA BTN_BORT BTN_AVS FILL-IN-AKT 
         FILL-IN-TILLF FILL-IN-EXT FILL-IN-DELAD FILL-IN_PROJEKT FILL-IN_DATUM 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

