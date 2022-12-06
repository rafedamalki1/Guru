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
{ALLDEF.I}
/* För DYNBRW.P */
&Scoped-define NEW
/*{EGENBVAR.I}*/
{GLOBVAR2DEL1.I}                                                
DEFINE NEW SHARED VARIABLE blobproch AS HANDLE NO-UNDO.    


                              
DEFINE VARIABLE computername AS CHARACTER NO-UNDO.
DEFINE VARIABLE retvalue AS INTEGER NO-UNDO.
DEFINE VARIABLE logresult AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE blobid AS INTEGER NO-UNDO.
DEFINE VARIABLE chrfilnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE flerafiler AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempi AS INTEGER NO-UNDO.
DEFINE VARIABLE pathstr AS CHARACTER NO-UNDO.
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
DEFINE VARIABLE fildir AS CHARACTER NO-UNDO.
DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE compdir NO-UNDO
      FIELD BIB AS CHARACTER. 
&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{BLOB.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_BLOB

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES blobinfotemp

/* Definitions for BROWSE BRW_BLOB                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_BLOB blobinfotemp.FILNAMN ~
blobinfotemp.FILFORMAT blobinfotemp.STORLEK blobinfotemp.EXEPROG ~
blobinfotemp.MODINFO blobinfotemp.VART blobinfotemp.VARTWC ~
blobinfotemp.SKAPDAT STRING(INTID,"hh:mm:ss") blobinfotemp.ANDDAT ~
STRING(ANDTID,"hh:mm:ss") 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BLOB 
&Scoped-define QUERY-STRING-BRW_BLOB FOR EACH blobinfotemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_BLOB OPEN QUERY BRW_BLOB FOR EACH blobinfotemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_BLOB blobinfotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BLOB blobinfotemp


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_BLOB}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_BLOB BTN_NY BTN_BORT BTN_AND BTN_KOR ~
BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "BLOB huvud" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_KOR 
     LABEL "Testkör" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BLOB FOR 
      blobinfotemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BLOB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BLOB C-Win _STRUCTURED
  QUERY BRW_BLOB NO-LOCK DISPLAY
      blobinfotemp.FILNAMN COLUMN-LABEL "Filnamn" FORMAT "X(256)":U
            WIDTH 20
      blobinfotemp.FILFORMAT COLUMN-LABEL "Format" FORMAT "X(6)":U
      blobinfotemp.STORLEK COLUMN-LABEL "Storlek(kB)" FORMAT ">>>>>>9":U
      blobinfotemp.EXEPROG COLUMN-LABEL "Program (.exe)" FORMAT "X(15)":U
            WIDTH 14.63
      blobinfotemp.MODINFO COLUMN-LABEL "Användning" FORMAT "X(11)":U
      blobinfotemp.VART COLUMN-LABEL "Klient mapp" FORMAT "X(256)":U
            WIDTH 11
      blobinfotemp.VARTWC COLUMN-LABEL "WC-mapp" FORMAT "X(256)":U
            WIDTH 10
      blobinfotemp.SKAPDAT COLUMN-LABEL "Inlagd" FORMAT "99/99/99":U
            WIDTH 9
      STRING(INTID,"hh:mm:ss") COLUMN-LABEL "klockan" WIDTH 9
      blobinfotemp.ANDDAT COLUMN-LABEL "Senast" FORMAT "99/99/99":U
            WIDTH 9
      STRING(ANDTID,"hh:mm:ss") COLUMN-LABEL "Ändrad" WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 121.38 BY 17.71
         TITLE "Blobinfo".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_BLOB AT ROW 3.79 COL 1.5
     BTN_NY AT ROW 22.21 COL 26
     BTN_BORT AT ROW 22.21 COL 41.13
     BTN_AND AT ROW 22.21 COL 56.25
     BTN_KOR AT ROW 22.21 COL 71.38
     BTN_AVB AT ROW 22.21 COL 108.88
     "BLOB admin!" VIEW-AS TEXT
          SIZE 18.13 BY 1.04 AT ROW 1.63 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.38 BY 22.79.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: blobinfotemp T "?" NO-UNDO temp-db blobinfotemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "BLOB tester!"
         HEIGHT             = 22.75
         WIDTH              = 123.75
         MAX-HEIGHT         = 30.04
         MAX-WIDTH          = 128
         VIRTUAL-HEIGHT     = 30.04
         VIRTUAL-WIDTH      = 128
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
/* BROWSE-TAB BRW_BLOB TEXT-1 DEFAULT-FRAME */
ASSIGN 
       BRW_BLOB:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BRW_BLOB:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BLOB
/* Query rebuild information for BROWSE BRW_BLOB
     _TblList          = "Temp-Tables.blobinfotemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.blobinfotemp.FILNAMN
"FILNAMN" "Filnamn" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.blobinfotemp.FILFORMAT
"FILFORMAT" "Format" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.blobinfotemp.STORLEK
"STORLEK" "Storlek(kB)" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.blobinfotemp.EXEPROG
"EXEPROG" "Program (.exe)" ? "character" ? ? ? ? ? ? no ? no no "14.63" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.blobinfotemp.MODINFO
"MODINFO" "Användning" "X(11)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.blobinfotemp.VART
"VART" "Klient mapp" "X(256)" "character" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.blobinfotemp.VARTWC
"VARTWC" "WC-mapp" "X(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.blobinfotemp.SKAPDAT
"SKAPDAT" "Inlagd" ? "date" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"STRING(INTID,""hh:mm:ss"")" "klockan" ? ? ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.blobinfotemp.ANDDAT
"ANDDAT" "Senast" ? "date" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"STRING(ANDTID,""hh:mm:ss"")" "Ändrad" ? ? ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_BLOB */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* BLOB tester! */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* BLOB tester! */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BLOB
&Scoped-define SELF-NAME BRW_BLOB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BLOB C-Win
ON VALUE-CHANGED OF BRW_BLOB IN FRAME DEFAULT-FRAME /* Blobinfo */
DO:
   /* ccc */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND C-Win
ON CHOOSE OF BTN_AND IN FRAME DEFAULT-FRAME /* BLOB huvud */
DO:      
   BRW_BLOB:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE blobinfotemp THEN DO:
      blobid = blobinfotemp.ID.
      RUN BLOBNYAND.W (INPUT-OUTPUT blobid, INPUT FALSE).
      RUN openbdynspec_UI IN brwproc[1].
      FIND FIRST blobinfotemp WHERE blobinfotemp.ID = blobid NO-LOCK NO-ERROR.
      IF AVAILABLE blobinfotemp THEN RUN selectbyrowid_UI IN brwproc[1] (INPUT ROWID(blobinfotemp)).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT C-Win
ON CHOOSE OF BTN_BORT IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
   BRW_BLOB:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE blobinfotemp THEN DO:
      MESSAGE "Vill du ta bort alla markerade rader?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar.
      IF svar = TRUE THEN DO:
         {muswait.i}
         REPEAT tempi = 1 TO BRW_BLOB:NUM-SELECTED-ROWS:
            BRW_BLOB:FETCH-SELECTED-ROW(tempi).
            IF tempi = BRW_BLOB:NUM-SELECTED-ROWS THEN DO:
               blobid = blobinfotemp.ID.
               RUN openbdynspec_UI IN brwproc[1].
               FIND FIRST blobinfotemp WHERE blobinfotemp.ID = blobid NO-LOCK NO-ERROR.
               RUN selectbyrowid_UI IN brwproc[1] (INPUT ROWID(blobinfotemp)).
               RUN selnextprevrow_UI IN brwproc[1].
               RUN blobtabort_UI IN blobproch (INPUT blobinfotemp.ID).
               RUN openbdynspec_UI IN brwproc[1].
               RUN lastselectdyn_UI IN brwproc[1].
            END.
            ELSE RUN blobtabort_UI IN blobproch (INPUT blobinfotemp.ID).
         END.         
         {musarrow.i}
      END.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOR C-Win
ON CHOOSE OF BTN_KOR IN FRAME DEFAULT-FRAME /* Testkör */
DO:
   BRW_BLOB:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE blobinfotemp THEN DO:      
      RUN bloboppnafil_UI IN blobproch (INPUT blobinfotemp.ID).      
      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY C-Win
ON CHOOSE OF BTN_NY IN FRAME DEFAULT-FRAME /* Ny */
DO:      
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
   OS-CREATE-DIR VALUE(fildir) NO-ERROR.
   mappvar = fildir.
   SYSTEM-DIALOG GET-FILE fildir
   TITLE          "Välj den fil som du vill lägga in i databasen"
   /*FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"*/
   INITIAL-DIR    mappvar
   UPDATE OKvald.      
   IF OKvald = TRUE THEN DO:      
         RUN skapablobbar_UI (INPUT fildir).                  
   END.   
   
    /*       
   
   RUN FILEMULTISEL.P ( INPUT "All Files (*.*)|*.*",
                        INPUT ?,
                        INPUT "Välj de filer som du vill lägga in i databasen.",
                        OUTPUT flerafiler,
                        OUTPUT logresult).
   {muswait.i}
      
   IF logresult = TRUE THEN DO:                 
      REPEAT tempi = 1 TO NUM-ENTRIES(flerafiler):
         RUN skapablobbar_UI (INPUT ENTRY(tempi,flerafiler)).
      END.
   END.
   {musarrow.i}*/
   RUN openbdynspec_UI IN brwproc[1].
   
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
   IF VALID-HANDLE(blobproch) THEN RUN deleteproc_UI IN blobproch.
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
   {ALLSTARTDYN.I}   
   RUN enable_UI.
      /*
   IF Guru.Konstanter:varforetypval[14] = 1 OR SESSION:CLIENT-TYPE = "WEBCLIENT" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      BTN_TEST:HIDDEN = FALSE.
   END.
   ELSE BTN_TEST:HIDDEN = TRUE.
   */
   {FRMSIZE.I}
      
   IF logresult = TRUE THEN DO:
    
     {FINNSDYNBLOB.I}
      MESSAGE "Vill du hämta gammla?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL UPDATE svarkopp AS LOGICAL.         
      CASE svarkopp:
         WHEN TRUE THEN DO:
            RUN blobladda_UI IN blobproch (INPUT "").
         END.
         WHEN FALSE THEN DO:
            
         END.      
      END CASE.         
      
      RUN setdefaultcolbyname_UI IN brwproc[1] (INPUT "ANDDAT").
      RUN setpdatacolsort_UI IN brwproc[1] (INPUT "ANDDAT", INPUT "ANDTID DESCENDING").
      RUN colsortdynbrw_UI IN brwproc[1] (INPUT "").
      RUN setdescvarcol_UI IN brwproc[1] (INPUT TRUE).
      RUN openbdynspec_UI IN brwproc[1].
   END.
   ELSE DO: 
      MESSAGE "Finns ingen BLOBINFO tabell i denna databas!" VIEW-AS ALERT-BOX.
      APPLY "CLOSE" TO THIS-PROCEDURE.
      LEAVE MAIN-BLOCK.
   END.   
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
   RUN DYNBRW.P PERSISTENT SET brwproc[1] (INPUT BRW_BLOB:HANDLE IN FRAME {&FRAME-NAME}).
   IF Guru.Konstanter:appcon = TRUE THEN DO:
      RUN FINNSTABELL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "BLOBINFO", OUTPUT logresult).
   END.
   ELSE DO:
      RUN FINNSTABELL.P (INPUT "BLOBINFO", OUTPUT logresult).
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
  ENABLE BRW_BLOB BTN_NY BTN_BORT BTN_AND BTN_KOR BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdisp_UI C-Win 
PROCEDURE rowdisp_UI :
/*------------------------------------------------------------------------------
  Purpose:     -
  Parameters:  <none>
  Notes:       -
------------------------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapablobbar_UI C-Win 
PROCEDURE skapablobbar_UI :
/*------------------------------------------------------------------------------
  Purpose:     -
  Parameters:  <none>
  Notes:       -
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER fildir AS CHARACTER NO-UNDO.
   DEFINE VARIABLE exeinfovar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vartvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vartvarwc AS CHARACTER NO-UNDO.
   DEFINE VARIABLE dummy AS CHARACTER NO-UNDO.
   DEFINE VARIABLE tempfil AS CHARACTER NO-UNDO.
   DEFINE VARIABLE searchdir AS CHARACTER NO-UNDO.
   DEFINE VARIABLE filnamn AS CHARACTER FORMAT "x(25)" LABEL "File" NO-UNDO.
   DEFINE VARIABLE attrlist AS CHARACTER FORMAT "x(6)" LABEL "Attributes" NO-UNDO.
   DEFINE VARIABLE orgdir AS CHARACTER NO-UNDO.
   ASSIGN
   searchdir = Guru.Konstanter:guruvar.  /*Sökväg till de filer som ska kompileras.*/
   tempfil = SUBSTRING(fildir,R-INDEX(fildir,"\") + 1).
   orgdir = REPLACE(fildir,tempfil,"").
   RUN blobcheck_UI IN blobproch (INPUT fildir, OUTPUT blobid).
   ASSIGN
   vartvar    = ""
   vartvarwc  = ""
   exeinfovar = "".
   EMPTY TEMP-TABLE compdir NO-ERROR.  
   IF entry(2,tempfil,".") = "R" THEN DO: 
      vartvarwc = SEARCH((ENTRY(1,tempfil,".") + ".w")).
      IF vartvarwc = ? OR vartvarwc BEGINS "." THEN vartvarwc = SEARCH((ENTRY(1,tempfil,".") + ".p")).
      IF vartvarwc = ? OR vartvarwc BEGINS "." THEN DO:
         INPUT FROM OS-DIR(searchdir) NO-ECHO.
         REPEAT:
            SET filnamn ^ attrlist NO-ERROR.
            /*Kolla om filnamnet börjar på 'c' och om filen är en mapp.*/
            IF filnamn NE ? THEN DO:
               IF (filnamn BEGINS "c" OR filnamn BEGINS "W") AND attrlist = "D" THEN DO: 
                  CREATE compdir.
                  compdir.BIB = searchdir + filnamn + "\".      
               END.
            END.
         END.
         TRAFF:
         FOR EACH compdir NO-LOCK:
            vartvarwc = SEARCH((ENTRY(1,compdir.BIB + tempfil,".") + ".w")).
            IF vartvarwc = ? THEN vartvarwc = SEARCH((ENTRY(1,compdir.BIB + tempfil,".") + ".p")).
            IF vartvarwc NE ? THEN LEAVE TRAFF.
            vartvarwc = "".
         END.
         
      END.
      IF vartvarwc = ? OR vartvarwc = "" THEN vartvarwc = vartvarwc.
      ELSE DO:
         dummy = ENTRY(2, vartvarwc, ".").
         vartvarwc = "\" + ENTRY(NUM-ENTRIES(vartvarwc,"\") - 1, vartvarwc, "\").
      END.
      vartvar = "\WTID".
      vartvarwc = REPLACE(vartvarwc,"\W","\C").
   END.
   ELSE DO:
      ASSIGN
      vartvar = REPLACE(fildir,Guru.Konstanter:guruvar,"\").
      vartvar = REPLACE(vartvar,"\" + tempfil,"").
      vartvar = REPLACE(vartvar,"\C","\W").
      IF SUBSTRING(vartvar,1,6) = "\komp10" THEN DO:
         SUBSTRING(vartvar,1,6) = " \WTID".      
         vartvar = TRIM(vartvar).
      END.
      IF SUBSTRING(vartvar,1,5) = "\WTID" THEN vartvar = vartvar.
      ELSE DO:
         vartvar = "\WTID".
         IF orgdir = Guru.Konstanter:guruvar THEN vartvar = "".
      END.
      INPUT FROM OS-DIR(searchdir) NO-ECHO.
      REPEAT:
         SET filnamn ^ attrlist NO-ERROR.
         /*Kolla om filnamnet börjar på 'c' och om filen är en mapp.*/
         IF filnamn NE ? THEN DO:
            IF (filnamn BEGINS "c" OR filnamn BEGINS "W") AND attrlist = "D" THEN DO: 
               CREATE compdir.
               compdir.BIB = searchdir + filnamn + "\".                  
            END.
         END.
      END.
      
      TRAFFWC:
      FOR EACH compdir NO-LOCK:
         vartvarwc = SEARCH(compdir.BIB + tempfil).
         IF vartvarwc NE ? THEN LEAVE TRAFFWC.
         vartvarwc = "".
      END.
      IF vartvarwc = "" THEN DO:
         vartvarwc = vartvar.
      END.
      ELSE vartvarwc = "\" + ENTRY(NUM-ENTRIES(vartvarwc,"\") - 1, vartvarwc, "\").
      vartvarwc = REPLACE(vartvarwc,"\W","\C").        
   END.
   
   RUN blobskapa_UI IN blobproch (INPUT fildir, INPUT-OUTPUT blobid, INPUT "PROG",
                                  INPUT exeinfovar,INPUT vartvar,INPUT vartvarwc, OUTPUT felmedd).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

