&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
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
&SCOPED-DEFINE NEW
{GLOBVAR2DEL1.I}


DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
DEFINE VARIABLE upptime AS INTEGER NO-UNDO.
DEFINE VARIABLE blobproch AS HANDLE NO-UNDO.    
DEFINE VARIABLE sendat AS DATE NO-UNDO.
DEFINE VARIABLE sentid AS INTEGER NO-UNDO.
DEFINE VARIABLE dirlist AS CHARACTER NO-UNDO FORMAT "x(72)".
DEFINE VARIABLE attribut AS CHARACTER NO-UNDO .
DEFINE VARIABLE cdirlist AS CHARACTER NO-UNDO.
DEFINE VARIABLE cdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempi AS INTEGER NO-UNDO.
DEFINE VARIABLE clientpath AS CHARACTER NO-UNDO.
DEFINE VARIABLE numfil AS INTEGER NO-UNDO.
DEFINE VARIABLE antalupp AS CHARACTER NO-UNDO.
DEFINE VARIABLE logresult AS LOGICAL NO-UNDO.
DEFINE VARIABLE soklog AS LOGICAL NO-UNDO.
DEFINE VARIABLE tempquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE spardatum AS DATE NO-UNDO.
DEFINE VARIABLE spartid   AS INTEGER NO-UNDO.
&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{BLOB.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FBTN_UPPDAT BTN_AVB FILL-IN_DAT 
&Scoped-Define DISPLAYED-OBJECTS EDT_INFO FILL-IN_DAT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON FBTN_ALLA 
     LABEL "Uppdatera allt!" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON FBTN_UPPDAT 
     LABEL "Uppdatera" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE EDT_INFO AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 53.38 BY 8.5 NO-UNDO.

DEFINE VARIABLE FILL-IN_DAT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Uppdaterad" 
      VIEW-AS TEXT 
     SIZE 27.63 BY .63 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     EDT_INFO AT ROW 3.88 COL 1.5 NO-LABEL
     FBTN_UPPDAT AT ROW 4.5 COL 57
     FBTN_ALLA AT ROW 5.75 COL 57
     BTN_AVB AT ROW 11.38 COL 57
     FILL-IN_DAT AT ROW 3 COL 24.88 COLON-ALIGNED
     "Uppdaterar r-filer:" VIEW-AS TEXT
          SIZE 20 BY 1.25 AT ROW 1.46 COL 1.5
          FONT 17
     "Info:" VIEW-AS TEXT
          SIZE 8 BY .63 AT ROW 3 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.13 ROW 1
         SIZE 71 BY 11.83.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Uppdatera program"
         HEIGHT             = 11.79
         WIDTH              = 71
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
                                                                        */
/* SETTINGS FOR EDITOR EDT_INFO IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       EDT_INFO:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR BUTTON FBTN_ALLA IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_ALLA:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Uppdatera program */
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
ON WINDOW-CLOSE OF C-Win /* Uppdatera program */
DO:
  /* This event will close the window and terminate the procedure.  */
   APPLY "CLOSE":U TO THIS-PROCEDURE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_ALLA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_ALLA C-Win
ON CHOOSE OF FBTN_ALLA IN FRAME DEFAULT-FRAME /* Uppdatera allt! */
DO:
   /* Uppdatera */   
          
   MESSAGE "Detta kommer att ta några minuter. Vill du forstätta?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
   IF val = TRUE THEN DO:
      {muswait.i}
         
         /*
      kommando = "xcopy \\" + Guru.Konstanter:InternNr(STRING(151)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(156)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(177)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(197)) + "\delad\pro9\guru\CTID\*.r " + Guru.Konstanter:wtidvar + " /c/f".
      OS-COMMAND VALUE(kommando).   
      */
      kommando = "DEL " + Guru.Konstanter:wtidvar + "*.r".
     OS-COMMAND SILENT VALUE(kommando).
      /*
      
      
      OS-DELETE VALUE(kommando).
      */
      RUN uppdatera_UI (INPUT 2).
      MESSAGE "OBS! Nu kan du har ett svart fönster som rullar en massa text på din skärm. Avsluta inte detta program för än det fönstret är släckt!" VIEW-AS ALERT-BOX.
      {musarrow.i}
   END.

   /*
   RUN uppdatera_UI (INPUT 1).   
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_UPPDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_UPPDAT C-Win
ON CHOOSE OF FBTN_UPPDAT IN FRAME DEFAULT-FRAME /* Uppdatera */
DO:
   
   RUN uppdatera_UI (INPUT 2).
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
   RUN deleteproc_UI IN blobproch.
   IF VALID-HANDLE(blobproch) THEN DELETE PROCEDURE blobproch NO-ERROR.
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
   
   RUN DYNBLOBB.P PERSISTENT SET blobproch.
   RUN huvud_UI.
   RUN enable_UI.
   IF Guru.Konstanter:globforetag = "elpa" {GLOBVES.I} THEN DO:
      ENABLE FBTN_ALLA WITH FRAME {&FRAME-NAME}. 
   END.
   EDT_INFO:SENSITIVE = TRUE.
   IF sendat = ? THEN FILL-IN_DAT:SCREEN-VALUE = "?".
   ELSE FILL-IN_DAT:SCREEN-VALUE = "(" + STRING(sentid,"hh:mm:ss") + ")" + " " + STRING(sendat,"9999-99-99") NO-ERROR.
   IF soklog = TRUE THEN DO:
      APPLY "CLOSE" TO THIS-PROCEDURE.
      LEAVE MAIN-BLOCK.
   END.
   {FRMSIZE.I}
   {WIN_M_SLUT.I}
   {musarrow.i}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY EDT_INFO FILL-IN_DAT 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FBTN_UPPDAT BTN_AVB FILL-IN_DAT 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI C-Win 
PROCEDURE huvud_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes
------------------------------------------------------------------------------*/   
   DEFINE VARIABLE lefti AS INTEGER NO-UNDO.
   DEFINE VARIABLE righti AS INTEGER NO-UNDO.
   DEFINE VARIABLE tempch AS CHARACTER NO-UNDO.
   /*hämtar alla blobinfotemp varför ?
   
   */
   /*RUN senastuppdat_UI IN blobproch (OUTPUT sendat, OUTPUT sentid).   */

   /*
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
      FILE-INFO:FILE-NAME = "guru.r".
      clientpath = SUBSTRING(FILE-INFO:FULL-PATHNAME, 1, R-INDEX(FILE-INFO:FULL-PATHNAME,"CSTART") - 1).
   END.
   ELSE IF Guru.Konstanter:globanv = "cELPAO" THEN DO:
      
   END.
   ELSE IF Guru.Konstanter:varforetypval[14] = 1 THEN DO:
      IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
         clientpath = "D:\DELAD\PRO9\GURU\KOMP10\".         
      END.
       
      ELSE DO:
         tempch = SUBSTRING(PROPATH,1,R-INDEX(PROPATH,"WTID")).
         lefti = R-INDEX(tempch,",") + 1. 
         tempch = SUBSTRING(PROPATH,R-INDEX(PROPATH,"WTID")).
         righti = R-INDEX(PROPATH,"WTID") + INDEX(tempch,",") - 1.
         clientpath = SUBSTRING(PROPATH,lefti,(righti - lefti)).
         clientpath = clientpath + "\".
      END.         
   END.
   ASSIGN
   cdirlist = ""
   antalupp = ""
   EDT_INFO:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
   FILE-INFO:FILE-NAME = clientpath.
   IF FILE-INFO:FULL-PATHNAME = ? THEN DO: 
      MESSAGE "Hittar inte sökvägen:" SKIP clientpath VIEW-AS ALERT-BOX.
      soklog = TRUE.
      RETURN.
   END.
   ELSE soklog = FALSE.      
   
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:      

      /* DETTA SKA ENDAST GÄLLA FÖR WEBCLIENT!! */
      INPUT FROM OS-DIR(clientpath) NO-ECHO.
      REPEAT:
         SET ^ dirlist attribut.
         IF attribut = "D" AND ENTRY(NUM-ENTRIES(dirlist,"\"),dirlist,"\") BEGINS "c" THEN DO: 
            cdirlist = cdirlist + ENTRY(NUM-ENTRIES(dirlist,"\"),dirlist,"\") + ",".         
         END.
      END.
      INPUT CLOSE.
      /*
      IF cdirlist = "" THEN RETURN.
      cdirlist = TRIM(cdirlist,",").
      tempquery = "BLOBINFO.EXEPROG = '".
      REPEAT tempi=1 TO NUM-ENTRIES(cdirlist,","):
         cdir = ENTRY(tempi,cdirlist,",").
         IF tempi = 1 THEN DO: 
            tempquery = tempquery + cdir + "' ".
         END.
         ELSE DO: 
            /*tempquery = tempquery + "OR BLOBINFO.EXEPROG = '" + cdir + "' ".*/
         END.
      END. 
      */           
      tempquery = "".
      
   END.
   ELSE DO:
      tempquery = "".
      /*
      tempquery = "BLOBINFO.FILFORMAT = 'R'".
      */
   END.   
   
   RUN blobladda_UI IN blobproch (INPUT tempquery).
   RUN senastuppdat_UI IN blobproch (OUTPUT sendat, OUTPUT sentid).   
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE uppdatera_UI C-Win 
PROCEDURE uppdatera_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes
------------------------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
   DEFINE VARIABLE kom AS CHARACTER NO-UNDO.
   
   upptime = TIME.
   /*
   FIND FIRST blobinfotemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE blobinfotemp THEN RETURN.
   */ 
   
   IF Guru.Konstanter:globforetag = "cELPA" {GLOBVES.I} THEN DO:            
      kommando = "C:\ftrack\Bin\Appstart.exe Guru_UppdateGURU".
      OS-COMMAND SILENT VALUE(kommando).         
      RUN blobladdaen_UI IN blobproch (INPUT "vss.pf").
      FIND FIRST blobinfotemp NO-LOCK NO-ERROR.
      IF AVAILABLE blobinfotemp THEN RUN uppklient_UI.
   END.   
   ASSIGN
   EDT_INFO:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
   numfil = 0.   
   RUN senastuppdat_UI IN blobproch (OUTPUT sendat, OUTPUT sentid).
   {muswait.i}
   /*SKAPAR BLOBCOMP FÖR SENAST UPPDATERINGEN*/
   IF sendat = ? THEN DO:
      RUN blobladda_UI IN blobproch (INPUT "").
      RUN skapauppdat_UI IN blobproch.
   END.
   ELSE DO:
      kom = "BLOBINFO.SKAPDAT >= " + STRING(sendat).
      RUN blobladda_UI IN blobproch (INPUT kom).
   END.
   DEFINE VARIABLE AA AS INTEGER NO-UNDO.
   IF sendat = ? OR vart = 1 THEN DO:
      FOR EACH blobinfotemp NO-LOCK:
         RUN uppklient_UI.         
      END.
      EDT_INFO:SCREEN-VALUE = EDT_INFO:SCREEN-VALUE +  CHR(10) +  CHR(10)  + "Klar!".
      EDT_INFO:SCREEN-VALUE = EDT_INFO:SCREEN-VALUE +  CHR(10) + "OBS! Nu kan du har ett svart fönster som rullar en massa text på din skärm. Avsluta inte detta program för än det fönstret är släckt!".
   END.
   ELSE DO:
      FOR EACH blobinfotemp WHERE blobinfotemp.SKAPDAT >= sendat NO-LOCK:
         RUN uppdat_UI.
         IF blobinfotemp.SKAPDAT > sendat THEN DO:
            RUN uppklient_UI.
         END.
         ELSE DO:
            IF blobinfotemp.SKAPDAT = sendat AND blobinfotemp.INTID > sentid THEN RUN uppklient_UI.                       
         END.         
      END.
      EDT_INFO:SCREEN-VALUE = EDT_INFO:SCREEN-VALUE +  CHR(10) +  CHR(10)  + "Klar!".
      EDT_INFO:SCREEN-VALUE = EDT_INFO:SCREEN-VALUE +  CHR(10) + "OBS! Nu kan du har ett svart fönster som rullar en massa text på din skärm. Avsluta inte detta program för än det fönstret är släckt!".
   END.
   {musarrow.i}
    
   IF spardatum NE ? THEN RUN andrauppdat_UI IN blobproch (INPUT TODAY, INPUT TIME).
   IF sendat = ? THEN FILL-IN_DAT:SCREEN-VALUE = "?".
   ELSE FILL-IN_DAT:SCREEN-VALUE = "(" + STRING(sentid,"hh:mm:ss") + ")" + " " + STRING(sendat,"9999-99-99") .   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE uppdat_UI C-Win 
PROCEDURE uppdat_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes
------------------------------------------------------------------------------*/      
   IF spardatum = ? THEN DO:
      ASSIGN
      spardatum = blobinfotemp.SKAPDAT
      spartid = blobinfotemp.INTID.
   END.
   IF blobinfotemp.SKAPDAT > spardatum THEN DO:
      ASSIGN
      spardatum = blobinfotemp.SKAPDAT
      spartid = blobinfotemp.INTID. 
   END.
   ELSE IF blobinfotemp.SKAPDAT = spardatum AND blobinfotemp.INTID > spartid THEN DO:
      ASSIGN
      spardatum = blobinfotemp.SKAPDAT
      spartid = blobinfotemp.INTID. 
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE uppklient_UI C-Win 
PROCEDURE uppklient_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes
------------------------------------------------------------------------------*/     
   DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
   RUN uppdat_UI.
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
      mappvar = SUBSTRING(Guru.Konstanter:guruvar,1,LENGTH(Guru.Konstanter:guruvar) - 1) +  blobinfotemp.VARTWC + "\".
   END.
   ELSE DO:
      mappvar = SUBSTRING(Guru.Konstanter:guruvar,1,LENGTH(Guru.Konstanter:guruvar) - 1) + blobinfotemp.VART + "\".
      IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) 
      THEN DO:
         IF INDEX(mappvar,"WTID") NE 0 THEN DO:
            mappvar = REPLACE(mappvar,"WTID","KOMP10").
         END.
      END.
   END.
   /*
   EDT_INFO:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
   EDT_INFO:SCREEN-VALUE = "Uppdaterar: " + CHR(10) + mappvar +  blobinfotemp.FILNAMN + CHR(10) + CHR(10).       
   RUN klientprog_UI IN blobproch (INPUT blobinfotemp.ID, INPUT mappvar).
   numfil = numfil + 1.
   EDT_INFO:SCREEN-VALUE = EDT_INFO:SCREEN-VALUE + "Antal uppdaterade: "  +  STRING(numfil) + CHR(10) + "Total tid: " + STRING((TIME - upptime),"hh:mm:ss").
   */
   
   EDT_INFO = "".
   EDT_INFO = "Uppdaterar: " + CHR(10) + mappvar +  blobinfotemp.FILNAMN + CHR(10) + CHR(10).       
   RUN klientprog_UI IN blobproch (INPUT blobinfotemp.ID, INPUT mappvar).
   numfil = numfil + 1.
   EDT_INFO = EDT_INFO + "Antal uppdaterade: "  +  STRING(numfil) + CHR(10) + "Total tid: " + STRING((TIME - upptime),"hh:mm:ss").
   DISPLAY EDT_INFO WITH FRAME {&FRAME-NAME}. 

   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

