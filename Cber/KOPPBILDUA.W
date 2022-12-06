&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
&Scoped-define SHARED SHARED
{KONSTRMTRL.I}
{BBENAMNTEMP.I}
&Scoped-define NEW NEW 
&Scoped-define SHARED SHARED
{BERBILD.I}
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
DEFINE VARIABLE filedir AS CHARACTER NO-UNDO.
DEFINE VARIABLE hjfilvar AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE filname AS CHARACTER NO-UNDO.



DEFINE VARIABLE nybild AS LOGICAL NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE koppbildapph AS HANDLE NO-UNDO.
DEFINE VARIABLE rowidvar AS ROWID NO-UNDO.
DEFINE VARIABLE frow AS ROWID NO-UNDO.        
DEFINE VARIABLE trow AS ROWID NO-UNDO.   
DEFINE VARIABLE frannr AS INTEGER NO-UNDO.  
DEFINE VARIABLE tillnr AS INTEGER NO-UNDO.
DEFINE VARIABLE bortrow AS ROWID NO-UNDO. 
DEFINE VARIABLE bildant AS INTEGER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE initdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE blobproch AS HANDLE NO-UNDO.   
DEFINE VARIABLE blobid AS INTEGER NO-UNDO.
DEFINE VARIABLE logresult AS LOGICAL NO-UNDO.
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
&Scoped-define BROWSE-NAME BRW_BILD

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES berbildtemp

/* Definitions for BROWSE BRW_BILD                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_BILD berbildtemp.KTYPKOD ~
berbildtemp.HJKONSTGRUPPB berbildtemp.HJUNDERGRUPPB berbildtemp.FILNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BILD 
&Scoped-define QUERY-STRING-BRW_BILD FOR EACH berbildtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BILD OPEN QUERY BRW_BILD FOR EACH berbildtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BILD berbildtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BILD berbildtemp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_BILD BTN_VISA BTN_BORT BTN_AVB 

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

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BILD FOR 
      berbildtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BILD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BILD C-Win _STRUCTURED
  QUERY BRW_BILD NO-LOCK DISPLAY
      berbildtemp.KTYPKOD FORMAT "X(256)":U WIDTH 20
      berbildtemp.HJKONSTGRUPPB COLUMN-LABEL "Konstruktionsgrupp" FORMAT "x(256)":U
            WIDTH 20
      berbildtemp.HJUNDERGRUPPB COLUMN-LABEL "Undergrupp" FORMAT "x(256)":U
            WIDTH 20
      berbildtemp.FILNAMN FORMAT "X(25)":U WIDTH 40
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 87.5 BY 25
         TITLE "Bilder".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_BILD AT ROW 2 COL 1.5
     BTN_VISA AT ROW 2.08 COL 91
     BTN_BORT AT ROW 27.5 COL 37.25
     BTN_AVB AT ROW 27.92 COL 91
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 105.75 BY 28.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: berbildtemp T "?" NO-UNDO temp-db berbildtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 28.42
         WIDTH              = 105.75
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 105.75
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 105.75
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
/* BROWSE-TAB BRW_BILD 1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BILD
/* Query rebuild information for BROWSE BRW_BILD
     _TblList          = "Temp-Tables.berbildtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.berbildtemp.KTYPKOD
"KTYPKOD" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.berbildtemp.HJKONSTGRUPPB
"HJKONSTGRUPPB" "Konstruktionsgrupp" "x(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.berbildtemp.HJUNDERGRUPPB
"HJUNDERGRUPPB" "Undergrupp" "x(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.berbildtemp.FILNAMN
"FILNAMN" ? ? "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BILD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BILD
&Scoped-define SELF-NAME BRW_BILD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BILD C-Win
ON VALUE-CHANGED OF BRW_BILD IN FRAME DEFAULT-FRAME /* Bilder */
DO:
   status-ok = BRW_BILD:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT C-Win
ON CHOOSE OF BTN_BORT IN FRAME DEFAULT-FRAME /* Ta bort */
DO: 
   felmedd = "".
   status-ok = BRW_BILD:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
   IF AVAILABLE berbildtemp THEN DO:   
      ASSIGN      
      bortrow = berbildtemp.BBROW.
      MESSAGE "Vill Ni ta bort bild: " + berbildtemp.FILNAMN + "?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar AS LOGICAL.         
      IF svar THEN DO:
         RUN bort_UI IN koppbildapph (INPUT bortrow,OUTPUT felmedd).
         IF felmedd NE "" THEN DO:
            MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
            felmedd = "".
         END.
         ELSE DO:
            FIND FIRST berbildtemp WHERE berbildtemp.BBROW = bortrow NO-LOCK NO-ERROR.
            IF AVAILABLE berbildtemp THEN DO:
               DELETE berbildtemp.
            END.         
            RUN selnextprevrow_UI IN brwproc[1].
         END.
      END.
      ELSE DO:
         musz = musz.
      END.
      RUN openbdynspec_UI IN brwproc[1]. 
      RUN lastselectdyn_UI IN brwproc[1].     
   END.
   ELSE DO:
      MESSAGE "Det finns ingen bild att ta bort."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISA C-Win
ON CHOOSE OF BTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO:
   DEFINE VARIABLE hjfilvar AS CHARACTER NO-UNDO.
   IF AVAILABLE berbildtemp THEN DO:
      hjfilvar = initdir + berbildtemp.FILNAMN.
      IF Guru.Konstanter:appcon THEN RUN FINNSTABELL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "BLOBINFO", OUTPUT bloblog).
      ELSE RUN FINNSTABELL.P (INPUT "BLOBINFO", OUTPUT bloblog).
      IF bloblog = TRUE THEN DO:
         IF NOT VALID-HANDLE(blobproch) THEN DO:
            {FINNSDYNBLOB.I}
         END.    
         DEFINE VARIABLE resid AS INTEGER NO-UNDO.
         RUN blobfil_UI IN blobproch (INPUT berbildtemp.FILNAMN, OUTPUT resid).
         IF resid = ? THEN DO: 
            DEFINE VARIABLE sokbild AS CHARACTER NO-UNDO.
            sokbild = hjfilvar.
            hjfilvar = SEARCH(sokbild).
         END.
         ELSE DO:
            FIND FIRST blobinfotemp WHERE blobinfotemp.ID = resid NO-LOCK NO-ERROR.
            RUN blobopen_UI IN blobproch (INPUT blobinfotemp.FILNAMN, OUTPUT hjfilvar).
         END.
         IF hjfilvar NE ? THEN DO: 
            RUN OPENDOC.P (hjfilvar,"","",NO).
         END.
         ELSE DO:      
            MESSAGE "Filen " + berbildtemp.FILNAMN + " finns inte!".
         END.
      END.
      ELSE DO: 
         hjfilvar = filedir +  berbildtemp.FILNAMN.
         hjfilvar = SEARCH(hjfilvar).
         IF hjfilvar NE ? THEN DO: 
            RUN OPENDOC.P (hjfilvar,"","",NO).
         END.
         ELSE DO:      
            MESSAGE "Filen " + berbildtemp.FILNAMN + " finns inte!".
         END.
      END.
   END.
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
   IF VALID-HANDLE(blobproch) THEN DO: 
      RUN deleteproc_UI IN blobproch.
      DELETE PROCEDURE blobproch NO-ERROR.
   END.
   IF VALID-HANDLE(koppbildapph) THEN DELETE PROCEDURE koppbildapph.
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
   initdir = Guru.Konstanter:gurubilder.
   {&WINDOW-NAME}:TITLE = "Konstruktioner med kopplade bilder.".     
   EMPTY TEMP-TABLE berbildtemp  NO-ERROR. 
   RUN laddabild2_UI IN koppbildapph (OUTPUT TABLE berbildtemp).
   RUN setcolindex_UI IN brwproc[1] (INPUT "HJKONSTGRUPPB BY HJUNDERGRUPPB BY KTYPKOD").    
   RUN openbdynspec_UI IN brwproc[1]. 
   RUN enable_UI.         
   APPLY "VALUE-CHANGED" TO BRW_BILD.
   {FRMSIZE.I}  
   {musarrow.i}
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
  Purpose:    
  Parameters:  
  Notes:       
-------------------------------------------------------------*/       
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_BILD:HANDLE IN FRAME {&FRAME-NAME}).
   RUN brwsetupstop_UI IN brwproc[1] (INPUT 1).
   IF Guru.Konstanter:appcon THEN DO:
      RUN KOPPBILDAPP.P PERSISTENT SET koppbildapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN KOPPBILDAPP.P PERSISTENT SET koppbildapph.
   END.  
   IF Guru.Konstanter:appcon = TRUE THEN DO:
      RUN FINNSTABELL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "BLOBINFO", OUTPUT logresult).
   END.
   ELSE DO:
      RUN FINNSTABELL.P (INPUT "BLOBINFO", OUTPUT logresult).
   END.
   IF logresult = TRUE THEN DO:
      {FINNSDYNBLOB.I}
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
  ENABLE BRW_BILD BTN_VISA BTN_BORT BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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
   IF tempfil NE "" THEN orgdir = REPLACE(fildir,tempfil,"").      
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

