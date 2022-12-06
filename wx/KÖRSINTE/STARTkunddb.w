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

{ALLDEF2.I}
{APPCONDEF.I}
{VALDBDEF.I}
&Scoped-define NEW NEW
/*{EGENBVAR.I}*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_VDB

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES valdbtemp

/* Definitions for BROWSE BRW_VDB                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VDB valdbtemp.FORETAG valdbtemp.VALDB 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VDB 
&Scoped-define QUERY-STRING-BRW_VDB FOR EACH valdbtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VDB OPEN QUERY BRW_VDB FOR EACH valdbtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VDB valdbtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VDB valdbtemp


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_VDB}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_VDB FILL-IN-ANV FILL-IN-LOS ~
FILL-IN-program-2 FILL-IN-1 BTN_START-2 BTN_START-3 FILL-IN-APPSERVER ~
FILL-IN-program FILL-IN-2 BTN_START BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ANV FILL-IN-LOS FILL-IN-program-2 ~
FILL-IN-1 FILL-IN-APPSERVER FILL-IN-program FILL-IN-2 

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
     SIZE 18 BY 2.

DEFINE BUTTON BTN_START AUTO-GO 
     LABEL "Starta special" 
     SIZE 18 BY 2.

DEFINE BUTTON BTN_START-2 AUTO-GO 
     LABEL "Visa innehåll" 
     SIZE 18 BY 2.

DEFINE BUTTON BTN_START-3 AUTO-GO 
     LABEL "Starta Guru" 
     SIZE 18 BY 2.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "normalt ADMdynbrowse.w" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL "normalt STARTUPPDAT.P" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ANV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Användare" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-APPSERVER AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "APPSERVER" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LOS AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lösen" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-program AS CHARACTER FORMAT "X(256)":U 
     LABEL "Programm" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-program-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Programm" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

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
      valdbtemp.VALDB COLUMN-LABEL "Databas" FORMAT "X(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 56.5 BY 23
         TITLE "Databaser" TOOLTIP "Välj databas".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_VDB AT ROW 3.75 COL 4
     FILL-IN-ANV AT ROW 6.5 COL 70.5 COLON-ALIGNED
     FILL-IN-LOS AT ROW 8.67 COL 70.5 COLON-ALIGNED BLANK 
     FILL-IN-program-2 AT ROW 10.5 COL 69.5 COLON-ALIGNED
     FILL-IN-1 AT ROW 10.54 COL 96.13 COLON-ALIGNED NO-LABEL
     BTN_START-2 AT ROW 12 COL 63.63
     BTN_START-3 AT ROW 12 COL 83.5
     FILL-IN-APPSERVER AT ROW 22 COL 70.5 COLON-ALIGNED
     FILL-IN-program AT ROW 23.75 COL 70.5 COLON-ALIGNED
     FILL-IN-2 AT ROW 23.75 COL 96 COLON-ALIGNED NO-LABEL
     BTN_START AT ROW 25.25 COL 63.63
     BTN_AVB AT ROW 25.25 COL 83.5
     "Välj databas för start av Guru !" VIEW-AS TEXT
          SIZE 35.25 BY 2 AT ROW 1.25 COL 5.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 121.63 BY 27.75.


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
         HEIGHT             = 27.75
         WIDTH              = 121.63
         MAX-HEIGHT         = 29.83
         MAX-WIDTH          = 121.63
         VIRTUAL-HEIGHT     = 29.83
         VIRTUAL-WIDTH      = 121.63
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
/* BROWSE-TAB BRW_VDB TEXT-2 DEFAULT-FRAME */
ASSIGN 
       FILL-IN-1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VDB
/* Query rebuild information for BROWSE BRW_VDB
     _TblList          = "Temp-Tables.valdbtemp"
     _FldNameList[1]   = Temp-Tables.valdbtemp.FORETAG
     _FldNameList[2]   > Temp-Tables.valdbtemp.VALDB
"valdbtemp.VALDB" "Databas" "X(40)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
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
ON ROW-DISPLAY OF BRW_VDB IN FRAME DEFAULT-FRAME /* Databaser */
DO:
   
   IF valdbtemp.VALDB = "Vattenfall Service AB" OR valdbtemp.VALDB = "Graninge Nät AB" OR 
      valdbtemp.VALDB = "Sundsvall Energi Elnät AB"THEN DO:
      ASSIGN
      valdbtemp.FORETAG:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12. 
      valdbtemp.VALDB:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12. 
   END.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VDB C-Win
ON VALUE-CHANGED OF BRW_VDB IN FRAME DEFAULT-FRAME /* Databaser */
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:       
  /* IF Guru.Konstanter:globforetag = "SOLE" THEN QUIT.*/
   SESSION:PRINTER-CONTROL-HANDLE = 0.
  /* RUN val_UI.*/
 /*  DEFAULT-WINDOW:HIDDEN = FALSE.*/
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON ENDKEY OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
  SESSION:PRINTER-CONTROL-HANDLE = 0.
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_START C-Win
ON CHOOSE OF BTN_START IN FRAME DEFAULT-FRAME /* Starta special */
DO:  
   ASSIGN
   FILL-IN-APPSERVER = INPUT FILL-IN-APPSERVER
   FILL-IN-ANV     =   INPUT FILL-IN-ANV
   FILL-IN-LOS     = INPUT FILL-IN-LOS    
   FILL-IN-program = INPUT FILL-IN-program.
   /*För avtalsprogram STARTAVT.P*/
   FILL-IN-APPSERVER = FALSE.
   MESSAGE "går ej med appserver"
   VIEW-AS ALERT-BOX.
   IF FILL-IN-ANV = CHR(107) + CHR(97) + CHR(103) + CHR(103) + CHR(101) + CHR(110) AND FILL-IN-LOS = CHR(107) + CHR(97) + CHR(103) + CHR(103) + CHR(101) + CHR(110)  THEN DO:
      RUN val_UI.
      IF musz = FALSE THEN DO:
         BTN_AVB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         DISABLE BRW_VDB BTN_START WITH FRAME {&FRAME-NAME}.      
         Guru.SharedVariable:singel = FALSE.     
            IF FILL-IN-program = "STARTUPPDAT.P" OR FILL-IN-APPSERVER = TRUE THEN DO:
               RUN VALUE(FILL-IN-program) (INPUT FILL-IN-APPSERVER).
            END.
            ELSE IF FILL-IN-program = "STARTAVT.P" THEN DO:
               RUN VALUE(FILL-IN-program) (INPUT FILL-IN-APPSERVER).
            END.

            ELSE DO:
               RUN ALIASSATT.P.
               RUN VALUE(FILL-IN-program).
            END.           
              
         ENABLE BRW_VDB BTN_START WITH FRAME {&FRAME-NAME}.
         BTN_AVB:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.

         MESSAGE "klart " VIEW-AS ALERT-BOX.
      END.
      musz = FALSE.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_START-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_START-2 C-Win
ON CHOOSE OF BTN_START-2 IN FRAME DEFAULT-FRAME /* Visa innehåll */
DO:  
   ASSIGN
   FILL-IN-APPSERVER = INPUT FILL-IN-APPSERVER
   FILL-IN-ANV     =   INPUT FILL-IN-ANV
   FILL-IN-LOS     = INPUT FILL-IN-LOS    
   FILL-IN-program-2 = INPUT FILL-IN-program-2.
   /*För avtalsprogram STARTAVT.P*/
   IF FILL-IN-ANV = CHR(107) + CHR(97) + CHR(103) + CHR(103) + CHR(101) + CHR(110) AND FILL-IN-LOS = CHR(107) + CHR(97) + CHR(103) + CHR(103) + CHR(101) + CHR(110)  THEN DO:
      RUN val_UI.
      IF musz = FALSE THEN DO:
         BTN_AVB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         DISABLE BRW_VDB BTN_START WITH FRAME {&FRAME-NAME}.      
         Guru.SharedVariable:singel = FALSE.     
         RUN VALUE(FILL-IN-program-2).    
         ENABLE BRW_VDB BTN_START WITH FRAME {&FRAME-NAME}.
         BTN_AVB:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.

         MESSAGE "klart " VIEW-AS ALERT-BOX.
      END.
      musz = FALSE.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_START-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_START-3 C-Win
ON CHOOSE OF BTN_START-3 IN FRAME DEFAULT-FRAME /* Starta Guru */
DO:  
   RUN val_UI.
   IF musz = FALSE THEN DO:
      {&WINDOW-NAME}:HIDDEN = TRUE.
         DEFAULT-WINDOW:HIDDEN = TRUE.
         {&WINDOW-NAME}:ALWAYS-ON-TOP = FALSE.       
         {&WINDOW-NAME}:HIDDEN = TRUE.       
         IF valdbtemp.GFORETAG = "dELPA" OR valdbtemp.GFORETAG = "classELPA" THEN demokvar = TRUE.
         ELSE demokvar = FALSE.
        
         RUN Spring.p (FALSE, valdbtemp.GFORETAG ).
         {&WINDOW-NAME}:HIDDEN = FALSE.
         {&WINDOW-NAME}:HIDDEN = FALSE.
         {&WINDOW-NAME}:MOVE-TO-TOP ().
          {musarrow.i}   
   END.
   musz = FALSE.   
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
   /*{WIN_M_START.I}*/   
   /*CMB_DB:SCREEN-VALUE = "Energiadministration". */
   {VALDBALL.I} 
    
   SESSION:DEBUG-ALERT = YES.
   C-Win:TITLE = "Start av Guru " + PROVERSION + " " + SESSION:CLIENT-TYPE .
   RUN enable_UI.
   status-ok = BRW_VDB:DESELECT-FOCUSED-ROW().
   RELEASE valdbtemp.
   {SLUTWIN.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE con_UI C-Win 
PROCEDURE con_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE koppla AS CHARACTER NO-UNDO.
   koppla = valdbtemp.DBPLATS + valdbtemp.DBNAMN.
   MESSAGE koppla
   VIEW-AS ALERT-BOX.
   koppla = koppla + " " + " -P " + QUOTER({setpwd.I}) +  " -U " + QUOTER({setuser.I}).
   CONNECT VALUE(koppla) NO-ERROR.  
   IF NOT CONNECTED(valdbtemp.DBNAMN) THEN DO: 
      koppla = valdbtemp.DBCON.
      koppla = REPLACE(koppla,"www.guruonweb.se","webguru") NO-ERROR.
      koppla = REPLACE(koppla,"www2.guruonweb.se","webguru") NO-ERROR.
      MESSAGE koppla
      VIEW-AS ALERT-BOX.
      koppla = koppla + " " + " -P " + QUOTER({setpwd.I}) +  " -U " + QUOTER({setuser.I}).  
      CONNECT VALUE(koppla) NO-ERROR. 
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
  DISPLAY FILL-IN-ANV FILL-IN-LOS FILL-IN-program-2 FILL-IN-1 FILL-IN-APPSERVER 
          FILL-IN-program FILL-IN-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BRW_VDB FILL-IN-ANV FILL-IN-LOS FILL-IN-program-2 FILL-IN-1 
         BTN_START-2 BTN_START-3 FILL-IN-APPSERVER FILL-IN-program FILL-IN-2 
         BTN_START BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val_UI C-Win 
PROCEDURE val_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
  /* APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.*/
   
   {muswait.i}      
    status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   SESSION:PRINTER-CONTROL-HANDLE = 0.
   IF isweb = TRUE THEN RETURN.
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   IF NOT AVAILABLE valdbtemp THEN musz = musz.
   ELSE DO:
      IF LDBNAME(1) NE valdbtemp.DBNAMN THEN DO:
         IF LDBNAME(1) NE ? THEN DISCONNECT VALUE(LDBNAME(1)) NO-ERROR.         
      END.
      IF NOT CONNECTED(valdbtemp.DBNAMN) THEN DO :
         RUN con_UI.            
      END.
      varmess = "".
      IF ERROR-STATUS:NUM-MESSAGES > 0  THEN DO:
         DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
            IF ERROR-STATUS:GET-NUMBER(i) = 6126 THEN DO:
               ASSIGN
               varerror = i
               varmess = "6126".         
            END.
         END.
         IF ERROR-STATUS:GET-NUMBER(ERROR-STATUS:NUM-MESSAGES) = 4069 THEN musz = musz.
         ELSE IF ERROR-STATUS:GET-NUMBER(ERROR-STATUS:NUM-MESSAGES) = 565 THEN musz = musz.
         ELSE RUN medd_UI.
      END.
      IF CONNECTED(valdbtemp.DBNAMN) THEN DO:
         {VERALIAS.I}
         
         RUN FORVER.P (INPUT valdbtemp.GFORETAG,INPUT valdbtemp.APPCON).         
         
         musz = FALSE.
      END.
      ELSE musz = TRUE.      
   END.  
   {musarrow.i} 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

