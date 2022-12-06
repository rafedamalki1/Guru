&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-2


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
{ALLDEF.I}
&Scoped-define NEW                          
{FAKTTEMP.I}
{FAKTPLANTEMP.I}
{ExcelDS.I}
DEFINE INPUT PARAMETER infakplannr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER kreditnrvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER direkt AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR efaktkredtemp.
DEFINE INPUT PARAMETER TABLE FOR sumtidtemp.


/* Local Variable Definitions ---                                       */ 
&Scoped-define NEW
{FAKTTYPDEF.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/

DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.   
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE VARIABLE ctidvar AS CHARACTER NO-UNDO.

DEFINE VARIABLE skarput AS LOGICAL NO-UNDO.
DEFINE VARIABLE utfil AS CHARACTER NO-UNDO.

{ANMARKD.I}
{FAKTBILAG.I}
  
              
{TIDUTTTNEW.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-VINST
&Scoped-define BROWSE-NAME BRW_UT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidut

/* Definitions for BROWSE BRW_UT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_UT tidut.ut 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UT 
&Scoped-define QUERY-STRING-BRW_UT FOR EACH tidut NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut


/* Definitions for FRAME FRAME-VINST                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-VINST ~
    ~{&OPEN-QUERY-BRW_UT}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_SKRIV BTN_AVS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_UT FOR 
      tidut SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT WINDOW-2 _STRUCTURED
  QUERY BRW_UT NO-LOCK DISPLAY
      tidut.ut FORMAT "X(132)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 124.5 BY 26.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.5 COL 1
     BTN_SKRIV AT ROW 28.25 COL 96.5
     BTN_AVS AT ROW 28.25 COL 111
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db tidut
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Visning arbetskopia"
         HEIGHT             = 28.42
         WIDTH              = 125
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125
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
/* SETTINGS FOR FRAME FRAME-VINST
                                                                        */
/* BROWSE-TAB BRW_UT 1 FRAME-VINST */
/* SETTINGS FOR BROWSE BRW_UT IN FRAME FRAME-VINST
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UT:HIDDEN  IN FRAME FRAME-VINST                = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "Temp-Tables.tidut"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.tidut.ut
     _Query            is OPENED
*/  /* BROWSE BRW_UT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-VINST
/* Query rebuild information for FRAME FRAME-VINST
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-VINST */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-VINST /* Avsluta */
DO:
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO: 
   RUN SKRIVVAL.W (INPUT TRUE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:    
      RUN ut_UI.      
      skrivut = FALSE.              
   END.   
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UT
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
   {muswait.i}
   {ALLSTARTDYN.I} 
   skarput = FALSE.
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   FIND FIRST vfaktplantemp WHERE vfaktplantemp.FAKTNR = infakplannr NO-LOCK NO-ERROR.
   FIND FIRST efaktkredtemp WHERE efaktkredtemp.FAKTNR = infakplannr AND efaktkredtemp.FDELNR = kreditnrvar NO-ERROR.
   IF musz = FALSE THEN DO:
      EMPTY TEMP-TABLE fakbilag NO-ERROR. 
      RUN VALLKREA.W (INPUT infakplannr,INPUT kreditnrvar,INPUT TABLE efaktkredtemp,OUTPUT TABLE fakbilag).      
   END.     
   IF musz = TRUE THEN DO:
      musz = FALSE.  
      LEAVE MAIN-BLOCK.
   END.         
   IF efaktkredtemp.VKREDIT = 0 THEN DO:   
      musz = musz.
   END.   
   ELSE DO:  
      WINDOW-2:TITLE = "Visning faktura".
   END.
   RUN skarputfrag_UI.
   IF skarput = TRUE THEN DO:
      /*FAKTFOR*/
      IF visvalvar = 3 THEN DO:
         IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "LULE" THEN DO:
            IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
               IF Guru.Konstanter:appcon THEN DO:      
                  RUN GRFAKTKHTML.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
                  (INPUT infakplannr,
                   INPUT kreditnrvar,
                   INPUT TRUE,
                   INPUT direkt, 
                   INPUT FALSE, 
                   INPUT TABLE fakbilag,
                   INPUT TABLE sumtidtemp, 
                   OUTPUT TABLE tidut).      
               END.
               ELSE DO:            
                  RUN GRFAKTKHTML.P 
                  (INPUT infakplannr,
                   INPUT kreditnrvar,
                   INPUT TRUE,       
                   INPUT direkt,     
                   INPUT FALSE,       
                   INPUT TABLE fakbilag, 
                   INPUT TABLE sumtidtemp, 
                   OUTPUT TABLE tidut).
               END.                       
            END.
            ELSE IF Guru.Konstanter:globforetag = "CELPA" OR Guru.Konstanter:globforetag = "LULE" THEN DO:
               ctidvar = Guru.Konstanter:wtidvar.
               IF Guru.Konstanter:globforetag = "ELPA" THEN ctidvar = REPLACE(ctidvar,"wtid","ctid").
               IF Guru.Konstanter:appcon THEN DO:      
                  RUN LULFAKTKHTML.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
                  (INPUT infakplannr,
                   INPUT kreditnrvar,
                   INPUT TRUE,
                   INPUT direkt, 
                   INPUT FALSE,
                   INPUT ctidvar + "luletyp1print.css",
                   INPUT ctidvar + "luletyp1screen.css",
                   INPUT 20,
                   INPUT TABLE fakbilag,
                   INPUT TABLE sumtidtemp, 
                   OUTPUT TABLE tidut).      
               END.
               ELSE DO:            
                  RUN LULFAKTKHTML.P 
                  (INPUT infakplannr,
                   INPUT kreditnrvar, 
                   INPUT TRUE,        
                   INPUT direkt,      
                   INPUT FALSE,       
                   INPUT ctidvar + "luletyp1print.css",
                   INPUT ctidvar + "luletyp1screen.css",
                   INPUT 20,
                   INPUT TABLE fakbilag, 
                   INPUT TABLE sumtidtemp, 
                   OUTPUT TABLE tidut).
               END.
               IF Guru.Konstanter:globforetag = "ELPA" THEN Guru.Konstanter:wtidvar = REPLACE(Guru.Konstanter:wtidvar,"ctid","wtid").            
            END.
            ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
               ctidvar = Guru.Konstanter:wtidvar.
               IF Guru.Konstanter:globforetag = "ELPA" THEN ctidvar = REPLACE(ctidvar,"wtid","ctid").
               IF Guru.Konstanter:appcon THEN DO:      
                  RUN ELPFAKTKHTML.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
                  (INPUT infakplannr,
                   INPUT kreditnrvar,
                   INPUT TRUE,
                   INPUT direkt, 
                   INPUT FALSE,
                   INPUT ctidvar + "bilder\",
                   INPUT "",
                   INPUT TABLE fakbilag,
                   INPUT TABLE sumtidtemp, 
                   OUTPUT TABLE tidut).      
               END.
               ELSE DO:            
                  RUN ELPFAKTKHTML.P 
                  (INPUT infakplannr,
                   INPUT kreditnrvar, 
                   INPUT TRUE,        
                   INPUT direkt,      
                   INPUT FALSE,       
                   INPUT ctidvar + "bilder\",
                   INPUT "",
                   INPUT 20,
                   INPUT TABLE fakbilag, 
                   INPUT TABLE sumtidtemp, 
                   OUTPUT TABLE tidut).
               END.
               Guru.Konstanter:wtidvar = REPLACE(Guru.Konstanter:wtidvar,"ctid","wtid").            
            END.
            utfil = SESSION:TEMP-DIR + "f" + Guru.Konstanter:globanv + ".HTML".               
            OUTPUT TO VALUE(utfil).
            FOR EACH tidut:
               PUT UNFORMATTED tidut.UT SKIP.
            END.
            OUTPUT CLOSE.
            RUN OPENDOC.P (utfil,"","",skrivut).
            /*RUN KOLLPROCESS.P (INPUT "IEXPLORE.EXE").*/
            /*OS-DELETE VALUE(utfil).*/
            LEAVE MAIN-BLOCK.
         END.
      END.
      
      
      IF visvalvar = 5 THEN DO:
         
         IF Guru.Konstanter:appcon THEN DO:
            RUN VIFAPPEXCELKREDIT.p ON Guru.Konstanter:apphand TRANSACTION DISTINCT
            (INPUT infakplannr,INPUT kreditnrvar, INPUT FALSE, INPUT direkt, INPUT FALSE, 
            INPUT TABLE fakbilag, INPUT TABLE sumtidtemp, OUTPUT TABLE fakturaexcelTT, OUTPUT TABLE faktposterexcelTT). 
         END.
         ELSE DO:            
            RUN VIFAPPEXCELKREDIT.p
            (INPUT infakplannr,INPUT kreditnrvar, INPUT FALSE, INPUT direkt, INPUT FALSE, 
            INPUT TABLE fakbilag, INPUT TABLE sumtidtemp, OUTPUT TABLE fakturaexcelTT, OUTPUT TABLE faktposterexcelTT).
         END.
         RUN FAKTEXCEL.P(INPUT FALSE,INPUT TRUE,INPUT TABLE fakturaexcelTT, INPUT TABLE faktposterexcelTT).
         IF AVAILABLE skrivutfakttemp THEN DELETE skrivutfakttemp.
         LEAVE MAIN-BLOCK.
         /*
         FIND FIRST skrivutfakttemp NO-LOCK NO-ERROR.
         IF NOT AVAILABLE skrivutfakttemp THEN DO:
            IF atervar = FALSE THEN LEAVE MAIN-BLOCK.
         END.
         */
      END.  
   END.
   ELSE DO:
      IF Guru.Konstanter:appcon THEN DO:      
         RUN VIFAKAPPA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT infakplannr,  
          INPUT kreditnrvar,  
          INPUT direkt,       
          INPUT TABLE fakbilag, INPUT TABLE sumtidtemp, OUTPUT TABLE tidut).      
      END.
      ELSE DO:            
         RUN VIFAKAPPA.P 
         (INPUT infakplannr,  
          INPUT kreditnrvar,  
          INPUT direkt,       
          INPUT TABLE fakbilag, INPUT TABLE sumtidtemp, OUTPUT TABLE tidut).      
      END.
      IF skrivut = FALSE THEN DO:
         ENABLE BRW_UT WITH FRAME FRAME-VINST.
         BRW_UT:HIDDEN = FALSE.   
         RUN enable_UI.   
         {FRMSIZE.I}  
          ASSIGN
         Guru.GlobalaVariabler:collefth = ?.
         Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.           
         RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
         Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.      
         RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
      END.
      ELSE DO:
         APPLY "CHOOSE" TO BTN_SKRIV.
         LEAVE MAIN-BLOCK.
      END.           
   END.
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
      (INPUT BRW_UT:HANDLE IN FRAME {&FRAME-NAME}).
  
      
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
  ENABLE BTN_SKRIV BTN_AVS 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skarputfrag_UI WINDOW-2 
PROCEDURE skarputfrag_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*FAKTFOR*/
    IF visvalvar = 3 OR visvalvar = 5 THEN skarput = TRUE.
   ELSE skarput = FALSE.
        /*
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "LULE" THEN DO:   
      MESSAGE "Vill Du visa fakturan via HTML?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Skarp faktura"
      UPDATE val1 AS LOGICAL.   
      CASE val1:
         WHEN TRUE THEN DO:
            skarput = TRUE.                                              
         END.
         WHEN FALSE THEN DO:
            skarput = FALSE.          
         END.
      END CASE.   
   END.        
   ELSE skarput = FALSE.
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI WINDOW-2 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  /*UT*/                         
   FIND LAST tidut NO-LOCK NO-ERROR.     
   RUN EKLOGL.P.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

