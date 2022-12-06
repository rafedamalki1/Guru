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
DEFINE INPUT PARAMETER FILL-IN-STATUS AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
&Scoped-define NEW                       
{GLOBVAR2DEL1.I}
{ANMARKD.I}
/*/*{EGENBEN.I}*/*/

&Scoped-define SHARED SHARED
{DEFBESTU.I}
{LEVTEMP.I}
{HUVLEVTEMP.I}
{KONVALTEMP.I}
{HOPPSEK2W.I}
{EXECLIN.I}
DEFINE TEMP-TABLE elev_temp NO-UNDO LIKE lev_temp.

DEFINE SHARED VARIABLE priset AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE detvar AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE datvar AS DATE NO-UNDO.  
DEFINE SHARED VARIABLE alla AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 

DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.

DEFINE SHARED VARIABLE epostvar AS CHARACTER NO-UNDO.

DEFINE SHARED VARIABLE delbest AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE mailvar AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE edivar AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.        
DEFINE VARIABLE arrhjsum AS DECIMAL NO-UNDO.    
DEFINE VARIABLE str AS CHARACTER FORMAT "X(130)" NO-UNDO. 
DEFINE VARIABLE str1 AS CHARACTER FORMAT "X(130)" NO-UNDO.
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(130)" NO-UNDO. 
DEFINE VARIABLE str0 AS CHARACTER FORMAT "X(130)" NO-UNDO.
DEFINE VARIABLE totalt AS DECIMAL NO-UNDO. 
DEFINE VARIABLE totalt2 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE leverant AS CHARACTER NO-UNDO.
DEFINE VARIABLE lev AS CHARACTER NO-UNDO.  
DEFINE VARIABLE sumpris AS DECIMAL NO-UNDO.    
DEFINE VARIABLE totalsum AS DECIMAL NO-UNDO.
DEFINE VARIABLE sumantal AS INTEGER NO-UNDO.
DEFINE VARIABLE xkordvar AS INTEGER NO-UNDO. 
DEFINE VARIABLE stopvar AS LOGICAL NO-UNDO. 
DEFINE VARIABLE prisvar AS INTEGER NO-UNDO.
DEFINE VARIABLE antvar AS INTEGER NO-UNDO. 
DEFINE VARIABLE antvar2 AS INTEGER NO-UNDO.  
DEFINE VARIABLE firstlev AS CHARACTER NO-UNDO.   
DEFINE VARIABLE upp AS LOGICAL NO-UNDO.   
DEFINE VARIABLE bytgrupp AS CHARACTER NO-UNDO. 
DEFINE VARIABLE enrvar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE utrec AS RECID NO-UNDO.
DEFINE VARIABLE utrec2 AS RECID NO-UNDO.
DEFINE VARIABLE kant AS INTEGER NO-UNDO.
DEFINE VARIABLE tant AS INTEGER NO-UNDO.
DEFINE VARIABLE uppvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE sidlangd AS INTEGER NO-UNDO.
DEFINE VARIABLE ediraknare AS INTEGER NO-UNDO.
DEFINE VARIABLE anmlangd AS INTEGER NO-UNDO.
DEFINE VARIABLE idvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE numsok AS LOGICAL NO-UNDO.
DEFINE VARIABLE byggnr AS INTEGER NO-UNDO.
DEFINE VARIABLE skapkab AS INTEGER NO-UNDO.
DEFINE VARIABLE berbestapph AS HANDLE NO-UNDO.
DEFINE VARIABLE utval AS LOGICAL NO-UNDO.
DEFINE VARIABLE medvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE kommando3 AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE orgdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 0.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE postvar AS LOGICAL NO-UNDO.


DEFINE VARIABLE MED_EDITOR AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 53 BY 11.73
     BGCOLOR 8  NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS BTN_EXCEL BTN_SKRIV BTN_AVS 

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

DEFINE BUTTON BTN_EPOST 
     LABEL "Best. E-post":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXCEL 
     LABEL "Till excel":L 
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
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 99.5 BY 26.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_EXCEL AT ROW 27.75 COL 40
     BTN_EPOST AT ROW 27.75 COL 56
     BTN_SKRIV AT ROW 27.75 COL 71.5
     BTN_AVS AT ROW 27.75 COL 87
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 101 BY 28.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: tidut T "?" NO-UNDO temp-db tidut
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Specifikation av beställning"
         HEIGHT             = 28
         WIDTH              = 101
         MAX-HEIGHT         = 28
         MAX-WIDTH          = 101
         VIRTUAL-HEIGHT     = 28
         VIRTUAL-WIDTH      = 101
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

/* SETTINGS FOR BUTTON BTN_EPOST IN FRAME FRAME-VINST
   NO-ENABLE                                                            */
ASSIGN 
       BTN_EPOST:HIDDEN IN FRAME FRAME-VINST           = TRUE.

ASSIGN 
       BTN_EXCEL:HIDDEN IN FRAME FRAME-VINST           = TRUE.

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


&Scoped-define SELF-NAME BTN_EPOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EPOST WINDOW-2
ON CHOOSE OF BTN_EPOST IN FRAME FRAME-VINST /* Best. E-post */
DO:       
   {muswait.i} 
   RUN epexcel_UI (INPUT 1) .      
   /*RUN EMEDDSE.W (kommando2,kommando3,rbrik) .*/
   file-info:file-name = ".".
   orgdir = file-info:full-pathname.   
   RUN SPECIALMAPI.P (INPUT "Beställning", INPUT "", INPUT kommando2).
   RUN SetCurrentDirectoryA IN Guru.Konstanter:hpApi  (INPUT orgdir).
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXCEL WINDOW-2
ON CHOOSE OF BTN_EXCEL IN FRAME FRAME-VINST /* Till excel */
DO:       
   {muswait.i} 
   RUN epexcel_UI (INPUT 2) .      
   
   /*file-info:file-name = ".".
   orgdir = file-info:full-pathname.   
   RUN SPECIALMAPI.P (INPUT "Beställning", INPUT "", INPUT kommando2).
   RUN SetCurrentDirectoryA IN Guru.Konstanter:hpApi  (INPUT orgdir).*/
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO: 
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:            
      RUN ut_UI.  
      IF vald_lev = "99" THEN DO:
         RUN ut2_UI. 
      END.   
   END.   
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
DO:
   IF VALID-HANDLE(berbestapph) THEN DELETE PROCEDURE berbestapph.
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
   FIND best_mtrl WHERE best_mtrl.LEVKOD = vald_lev NO-LOCK NO-ERROR.
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_lev NO-LOCK NO-ERROR.        
   EMPTY TEMP-TABLE tidut NO-ERROR.
   ASSIGN      
   tant = 0
   sidlangd = globsids - 3.  
   str=
"=============================================================================".  
   str1 =
"*****************************************************************************".  
   str0 = 
"-----------------------------------------------------------------------------".  
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN priset = FALSE.   
   
   RUN skapalista_UI IN berbestapph (INPUT Guru.Konstanter:globforetag, INPUT epostvar, INPUT globsids,INPUT vald_lev,INPUT
    valaonr,INPUT valomrade,
                                     INPUT priset,INPUT detvar,INPUT mailvar,INPUT edivar,INPUT datvar,
                                     INPUT alla,OUTPUT musz,INPUT TABLE lev_temp,
                                     INPUT TABLE trp_mtrl,INPUT TABLE skapa_mtrl,OUTPUT TABLE tidut, INPUT TABLE kon_val,INPUT delbest).

   IF musz = TRUE THEN DO:
      musz = FALSE.
      status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
      LEAVE MAIN-BLOCK. 
   END.                 
   ELSE DO:
      IF skrivut = FALSE THEN DO:
         ENABLE BRW_UT WITH FRAME FRAME-VINST.
         BRW_UT:HIDDEN = FALSE.       
      END.
      ELSE DO:   
         RUN ut_UI.
         IF vald_lev = "99" THEN DO:
            RUN ut2_UI.
         END.   
         status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
         APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
         LEAVE MAIN-BLOCK. 
      END.
   END.   
   IF Guru.Konstanter:globforetag = "FORS" OR  Guru.Konstanter:globforetag = "GRAN" OR  Guru.Konstanter:globforetag = "elpa" THEN DO:                         
      IF FILL-IN-STATUS NE "Ej beställd" THEN.
      ELSE DO:
         ENABLE BTN_EPOST WITH FRAME {&FRAME-NAME}. 
         BTN_EPOST:HIDDEN = FALSE.
      END.     
   END.
   ELSE BTN_EPOST:HIDDEN = TRUE.
   RUN enable_UI.
   {FRMSIZE.I}   
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_EPOST:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_EXCEL:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
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
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_UT:HANDLE IN FRAME {&FRAME-NAME}).
   IF Guru.Konstanter:appcon THEN DO:
      RUN BERBESTAPP.P PERSISTENT SET berbestapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN BERBESTAPP.P PERSISTENT SET berbestapph.
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
  ENABLE BTN_EXCEL BTN_SKRIV BTN_AVS 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE epexcel_UI WINDOW-2 
PROCEDURE epexcel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   DEFINE VARIABLE filformat AS CHARACTER NO-UNDO.
   ASSIGN
   allac[1] = "A"          
   allac[2] = "B"          
   allac[3] = "C"          
   allac[4] = "D"          
   allac[5] = "E"          
   allac[6] = "F".          
   
   IF priset = TRUE THEN DO:      
      ASSIGN           
      bredd[1] = 10
      bredd[2] = 35
      bredd[3] = 6
      bredd[4] = 5
      bredd[5] = 9
      bredd[6] = 10
      slutbredd = 10.
      breddantal = 6.
      i = 6.     
   END.
   ELSE DO:
     ASSIGN      
     bredd[1] = 10
     bredd[2] = 35
     bredd[3] = 6
     bredd[4] = 5.    
     slutbredd = 8.
     breddantal = 4.
     i = 4.
     
   END.
   ASSIGN
   allachar[1] = TRUE
   bladvar = 0.
   iRad = 1.
   
   RUN colbredd_UI.  
   IF vad = 1 THEN RUN startexceld_UI.
   ELSE IF vad = 2 THEN RUN startexcel_UI.
   IF vad = 1 THEN RUN visaexel_UI (FALSE).
   ELSE IF vad = 2 THEN RUN visaexel_UI (TRUE).
   ASSIGN
   iColumn = 1
   cColumn = STRING(iColumn)
   cRange = "A" + cColumn   
   link = ?.
   /*FOREBILDER*/
   {LOGGOR.I}
   IF link NE ? THEN DO:
      IF raknare = 0 THEN RUN imageexcel_UI (INPUT link,INPUT "A",INPUT 1).
      ELSE RUN imageexcel_UI (INPUT link,INPUT "A",INPUT raknare).
      iColumn = iColumn + 5.
      cColumn = STRING(iColumn).
      cRange = "A" + cColumn.
      irad = iColumn.
   END.   
   IF vad = 1 THEN chExcelApplication:VISIBLE = FALSE.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.                                    
      IF SUBSTRING(tidut.UT,estartnr[1],3) = "ENR" THEN DO:         
         LEAVE.         
      END.           
      IF SUBSTRING(tidut.UT,estartnr[1],12) = "============" OR SUBSTRING(tidut.UT,estartnr[1],12) = "************" OR SUBSTRING(tidut.UT,estartnr[1],12) = "------------" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).  
         FIND NEXT tidut NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidut THEN DO:
            LEAVE.
         END. 
      END.              
   END.
   
   /*Poster*/ 
   IF NOT AVAILABLE tidut THEN DO:
      FIND NEXT tidut NO-ERROR.
   END.
   IF AVAILABLE tidut THEN DO:
      raknare = 1.
      REPEAT:
         IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
            RUN understryk_UI (INPUT 4,INPUT 2).                
         END.              
         ELSE IF SUBSTRING(tidut.UT,58,13) = "Summa totalt:" THEN DO:                     
            LEAVE.
         END.
         ELSE IF SUBSTRING(tidut.UT,1,12) = "============" OR SUBSTRING(tidut.UT,1,12) = "************" OR SUBSTRING(tidut.UT,1,12) = "------------" THEN DO:         
            LEAVE.
         END.         
         ELSE DO:            
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).           
            
         END.   
         FIND NEXT tidut NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidut THEN DO:
            LEAVE.
         END.      
      END.   
      IF NOT AVAILABLE tidut THEN DO:
         FIND NEXT tidut NO-ERROR.      
      END.
      IF AVAILABLE tidut THEN DO:
         /*REPEAT:
            IF SUBSTRING(tidut.UT,1,12) = "============" OR SUBSTRING(tidut.UT,1,12) = "************" OR SUBSTRING(tidut.UT,1,12) = "------------" THEN DO:
               RUN understryk_UI (INPUT 4,INPUT 2).  
               FIND NEXT tidut NO-LOCK NO-ERROR.
               IF NOT AVAILABLE tidut THEN DO:
                  LEAVE.
               END. 
            END.              
            RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
            FIND NEXT tidut NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tidut THEN DO:
               LEAVE.
            END.                                    
         END.*/
         postvar = FALSE.
         REPEAT:
            
            IF SUBSTRING(tidut.UT,1,12) = "============" OR SUBSTRING(tidut.UT,1,12) = "************" OR SUBSTRING(tidut.UT,1,12) = "------------" THEN DO:
               postvar = FALSE.
               RUN understryk_UI (INPUT 4,INPUT 2).  
               FIND NEXT tidut NO-LOCK NO-ERROR.
               IF NOT AVAILABLE tidut THEN DO:
                  LEAVE.
               END. 
            END.              
            ELSE IF SUBSTRING(tidut.UT,1,3) = "ENR" THEN DO:
               postvar = TRUE.               
            END.              
            IF postvar = TRUE THEN DO:
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).           
            END.
            ELSE DO:            
               RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
            END.
            FIND NEXT tidut NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tidut THEN DO:
               LEAVE.
            END.                                    
         END.   
      END.       
      
   END.
   IF vad = 1 THEN DO:   
      kommando2 = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN kommando2 = webclienttempdir.
      OS-CREATE-DIR VALUE(kommando2) NO-ERROR.
      kommando3 = Guru.Konstanter:globanv  + STRING(TIME) + ".xls".   
      kommando2 = kommando2 +  kommando3.
      chExcelApplication:VISIBLE = FALSE.
      chExcelApplication:displayalerts = FALSE.
      chWorkbook:SaveAs(kommando2,56,,,,,,,,). 
      RUN slutexceld_UI. 
   END.
   ELSE IF vad = 2 THEN RUN slutexcel_UI. 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut2_UI WINDOW-2 
PROCEDURE ut2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FOR EACH trp_mtrl WHERE trp_mtrl.LEVKOD = vald_lev AND trp_mtrl.DBEST NE "RETUR": 
      medvar = "".
      RUN hmtmed_UI IN berbestapph (INPUT vald_lev,INPUT trp_mtrl.ENR,OUTPUT medvar).
      IF medvar NE "" THEN DO:
         MED_EDITOR = medvar.
         {PRINTSTAENDE.I}
         DISPLAY "Kompletterande uppgifter till materiel" AT 6           
         TODAY AT  51
         "=====================================================" AT 6 
         "LEVERANTÖR :     SPECIAL" AT 6    
         CAPS(Guru.Konstanter:genk) + " :" AT 6 
         trp_mtrl.ENR AT 23 NO-LABEL 
         "BENÄMNING :" AT 6 
         trp_mtrl.BENAMNING AT 23 NO-LABEL
         "=====================================================" AT 6 
         MED_EDITOR AT 6 VIEW-AS EDITOR SIZE 50 BY 11.73 NO-LABEL.
         OUTPUT CLOSE.  
      END.
   END.      
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
   skrivut = FALSE.                         
   FIND LAST tidut NO-LOCK NO-ERROR. 
   RUN EKLOGS.P. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

