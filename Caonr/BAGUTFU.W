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
{DIRDEF.I}


DEFINE TEMP-TABLE valtemp
   FIELD VALDLISTA AS CHARACTER
   FIELD BAVAL AS INTEGER
   FIELD ALLTID AS LOGICAL
   FIELD STARTDATUM AS DATE
   FIELD SLUTDATUM AS DATE.
DEFINE INPUT PARAMETER vallista AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR uppvaltemp.
DEFINE INPUT PARAMETER TABLE FOR valtemp.
DEFINE INPUT PARAMETER TABLE FOR evaldaao.

/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{EXECLIN.I}
DEFINE SHARED VARIABLE SEL_UPP AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE str3 AS CHARACTER FORMAT "X(92)" NO-UNDO.   
   
{TIDUTTTNEW.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

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
&Scoped-Define ENABLED-OBJECTS FBTN_SKRIV FBTN_EXCEL BTN_AVS 

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

DEFINE BUTTON FBTN_EXCEL 
     LABEL "Till Excel":L 
     SIZE 14 BY 1 TOOLTIP "Visa i Excel format".

DEFINE BUTTON FBTN_SKRIV 
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
      tidut.ut FORMAT "X(130)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 120 BY 24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.5 COL 1.5
     FBTN_SKRIV AT ROW 26 COL 78.5
     FBTN_EXCEL AT ROW 26 COL 92.5
     BTN_AVS AT ROW 26 COL 107.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 121.63 BY 26.21.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
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
         TITLE              = "Uppf?ljning"
         HEIGHT             = 26.29
         WIDTH              = 121.75
         MAX-HEIGHT         = 28.79
         MAX-WIDTH          = 122.75
         VIRTUAL-HEIGHT     = 28.79
         VIRTUAL-WIDTH      = 122.75
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
       BRW_UT:HIDDEN  IN FRAME FRAME-VINST                = TRUE
       BRW_UT:MAX-DATA-GUESS IN FRAME FRAME-VINST         = 4000.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "Temp-Tables.tidut"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.tidut.ut
"tidut.ut" ? "X(130)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
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

&Scoped-define BROWSE-NAME BRW_UT
&Scoped-define SELF-NAME BRW_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UT WINDOW-2
ON RIGHT-MOUSE-CLICK OF BRW_UT IN FRAME FRAME-VINST
DO:
   MESSAGE "Vill du visa listan i din webl?sare?" VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO TITLE "Visa i din webl?sare?" UPDATE svar AS LOGICAL.         
   CASE svar:
      WHEN TRUE THEN DO:
         RUN htmlut.p (INPUT vallista,INPUT TABLE tidut).
      END.
      WHEN FALSE THEN DO:
         musz = musz.
      END.
   END CASE.   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-VINST /* Avsluta */
DO:
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EXCEL WINDOW-2
ON CHOOSE OF FBTN_EXCEL IN FRAME FRAME-VINST /* Till Excel */
DO:
   {muswait.i}    
   RUN excel_UI. 
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-2
ON CHOOSE OF FBTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO:   
   FIND FIRST uppvaltemp NO-ERROR.  
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF FBTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO:
   RUN SIDLANGD.W.
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
   {muswait.i}  
   {ALLSTARTDYN.I} 
   EMPTY TEMP-TABLE tidut NO-ERROR.    
   FIND FIRST uppvaltemp NO-ERROR.
   FIND FIRST valtemp NO-ERROR.
   {AMERICANEUROPEAN.I}
   IF vallista = 37 THEN DO:   
      IF Guru.Konstanter:appcon THEN DO:
         RUN BAGAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT TABLE uppvaltemp,INPUT TABLE valtemp, INPUT TABLE evaldaao, 
         OUTPUT TABLE tidut,OUTPUT str, OUTPUT str2, OUTPUT str3).
      END.
      ELSE DO:    
         RUN BAGAPP.P 
         (INPUT TABLE uppvaltemp,INPUT TABLE valtemp, INPUT TABLE evaldaao, 
         OUTPUT TABLE tidut,OUTPUT str, OUTPUT str2, OUTPUT str3).
      END.         
   END.

   musz = FALSE.                          
   ENABLE BRW_UT WITH FRAME FRAME-VINST.
   BRW_UT:HIDDEN = FALSE.                      
   RUN enable_UI.   
   {FRMSIZE.I}
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
     
   Guru.GlobalaVariabler:colrighth = FBTN_EXCEL:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).  
    
   Guru.GlobalaVariabler:colrighth = FBTN_SKRIV:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).  
    
   {musarrow.i}  
   IF visvalvar = 2 THEN DO:
      APPLY "CHOOSE" TO  FBTN_EXCEL.
      APPLY "CHOOSE" TO BTN_AVS IN FRAME {&FRAME-NAME}. 
      RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
      Guru.GlobalaVariabler:retvalkoll = FALSE.
      LEAVE MAIN-BLOCK.
   END.
   {EUROPEANAMERICAN.I}
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
  ENABLE FBTN_SKRIV FBTN_EXCEL BTN_AVS 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel_UI WINDOW-2 
PROCEDURE excel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
       /*Vilka kolumner*/
   IF vallista = 20 THEN DO:   
      ASSIGN
      startc = "A"
      slutc = "I"
      slutbredd = 5
      utnr[1] = 1
      utnr[2] = 12
      utnr[3] = 30
      utnr[4] = 41
      utnr[5] = 51
      utnr[6] = 58
      utnr[7] = 70
      utnr[8] = 79
      utnr[9] = 85.
      FIND FIRST uppvaltemp NO-ERROR.      
      allachar[7] = TRUE.    /*vilka kolumner skall vara character*/
      RUN satestat_UI. /*BARA F?R DE SOM HAR UTNR ANARS COLBREDD_ui*/
      RUN startexcel_UI.
      FIND FIRST tidut NO-LOCK NO-ERROR.
      /*Kolumnbredd*/
      raknare = 1.
      RUN kolumnexcel_UI.
      /*Rubriker*/   
      REPEAT:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
         FIND NEXT tidut NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidut THEN DO:
            LEAVE.
         END.
         IF SUBSTRING(tidut.UT,utnr[2],5) = "ORT/B" THEN DO:    /*Rubrikslut F?RSTA */         
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
            LEAVE.
         END.
      END.   
      /*Poster*/
      FIND NEXT tidut NO-LOCK NO-ERROR.
      raknare = 1.
      REPEAT:
         IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
            RUN understryk_UI (INPUT 4,INPUT 2). 
            musz = musz.
         END.
         ELSE IF SUBSTRING(tidut.UT,1,5) = "-----" THEN DO:
            rubrikvar = TRUE.
            RUN understryk_UI (INPUT 4,INPUT 2). 
            musz = musz.
         END.
         ELSE IF SUBSTRING(tidut.UT,utnr[3],9) = "SIGN NAMN" THEN DO:    /*Rubrikslut ANDRA */         
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
         END.
         ELSE DO:
            IF rubrikvar = TRUE THEN do:
               RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0).
            END.
            ELSE DO:  
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).        
            END.
         END.   
         FIND NEXT tidut NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidut THEN DO:
            LEAVE.
         END.
      END.
   END.
   IF vallista = 29 THEN DO:
      ASSIGN
      startc = "A"
      slutc = "H" 
      slutbredd = 40
      utnr[1] = 1
      utnr[2] = 7
      utnr[3] = 13
      utnr[4] = 22
      utnr[5] = 28
      utnr[6] = 35
      utnr[7] = 42
      utnr[8] = 53.      
      FIND FIRST uppvaltemp NO-ERROR.      
      allachar[3] = TRUE.    /*vilka kolumner skall vara character*/
      allachar[7] = TRUE. 
      RUN satestat_UI. /*BARA F?R DE SOM HAR UTNR ANARS COLBREDD_ui*/
      RUN startexcel_UI.
      FIND FIRST tidut NO-LOCK NO-ERROR.
      /*Kolumnbredd*/
      raknare = 1.
      RUN kolumnexcel_UI.
      /*Rubriker*/   
      REPEAT:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
         FIND NEXT tidut NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidut THEN DO:
            LEAVE.
         END.
         IF SUBSTRING(tidut.UT,utnr[1],6) = "ENHET/" THEN DO:    /*Rubrikslut F?RSTA */         
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
            LEAVE.
         END.
      END.   
      /*Poster*/
      FIND NEXT tidut NO-LOCK NO-ERROR.
      raknare = 1.
      REPEAT:
         IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
            RUN understryk_UI (INPUT 4,INPUT 2). 
            musz = musz.
         END.
         ELSE IF SUBSTRING(tidut.UT,1,5) = "-----" THEN DO:
            rubrikvar = TRUE.
            RUN understryk_UI (INPUT 4,INPUT 2). 
            musz = musz.
         END.
         ELSE IF SUBSTRING(tidut.UT,1,17) = "?vertidsbeordrare" THEN DO:    
            RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).            
         END.
         ELSE IF SUBSTRING(tidut.UT,utnr[1],4) = "SIGN" THEN DO:    /*Rubrikslut ANDRA */         
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
         END.
         ELSE DO:
            IF rubrikvar = TRUE THEN do:
               RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0).
            END.
            ELSE DO:  
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).        
            END.
         END.   
         FIND NEXT tidut NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidut THEN DO:
            LEAVE.
         END.
      END.
   END.
   IF vallista = 37 THEN DO:   
      ASSIGN
      startc = "A"
      slutc = "I"
      slutbredd = 5
      utnr[1] = 1
      utnr[2] = 12
      utnr[3] = 30
      utnr[4] = 41
      utnr[5] = 51
      utnr[6] = 58
      utnr[7] = 70
      utnr[8] = 79
      utnr[9] = 85.
      FIND FIRST uppvaltemp NO-ERROR.      
      allachar[7] = TRUE.    /*vilka kolumner skall vara character*/
      RUN satestat_UI. /*BARA F?R DE SOM HAR UTNR ANARS COLBREDD_ui*/
      RUN startexcel_UI.
      FIND FIRST tidut NO-LOCK NO-ERROR.
      /*Kolumnbredd*/
      raknare = 1.
      RUN kolumnexcel_UI.
      /*Rubriker*/   
      REPEAT:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
         FIND NEXT tidut NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidut THEN DO:
            LEAVE.
         END.
         IF SUBSTRING(tidut.UT,utnr[2],5) = "ORT/B" THEN DO:    /*Rubrikslut F?RSTA */         
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
            LEAVE.
         END.
      END.   
      /*Poster*/
      FIND NEXT tidut NO-LOCK NO-ERROR.
      raknare = 1.
      REPEAT:
         IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
            RUN understryk_UI (INPUT 4,INPUT 2). 
            musz = musz.
         END.
         ELSE IF SUBSTRING(tidut.UT,1,5) = "-----" THEN DO:
            rubrikvar = TRUE.
            RUN understryk_UI (INPUT 4,INPUT 2). 
            musz = musz.
         END.
         ELSE IF SUBSTRING(tidut.UT,utnr[3],9) = "SIGN NAMN" THEN DO:    /*Rubrikslut ANDRA */         
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
         END.
         ELSE DO:
            IF rubrikvar = TRUE THEN do:
               RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0).
            END.
            ELSE DO:  
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).        
            END.
         END.   
         FIND NEXT tidut NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidut THEN DO:
            LEAVE.
         END.
      END.
   END.
   RUN slutexcel_UI.
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
   FIND FIRST uppvaltemp NO-ERROR.   
   {PRINTSTAENDE.I}                 
   PUT SKIP (2).   
   PUT PAGE-NUMBER AT 80 SKIP.              /*RAD 3*/  
   FOR EACH tidut:  
      IF SUBSTRING(tidut.UT,1,6) = "BILAGA" THEN DO:
         PAGE.
         PUT SKIP (2).
         PUT PAGE-NUMBER AT 80  SKIP. 
         PUT tidut.UT AT 6 SKIP .       
      END.
      ELSE IF SUBSTRING(tidut.UT,132,1) = "$" THEN DO:
         PAGE.
         PUT SKIP (2).
         PUT PAGE-NUMBER AT 80  SKIP. 
         PUT tidut.UT AT 6 SKIP .      
      END.
      ELSE DO:                               
         IF LINE-COUNTER = 1 THEN DO:
           /* PAGE.*/
            PUT SKIP (2).
            PUT PAGE-NUMBER AT 80 SKIP.
            PUT UNFORMATTED str2 AT 6 SKIP.
            PUT UNFORMATTED str3 AT 6 SKIP.
            PUT UNFORMATTED str AT 6 SKIP.
         END.         
         PUT tidut.UT AT 6 SKIP.     
         IF LINE-COUNTER > PAGE-SIZE THEN DO:
            PAGE.
            PUT SKIP (2).
            PUT PAGE-NUMBER AT 80 SKIP.
            PUT UNFORMATTED str2 AT 6 SKIP.
            PUT UNFORMATTED str3 AT 6 SKIP.
            PUT UNFORMATTED str AT 6 SKIP.
         END.
         
      END.                     
   END.  
   OUTPUT CLOSE.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

