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
DEFINE INPUT PARAMETER RAD_ALLTID AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TOG_EJSUM AS LOGICAL NO-UNDO.


/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW
DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE SHARED VARIABLE SEL_UPP AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE tillrec AS RECID NO-UNDO.   
&Scoped-define NEW
&Scoped-define SHARED SHARED
{KODERAVT.I}

DEFINE VARIABLE arrhjsum AS DECIMAL NO-UNDO.    
DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE forsta AS CHARACTER NO-UNDO. 
DEFINE VARIABLE vpers AS CHARACTER NO-UNDO. 
DEFINE VARIABLE vomr AS CHARACTER NO-UNDO.
DEFINE VARIABLE tot AS DECIMAL NO-UNDO.

DEFINE SHARED TEMP-TABLE perstill    
   FIELD EFTERNAMN AS CHARACTER 
   FIELD FORNAMN AS CHARACTER
   FIELD PERSONALKOD AS CHARACTER
   FIELD PERSTILLREC AS RECID
   FIELD OMRADE AS CHARACTER
   FIELD ANSVARIGTIDR AS CHARACTER
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD ASCENDING. 
   
DEFINE SHARED TEMP-TABLE tillpers
   FIELD LONTILLAGG AS CHARACTER
   FIELD VILART AS CHARACTER
   FIELD LONKODTEXT AS CHARACTER
   FIELD ENHET AS CHARACTER
   FIELD TILLPERSREC AS RECID 
   INDEX LONTILLAGG IS PRIMARY LONTILLAGG ASCENDING.        
   
DEFINE TEMP-TABLE slutsum           
   FIELD PERSONALKOD AS CHARACTER
   FIELD NAMN AS CHARACTER FORMAT "X(30)"
   FIELD LONTILLAGG AS CHARACTER
   FIELD VILART AS CHARACTER
   FIELD OMRADE AS CHARACTER
   FIELD ANSVARIGTIDR AS CHARACTER
   FIELD ANTAL AS DECIMAL
   FIELD LONKODTEXT AS CHARACTER
   FIELD ENHET AS CHARACTER
   INDEX PERS IS PRIMARY PERSONALKOD LONTILLAGG
   INDEX LON LONTILLAGG PERSONALKOD 
   INDEX ANSV ANSVARIGTIDR PERSONALKOD LONTILLAGG 
   INDEX ANSV2 ANSVARIGTIDR LONTILLAGG PERSONALKOD
   INDEX OMR OMRADE PERSONALKOD LONTILLAGG 
   INDEX OMR2 OMRADE LONTILLAGG PERSONALKOD.   
    
DEFINE TEMP-TABLE slutsum1           
   FIELD PERSONALKOD AS CHARACTER
   FIELD NAMN AS CHARACTER FORMAT "X(5)"
   FIELD GEOMRADE AS CHARACTER
   FIELD OMRADE AS CHARACTER.     
DEFINE TEMP-TABLE uppfoltemp   
   FIELD FORETAG AS CHARACTER
   FIELD SORT AS INTEGER   
   FIELD ALLVAL AS INTEGER   
   FIELD ALLTID AS INTEGER
   FIELD INDATUM AS DATE
   FIELD UTDATUM AS DATE.
{TIDUTTTNEW.I} 
   

DEFINE {&NEW} SHARED VARIABLE RAD_SORT AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Personal", 1,
"Tillägg", 2
     SIZE 11 BY 3 BGCOLOR 8 NO-UNDO.   

     
DEFINE {&NEW} SHARED VARIABLE RAD_ALLVAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ansvarig tidredovisare", 1,
"Område", 2,
"Alla", 3,
"Enhet/signatur",4
     SIZE 76.5 BY 1.5
     BGCOLOR 8  NO-UNDO.
{EXECLIN.I}

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
&Scoped-Define ENABLED-OBJECTS BTN_SKRIV BTN_EXCEL BTN_AVS 

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

DEFINE BUTTON BTN_EXCEL 
     LABEL "Visa listan i Excel":L 
     SIZE 14 BY 1 TOOLTIP "Visa i Excel format".

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
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 110 BY 23.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_SKRIV AT ROW 25.25 COL 67.5
     BTN_EXCEL AT ROW 25.25 COL 82.5
     BTN_AVS AT ROW 25.25 COL 97.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 111.88 BY 25.63.


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
         TITLE              = "Uppföljning personal-lönetillägg-personal"
         HEIGHT             = 25.75
         WIDTH              = 112
         MAX-HEIGHT         = 27.08
         MAX-WIDTH          = 125.5
         VIRTUAL-HEIGHT     = 27.08
         VIRTUAL-WIDTH      = 125.5
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


&Scoped-define SELF-NAME BTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXCEL WINDOW-2
ON CHOOSE OF BTN_EXCEL IN FRAME FRAME-VINST /* Visa listan i Excel */
DO:
   {muswait.i}    
   RUN excel_UI. 
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO:    
    IF TOG_EJSUM = FALSE  THEN   RUN SKRIVVAL.W (INPUT FALSE).
    ELSE  RUN SKRIVVAL.W (INPUT FALSE).  
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:    
     RUN ut_UI.      
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
   tot = 0.   
   EMPTY TEMP-TABLE tidut NO-ERROR.    
   EMPTY TEMP-TABLE uppfoltemp NO-ERROR.     
   CREATE uppfoltemp.
   ASSIGN
   uppfoltemp.FORETAG = Guru.Konstanter:globforetag
   uppfoltemp.SORT = RAD_SORT            
   uppfoltemp.ALLVAL = RAD_ALLVAL   
   uppfoltemp.ALLTID = RAD_ALLTID
   uppfoltemp.INDATUM = bdatum
   uppfoltemp.UTDATUM = avdatum.                                                   

   IF Guru.Konstanter:appcon THEN DO:                           
      musz = musz.
      RUN SKAPLPER.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT TABLE uppfoltemp,INPUT TABLE perstill,
      INPUT TABLE tillpers,INPUT TABLE koder, INPUT TOG_EJSUM ,OUTPUT TABLE tidut).
   END.
   ELSE DO:
      RUN SKAPLPER.P 
     (INPUT TABLE uppfoltemp,INPUT TABLE perstill,
      INPUT TABLE tillpers,INPUT TABLE koder, INPUT TOG_EJSUM, OUTPUT TABLE tidut).
   END.
   FIND LAST tidut NO-LOCK NO-ERROR.     
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN FELTEXTW.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT Guru.Konstanter:globanv,OUTPUT TABLE felmeddtemp,OUTPUT musz).     
   END.
   ELSE DO:
      RUN FELTEXTW.P  
      (INPUT Guru.Konstanter:globanv,OUTPUT TABLE felmeddtemp,OUTPUT musz).     
   END. 
   ENABLE BRW_UT WITH FRAME FRAME-VINST.
   OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK. 
   BRW_UT:HIDDEN = FALSE.       
   RUN enable_UI.   
   {FRMSIZE.I}          
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth). 
   IF BTN_EXCEL:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = BTN_EXCEL:HANDLE.           
      RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).   
   END.
   Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).   
   {musarrow.i}   
   IF visvalvar = 2 THEN DO:
      APPLY "CHOOSE" TO BTN_EXCEL.
      APPLY "CHOOSE" TO BTN_AVS IN FRAME {&FRAME-NAME}. 
      RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
      Guru.GlobalaVariabler:retvalkoll = FALSE.
      LEAVE MAIN-BLOCK.
   END.
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
  ENABLE BTN_SKRIV BTN_EXCEL BTN_AVS 
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
   ASSIGN
   slutbredd = 7
   utnr[1] = 1 
   utnr[2] = 8
   utnr[3] = 34
   utnr[4] = 40
   utnr[5] = 64
   utnr[6] = 69.
   IF TOG_EJSUM = TRUE THEN DO:
      utnr[7] = 78.
      utnr[8] = 88.
      slutbredd = 50.
   END.      
   RUN satestat_UI. /*BARA FÖR DE SOM HAR UTNR ANARS COLBREDD_ui*/
   RUN startexcel_UI.   
   /*Kolumnbredd*/
   raknare = 1.
   RUN kolumnexcel_UI.
   FIND FIRST tidut NO-LOCK NO-ERROR.   
   /*Rubriker*/   
   REPEAT:
      IF musz = FALSE THEN DO:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      END.
      FIND NEXT tidut NO-LOCK NO-ERROR.
      musz = FALSE.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2). 
         musz = TRUE.
      END.
      IF musz = FALSE THEN DO:
         IF SUBSTRING(tidut.UT,utnr[1],6) = "ENHET/" THEN DO:    /*Rubrikslut FÖRSTA */         
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
            LEAVE.
         END.
      END.
   END.   
   /*Poster*/
   FIND NEXT tidut NO-LOCK NO-ERROR.
   IF AVAILABLE tidut THEN DO:
      raknare = 1.
      REPEAT:
         IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
            RUN understryk_UI (INPUT 4,INPUT 2). 
            musz = musz.
         END.
         ELSE IF SUBSTRING(tidut.UT,70,5) = "=====" THEN DO:                          
            RUN understryk_UI (INPUT 4,INPUT 2). 
            musz = musz.
         END.
         ELSE IF SUBSTRING(tidut.UT,1,5) = "-----" THEN DO:
            rubrikvar = TRUE.
            RUN understryk_UI (INPUT 4,INPUT 2). 
            musz = musz.
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
   skrivut = FALSE.                         
   FIND LAST tidut NO-LOCK NO-ERROR.     
   RUN EKLOGS.P. 
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

