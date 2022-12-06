&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-2


/* Temp-Table and Buffer definitions                                    */



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
DEFINE INPUT PARAMETER rapptyp AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
{GLOBVAR2DEL1.I}


&Scoped-define NEW 
&Scoped-define SHARED SHARED
{DIRDEF.I}
{LEVTEMP.I}
{DEPATEMP.I}
{HOPPSEK2W.I}
&Scoped-define NEW                                          
DEFINE SHARED VARIABLE akval AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.   
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valkund AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valnamn AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE ny AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE datvar AS DATE NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE nytt_bestnr AS INTEGER. 
DEFINE SHARED VARIABLE alla AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE mellanrum AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE VARIABLE valknrvis AS CHARACTER NO-UNDO.

DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(86)" NO-UNDO.    
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(86)" NO-UNDO.
DEFINE VARIABLE totalt AS DECIMAL NO-UNDO.
DEFINE VARIABLE totpris AS DECIMAL NO-UNDO.
DEFINE VARIABLE varpaslag AS DECIMAL FORMAT "->9.99" NO-UNDO.
DEFINE VARIABLE vald_depa2 AS INTEGER.
DEFINE VARIABLE totalt2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE anamn AS CHARACTER  NO-UNDO.


{TIDUTTTNEW.I}
DEFINE  TEMP-TABLE ktotemp               
   FIELD KONTO AS CHARACTER                        
   FIELD KONTONR AS CHARACTER                      
   FIELD BENAMNING AS CHARACTER                    
   FIELD JUDID AS CHARACTER                        
   FIELD VIJUDID AS CHARACTER                      
   FIELD OMRADE AS CHARACTER                       
   FIELD AVDELNINGNR AS INTEGER                    
   FIELD AKTIV AS LOGICAL                          
   FIELD RECTIDVIS AS RECID                        
   INDEX KONTO KONTO KONTONR.         
DEFINE TEMP-TABLE bestnr_tab NO-UNDO
   FIELD BESTNR AS INTEGER
   FIELD PASLAG AS DECIMAL
   FIELD TOTPRIS AS DECIMAL
   INDEX BEST IS PRIMARY BESTNR ASCENDING.   
   
DEFINE TEMP-TABLE temp_mtrl NO-UNDO
    FIELD BESTNR AS INTEGER   
    FIELD ENR AS CHARACTER
    FIELD BENAMNING AS CHARACTER
    FIELD ENHET AS CHARACTER  
    FIELD PRIS AS DECIMAL
    FIELD LEVDATUM AS DATE
    FIELD ANTAL AS INTEGER              
    FIELD OFFERT AS LOGICAL
    FIELD REKNR AS INTEGER.
    
DEFINE TEMP-TABLE temp_mtrl2 NO-UNDO
    FIELD BESTNR AS INTEGER   
    FIELD PRIS AS DECIMAL
    FIELD BENAMNING AS CHARACTER
    FIELD BESTALLARE AS CHARACTER
    FIELD DATUM AS DATE
    FIELD DATUM2 AS DATE
    FIELD REKNR AS INTEGER.  
{TEMPUT.I}

DEFINE TEMP-TABLE berbtemp NO-UNDO
   FIELD UTTAG AS LOGICAL 
   FIELD SVINN AS LOGICAL 
   FIELD REST AS LOGICAL 
   FIELD Pris AS DECIMAL 
   FIELD OFFERT AS LOGICAL  
   FIELD LEVKOD AS CHARACTER 
   FIELD LEVDATUM AS DATE
   FIELD Enr AS CHARACTER 
   FIELD Enhet AS CHARACTER 
   FIELD Depnr AS INTEGER 
   FIELD DELNR AS INTEGER 
   FIELD Bestnr AS INTEGER 
   FIELD BESTID AS CHARACTER 
   FIELD Bestdatum AS DATE 
   FIELD BESTALLARE AS CHARACTER 
   FIELD BENAMNING AS CHARACTER 
   FIELD AONR AS CHARACTER 
   FIELD Antal AS INTEGER
   FIELD REKNR AS INTEGER
   INDEX UTRE UTTAG LEVDATUM
   INDEX DAT LEVDATUM UTTAG
   INDEX ENR ENR LEVDATUM UTTAG
   INDEX LISTA AONR DELNR DEPNR UTTAG LEVDATUM.




   /*DEFINE QUERY utq FOR BERBEST.*/


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
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
&Scoped-Define ENABLED-OBJECTS BTN_NAS BTN_SKRIV BTN_AVS 

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
     LABEL "Till Excel":L 
     SIZE 14 BY 1 TOOLTIP "Aktivera investering i Excel format".

DEFINE BUTTON BTN_NAS 
     LABEL "Nästa":L 
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
    WITH NO-LABELS NO-COLUMN-SCROLLING MULTIPLE SIZE 101.5 BY 26.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_NAS AT ROW 28 COL 43.5
     BTN_SKRIV AT ROW 28 COL 58.5
     BTN_EXCEL AT ROW 28 COL 73.5
     BTN_AVS AT ROW 28 COL 89
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 103 BY 28.42.


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
         TITLE              = "Lista Arbetsorder - Uttag/Returer"
         HEIGHT             = 28.42
         WIDTH              = 103
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 103
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 103
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_UT 1 FRAME-VINST */
/* SETTINGS FOR BROWSE BRW_UT IN FRAME FRAME-VINST
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UT:HIDDEN  IN FRAME FRAME-VINST                = TRUE
       BRW_UT:MAX-DATA-GUESS IN FRAME FRAME-VINST         = 1000.

/* SETTINGS FOR BUTTON BTN_EXCEL IN FRAME FRAME-VINST
   NO-ENABLE                                                            */
ASSIGN 
       BTN_EXCEL:HIDDEN IN FRAME FRAME-VINST           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "temp-db.tidut"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = temp-db.tidut.ut
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
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXCEL WINDOW-2
ON CHOOSE OF BTN_EXCEL IN FRAME FRAME-VINST /* Till Excel */
DO:
   /*IF visa.UPPFOLJVAL = 1 THEN DO:
      RUN aoutexcel_UI.
   END.
   ELSE IF visa.UPPFOLJVAL = 4 THEN DO:
      RUN aopexcel_UI.
   END.   
   ELSE IF visa.UPPFOLJVAL = 5 THEN DO:
      RUN aodexcel_UI.
   END.
   ELSE DO:
      RUN AKIVINEX.P (INPUT excellista,INPUT TABLE tidut).        
   END.*/
   
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NAS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NAS WINDOW-2
ON CHOOSE OF BTN_NAS IN FRAME FRAME-VINST /* Nästa */
DO:
   {muswait.i}    
   EMPTY TEMP-TABLE tidut NO-ERROR.      
   FIND NEXT valdaao NO-ERROR.
   IF NOT AVAILABLE valdaao THEN DO:    
      APPLY "WINDOW-CLOSE":U TO WINDOW-2. 
      status-mus2 = SESSION:SET-WAIT-STATE("").      
      RETURN.
   END.
   ASSIGN
   valaonr = valdaao.AONR
   valdelnr = valdaao.DELNR.
   RUN rapport_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      status-mus2 = SESSION:SET-WAIT-STATE("").      
      LEAVE.
      
   END.                 
   ELSE DO:
      IF skrivut = FALSE THEN DO:
         ENABLE BRW_UT WITH FRAME FRAME-VINST.
         BRW_UT:HIDDEN = FALSE.                
      END.
      ELSE DO:   
         RUN ut_UI.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         LEAVE.
         
      END.
   END.
   RUN enable_UI.    
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
     {muswait.i}   
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
ON CLOSE OF THIS-PROCEDURE DO: 
   {BORTBRWPROC.I}
   IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE mtrlbapph.
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
   EMPTY TEMP-TABLE tidut NO-ERROR.
   FIND FIRST depatemp WHERE depatemp.DEP-NR = vald_depa USE-INDEX DEP-NR NO-LOCK NO-ERROR.
   IF valaonr NE "" THEN DO:
      FIND FIRST valdaao WHERE valdaao.AONR = valaonr AND valdaao.DELNR = valdelnr NO-ERROR.
   END.
   IF NOT AVAILABLE valdaao THEN DO:   
      FIND FIRST valdaao NO-ERROR.
   END.
   ASSIGN
   valaonr = valdaao.AONR
   valdelnr = valdaao.DELNR.
   &Scoped-define FORMATNAMN valaonr
   {AOFORMAT2.I}    
   

   RUN rapport_UI.
   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      status-mus2 = SESSION:SET-WAIT-STATE("").
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
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
         LEAVE MAIN-BLOCK. 
      END.
   END.
   RUN enable_UI.   
   {FRMSIZE.I}          
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   IF BTN_EXCEL:VISIBLE = TRUE THEN DO:  
      Guru.GlobalaVariabler:colrighth = BTN_EXCEL:HANDLE.      
      RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_NAS:HANDLE.      
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
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/       
    RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_UT:HANDLE IN FRAME {&FRAME-NAME}).
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph.
   END.   
   IF Guru.Konstanter:appcon THEN DO:                           
         RUN KHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 1,INPUT Guru.Konstanter:globforetag,OUTPUT TABLE ktotemp).                        
      END.
      ELSE DO:
         RUN KHMT.P 
         (INPUT 1,INPUT Guru.Konstanter:globforetag,OUTPUT TABLE ktotemp).      
      END.
      FIND FIRST ktotemp WHERE ktotemp.KONTO = "K3" AND ktotemp.KONTONR = "0" NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ktotemp THEN DO:
         CREATE ktotemp.
         ASSIGN
         ktotemp.KONTO = "K3"
         ktotemp.KONTONR = "0". 
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
  ENABLE BTN_NAS BTN_SKRIV BTN_AVS 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI WINDOW-2 
PROCEDURE huvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     /*HUVUD*/ 
   &Scoped-define FORMATNAMN valaonr
   {AOFORMAT2.I}    
   DO TRANSACTION:   
      
      IF musz = TRUE THEN musz = musz.   
      ELSE DO:
         IF alla = FALSE THEN DO:
            FIND depatemp WHERE depatemp.DEP-NR = vald_depa NO-LOCK NO-ERROR.                   
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = "DEPÅ"
            SUBSTRING(tidut.UT,6) = depatemp.BENAMNING.
            CREATE tidut.
         END.   
         CREATE tidut. 
         IF akval = 1 THEN DO:
            SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok) + " - UTTAG/RETURER".
            ASSIGN                             
            SUBSTRING(tidut.UT,47) = STRING(TODAY)
            SUBSTRING(tidut.UT,57) = STRING(TIME,"HH:MM:SS").                                                                       
            CREATE tidut.                 
            ASSIGN                                                               
            SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaonamnk) + ": " + valort.
            CREATE tidut.  
            ASSIGN                                                               
            SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok) + ": " + formataonr + " " + 
            "DELNR: " + STRING(valdelnr, ">99").   
         END.
         CREATE tidut.   
      END.
   END.                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE klar2_UI WINDOW-2 
PROCEDURE klar2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE klar_UI WINDOW-2 
PROCEDURE klar_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   str2 = 
   "==========================================================================================".
   
   CREATE tidut.
   SUBSTRING(tidut.UT,1) = str2.
   CREATE tidut.
   SUBSTRING(tidut.UT,1) = "UTTAG".
   CREATE tidut.    
   SUBSTRING(tidut.UT,1) = str2.   
   
   totalt = 0.   
   RUN rubrik_UI.        
   OPEN QUERY utq FOR EACH berbtemp WHERE berbtemp.AONR = valaonr AND
   berbtemp.DELNR = valdelnr AND berbtemp.DEPNR = vald_depa AND
   berbtemp.UTTAG = TRUE AND berbtemp.LEVDATUM NE ? /*AND berbtemp.BESTNR = bestnr_tab.BESTNR*/
   NO-LOCK.
   GET FIRST utq NO-LOCK.
   DO WHILE AVAILABLE(berbtemp):              
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,1) = berbtemp.ENR.
      IF berbtemp.REKNR NE 0 THEN SUBSTRING(tidut.UT,13) = STRING(berbtemp.REKNR).
         ASSIGN
     
      SUBSTRING(tidut.UT,18) = SUBSTRING(berbtemp.BENAMNING,1,30)  
      SUBSTRING(tidut.UT,50) = STRING(berbtemp.LEVDATUM)       
      SUBSTRING(tidut.UT,60) = STRING(berbtemp.PRIS,">>>>>9.99")
      SUBSTRING(tidut.UT,70) = berbtemp.ENHET    
      SUBSTRING(tidut.UT,76) = STRING(berbtemp.ANTAL,">>>>9")
      SUBSTRING(tidut.UT,82) = STRING(berbtemp.PRIS * berbtemp.ANTAL,">>>>>>>>9")
      totalt = totalt + (berbtemp.PRIS * berbtemp.ANTAL). 
      IF mellanrum = TRUE THEN DO: 
         CREATE tidut.      
      END.
      GET NEXT utq NO-LOCK.
   END.                    
   CLOSE QUERY utq.     
   str=
"================================================================================.=========".         
   CREATE tidut.      
   SUBSTRING(tidut.UT,1) = str.
   CREATE tidut.
   CREATE tidut.         
   SUBSTRING(tidut.UT,64) = "SUMMA UTTAG:".
   CREATE tidut.
   SUBSTRING(tidut.UT,82) = STRING(totalt,">>>>>>>>9").
   CREATE tidut.                                       
   SUBSTRING(tidut.UT,82) = "=========".                             
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kund2_UI WINDOW-2 
PROCEDURE kund2_UI :
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kund_UI WINDOW-2 
PROCEDURE kund_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   str2 = 
   "==========================================================================================". 
   CREATE tidut.
   SUBSTRING(tidut.UT,1) = str2.
   CREATE tidut.
   SUBSTRING(tidut.UT,1) = "UTTAG".
   CREATE tidut.    
   SUBSTRING(tidut.UT,1) = str2.
   RUN rubrik_UI.
   CREATE tidut.  
   totalt = 0.
   OPEN QUERY utq FOR EACH berbtemp WHERE berbtemp.BESTID = valkund AND 
   berbtemp.DEPNR = vald_depa AND berbtemp.UTTAG = TRUE AND
   berbtemp.LEVDATUM NE ? USE-INDEX LISTA NO-LOCK.
   GET FIRST utq NO-LOCK.
   DO WHILE AVAILABLE(berbtemp):              
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,1) = berbtemp.ENR.
       IF berbtemp.REKNR NE 0 THEN SUBSTRING(tidut.UT,13) = STRING(berbtemp.REKNR).
         ASSIGN   
        
      SUBSTRING(tidut.UT,18) = SUBSTRING(berbtemp.BENAMNING,1,30)  
      SUBSTRING(tidut.UT,50) = STRING(berbtemp.LEVDATUM)       
      SUBSTRING(tidut.UT,60) = STRING(berbtemp.PRIS,">>>>>9.99")
      SUBSTRING(tidut.UT,70) = berbtemp.ENHET    
      SUBSTRING(tidut.UT,76) = STRING(berbtemp.ANTAL,">>>>9")
      SUBSTRING(tidut.UT,82) = STRING(berbtemp.PRIS * berbtemp.ANTAL,">>>>>>>>9")
      totalt = totalt + (berbtemp.PRIS * berbtemp.ANTAL). 
      IF mellanrum = TRUE THEN DO: 
         CREATE tidut.      
      END.
      GET NEXT utq NO-LOCK.
   END.                    
   CLOSE QUERY utq.  
    str=
"================================================================================.=========".         
   CREATE tidut.      
   SUBSTRING(tidut.UT,1) = str.
   CREATE tidut.
   CREATE tidut.         
   SUBSTRING(tidut.UT,64) = "SUMMA UTTAG:".
   CREATE tidut.         
   SUBSTRING(tidut.UT,82) = STRING(totalt,">>>>>>>>9").
   CREATE tidut.                                       
   SUBSTRING(tidut.UT,82) = "=========".                          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rapport_UI WINDOW-2 
PROCEDURE rapport_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   EMPTY TEMP-TABLE berbtemp NO-ERROR.      
   RUN hmtberbtemp_UI IN mtrlbapph (INPUT valaonr, INPUT valdelnr,INPUT vald_depa , OUTPUT TABLE berbtemp).                                        
   &Scoped-define FORMATNAMN valaonr
   {AOFORMAT2.I}    
   IF akval = 1 THEN DO:       
         {&WINDOW-NAME}:TITLE = "Uttag/Returer till " + CAPS(Guru.Konstanter:gaok) + ": " + formataonr + " DELNR: " + 
         STRING(valdelnr,Guru.Konstanter:varforetypchar[1]) + CAPS(Guru.Konstanter:gaonamnk) + ": " + valort 
         + " från depå - " + depatemp.BENAMNING.                      
   END.   
   /*ELSE DO:
      FIND FIRST BESTTAB WHERE BESTTAB.BESTID = valkund NO-LOCK NO-ERROR.
      valknrvis = BESTTAB.VIBESTID.
      {&WINDOW-NAME}:TITLE = "Uttag/Returer till kund med Id: " + STRING(valknrvis) + " Namn: " + 
      STRING(valnamn) + " från depå - " + depatemp.BENAMNING.
   END.   */
str=
"==========================================================================================".      
   RUN huvud_UI. 
   IF akval = 1 THEN DO:      
      RUN hmttemput_UI IN mtrlbapph (INPUT valaonr, INPUT valdelnr,INPUT vald_depa , OUTPUT TABLE temp_ut).    
     
      FOR EACH temp_ut WHERE temp_ut.LEVDATUM < bdatum:
         DELETE temp_ut.
      END.
      FOR EACH temp_ut WHERE temp_ut.LEVDATUM > avdatum:
         DELETE temp_ut.
      END.
                                                      
      RUN rapp_UI.      
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rapp_UI WINDOW-2 
PROCEDURE rapp_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
  
   IF rapptyp = 1 THEN DO:   
       
      FOR EACH ktotemp WHERE ktotemp.KONTO = "K3" BY KONTONR: 
         FIND FIRST temp_ut  WHERE STRING(temp_ut.REKNR) = ktotemp.KONTONR  NO-LOCK NO-ERROR.
         IF NOT AVAILABLE temp_ut THEN NEXT.
         
         str2 = 
         "==========================================================================================".   
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = str2.
         CREATE tidut.
         IF ktotemp.KONTONR = "0" THEN SUBSTRING(tidut.UT,1) = "Uttag  Artikelkod ".
         ELSE SUBSTRING(tidut.UT,1) = "Uttag  Artikelkod " + ktotemp.KONTONR.
         CREATE tidut.    
         SUBSTRING(tidut.UT,1) = str2.   
         
         totalt = 0.   
         RUN rubrik_UI.           
         FOR EACH temp_ut WHERE temp_ut.UTTAG = TRUE AND STRING(temp_ut.REKNR) = ktotemp.KONTONR NO-LOCK BY UTTAG BY LEVDATUM DESCENDING : 
            CREATE tidut.      
            ASSIGN  
            SUBSTRING(tidut.UT,1) = temp_ut.ENR.
            IF temp_ut.REKNR NE 0 THEN SUBSTRING(tidut.UT,13) = STRING(temp_ut.REKNR).
            ASSIGN  
            SUBSTRING(tidut.UT,18) = SUBSTRING(temp_ut.BENAMNING,1,30)  
            SUBSTRING(tidut.UT,50) = STRING(temp_ut.LEVDATUM)                
            SUBSTRING(tidut.UT,70) = temp_ut.ENHET    
            SUBSTRING(tidut.UT,76) = STRING(temp_ut.ANTAL,">>>>9")         
            totalt = totalt + (temp_ut.PRIS * temp_ut.ANTAL).                
            IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN DO:
               ASSIGN
               SUBSTRING(tidut.UT,60) = STRING(temp_ut.PRIS,">>>>>9.99")
               SUBSTRING(tidut.UT,82) = STRING(temp_ut.PRIS * temp_ut.ANTAL,">>>>>>>>9").
            END.
            IF mellanrum = TRUE THEN DO: 
               CREATE tidut.      
            END.
         END.                       
         str=
      "================================================================================.=========".         
         IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN DO:
            CREATE tidut.      
            SUBSTRING(tidut.UT,1) = str.
            CREATE tidut.
            CREATE tidut.         
            SUBSTRING(tidut.UT,64) = "Summa uttag".
            CREATE tidut.
            SUBSTRING(tidut.UT,82) = STRING(totalt,">>>>>>>>9").
            CREATE tidut.                                       
            SUBSTRING(tidut.UT,82) = "=========".                         
         END.
         str2 = 
      "==========================================================================================". 
         CREATE tidut.
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = str2.
         CREATE tidut.
         IF ktotemp.KONTONR = "0" THEN SUBSTRING(tidut.UT,1) = "Returer  Artikelkod ".
         ELSE SUBSTRING(tidut.UT,1) = "Returer  Artikelkod " + ktotemp.KONTONR.
         CREATE tidut.    
         SUBSTRING(tidut.UT,1) = str2.
         RUN rubrik_UI.
         CREATE tidut.  
         totalt = 0.
         /*FOR EACH temp_ut WHERE temp_ut.UTTAG = FALSE USE-INDEX UTRE NO-LOCK:   */
         FOR EACH temp_ut WHERE temp_ut.UTTAG = FALSE AND  STRING(temp_ut.REKNR) = ktotemp.KONTONR NO-LOCK BY UTTAG BY LEVDATUM DESCENDING:   
            CREATE tidut.      
            ASSIGN  
            SUBSTRING(tidut.UT,1) = temp_ut.ENR.
            IF temp_ut.REKNR NE 0 THEN SUBSTRING(tidut.UT,13) = STRING(temp_ut.REKNR).
            ASSIGN  
            SUBSTRING(tidut.UT,18) = SUBSTRING(temp_ut.BENAMNING,1,30) 
            SUBSTRING(tidut.UT,50) = STRING(temp_ut.LEVDATUM)                        
            SUBSTRING(tidut.UT,70) = temp_ut.ENHET    
            SUBSTRING(tidut.UT,76) = STRING(temp_ut.ANTAL,">>>>9").         
            totalt = totalt + (temp_ut.PRIS * temp_ut.ANTAL).       
            IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN DO:
               ASSIGN
               SUBSTRING(tidut.UT,60) = STRING(temp_ut.PRIS,">>>>>9.99")
               SUBSTRING(tidut.UT,82) = STRING(temp_ut.PRIS * temp_ut.ANTAL,">>>>>>>>9").
            END.
            IF mellanrum = TRUE THEN DO: 
               CREATE tidut.      
            END.
         END.                       
         str=
      "================================================================================.=========".         
         
         IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN DO:
            CREATE tidut.      
            SUBSTRING(tidut.UT,1) = str.
            CREATE tidut.
            CREATE tidut.         
            SUBSTRING(tidut.UT,62) = "Summa returer".
            CREATE tidut.         
            SUBSTRING(tidut.UT,82) = STRING(totalt,">>>>>>>>9").
            CREATE tidut.                                       
            SUBSTRING(tidut.UT,82) = "=========".                         
         END.
      END.   
   END.
   ELSE IF rapptyp = 2 THEN DO:
              
      /*FOR EACH temp_ut USE-INDEX DAT NO-LOCK:    */
      FOR EACH ktotemp WHERE ktotemp.KONTO = "K3" BY KONTONR: 
         FIND FIRST temp_ut  WHERE STRING(temp_ut.REKNR) = ktotemp.KONTONR  NO-LOCK NO-ERROR.
         IF NOT AVAILABLE temp_ut THEN NEXT.
         str2 = 
         "==========================================================================================".   
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = str2.
         CREATE tidut.
         IF ktotemp.KONTONR = "0" THEN SUBSTRING(tidut.UT,1) = "Uttag - Returer i datumordning  Artikelkod ".
         ELSE SUBSTRING(tidut.UT,1) = "Uttag - Returer i datumordning  Artikelkod " + ktotemp.KONTONR.
         
         CREATE tidut.    
         SUBSTRING(tidut.UT,1) = str2.   
         totalt = 0.   
         RUN rubrik_UI.
         FOR EACH temp_ut WHERE STRING(temp_ut.REKNR) = ktotemp.KONTONR NO-LOCK BY LEVDATUM DESCENDING BY UTTAG :   
            CREATE tidut.      
            ASSIGN  
            SUBSTRING(tidut.UT,1) = temp_ut.ENR.
            IF temp_ut.REKNR NE 0 THEN SUBSTRING(tidut.UT,13) = STRING(temp_ut.REKNR).
            ASSIGN  
            SUBSTRING(tidut.UT,18) = SUBSTRING(temp_ut.BENAMNING,1,30) 
            SUBSTRING(tidut.UT,50) = STRING(temp_ut.LEVDATUM)                        
            SUBSTRING(tidut.UT,70) = temp_ut.ENHET    
            SUBSTRING(tidut.UT,76) = STRING(temp_ut.ANTAL,">>>>9").
            IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN DO:
               SUBSTRING(tidut.UT,60) = STRING(temp_ut.PRIS,">>>>>9.99").
               IF temp_ut.UTTAG = TRUE THEN DO:
                  ASSIGN
                  SUBSTRING(tidut.UT,82) = STRING(temp_ut.PRIS * temp_ut.ANTAL,">>>>>>>>9")
                  totalt = totalt + (temp_ut.PRIS * temp_ut.ANTAL).       
               END.         
               ELSE DO:
                  ASSIGN
                  SUBSTRING(tidut.UT,82) = STRING(temp_ut.PRIS * temp_ut.ANTAL * -1,"->>>>>>>9")
                  totalt = totalt - (temp_ut.PRIS * temp_ut.ANTAL).       
               END.     
            END.
            IF mellanrum = TRUE THEN DO: 
               CREATE tidut.      
            END.
         END.                       
         str=
      "================================================================================.=========".         
         IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN DO:
            CREATE tidut.      
            SUBSTRING(tidut.UT,1) = str.
            CREATE tidut.
            CREATE tidut.         
            SUBSTRING(tidut.UT,56) = "Summa uttag/returer".
            CREATE tidut.         
            SUBSTRING(tidut.UT,82) = STRING(totalt,"->>>>>>>>9").
            CREATE tidut.                                       
            SUBSTRING(tidut.UT,82) = "=========".                        
         END.
      END.   
   END.
   ELSE DO:
      FOR EACH ktotemp WHERE ktotemp.KONTO = "K3" BY KONTONR: 
         FIND FIRST temp_ut  WHERE STRING(temp_ut.REKNR) = ktotemp.KONTONR  NO-LOCK NO-ERROR.
         IF NOT AVAILABLE temp_ut THEN NEXT.
         str2 = 
         "==========================================================================================".   
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = str2.
         CREATE tidut.
         IF ktotemp.KONTONR = "0" THEN SUBSTRING(tidut.UT,1) = "Uttag - Returer i E-nummerordning  Artikelkod ".
         ELSE SUBSTRING(tidut.UT,1) = "Uttag - Returer i E-nummerordning  Artikelkod " + ktotemp.KONTONR.
         
         CREATE tidut.    
         SUBSTRING(tidut.UT,1) = str2.   
         totalt = 0.   
         RUN rubrik_UI.        
      /*FOR EACH temp_ut USE-INDEX ENR NO-LOCK:   */
         FOR EACH temp_ut WHERE STRING(temp_ut.REKNR) = ktotemp.KONTONR NO-LOCK BY ENR BY LEVDATUM DESCENDING BY UTTAG:   
            CREATE tidut.      
            ASSIGN  
            SUBSTRING(tidut.UT,1) = temp_ut.ENR.
            IF temp_ut.REKNR NE 0 THEN SUBSTRING(tidut.UT,13) = STRING(temp_ut.REKNR).
            ASSIGN 
            SUBSTRING(tidut.UT,18) = SUBSTRING(temp_ut.BENAMNING,1,30) 
            SUBSTRING(tidut.UT,50) = STRING(temp_ut.LEVDATUM)                        
            SUBSTRING(tidut.UT,70) = temp_ut.ENHET    
            SUBSTRING(tidut.UT,76) = STRING(temp_ut.ANTAL,">>>>9").
            IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN DO:
               SUBSTRING(tidut.UT,60) = STRING(temp_ut.PRIS,">>>>>9.99").
               IF temp_ut.UTTAG = TRUE THEN DO:
                  ASSIGN
                  SUBSTRING(tidut.UT,82) = STRING(temp_ut.PRIS * temp_ut.ANTAL,">>>>>>>>9")
                  totalt = totalt + (temp_ut.PRIS * temp_ut.ANTAL).       
               END.         
               ELSE DO:
                  ASSIGN
                  SUBSTRING(tidut.UT,82) = STRING(temp_ut.PRIS * temp_ut.ANTAL * -1,"->>>>>>>9")
                  totalt = totalt - (temp_ut.PRIS * temp_ut.ANTAL).       
               END.     
            END.
            IF mellanrum = TRUE THEN DO: 
               CREATE tidut.      
            END.
         END.                       
         str=
      "================================================================================.=========".         
         IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN DO:
            CREATE tidut.      
            SUBSTRING(tidut.UT,1) = str.
            CREATE tidut.
            CREATE tidut.         
            SUBSTRING(tidut.UT,56) = "Summa uttag/returer".
            CREATE tidut.         
            SUBSTRING(tidut.UT,82) = STRING(totalt,"->>>>>>>>9").
            CREATE tidut.                                       
            SUBSTRING(tidut.UT,82) = "=========".                         
         END.
      END.   
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rubrik_UI WINDOW-2 
PROCEDURE rubrik_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   /*
  ASSIGN
   nrcol[1] = 1
   nrcol[2] = 2
   nrcol[3] = 3
   nrcol[4] = 4
   nrcol[5] = 5
   nrcol[6] = 6
   nrcol[7] = 7
   nrcol[8] = 8
   breddantal = 8   /*antal kolumner*/
   bredd[1] = 27
   bredd[2] = 9
   bredd[3] = 9
   bredd[4] = 9
   bredd[5] = 9
   bredd[6] = 9
   bredd[7] = 13
   bredd[8] = 13.   
   ASSIGN
   i = 2.     
   utnr[nrcol[1]] = 1.
   DO WHILE i <= breddantal:             
      utnr[i] = utnr[i - 1] + bredd[i - 1] + 2.
             
      i = i + 1.
   END.     
   
   */
   CREATE tidut.
   SUBSTRING(tidut.UT,50) = "LEVERANS".     
   ASSIGN
   str=                                                                    
"===========.====.===============================.=========.=========.=====.=====.=========".                  
   CREATE tidut.      
   ASSIGN  
   SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:genk)
   SUBSTRING(tidut.UT,13) = "ARTK"                                                                              
   SUBSTRING(tidut.UT,18) = "BENÄMNING"   
   SUBSTRING(tidut.UT,50) = "DATUM"                  
   SUBSTRING(tidut.UT,60) = "PRIS"  
   SUBSTRING(tidut.UT,70) = "ENHET"
   SUBSTRING(tidut.UT,76) = "ANTAL"
   SUBSTRING(tidut.UT,82) = "SUMMA".                         
   CREATE tidut.       
   SUBSTRING(tidut.UT,1) = str.                                                            
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

