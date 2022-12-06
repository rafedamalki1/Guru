&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
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

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW 
DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE SHARED VARIABLE treminuter AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vallista AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE alla AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE alla2 AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE forvar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE uttyp AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE period AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valfore AS LOGICAL NO-UNDO.
DEFINE VARIABLE antalomr AS INTEGER NO-UNDO.
DEFINE VARIABLE timmar100 AS DECIMAL NO-UNDO.
DEFINE VARIABLE timmar60 AS DECIMAL NO-UNDO.
DEFINE VARIABLE distvar AS INTEGER NO-UNDO.
DEFINE VARIABLE driftant AS INTEGER NO-UNDO.
DEFINE VARIABLE planant AS INTEGER NO-UNDO.
DEFINE VARIABLE hspant AS INTEGER NO-UNDO.
DEFINE VARIABLE lspant AS INTEGER NO-UNDO.
DEFINE VARIABLE totavbrott AS DECIMAL NO-UNDO.
DEFINE VARIABLE totbortfall AS DECIMAL NO-UNDO.
DEFINE VARIABLE bortbelast AS DECIMAL NO-UNDO.
DEFINE VARIABLE totkunder AS INTEGER NO-UNDO.
DEFINE VARIABLE Tdriftant AS INTEGER NO-UNDO.
DEFINE VARIABLE Tplanant AS INTEGER NO-UNDO.
DEFINE VARIABLE Thspant AS INTEGER NO-UNDO.
DEFINE VARIABLE Tlspant AS INTEGER NO-UNDO.
DEFINE VARIABLE Ttotavbrott AS DECIMAL NO-UNDO.
DEFINE VARIABLE Ttotbortfall AS DECIMAL NO-UNDO.
DEFINE VARIABLE Tbortbelast AS DECIMAL NO-UNDO.
DEFINE VARIABLE Ttotkunder AS INTEGER NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED SHARED 
{STORTEMP.I}
{AVDELNINGTEMP.I}
{EXECLIN.I}
{TIDUTTTNEW.I}
   
DEFINE SHARED TEMP-TABLE omr_temp
   FIELD AVDELNINGNR AS INTEGER
   FIELD DISTRIKTID AS INTEGER
   FIELD NAMN AS CHARACTER
   INDEX OMR IS PRIMARY AVDELNINGNR DISTRIKTID.

DEFINE SHARED TEMP-TABLE avd_temp
   FIELD AVDELNINGNR AS INTEGER
   FIELD NAMN AS CHARACTER
   INDEX AVD IS PRIMARY AVDELNINGNR.
   
DEFINE SHARED TEMP-TABLE spann_temp2    
   FIELD SPANID AS INTEGER
   FIELD NAMN AS CHARACTER
   INDEX SPAN SPANID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-TIDS
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


/* Definitions for FRAME FRAME-TIDS                                     */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_SKRIV FBTN_EXCEL BTN_AVS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD klockan100 WINDOW-2 
FUNCTION klockan100 RETURNS DECIMAL
  (INPUT ber60 AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD klockan60 WINDOW-2 
FUNCTION klockan60 RETURNS DECIMAL
  (INPUT ber100 AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_EXCEL 
     LABEL "Till Excel":L 
     SIZE 14 BY 1 TOOLTIP "Aktivera investering i Excel format".

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
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 123.5 BY 26.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-TIDS
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_SKRIV AT ROW 27.96 COL 81
     FBTN_EXCEL AT ROW 27.96 COL 96
     BTN_AVS AT ROW 27.96 COL 111
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.


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
         TITLE              = "SAIFI - Medelavbrottsfrekvensen för 'alla kunder'"
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
/* SETTINGS FOR FRAME FRAME-TIDS
                                                                        */
/* BROWSE-TAB BRW_UT 1 FRAME-TIDS */
/* SETTINGS FOR BROWSE BRW_UT IN FRAME FRAME-TIDS
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UT:MAX-DATA-GUESS IN FRAME FRAME-TIDS         = 1000.

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
     _Query            is NOT OPENED
*/  /* BROWSE BRW_UT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-TIDS
/* Query rebuild information for FRAME FRAME-TIDS
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-TIDS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-TIDS /* Avsluta */
DO:   
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-TIDS /* Skriv ut */
DO:
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE.
   ELSE RUN EKLOGS.P.        
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME FRAME-TIDS /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EXCEL WINDOW-2
ON CHOOSE OF FBTN_EXCEL IN FRAME FRAME-TIDS /* Till Excel */
DO:
   RUN  nrcol_UI.
   RUN safiexcel_UI.
   {musarrow.i}    
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
   {BORTBRWPROC.I}
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
   RUN  nrcol_UI.
  
   ASSIGN
   str = "".  
   i = 1.
   DO WHILE i <= utnr[breddantal] + bredd[breddantal] - 1:
      str = str + "=".     
      i = i + 1.
   END.   
   i = 2.      
   DO WHILE i <= breddantal:             
      SUBSTRING(str,(utnr[i] - 1),1) = ".".      
      i = i + 1.
   END.
   RUN huvud_UI.   
   RUN fore_UI.
   FIND FIRST tidut  NO-LOCK NO-ERROR. 
   IF AVAILABLE tidut THEN DO:    
      ENABLE BRW_UT WITH FRAME {&FRAME-NAME}.
      BRW_UT:HIDDEN = FALSE.
      OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
   END.
   ELSE DO:
      /*status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").*/
      status-mus2 = SESSION:SET-WAIT-STATE("").
      LEAVE MAIN-BLOCK.
                 
   END.
   RUN enable_UI.   
   {FRMSIZE.I}    
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = FBTN_EXCEL:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   ASSIGN
   Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   {musarrow.i}    
   IF visvalvar = 2 THEN DO:
      APPLY "CHOOSE" TO  FBTN_EXCEL.
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
  ENABLE BTN_SKRIV FBTN_EXCEL BTN_AVS 
      WITH FRAME FRAME-TIDS IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-TIDS}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fore_UI WINDOW-2 
PROCEDURE fore_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/

   
   CREATE tidut.
   tidut.UT = str.
   ASSIGN
   Tdriftant = 0
   Tplanant = 0
   Thspant = 0
   Tlspant = 0
   Ttotavbrott = 0
   Ttotbortfall = 0
   Tbortbelast = 0
   Ttotkunder = 0
   antalomr = 0.
   IF valfore = TRUE THEN DO:
      RUN storning_UI.
      RUN totalt_UI.
   END.
   ELSE DO:
      IF alla = TRUE THEN DO:
         RUN storning_UI.
         RUN totalt_UI.
      END.
      ELSE DO:
         IF alla2 = TRUE THEN DO:
            OPEN QUERY kq FOR EACH stordistemp WHERE stordistemp.AVDELNINGNR = forvar
            AND stordistemp.ARTAL = YEAR(bdatum) USE-INDEX AVDARTAL NO-LOCK.
            GET FIRST kq NO-LOCK.
            DO WHILE AVAILABLE(stordistemp):
               ASSIGN
               distvar = stordistemp.DISTRIKTID
               antalomr = antalomr + 1.
               CREATE tidut. 
               CREATE tidut.
               ASSIGN           
               SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Distrikt:" + stordistemp.NAMN.           
               RUN storning_UI.            
               RUN utlista_UI.
               GET NEXT kq NO-LOCK.
            END.
            CLOSE QUERY kq.
            IF antalomr > 1 THEN DO:
               RUN totalt_UI.
            END.
         END.
         ELSE DO:
            FOR EACH omr_temp USE-INDEX OMR NO-LOCK:
               ASSIGN
               distvar = omr_temp.DISTRIKTID
               antalomr = antalomr + 1.                       
               CREATE tidut. 
               CREATE tidut.
               ASSIGN            
               SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Distrikt:" + omr_temp.NAMN.            
               RUN storning_UI.
               RUN utlista_UI.
            END.
            IF antalomr > 1 THEN DO:
               RUN totalt_UI.
            END.
         END.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nrcol_UI WINDOW-2 
PROCEDURE nrcol_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN
   nrcol[1] = 1
   nrcol[2] = 2
   nrcol[3] = 3
   breddantal = 3   /*antal kolumner*/
   bredd[1] = 58
   bredd[2] = 11
   bredd[3] = 11.      
   ASSIGN
   i = 2.     
   utnr[nrcol[1]] = 1.
   DO WHILE i <= breddantal:             
      utnr[i] = utnr[i - 1] + bredd[i - 1] + 1.            
      i = i + 1.
   END.   
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
   DO TRANSACTION:              
      CREATE tidut.               
      IF vallista = 1 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SAIFI - Medelavbrottsfrekvensen för 'alla kunder'".
      ELSE IF vallista = 2 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SAIDI - Medelavbrottstid för 'alla kunder'".
      ELSE IF vallista = 3 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "CAIDI - Medelavbrottstid för 'berörda kunder'".
      ELSE IF vallista = 4 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "ASAI - tillgängligheten i nätet".
      ELSE IF vallista = 5 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SAIFI - Medelavbrottsfrekvensen för 'alla kunder' > 3 min".
      ELSE IF vallista = 6 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SAIDI - Medelavbrottstid för 'alla kunder' > 3 min".
      ELSE IF vallista = 7 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "CAIDI - Medelavbrottstid för 'berörda kunder' > 3 min".
      ELSE IF vallista = 8 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "ASAI - tillgängligheten i nätet > 3 min".
      WINDOW-2:TITLE = SUBSTRING(tidut.UT,utnr[nrcol[1]]).
      SUBSTRING(tidut.UT,60) = STRING(TODAY). 
      CREATE tidut.
      CREATE tidut.
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Urval:".
      CREATE tidut.
      IF uttyp = 1 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Driftstörningar".
      ELSE IF uttyp = 2 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Planerade avbrott".
      ELSE
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Driftstörningar och planerade avbrott".
      CREATE tidut.
      IF period = 1 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Period:" + STRING(YEAR(bdatum)).
      ELSE
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Period:" + STRING(bdatum,"9999/99/99") + " - " + STRING(avdatum,"9999/99/99").
      CREATE tidut.
      IF valfore = TRUE THEN DO:
         SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Valda företag:".
         CREATE tidut.         
         FOR EACH avd_temp USE-INDEX AVD:
            SUBSTRING(tidut.UT,10) = avd_temp.NAMN.
            CREATE tidut.
         END.
      END.
      ELSE DO:
         IF alla = TRUE THEN DO:
            SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Alla företag".
         END.
         ELSE DO:
            FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNR = forvar NO-LOCK NO-ERROR.
            IF alla2 = TRUE THEN DO:
               SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Valt företag:" + avdelningtemp.AVDELNINGNAMN.
            END.
            ELSE DO:
               SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Valt företag:" + avdelningtemp.AVDELNINGNAMN.
               CREATE tidut.
               SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Distrikt:".
               FOR EACH omr_temp USE-INDEX OMR:
                  SUBSTRING(tidut.UT,10) = omr_temp.NAMN.
                  CREATE tidut.
               END.
            END.
         END.
      END.
   END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE safiexcel_UI WINDOW-2 
PROCEDURE safiexcel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
       /*Vilka kolumner*/
   ASSIGN
   slutbredd = 11
   bredd[1] = 58  
   bredd[2] = 11  
   bredd[3] = 11.    
   i = 4.
   /*
   REPEAT:
      IF i > 50 THEN LEAVE.
      ASSIGN
      bredd[i] = 5.
      i = i + 1.        
   END.
   */
   bladvar = 0.
   iRad = 1.
   RUN colbredd_UI.  
   RUN startexcel_UI.
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
      IF SUBSTRING(tidut.UT,estartnr[1],12) = "============" THEN DO:
         LEAVE.
      END.     
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).                
      END.
      ELSE DO:
         IF tidut.UT NE "" THEN DO:
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).           
         END.
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
   END.
   RUN slutexcel_UI. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE storning_UI WINDOW-2 
PROCEDURE storning_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF vallista >= 5 AND vallista < 8 THEN treminuter = TRUE.
   ELSE treminuter = FALSE.
 
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN STORSUM.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, INPUT bdatum, INPUT avdatum, INPUT period, 
      INPUT uttyp, INPUT vallista, INPUT alla, 
      OUTPUT driftant, OUTPUT planant, OUTPUT hspant, OUTPUT lspant, 
      OUTPUT totavbrott, OUTPUT totbortfall, OUTPUT bortbelast, OUTPUT totkunder, INPUT treminuter).      
   END.
   ELSE DO:
      RUN STORSUM.P 
      (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, INPUT bdatum, INPUT avdatum, INPUT period, 
      INPUT uttyp, INPUT vallista, INPUT alla, 
      OUTPUT driftant, OUTPUT planant,OUTPUT hspant, OUTPUT lspant, 
      OUTPUT totavbrott, OUTPUT totbortfall, OUTPUT bortbelast, OUTPUT totkunder, INPUT treminuter).
   END.
   ASSIGN
   Tdriftant = Tdriftant + driftant
   Tplanant = Tplanant + planant
   Thspant = Thspant + hspant
   Tlspant = Tlspant + lspant
   Ttotavbrott = Ttotavbrott + totavbrott
   Ttotbortfall = Ttotbortfall + totbortfall
   Tbortbelast = Tbortbelast + bortbelast
   Ttotkunder = Ttotkunder + totkunder.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE totalt_UI WINDOW-2 
PROCEDURE totalt_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   CREATE tidut.
   CREATE tidut.               
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Totalsummor:".            
   CREATE tidut.
   CREATE tidut.
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Driftavbrott"     
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(Tdriftant,">>>>>>>>>>9").
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Planerade avbrott"
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(Tplanant,">>>>>>>>>>9").
   CREATE tidut.
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,20) = "Summa avbrott"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(Tplanant + Tdriftant,">>>>>>>>>>9").
   CREATE tidut.
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,20) = "Summa kunder"     
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(Ttotkunder,">>>>>>>>>>9").
   CREATE tidut.
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Berörda lsp-kunder"
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(Tlspant,">>>>>>>>>>9").
   CREATE tidut.    
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Berörda hsp-kunder"
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(Thspant,">>>>>>>>>>9").
   CREATE tidut.
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,20) = "Summa berörda kunder"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(Tlspant + Thspant,">>>>>>>>>>9").
   CREATE tidut.
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Kundavbrottstid timmar"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(Ttotavbrott,">>>>>>>>>>9").
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Bortkopplad belastning MW"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(Tbortbelast,">>>>>>>>9.9").
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Energiförlust MWh"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(Ttotbortfall,">>>>>>>>>>9").
   CREATE tidut.                                
   CREATE tidut.
   
   IF vallista = 1 THEN DO:      
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SAIFI - Medelavbrottsfrekvens för 'alla kunder'".
      IF Ttotkunder = 0 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "Gr.data saknas".
      ELSE                                                              
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING((Tlspant + Thspant) / Ttotkunder,">>>>>9.9999").
   END.
   ELSE IF vallista = 2 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SAIDI - Medelavbrottstid för 'alla kunder'".
      IF Ttotkunder = 0 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "Gr.data saknas".
      ELSE
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(Ttotavbrott / Ttotkunder,">>>>>9.9999").
   END.
   ELSE IF vallista = 3 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "CAIDI - Medelavbrottstid för 'berörda kunder'".
      IF (Tlspant + Thspant) = 0 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "Kunder saknas".
      ELSE
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(Ttotavbrott / (Tlspant + Thspant),">>>>>9.9999").
   END.
   ELSE IF vallista = 4 THEN DO:
      RUN TIDBER.P (INPUT DATE(01,01,YEAR(bdatum)), INPUT DATE(12,31,YEAR(bdatum)), INPUT 00.00,
      INPUT 24.00, OUTPUT timmar100, OUTPUT timmar60).
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "ASAI - tillgängligheten i nätet".
      IF Ttotkunder = 0 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "Gr.data saknas".
      ELSE
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING((timmar100 - (Ttotavbrott / Ttotkunder)) / timmar100,">>>>9.99999").
   END.     
   ELSE IF vallista = 5 THEN DO:      
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SAIFI - Medelavbrottsfrekvens för 'alla kunder' > 3 min".
      IF Ttotkunder = 0 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "Gr.data saknas".
      ELSE                                                              
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING((Tlspant + Thspant) / Ttotkunder,">>>>>9.9999").
   END.
   ELSE IF vallista = 6 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SAIDI - Medelavbrottstid för 'alla kunder' > 3 min".
      IF Ttotkunder = 0 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "Gr.data saknas".
      ELSE
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(Ttotavbrott / Ttotkunder,">>>>>9.9999").
   END.
   ELSE IF vallista = 7 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "CAIDI - Medelavbrottstid för 'berörda kunder' > 3 min".
      IF (Tlspant + Thspant) = 0 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "Kunder saknas".
      ELSE
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(Ttotavbrott / (Tlspant + Thspant),">>>>>9.9999").
   END.
   ELSE IF vallista = 8 THEN DO:
      RUN TIDBER.P (INPUT DATE(01,01,YEAR(bdatum)), INPUT DATE(12,31,YEAR(bdatum)), INPUT 00.00,
      INPUT 24.00, OUTPUT timmar100, OUTPUT timmar60).
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "ASAI - tillgängligheten i nätet > 3 min".
      IF Ttotkunder = 0 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "Gr.data saknas".
      ELSE
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING((timmar100 - (Ttotavbrott / Ttotkunder)) / timmar100,">>>>9.99999").
   END.         

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE utlista_UI WINDOW-2 
PROCEDURE utlista_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   CREATE tidut.
   CREATE tidut.
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Driftavbrott"
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(driftant,">>>>>>>>>>9").
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Planerade avbrott"
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(planant,">>>>>>>>>>9").
   CREATE tidut.
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,20) = "Summa avbrott"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(planant + driftant,">>>>>>>>>>9").
   CREATE tidut.
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,20) = "Summa kunder"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(totkunder,">>>>>>>>>>9").
   CREATE tidut.
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Berörda lsp-kunder"
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(lspant,">>>>>>>>>>9").
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Berörda hsp-kunder"
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = STRING(hspant,">>>>>>>>>>9").
   CREATE tidut.
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,20) = "Summa berörda kunder"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(lspant + hspant,">>>>>>>>>>9").
   CREATE tidut.
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Kundavbrottstid timmar"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(totavbrott,">>>>>>>>>>9").
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Bortkopplad belastning MW"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(bortbelast,">>>>>>>>9.9").
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "Energiförlust MWh"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(totbortfall,">>>>>>>>9.9").
   CREATE tidut.
   CREATE tidut.
   IF vallista = 1 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SAIFI - Medelavbrottsfrekvens för 'alla kunder'".
      IF totkunder = 0 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "Gr.data saknas".
      ELSE
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING((lspant + hspant) / totkunder,">>>>>9.9999").
   END.
   ELSE IF vallista = 2 THEN DO:           
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SAIDI - Medelavbrottstid för 'alla kunder'".
      IF totkunder = 0 THEN DO:
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "Gr.data saknas".
      END.
      ELSE DO:
         ASSIGN
         SUBSTRING(tidut.UT,50) = "Timmar"
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(totavbrott / totkunder,">>>>>9.9999").
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,50) = "Minuter"
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING((totavbrott / totkunder) * 60,">>>>>>>9.99").
      END.
   END.
   ELSE IF vallista = 3 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "CAIDI - Medelavbrottstid för 'berörda kunder'".
      IF (lspant + hspant) = 0 THEN DO:
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "Kunder saknas".
      END.
      ELSE DO:
         ASSIGN
         SUBSTRING(tidut.UT,50) = "Timmar"
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(totavbrott / (lspant + hspant),">>>>>9.9999").
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,50) = "Minuter"
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING((totavbrott / (lspant + hspant)) * 60,">>>>>>>9.99").
      END.
   END.
   ELSE IF vallista = 4 THEN DO:
      RUN TIDBER.P (INPUT DATE(01,01,YEAR(bdatum)), INPUT DATE(12,31,YEAR(bdatum)), INPUT 00.00,
      INPUT 24.00, OUTPUT timmar100, OUTPUT timmar60).
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "ASAI - tillgängligheten i nätet".
      IF totkunder = 0 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "Gr.data saknas".
      ELSE
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING((timmar100 - (totavbrott / totkunder)) / timmar100,">>>>9.99999").
   END.
   ELSE IF vallista = 5 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SAIFI - Medelavbrottsfrekvens för 'alla kunder' > 3 min".
      IF totkunder = 0 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "Gr.data saknas".
      ELSE
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING((lspant + hspant) / totkunder,">>>>>9.9999").
   END.
   ELSE IF vallista = 6 THEN DO:           
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "SAIDI - Medelavbrottstid för 'alla kunder' > 3 min".
      IF totkunder = 0 THEN DO:
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "Gr.data saknas".
      END.
      ELSE DO:
         ASSIGN
         SUBSTRING(tidut.UT,50) = "Timmar"
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(totavbrott / totkunder,">>>>>9.9999").
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,50) = "Minuter"
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING((totavbrott / totkunder) * 60,">>>>>>>9.99").
      END.
   END.
   ELSE IF vallista = 7 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "CAIDI - Medelavbrottstid för 'berörda kunder' > 3 min".
      IF (lspant + hspant) = 0 THEN DO:
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "Kunder saknas".
      END.
      ELSE DO:
         ASSIGN
         SUBSTRING(tidut.UT,50) = "Timmar"
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(totavbrott / (lspant + hspant),">>>>>9.9999").
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,50) = "Minuter"
         SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING((totavbrott / (lspant + hspant)) * 60,">>>>>>>9.99").
      END.
   END.
   ELSE IF vallista = 8 THEN DO:
      RUN TIDBER.P (INPUT DATE(01,01,YEAR(bdatum)), INPUT DATE(12,31,YEAR(bdatum)), INPUT 00.00,
      INPUT 24.00, OUTPUT timmar100, OUTPUT timmar60).
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "ASAI - tillgängligheten i nätet > 3 min".
      IF totkunder = 0 THEN
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "Gr.data saknas".
      ELSE
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING((timmar100 - (totavbrott / totkunder)) / timmar100,">>>>9.99999").
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION klockan100 WINDOW-2 
FUNCTION klockan100 RETURNS DECIMAL
  (INPUT ber60 AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600. 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION klockan60 WINDOW-2 
FUNCTION klockan60 RETURNS DECIMAL
  (INPUT ber100 AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) / 100) * 60.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

