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
DEFINE SHARED VARIABLE nytt_bestnr2 AS INTEGER NO-UNDO.        
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.   
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.     
DEFINE SHARED VARIABLE valkund AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valnamn AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE akval AS INTEGER NO-UNDO.            
DEFINE SHARED VARIABLE fakt AS LOGICAL NO-UNDO.                         
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 


DEFINE VARIABLE str AS CHARACTER FORMAT "X(86)" NO-UNDO.
DEFINE VARIABLE totalt AS DECIMAL NO-UNDO.     
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{SMTRL.I}
DEFINE SHARED TEMP-TABLE off_mtrl NO-UNDO    
    FIELD TOTALT AS DECIMAL.  

DEFINE SHARED TEMP-TABLE skapa_mtrl NO-UNDO                
   FIELD FORE AS CHARACTER
   FIELD KADR AS CHARACTER  
   FIELD KPNR AS CHARACTER
   FIELD KORT AS CHARACTER
   FIELD BOX AS CHARACTER FORMAT "X(5)"
   FIELD FAX AS CHARACTER
   FIELD KIKONTAKT AS CHARACTER                            
   FIELD KITELE AS CHARACTER        
   FIELD BESTNAMN AS CHARACTER    
   FIELD TELE AS CHARACTER
   FIELD ADR AS CHARACTER
   FIELD PNR AS CHARACTER
   FIELD ORT AS CHARACTER
   FIELD KONTAKT AS CHARACTER 
   FIELD FAXK AS CHARACTER
   FIELD FAKADR AS CHARACTER
   FIELD FAKPNR AS CHARACTER
   FIELD FAKORT AS CHARACTER
   FIELD LEVADR AS CHARACTER
   FIELD LEVPNR AS CHARACTER
   FIELD LEVORT AS CHARACTER   
   FIELD DATUM AS DATE
   FIELD DATUM2 AS DATE
   FIELD MARK AS CHARACTER FORMAT "X(35)"
   FIELD KOM AS CHARACTER FORMAT "X(40)".

/*DEFINE SHARED TEMP-TABLE spec_mtrl    
    FIELD ENR LIKE MTRLDEP.ENR
    FIELD BENAMNING LIKE MTRLDEP.BENAMNING
    FIELD ENHET LIKE MTRLDEP.ENHET   
    FIELD BPRIS LIKE MTRLDEP.BPRIS       
    FIELD NPRIS LIKE MTRLDEP.NPRIS        
    FIELD LEVKOD LIKE MTRLDEP.LEVKOD
    FIELD MARK AS LOGICAL       
    FIELD SALDO LIKE MTRLDEP.SALDO. 
    
DEFINE SHARED TEMP-TABLE off_mtrl     
    FIELD TOTALT LIKE MTRL.BPRIS.         
    
*/   
   
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
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 95.5 BY 26.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_SKRIV AT ROW 28.25 COL 65.5
     BTN_AVS AT ROW 28.25 COL 83
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97 BY 28.42.


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
         TITLE              = "Följesedel"
         HEIGHT             = 28.42
         WIDTH              = 97
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 97
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 97
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
   {BORTBRWPROC.I}
   RETURN.
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
   {BORTBRWPROC.I}
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
   IF fakt = FALSE THEN {&WINDOW-NAME}:TITLE = "Följesedel".                
   ELSE {&WINDOW-NAME}:TITLE = "Fakturaunderlag".   
   EMPTY TEMP-TABLE tidut NO-ERROR.   
   IF fakt = FALSE THEN   
   str=   
   "--------------------------------------------------------------------------".    
   ELSE str=   
   "--------------------------------------------------------------------------------". 
   RUN huvud_UI.
   RUN rubrik_UI.
   IF musz = FALSE THEN RUN klar_UI.      
   RUN sidfot_UI.    
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
         RUN ut_UI.
         status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
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
   Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.      
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI WINDOW-2 
PROCEDURE huvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     /*HUVUD*/                                
   DO TRANSACTION:         
      IF musz = TRUE THEN musz = musz.   
      ELSE DO:                  
         CREATE tidut.
         IF fakt = FALSE THEN SUBSTRING(tidut.UT,1) = "FÖLJESEDEL / BESTÄLLNING NR:".
         ELSE SUBSTRING(tidut.UT,1) = "FAKTURAUNDERLAG BESTÄLL. NR:". 
         ASSIGN
         SUBSTRING(tidut.UT,29) = STRING(nytt_bestnr2)
         SUBSTRING(tidut.UT,40) = STRING(TODAY)
         SUBSTRING(tidut.UT,50) = STRING(TIME,"HH:MM:SS").
         CREATE tidut.                        
         FIND FIRST skapa_mtrl NO-LOCK NO-ERROR.
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Köpare"
         SUBSTRING(tidut.UT,30) = "Säljare".
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = skapa_mtrl.BESTNAMN
         SUBSTRING(tidut.UT,30) = skapa_mtrl.FORE. 
         CREATE tidut.    
         ASSIGN
         SUBSTRING(tidut.UT,1) = skapa_mtrl.ADR
         SUBSTRING(tidut.UT,30) = skapa_mtrl.KADR.
         IF skapa_mtrl.BOX NE "" THEN ASSIGN
         SUBSTRING(tidut.UT,55) = "BOX:"
         SUBSTRING(tidut.UT,60) = STRING(skapa_mtrl.BOX).    
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = STRING(skapa_mtrl.PNR)
         SUBSTRING(tidut.UT,8) = skapa_mtrl.ORT
         SUBSTRING(tidut.UT,30) = STRING(skapa_mtrl.KPNR)
         SUBSTRING(tidut.UT,38) = skapa_mtrl.KORT.  
         CREATE tidut. 
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Referens"
         SUBSTRING(tidut.UT,30) = "Referens".
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = skapa_mtrl.KONTAKT
         SUBSTRING(tidut.UT,30) = skapa_mtrl.KIKONTAKT.
         CREATE tidut.
         ASSIGN                                       
         SUBSTRING(tidut.UT,1) = "TELE:"
         SUBSTRING(tidut.UT,6) = STRING(skapa_mtrl.TELE)
         SUBSTRING(tidut.UT,30) = "TELE:"
         SUBSTRING(tidut.UT,36) = STRING(skapa_mtrl.KITELE).
         IF skapa_mtrl.FAX NE "" THEN ASSIGN
         SUBSTRING(tidut.UT,50) = "FAX:"
         SUBSTRING(tidut.UT,56) = STRING(skapa_mtrl.FAX).                                                           
         CREATE tidut. 
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = str.
         CREATE tidut.
         CREATE tidut.
         IF akval = 1 THEN DO:                 
            &Scoped-define FORMATNAMN valaonr
            {AOFORMAT2.I}
            ASSIGN                                                               
            SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaok) + ": " + formataonr + " " + 
            "DELNR: " + STRING(valdelnr,Guru.Konstanter:varforetypchar[1]).
            CREATE tidut.  
            ASSIGN                                                               
            SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gaonamnk) + ": " + valort.             
         END.
         ELSE DO:
            ASSIGN
            SUBSTRING(tidut.UT,1) = "LEVERANSADRESS:" 
            SUBSTRING(tidut.UT,40) = "ADRESS:".
            CREATE tidut. 
            CREATE tidut.
            IF skapa_mtrl.LEVADR = " " THEN DO:
               ASSIGN     
               SUBSTRING(tidut.UT,1) = "SE ADRESS!"
               SUBSTRING(tidut.UT,40) = skapa_mtrl.BESTNAMN.
               CREATE tidut.
               ASSIGN
               SUBSTRING(tidut.UT,40) = skapa_mtrl.ADR.            
               CREATE tidut.
               ASSIGN
               SUBSTRING(tidut.UT,40) = STRING(skapa_mtrl.PNR,"999 99")   
               SUBSTRING(tidut.UT,48) = skapa_mtrl.ORT.         
               CREATE tidut.               
            END.
            ELSE DO:
               ASSIGN     
               SUBSTRING(tidut.UT,1) = skapa_mtrl.BESTNAMN
               SUBSTRING(tidut.UT,40) = skapa_mtrl.BESTNAMN.
               CREATE tidut. 
               ASSIGN
               SUBSTRING(tidut.UT,1) = skapa_mtrl.LEVADR
               SUBSTRING(tidut.UT,40) = skapa_mtrl.ADR.
               CREATE tidut.
               ASSIGN    
               SUBSTRING(tidut.UT,1) = STRING(skapa_mtrl.LEVPNR,"999 99")
               SUBSTRING(tidut.UT,8) = skapa_mtrl.LEVORT
               SUBSTRING(tidut.UT,40) = STRING(skapa_mtrl.PNR,"999 99")   
               SUBSTRING(tidut.UT,48) = skapa_mtrl.ORT.         
               CREATE tidut.
            END. 
            CREATE tidut.        
            IF skapa_mtrl.DATUM NE ? THEN DO:
               ASSIGN       
               SUBSTRING(tidut.UT,1) = "LEVERANSDAG:"
               SUBSTRING(tidut.UT,13) = STRING(skapa_mtrl.DATUM).                                               
            END.
            ELSE DO:                                            
               ASSIGN       
               SUBSTRING(tidut.UT,1) = "LEVERANSDAG:"            
               SUBSTRING(tidut.UT,13) = "OMGÅENDE".          
            END.            
            ASSIGN                                                
            SUBSTRING(tidut.UT,40) = "FAKTURERINGSADRESS:".       
            CREATE tidut.
            CREATE tidut.          
            IF skapa_mtrl.FAKADR NE " " THEN DO:            
               ASSIGN                                                
               SUBSTRING(tidut.UT,40) = skapa_mtrl.BESTNAMN.
               CREATE tidut.
               ASSIGN                                                
               SUBSTRING(tidut.UT,40) = skapa_mtrl.FAKADR.
               CREATE tidut.
               ASSIGN                                           
               SUBSTRING(tidut.UT,40) = STRING(skapa_mtrl.FAKPNR,"999 99")
               SUBSTRING(tidut.UT,48) = skapa_mtrl.FAKORT. 
            END.
            ELSE DO:
               ASSIGN                                                
               SUBSTRING(tidut.UT,40) = "SE ADRESS". 
            END.      
            CREATE tidut.
            CREATE tidut.        
            ASSIGN                                                
            SUBSTRING(tidut.UT,40) = "KONTAKTPERSON:".        
            CREATE tidut.
            SUBSTRING(tidut.UT,40) = skapa_mtrl.KONTAKT.
            CREATE tidut.
            ASSIGN          
            SUBSTRING(tidut.UT,40) = "TELE:"
            SUBSTRING(tidut.UT,46) = STRING(skapa_mtrl.TELE)
            SUBSTRING(tidut.UT,60) = "FAX:"
            SUBSTRING(tidut.UT,64) = STRING(skapa_mtrl.FAXK).             
         END.   
         CREATE tidut.
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "UTTAGSDATUM:"
         SUBSTRING(tidut.UT,13) = STRING(skapa_mtrl.DATUM,"9999/99/99") + " TILL " + STRING(skapa_mtrl.DATUM2,"9999/99/99").
         CREATE tidut.
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "KOMMENTARER:"
         SUBSTRING(tidut.UT,13) = STRING(skapa_mtrl.KOM).                  
         CREATE tidut.           
         SUBSTRING(tidut.UT,1) = str.
         CREATE tidut.
         CREATE tidut.                   
      END.
   END.                    
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
   totalt = 0.
   CREATE tidut.      
   FOR EACH spec_mtrl: 
      IF fakt = FALSE THEN DO:          
         CREATE tidut.      
         ASSIGN  
         SUBSTRING(tidut.UT,1) = spec_mtrl.ENR
         SUBSTRING(tidut.UT,13) = SUBSTRING(spec_mtrl.BENAMNING,1,35)                         
         SUBSTRING(tidut.UT,64) = spec_mtrl.ENHET    
         SUBSTRING(tidut.UT,70) = STRING(spec_mtrl.SALDO,">>>>9").
      END.
      ELSE DO:                                                          
         CREATE tidut.      
         ASSIGN  
         SUBSTRING(tidut.UT,1) = spec_mtrl.ENR
         SUBSTRING(tidut.UT,13) = SUBSTRING(spec_mtrl.BENAMNING,1,35)                         
         SUBSTRING(tidut.UT,49) = spec_mtrl.ENHET    
         SUBSTRING(tidut.UT,55) = STRING(spec_mtrl.SALDO,">>>>9")          
         SUBSTRING(tidut.UT,61) = STRING(spec_mtrl.BPRIS,"->>>>.99")
         SUBSTRING(tidut.UT,72) = STRING(spec_mtrl.BPRIS * spec_mtrl.SALDO,"->>>>.99").
         totalt = totalt + (spec_mtrl.BPRIS * spec_mtrl.SALDO).
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
   IF fakt = FALSE THEN DO:  
      ASSIGN
      str=                                                                    
"===========.==================================================.=====.=====". 
      CREATE tidut.       
      SUBSTRING(tidut.UT,1) = str.                     
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:genk)                                                                          
      SUBSTRING(tidut.UT,13) = "BENÄMNING"                         
      SUBSTRING(tidut.UT,64) = "ENHET"
      SUBSTRING(tidut.UT,70) = "ANTAL".                        
      CREATE tidut.       
      SUBSTRING(tidut.UT,1) = str.         
   END.
   ELSE DO: 
      ASSIGN
      str=                                                                    
"===========.===================================.=====.=====.==========.=========". 
      CREATE tidut.       
      SUBSTRING(tidut.UT,1) = str.                     
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:genk)                                                                          
      SUBSTRING(tidut.UT,13) = "BENÄMNING"                         
      SUBSTRING(tidut.UT,49) = "ENHET"
      SUBSTRING(tidut.UT,55) = "ANTAL"
      SUBSTRING(tidut.UT,61) = "PRIS"
      SUBSTRING(tidut.UT,72) = "SUMMA".                        
      CREATE tidut.       
      SUBSTRING(tidut.UT,1) = str.   
   END.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sidfot_UI WINDOW-2 
PROCEDURE sidfot_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   IF fakt = TRUE THEN DO:
      CREATE tidut.
      ASSIGN
      str=                                                                    
"======================================================================.=========". 
      CREATE tidut.       
      SUBSTRING(tidut.UT,1) = str.                     
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,65) = "TOTALT:"
      SUBSTRING(tidut.UT,72) = STRING(totalt,"->>>>.99"). 
      FIND FIRST off_mtrl NO-LOCK NO-ERROR.
      IF AVAILABLE off_mtrl THEN DO:
         CREATE tidut.
         CREATE tidut.
         ASSIGN  
         SUBSTRING(tidut.UT,61) = "OFFERTPRIS:"
         SUBSTRING(tidut.UT,72) = STRING(off_mtrl.TOTALT,"->>>>.99").  
      END.   
   END.       
   CREATE tidut. 
   CREATE tidut.                                   
   /*ASSIGN
 *    SUBSTRING(tidut.UT,1) = "MÄRKNING:"
 *    SUBSTRING(tidut.UT,10) = STRING(skapa_mtrl.MARK).
 *    CREATE tidut.*/                                         
   IF fakt = FALSE THEN DO:
      CREATE tidut. 
      CREATE tidut.                                   
      ASSIGN
      SUBSTRING(tidut.UT,1) = "OVANSTÅENDE MATERIEL EMOTTAGET, I ENLIGHET MED GÄLLANDE LEVERANSVILLKOR.". 
      CREATE tidut.  
      CREATE tidut.          
      ASSIGN
      SUBSTRING(tidut.UT,1) = "KVITTERAS:"
      SUBSTRING(tidut.UT,34) = "NAMNFÖRTYDLIGANDE:".   
      CREATE tidut.   
      SUBSTRING(tidut.UT,11) = ".......................".
      SUBSTRING(tidut.UT,52) = ".......................".    
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

