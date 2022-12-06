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
DEFINE SHARED VARIABLE pris AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE alla AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE nytt_bestnr AS INTEGER NO-UNDO.                                          
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.        


DEFINE VARIABLE str AS CHARACTER FORMAT "X(86)" NO-UNDO.
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(86)" NO-UNDO.
DEFINE VARIABLE totalt AS DECIMAL NO-UNDO. 
DEFINE VARIABLE totalt2 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE leverant AS CHARACTER NO-UNDO.
DEFINE VARIABLE lev AS CHARACTER NO-UNDO.
DEFINE VARIABLE firstlev AS CHARACTER NO-UNDO.         
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 0.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE kommando3 AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE rbrik AS CHARACTER NO-UNDO.

DEFINE VARIABLE orgdir AS CHARACTER NO-UNDO.
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{SPECMTRLTEMP.I}
{SKAPAMTRL.I}
{LEVTEMP.I}
{DEPATEMP.I}
{EXECLIN.I}
   
DEFINE SHARED TEMP-TABLE lev_temp NO-UNDO
   FIELD LEVKOD AS CHARACTER. 
   
{TIDUTTTNEW.I}
/* */

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
&Scoped-Define ENABLED-OBJECTS CMB_LEV BTN_SKRIV BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS CMB_LEV 

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

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18 BY .96
     BGCOLOR 15  NO-UNDO.

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
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 81.5 BY 26.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.5 COL 1.5
     BTN_EPOST AT ROW 28 COL 17
     CMB_LEV AT ROW 28 COL 32 COLON-ALIGNED NO-LABEL
     BTN_SKRIV AT ROW 28 COL 53.5
     BTN_AVS AT ROW 28 COL 69
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 83 BY 28.42.


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
         TITLE              = "Specifikation av beställning"
         HEIGHT             = 28.42
         WIDTH              = 83
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 83
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 83
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


&Scoped-define SELF-NAME BTN_EPOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EPOST WINDOW-2
ON CHOOSE OF BTN_EPOST IN FRAME FRAME-VINST /* Best. E-post */
DO:       
   {muswait.i} 
   RUN epexcel_UI .   
   
   /*RUN EMEDDSE.W (kommando2,kommando3,rbrik) .*/
   file-info:file-name = ".".
   orgdir = file-info:full-pathname.
   RUN SPECIALMAPI.P (INPUT "", INPUT "", INPUT kommando2).
   RUN SetCurrentDirectoryA IN Guru.Konstanter:hpApi  (INPUT orgdir).
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EPOST WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_EPOST IN FRAME FRAME-VINST /* Best. E-post */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO: 
   /*vartprospar = vartpro.
   vartpro = "visdtrpv".*/
   RUN SKRIVVAL.W (INPUT FALSE).
   /*vartpro = vartprospar.*/
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:
      {muswait.i} 
      MESSAGE "OBS! Vill du skriva ut beställning för alla leverantörer?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Utskrift?" UPDATE svar AS LOGICAL.         
      IF svar THEN DO:
         alla = TRUE.
      END.                     
      ELSE DO:
         alla = FALSE.
      END.  
      IF alla = TRUE THEN DO: 
         RUN alla_UI.
      END.
      ELSE DO:
         RUN ut_UI.
      END.   
      alla = FALSE.           
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


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV WINDOW-2
ON VALUE-CHANGED OF CMB_LEV IN FRAME FRAME-VINST
DO:
   EMPTY TEMP-TABLE tidut NO-ERROR.   
   ASSIGN                                               
   leverant = INPUT CMB_LEV.                
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = leverant NO-LOCK NO-ERROR.
   vald_lev = levtemp.LEVKOD.
   FIND FIRST skapa_mtrl.
   ASSIGN
   skapa_mtrl.LEVNAMN = levtemp.LEVNAMN
   skapa_mtrl.LKONTAKT = levtemp.LEVKONTAKT
   skapa_mtrl.LTELE = levtemp.LEVTEL
   skapa_mtrl.LADR = levtemp.LEVADR
   skapa_mtrl.LPNR = levtemp.LEVPNR
   skapa_mtrl.LORT = levtemp.LEVORT.
   /*str2=
"======================================================================================".      */
   RUN huvud_UI.
   IF pris = TRUE THEN
   RUN rubrik_UI.
   ELSE RUN rubrik2_UI.
   IF musz = FALSE THEN DO:
      IF pris = TRUE THEN RUN klar_UI.   
      ELSE RUN klar2_UI.
   END.
   IF pris = TRUE THEN RUN totalt_UI.    
   RUN enable_UI.   
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
   IF alla = FALSE THEN DO:
      status-ok = CMB_LEV:DELETE("0"). 
      FIND FIRST spec_mtrl WHERE spec_mtrl.LEVKOD = vald_lev NO-LOCK NO-ERROR.
      FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_lev NO-LOCK NO-ERROR.     
      ASSIGN       
      status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
      CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN
      lev = " ".      
      FOR EACH spec_mtrl WHERE spec_mtrl.LEVKOD NE vald_lev.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = spec_mtrl.LEVKOD NO-LOCK NO-ERROR.
         IF levtemp.LEVKOD NE lev THEN DO:
            ASSIGN  
            status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
            lev = spec_mtrl.LEVKOD. 
         END.                
      END.    
      EMPTY TEMP-TABLE tidut NO-ERROR.      
   str2=
   "=============================================================================".    
      RUN huvud_UI.
      IF pris = TRUE THEN
      RUN rubrik_UI.
      ELSE RUN rubrik2_UI.
      IF musz = FALSE THEN DO:
         IF pris = TRUE THEN RUN klar_UI.   
         ELSE RUN klar2_UI.
      END.
      IF pris = TRUE THEN RUN totalt_UI.       
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
            status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
            APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
            LEAVE MAIN-BLOCK. 
         END.
      END.
   END.
   ELSE DO:         
      RUN alla_UI.
      status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
      LEAVE MAIN-BLOCK.    
   END.  
   IF Guru.Konstanter:globforetag = "bode" OR  Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "KRAF" OR  Guru.Konstanter:globforetag = "snat"  OR  Guru.Konstanter:globforetag = "elpa" THEN DO:                         
      ENABLE BTN_EPOST WITH FRAME {&FRAME-NAME}. 
      BTN_EPOST:HIDDEN = FALSE.
   END.
   RUN enable_UI.        
   {FRMSIZE.I}  
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   ASSIGN
   Guru.GlobalaVariabler:colrighth = CMB_LEV:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   ASSIGN
   Guru.GlobalaVariabler:colrighth = BTN_EPOST:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).

   {musarrow.i}                                                
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE alla_UI WINDOW-2 
PROCEDURE alla_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   EMPTY TEMP-TABLE tidut NO-ERROR.
   FIND FIRST lev_temp WHERE lev_temp.LEVKOD = vald_lev.
   firstlev = vald_lev. 
   /*str=
   "=======================================================================================".    */
   RUN huvud_UI.
   IF pris = TRUE THEN
   RUN rubrik_UI.
   ELSE RUN rubrik2_UI.
   IF musz = FALSE THEN DO:
      IF pris = TRUE THEN RUN klar_UI.   
      ELSE RUN klar2_UI.
   END.
   IF pris = TRUE THEN RUN totalt_UI.    
   RUN ut_UI.        
   FOR EACH lev_temp WHERE lev_temp.LEVKOD NE firstlev:
      EMPTY TEMP-TABLE tidut NO-ERROR.      
      FIND FIRST levtemp WHERE levtemp.LEVKOD = lev_temp.LEVKOD NO-LOCK NO-ERROR.
      vald_lev = levtemp.LEVKOD.
      FIND FIRST skapa_mtrl.
      ASSIGN                                          
      skapa_mtrl.LEVNAMN = levtemp.LEVNAMN
      skapa_mtrl.LKONTAKT = levtemp.LEVKONTAKT
      skapa_mtrl.LTELE = levtemp.LEVTEL
      skapa_mtrl.LADR = levtemp.LEVADR
      skapa_mtrl.LPNR = levtemp.LEVPNR
      skapa_mtrl.LORT = levtemp.LEVORT.     
      /*ASSIGN      
      str=
      "=======================================================================================".    */
      RUN huvud_UI.
      IF pris = TRUE THEN
      RUN rubrik_UI.
      ELSE RUN rubrik2_UI.
      IF musz = FALSE THEN DO:
         IF pris = TRUE THEN RUN klar_UI.   
         ELSE RUN klar2_UI.
      END.
      IF pris = TRUE THEN RUN totalt_UI.       
      RUN ut_UI.                      
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY CMB_LEV 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  ENABLE CMB_LEV BTN_SKRIV BTN_AVS 
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
   ASSIGN
   allac[1] = "A"          
   allac[2] = "B"          
   allac[3] = "C"          
   allac[4] = "D"          
   allac[5] = "E"          
   allac[6] = "F"          
   allac[7] = "G"          
   allac[8] = "H"          
   allac[9] = "I"          
   allac[10] = "J"         
   allac[11] = "K"         
   allac[12] = "L"         
   allac[13] = "M"         
   allac[14] = "N"         
   allac[15] = "O"         
   allac[16] = "P"         
   allac[17] = "Q"         
   allac[18] = "R"         
   allac[19] = "S"         
   allac[20] = "T"         
   allac[21] = "U"         
   allac[22] = "V"         
   allac[23] = "W"         
   allac[24] = "X"         
   allac[25] = "Y"         
   allac[26] = "Z"         
   allac[27] = "AA"        
   allac[28] = "AB".
   IF pris = TRUE THEN DO:      
      ASSIGN           
      bredd[1] = 10
      bredd[2] = 34
      bredd[3] = 10
      bredd[4] = 7
      bredd[5] = 5
      bredd[6] = 7
      slutbredd = 8.
      IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "KRAF" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:         
         ASSIGN
         breddantal = 6
         i = 6.
      END.
      ELSE DO:         
         ASSIGN
         slutbredd = 12
         breddantal = 7        
         bredd[7] = 10
         i = 7.
      END.
   END.
   ELSE DO:
     ASSIGN      
     bredd[1] = 10
     bredd[2] = 34
     bredd[3] = 7
     bredd[4] = 5.    
     slutbredd = 8.
     IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "KRAF" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
        ASSIGN
        breddantal = 4
        i = 4.
      END.
      ELSE DO:         
         ASSIGN
         slutbredd = 12
         breddantal = 5            
         bredd[5] = 10
         i = 5.
      END.
   END.
   ASSIGN
   allachar[1] = TRUE
   bladvar = 0.
   iRad = 1.
   
   RUN colbredd_UI.  
   RUN startexceld_UI.
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
   chExcelApplication:VISIBLE = FALSE.
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
      IF SUBSTRING(tidut.UT,estartnr[1],12) = "===========." THEN DO:
         LEAVE.
      END.     
      IF SUBSTRING(tidut.UT,estartnr[1],12) = "============" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).  
         FIND NEXT tidut NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidut THEN DO:
            LEAVE.
         END. 
      END.              
   END.
   /*Poster*/ 
   
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).                
      END.              
      ELSE IF SUBSTRING(tidut.UT,58) = "TOTAL SUMMA:" THEN DO:         
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
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
   kommando2 = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN kommando2 = webclienttempdir.
   OS-CREATE-DIR VALUE(kommando2) NO-ERROR.   
   kommando3 = Guru.Konstanter:globanv + STRING(TODAY,"999999") + ".xls". 
   /*kommando3 = glob + ".xls".                          */
   kommando2 = kommando2 + kommando3.
   chExcelApplication:VISIBLE = FALSE.
   chExcelApplication:displayalerts = FALSE.
   chWorkbook:SaveAs(kommando2,,,,,,,).   

   RUN slutexceld_UI. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud2_UI WINDOW-2 
PROCEDURE huvud2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     /*HUVUD2*/       
   IF pris = TRUE THEN DO:      
      ASSIGN
      nrcol[1] = 1
      nrcol[2] = 2
      nrcol[3] = 3
      nrcol[4] = 4
      nrcol[5] = 5
      nrcol[6] = 6      
      bredd[1] = 10
      bredd[2] = 34
      bredd[3] = 9
      bredd[4] = 4
      bredd[5] = 4
      bredd[6] = 6.      
      IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "KRAF" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
          breddantal = 6.  
      END.
      ELSE DO:         
         ASSIGN
         breddantal = 7   
         nrcol[7] = 7
         bredd[7] = 10.
      END.
  END.
  ELSE DO:
     ASSIGN
      nrcol[1] = 1
      nrcol[2] = 2
      nrcol[3] = 3
      nrcol[4] = 4
      bredd[1] = 10
      bredd[2] = 34
      bredd[3] = 4
      bredd[4] = 5.      
     IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "KRAF" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
          breddantal = 4.  
      END.
      ELSE DO:         
         ASSIGN
         breddantal = 5   
         nrcol[5] = 5
         bredd[5] = 10.         
      END.
  END.

  
   ASSIGN
   i = 2.     
   utnr[nrcol[1]] = 1.
   DO WHILE i <= breddantal:             
      utnr[i] = utnr[i - 1] + bredd[i - 1] + 2.            
      i = i + 1.
   END.
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
   /*
   CREATE tidut.                     
   SUBSTRING(tidut.UT,1) = str.
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "ENR"
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "BENÄMNING"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "PRIS"
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "ENHET"
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "ANTAL"
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "SUMMA"
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "FACKID".
   CREATE tidut.   
   
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "------------"
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "------------------------------------"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "-----------"
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "------"   
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "------"
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "--------"
   SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "----------".*/
    
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
         FIND levtemp WHERE levtemp.LEVKOD = vald_lev NO-LOCK NO-ERROR.           
         CREATE tidut. 
         ASSIGN        
         SUBSTRING(tidut.UT,25) = "Beställning nr:" 
         SUBSTRING(tidut.UT,40) = STRING(nytt_bestnr)         
         SUBSTRING(tidut.UT,60) = STRING(TODAY)
         SUBSTRING(tidut.UT,70) = STRING(TIME,"HH:MM:SS").
         
         IF Guru.Konstanter:globforetag = "borl" OR Guru.Konstanter:globforetag = "Celpa" THEN DO:
            SUBSTRING(tidut.UT,40) = STRING(nytt_bestnr) + "-" + levtemp.LEVKOD.
            rbrik = "Beställning nr:" +  STRING(nytt_bestnr) + "-" + levtemp.LEVKOD + " från Borlänge".
         END.         
         
         CREATE tidut.  
         CREATE tidut.  
         CREATE tidut.    
         SUBSTRING(tidut.UT,1) = STRING(levtemp.LEVNAMN).
         FIND FIRST skapa_mtrl NO-LOCK NO-ERROR.
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = STRING(skapa_mtrl.LADR).   
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = STRING(skapa_mtrl.LPNR)   
         SUBSTRING(tidut.UT,8) = STRING(skapa_mtrl.LORT).  
         CREATE tidut. 
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = STRING(skapa_mtrl.LKONTAKT).
         CREATE tidut.
         ASSIGN                                       
         SUBSTRING(tidut.UT,1) = "Tele:"
         SUBSTRING(tidut.UT,6) = skapa_mtrl.LTELE.                                                                    
         CREATE tidut. 

         CREATE tidut.
         SUBSTRING(tidut.UT,1) = str2.
         CREATE tidut.
         CREATE tidut.
         ASSIGN                                       
         SUBSTRING(tidut.UT,1) = "Kundnummer:"
         SUBSTRING(tidut.UT,13) = STRING(skapa_mtrl.KUNDNR).
         CREATE tidut.
         CREATE tidut.
         FIND depatemp WHERE depatemp.DEP-NR = vald_depa NO-LOCK NO-ERROR.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Leveransadress:".
         ASSIGN                                                
         SUBSTRING(tidut.UT,40) = "Faktureringsadress:".                
         CREATE tidut. 
         CREATE tidut.
         IF skapa_mtrl.L1 = " " THEN DO:
            ASSIGN     
            SUBSTRING(tidut.UT,1) = "Se adress!".            
            ASSIGN SUBSTRING(tidut.UT,40) = depatemp.FIRMA.            
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,40) = depatemp.FAKADRESS.            
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,40) = depatemp.FAKPNR
            SUBSTRING(tidut.UT,48) = depatemp.FAKORT.            
            CREATE tidut.               
         END.
         ELSE DO:
            ASSIGN     
            SUBSTRING(tidut.UT,1) = STRING(skapa_mtrl.FORE)
            SUBSTRING(tidut.UT,40) = depatemp.FIRMA.
            CREATE tidut.             
            ASSIGN
            SUBSTRING(tidut.UT,1) = STRING(skapa_mtrl.L1).            
            ASSIGN
            SUBSTRING(tidut.UT,40) = depatemp.FAKADRESS.                        
            CREATE tidut.
            ASSIGN    
            SUBSTRING(tidut.UT,1) = STRING(skapa_mtrl.L2)   
            SUBSTRING(tidut.UT,8) = STRING(skapa_mtrl.L3)
            SUBSTRING(tidut.UT,40) = depatemp.FAKPNR
            SUBSTRING(tidut.UT,48) = depatemp.FAKORT.                     
            CREATE tidut.
         END. 
         CREATE tidut.           
         ASSIGN       
         SUBSTRING(tidut.UT,1) = "Leveransdag:" + STRING(skapa_mtrl.DATUM).                       
         IF skapa_mtrl.AVIS = TRUE THEN DO:
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = "Avisering:" + STRING(skapa_mtrl.AVISPERS).            
         END.                     
         CREATE tidut.         
         RUN sidfot_UI.     
         CREATE tidut.
         CREATE tidut.        
         ASSIGN                                                
         SUBSTRING(tidut.UT,40) = "Kontaktperson inköp:".        
         CREATE tidut.
         SUBSTRING(tidut.UT,40) = STRING(skapa_mtrl.KIKONTAKT).
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,40) = "Tele:"
         SUBSTRING(tidut.UT,46) = STRING(skapa_mtrl.KITELE)
         SUBSTRING(tidut.UT,60) = "Fax:"
         SUBSTRING(tidut.UT,65) = STRING(skapa_mtrl.FAX). 
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,40) = "Mobiltele:"
         SUBSTRING(tidut.UT,50) = STRING(skapa_mtrl.KIMOBIL).            
         IF STRING(skapa_mtrl.KTKONTAKT) NE "" THEN DO:         
            CREATE tidut.         
            CREATE tidut.
            ASSIGN                                                
            SUBSTRING(tidut.UT,40) = "Kontaktperson teknik:".         
            CREATE tidut.
            SUBSTRING(tidut.UT,40) = STRING(skapa_mtrl.KTKONTAKT).          
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,40) = "Tele:"
            SUBSTRING(tidut.UT,46) = STRING(skapa_mtrl.KTTELE).       
            CREATE tidut.         
         END.         
         CREATE tidut.         
      END.
   END.                    
   RUN huvud2_UI.

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
   CREATE tidut. 
   FOR EACH spec_mtrl WHERE spec_mtrl.LEVKOD = vald_lev.       
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = spec_mtrl.ENR
      SUBSTRING(tidut.UT,utnr[nrcol[2]]) = SUBSTRING(spec_mtrl.BENAMNING,1,35)                           
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = spec_mtrl.ENHET    
      SUBSTRING(tidut.UT,utnr[nrcol[4]]) = STRING(spec_mtrl.BERKVANT,">>>>9").
      IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" OR 
      Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "KRAF" THEN musz = musz.
      ELSE DO:            
         ASSIGN SUBSTRING(tidut.UT,utnr[nrcol[5]]) = spec_mtrl.FACKID.
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
   CREATE tidut. 
   FOR EACH spec_mtrl WHERE spec_mtrl.LEVKOD = vald_lev.
      spec_mtrl.SUMMA = spec_mtrl.NPRIS * spec_mtrl.BERKVANT.      
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = spec_mtrl.ENR
      SUBSTRING(tidut.UT,utnr[nrcol[2]]) = SUBSTRING(spec_mtrl.BENAMNING,1,35)                     
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(spec_mtrl.NPRIS,">>>>99.99")
      SUBSTRING(tidut.UT,utnr[nrcol[4]]) = spec_mtrl.ENHET    
      SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(spec_mtrl.BERKVANT,">>>>9")
      SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(spec_mtrl.SUMMA,">>>>>9").
      IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" OR 
      Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "KRAF" THEN musz = musz.
      ELSE DO:            
         ASSIGN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = spec_mtrl.FACKID.
      END.
   END.                                            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rubrik2_UI WINDOW-2 
PROCEDURE rubrik2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   CREATE tidut.                     
   SUBSTRING(tidut.UT,1) = str.
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "ENR"
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "BENÄMNING"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "ENHET"
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "ANTAL".
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" OR 
   Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "KRAF" THEN musz = musz.
   ELSE ASSIGN SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "FACKID".                         
   
   CREATE tidut.   
   CREATE tidut.                     
   SUBSTRING(tidut.UT,1) = str.
   
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
   CREATE tidut.                     
   SUBSTRING(tidut.UT,1) = str.
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "ENR"
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "BENÄMNING"
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "PRIS"
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "ENHET"
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "ANTAL"
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "SUMMA".
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" OR 
   Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "KRAF" THEN musz = musz.
   ELSE ASSIGN SUBSTRING(tidut.UT,utnr[nrcol[7]]) = "FACKID".                         
   
   CREATE tidut.   
   CREATE tidut.                     
   SUBSTRING(tidut.UT,1) = str.
 
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
   IF skapa_mtrl.MARK NE "" THEN DO:   
      CREATE tidut.                                   
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Märkning:" + STRING(skapa_mtrl.MARK).
   END.
   IF skapa_mtrl.KOM NE "" THEN DO:   
      CREATE tidut.                                    
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Kommentarer:" + STRING(skapa_mtrl.KOM).   
   END.
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
  /*SUMMERING AV ALLA ENR*/
   ASSIGN
   totalt = 0 
   totalt2 = 0.  
   FOR EACH spec_mtrl WHERE spec_mtrl.LEVKOD = vald_lev. 
      totalt = totalt + spec_mtrl.SUMMA.      
   END. 
   /*str=
"======================================================================.=======.========".      */
   CREATE tidut. 
   CREATE tidut.      
   SUBSTRING(tidut.UT,1) = str.
   CREATE tidut.
   CREATE tidut.         
   SUBSTRING(tidut.UT,58) = "TOTAL SUMMA:".   
   CREATE tidut.         
   SUBSTRING(tidut.UT,72) = STRING(totalt,">>>>>9").
   /*CREATE tidut.         
   SUBSTRING(tidut.UT,72) = "=======".               */

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

