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
{SOKMTRL.I}
DEFINE INPUT PARAMETER TABLE FOR skapa_mtrl.
/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
&Scoped-define NEW  
{GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE levapph AS HANDLE NO-UNDO.     /*LEVAPP.P*/
DEFINE SHARED VARIABLE valkalknr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER  NO-UNDO.  
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.        

DEFINE VARIABLE str AS CHARACTER FORMAT "X(86)" NO-UNDO.
DEFINE VARIABLE totalt AS DECIMAL NO-UNDO. 
DEFINE VARIABLE totalt2 AS DECIMAL NO-UNDO. 

DEFINE VARIABLE levvar AS CHARACTER  NO-UNDO.         

   
{TIDUTTTNEW.I}
&Scoped-define SHARED SHARED
{LEVTEMP.I}
/*{EGENBEN.I}*/

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

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18 BY 1
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
      tidut.ut FORMAT "X(85)":U WIDTH 83
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 90.5 BY 26.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.5 COL 1.5
     CMB_LEV AT ROW 27.75 COL 39.38 COLON-ALIGNED NO-LABEL
     BTN_SKRIV AT ROW 27.75 COL 61
     BTN_AVS AT ROW 27.75 COL 78
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 92 BY 28.42.


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
         WIDTH              = 92
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 92
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 92
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
     _FldNameList[1]   > temp-db.tidut.ut
"tidut.ut" ? "X(85)" "character" ? ? ? ? ? ? no ? no no "83" yes no no "U" "" ""
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


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV WINDOW-2
ON VALUE-CHANGED OF CMB_LEV IN FRAME FRAME-VINST
DO:
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   ASSIGN                                               
   CMB_LEV = INPUT CMB_LEV.                
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = CMB_LEV NO-LOCK NO-ERROR.
   vald_lev = levtemp.LEVKOD.
   FIND FIRST skapa_mtrl NO-ERROR.
   ASSIGN
   skapa_mtrl.LEVNAMN = levtemp.LEVNAMN
   skapa_mtrl.LKONTAKT = levtemp.LEVKONTAKT
   skapa_mtrl.LTELE = levtemp.LEVTEL
   skapa_mtrl.LADR = levtemp.LEVADR
   skapa_mtrl.LPNR = levtemp.LEVPNR
   skapa_mtrl.LORT = levtemp.LEVORT.
   str=
"======================================================================================".      
   RUN huvud_UI.
   RUN rubrik_UI.
   IF musz = FALSE THEN RUN klar_UI.   
   RUN totalt_UI. 
   run sidfot_UI.     
   RUN enable_UI.   
   
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
   status-ok = CMB_LEV:DELETE("0"). 
   FIND bmtrl_mtrl WHERE bmtrl_mtrl.LEVKOD = vald_lev NO-LOCK NO-ERROR.
   FIND FIRST levtemp NO-ERROR.
   IF NOT AVAILABLE levtemp THEN DO:
      RUN levhmt_UI IN levapph (OUTPUT TABLE levtemp).             
   END.
   FOR EACH bmtrl_mtrl BREAK BY bmtrl_mtrl.LEVKOD : 
         IF LAST-OF(bmtrl_mtrl.LEVKOD) THEN DO:
            FIND FIRST levtemp WHERE levtemp.LEVKOD = bmtrl_mtrl.LEVKOD NO-LOCK NO-ERROR.
            IF AVAILABLE levtemp THEN DO:
               ASSIGN  
               status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.               
            END.
         END.
      END.                                       
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_lev NO-LOCK NO-ERROR.     
   ASSIGN       
   CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN.
   EMPTY TEMP-TABLE tidut NO-ERROR.    
str=
"==============================================================================".      
   RUN huvud_UI.
   RUN rubrik_UI.
   IF musz = FALSE THEN RUN klar_UI.   
   RUN totalt_UI.
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
   ASSIGN
   Guru.GlobalaVariabler:colrighth = CMB_LEV:HANDLE.      
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
  DISPLAY CMB_LEV 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  ENABLE CMB_LEV BTN_SKRIV BTN_AVS 
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
         FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_lev NO-LOCK NO-ERROR.           
         CREATE tidut. 
         ASSIGN        
         SUBSTRING(tidut.UT,1) = "BESTÄLLNING" 
         SUBSTRING(tidut.UT,40) = STRING(TODAY)
         SUBSTRING(tidut.UT,50) = STRING(TIME,"HH:MM:SS").
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
         SUBSTRING(tidut.UT,1) = "TELE:"
         SUBSTRING(tidut.UT,6) = STRING(skapa_mtrl.LTELE).                                                           
         CREATE tidut. 
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = str.
         CREATE tidut.
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "LEVERANSADRESS:" 
         SUBSTRING(tidut.UT,40) = "FAKTURERINGSADRESS:".
         CREATE tidut. 
         CREATE tidut.
         IF skapa_mtrl.LEVERANS = " " THEN DO:
            ASSIGN     
            SUBSTRING(tidut.UT,1) = "SE FAKTURERINGSSADRESS!"
            SUBSTRING(tidut.UT,40) = STRING(skapa_mtrl.FORE).
            CREATE tidut.
            SUBSTRING(tidut.UT,40) = STRING(skapa_mtrl.KADR).               
         END.
         ELSE DO:
            ASSIGN     
            SUBSTRING(tidut.UT,1) = STRING(skapa_mtrl.FORE)
            SUBSTRING(tidut.UT,40) = STRING(skapa_mtrl.FORE).
            CREATE tidut. 
            ASSIGN
            SUBSTRING(tidut.UT,1) = STRING(skapa_mtrl.LEVERANS)
            SUBSTRING(tidut.UT,40) = STRING(skapa_mtrl.KADR)
            SUBSTRING(tidut.UT,65) = "BOX:"
            SUBSTRING(tidut.UT,70) = STRING(skapa_mtrl.BOX).
         END.
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,40) = STRING(skapa_mtrl.KPNR)   
         SUBSTRING(tidut.UT,48) = STRING(skapa_mtrl.KORT).  
         CREATE tidut.
         CREATE tidut.
         ASSIGN                                                
         SUBSTRING(tidut.UT,40) = "KONTAKTPERSON INKÖP:".        
         CREATE tidut.
         SUBSTRING(tidut.UT,40) = STRING(skapa_mtrl.KIKONTAKT).         
         CREATE tidut.
         IF skapa_mtrl.DATUM NE ? THEN DO:
            ASSIGN       
            SUBSTRING(tidut.UT,1) = "LEVERANSDAG:"
            SUBSTRING(tidut.UT,13) = STRING(skapa_mtrl.DATUM)
            SUBSTRING(tidut.UT,40) = "TELE:"
            SUBSTRING(tidut.UT,46) = STRING(skapa_mtrl.KITELE)                                 
            SUBSTRING(tidut.UT,60) = "FAX:"
            SUBSTRING(tidut.UT,65) = STRING(skapa_mtrl.FAX). 
         END.
         ELSE DO:                                            
            ASSIGN       
            SUBSTRING(tidut.UT,1) = "LEVERANSDAG:"            
            SUBSTRING(tidut.UT,40) = "TELE:"
            SUBSTRING(tidut.UT,46) = STRING(skapa_mtrl.KITELE)                                 
            SUBSTRING(tidut.UT,60) = "FAX:"
            SUBSTRING(tidut.UT,65) = STRING(skapa_mtrl.FAX).
         END.    
         CREATE tidut.
         CREATE tidut.
         ASSIGN                                                
         SUBSTRING(tidut.UT,40) = "KONTAKTPERSON TEKNIK:".         
         CREATE tidut.
         SUBSTRING(tidut.UT,40) = STRING(skapa_mtrl.KTKONTAKT).          
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,40) = "TELE:"
         SUBSTRING(tidut.UT,46) = STRING(skapa_mtrl.KTTELE).       
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
   CREATE tidut. 
   FOR EACH bmtrl_mtrl WHERE bmtrl_mtrl.LEVKOD = vald_lev.      
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,1) = bmtrl_mtrl.ENR
      SUBSTRING(tidut.UT,13) = SUBSTRING(bmtrl_mtrl.BENAMNING,1,25)               
      SUBSTRING(tidut.UT,39) = STRING(bmtrl_mtrl.BPRIS,">>>>99.99") 
      SUBSTRING(tidut.UT,50) = STRING(bmtrl_mtrl.NPRIS,">>>>99.99")
      SUBSTRING(tidut.UT,60) = bmtrl_mtrl.ENHET    
      SUBSTRING(tidut.UT,66) = STRING(bmtrl_mtrl.BERKVANT,">>>>9")
      SUBSTRING(tidut.UT,72) = STRING(bmtrl_mtrl.SUMMA,">>>>99").
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
      ASSIGN
      str=                                                                    
"===========.=========================.==========.=========.=====.=====.=======". 
      CREATE tidut.       
      SUBSTRING(tidut.UT,1) = str.                     
      CREATE tidut.      
      ASSIGN  
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:genk)                                                                          
      SUBSTRING(tidut.UT,13) = "BENÄMNING"                     
      SUBSTRING(tidut.UT,39) = "BRUTTOPRIS"   
      SUBSTRING(tidut.UT,50) = "NETTOPRIS"  
      SUBSTRING(tidut.UT,60) = "ENHET"
      SUBSTRING(tidut.UT,66) = "ANTAL"
      SUBSTRING(tidut.UT,72) = "SUMMA".                         
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
   str=
"==============================================================================". 
   CREATE tidut.         
   SUBSTRING(tidut.UT,1) = str.
   CREATE tidut. 
   CREATE tidut.                                   
   ASSIGN
   SUBSTRING(tidut.UT,1) = "MÄRKNING:"
   SUBSTRING(tidut.UT,10) = STRING(skapa_mtrl.MARK).
   CREATE tidut.                                    
   ASSIGN
   SUBSTRING(tidut.UT,1) = "KOMMENTARER:"
   SUBSTRING(tidut.UT,13) = STRING(skapa_mtrl.KOM).   
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
   FOR EACH bmtrl_mtrl WHERE bmtrl_mtrl.LEVKOD = vald_lev. 
      totalt = totalt + bmtrl_mtrl.SUMMA.      
   END. 
   str=
"======================================================================.=======".      
   CREATE tidut. 
   CREATE tidut.      
   SUBSTRING(tidut.UT,1) = str.
   CREATE tidut.
   CREATE tidut.         
   SUBSTRING(tidut.UT,58) = "TOTAL SUMMA:".
   CREATE tidut.         
   SUBSTRING(tidut.UT,72) = STRING(totalt,">>>>99").
   CREATE tidut.         
   SUBSTRING(tidut.UT,72) = "=======".            
   FOR EACH bmtrl_mtrl WHERE bmtrl_mtrl.LEVKOD = vald_lev. 
      totalt2 = totalt2 + (bmtrl_mtrl.BPRIS * bmtrl_mtrl.BERKVANT).      
   END.  
   CREATE tidut.         
   SUBSTRING(tidut.UT,52) = "TOTALT BRUTTOPRIS:".
   CREATE tidut.         
   SUBSTRING(tidut.UT,72) = STRING(totalt2,">>>>99").
   CREATE tidut.         
   SUBSTRING(tidut.UT,72) = "=======".  
      
   FIND FIRST off_mtrl WHERE off_mtrl.KALKNR = valkalknr AND off_mtrl.LEVKOD = vald_lev
   NO-LOCK NO-ERROR.
   IF AVAILABLE off_mtrl THEN DO: 
      CREATE tidut.         
      SUBSTRING(tidut.UT,52) = "TOTALT OFFERTPRIS:".
      CREATE tidut.         
      SUBSTRING(tidut.UT,72) = STRING(off_mtrl.RABTOT,">>>>99").
      CREATE tidut.         
      SUBSTRING(tidut.UT,72) = "=======".
   END.
   ELSE DO:
      CREATE tidut.         
      SUBSTRING(tidut.UT,52) = "TOTALT OFFERTPRIS:".
      CREATE tidut.  
      SUBSTRING(tidut.UT,72) = STRING(totalt,">>>>99").                         
      CREATE tidut.         
      SUBSTRING(tidut.UT,72) = "=======".
      CREATE tidut.
      CREATE tidut.
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

