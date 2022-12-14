&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          RT               PROGRESS
*/
&Scoped-define WINDOW-NAME    WINDOW-2
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
&Scoped-define NEW                                                         
DEFINE SHARED VARIABLE prisvar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO. 
DEFINE SHARED VARIABLE valaonr LIKE AONRTAB.AONR NO-UNDO.
DEFINE SHARED VARIABLE valdelnr LIKE AONRTAB.DELNR NO-UNDO.
DEFINE SHARED VARIABLE valort LIKE AONRTAB.ORT NO-UNDO. 
DEFINE SHARED VARIABLE valomrade LIKE AONRTAB.OMRADE NO-UNDO.  
DEFINE SHARED VARIABLE lin_recid AS RECID NO-UNDO.   
DEFINE VARIABLE arrhjsum LIKE TIDREGITAB.BERANTAL NO-UNDO.    
DEFINE VARIABLE str AS CHARACTER FORMAT "X(130)" NO-UNDO. 
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(130)" NO-UNDO.
DEFINE VARIABLE totalt LIKE MTRL.NPRIS NO-UNDO. 
DEFINE VARIABLE sumpris LIKE BERMTRL.PRIS NO-UNDO.
DEFINE VARIABLE sumantal LIKE BERMTRL.ANTAL NO-UNDO.
   
DEFINE NEW SHARED TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)".
   
DEFINE NEW SHARED TEMP-TABLE mtrl_temp 
   FIELD NUM LIKE BERMTRL.NUM  
   FIELD ENR LIKE MTRLBER.ENR
   FIELD BENAMNING LIKE MTRLBER.BENAMNING
   FIELD ENHET LIKE MTRLBER.ENHET
   FIELD ANTAL LIKE MTRLBER.ANTAL
   FIELD PRIS LIKE MTRLBER.PRIS      
   FIELD TOTPRIS LIKE MTRLBER.PRIS
   FIELD LEVKOD LIKE MTRLBER.LEVKOD
   FIELD UPPLAG LIKE BERVAL.UPPLAG      
   FIELD GRUPP LIKE BERVAL.KONSKOD 
   FIELD XKORD LIKE BERID.XKORD
   FIELD FORNR LIKE BERID.FORNR
   FIELD LINNR LIKE BERID.LINNR
   FIELD NATNR LIKE BERID.NATNR
   FIELD FRI1 LIKE BERID.FRI1
   FIELD FRI2 LIKE BERID.FRI2
   INDEX ENR IS PRIMARY ENR ASCENDING
   INDEX NUM NUM ASCENDING.               
                             
DEFINE NEW SHARED TEMP-TABLE lin_upp   
   FIELD METER LIKE BERLINKAB.METER      
   FIELD ENR LIKE BERLINKAB.ENR         
   FIELD BENAMNING LIKE BERLINKAB.BENAMNING 
   FIELD PRIS LIKE BERLINKAB.PRIS
   FIELD ENHET LIKE BERLINKAB.ENHET   
   FIELD TOTMETER LIKE BERLINKAB.TOTMETER
   FIELD UPPLAG LIKE BERLINKAB.UPPLAG 
   FIELD LEVKOD LIKE BERLINKAB.LEVKOD
   FIELD TOTPRIS LIKE BERMTRL.PRIS         
   INDEX ENR ENR ASCENDING.               
   
DEFINE TEMP-TABLE mtrl_temp2   
   FIELD ENR LIKE MTRLBER.ENR
   FIELD BENAMNING LIKE MTRLBER.BENAMNING
   FIELD ENHET LIKE MTRLBER.ENHET
   FIELD ANTAL LIKE MTRLBER.ANTAL
   FIELD PRIS LIKE MTRLBER.PRIS 
   FIELD TOTPRIS LIKE MTRLBER.PRIS
   FIELD LEVKOD LIKE MTRLBER.LEVKOD      
   INDEX ENR IS PRIMARY ENR ASCENDING. 
        
DEFINE QUERY mtrlprisq FOR BERMTRL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse (alphabetically)                   */
&Scoped-define FRAME-NAME  FRAME-VINST
&Scoped-define BROWSE-NAME BRW_UT

/* Custom List Definitions                                              */
&Scoped-define LIST-1 
&Scoped-define LIST-2 
&Scoped-define LIST-3 

/* Definitions for BROWSE BRW_UT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_UT tidut.ut 
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut 

/* Definitions for FRAME FRAME-VINST                                    */
&Scoped-define FIELDS-IN-QUERY-FRAME-VINST 
&Scoped-define ENABLED-FIELDS-IN-QUERY-FRAME-VINST 
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-VINST ~
    ~{&OPEN-QUERY-BRW_UT}

/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 12 BY 1.5.

DEFINE RECTANGLE RECT-35
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 68 BY 2.5
     BGCOLOR 8 .


/* Query definitions                                                    */
DEFINE QUERY BRW_UT FOR tidut SCROLLING.

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UT QUERY BRW_UT NO-LOCK DISPLAY 
      tidut.ut FORMAT "X(79)"
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 82 BY 18.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BTN_AVS AT ROW 1.5 COL 26.63
     BTN_SKRIV AT ROW 1.5 COL 45.13
     BRW_UT AT ROW 4.5 COL 2
     RECT-35 AT ROW 1 COL 8.63
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.41
         SIZE 83.75 BY 22.73
         BGCOLOR 8 .

 

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Lista materiel/artikel"
         COLUMN             = 8.88
         ROW                = 1.95
         HEIGHT             = 23.14
         WIDTH              = 83.75
         MAX-HEIGHT         = 25.14
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 25.14
         VIRTUAL-WIDTH      = 100
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-2
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR BROWSE BRW_UT IN FRAME FRAME-VINST
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UT:HIDDEN  IN FRAME FRAME-VINST            = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "rt.tidut"
     _Options          = "NO-LOCK"
     _OrdList          = ""
     _FldNameList[1]   = rt.tidut.ut
     _Query            is OPENED
*/  /* BROWSE BRW_UT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-VINST
/* Query rebuild information for FRAME FRAME-VINST
     _TblList          = ""
     _Options          = "NO-LOCK KEEP-EMPTY"
     _OrdList          = ""
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


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO: 
   RUN SKRIVVAL.W.       
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
   {muswait.i}    
   FOR EACH tidut:
      DELETE tidut.
   END.       
   RUN huvud_UI.   
   IF musz = FALSE THEN RUN rubrik_UI. 
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
   {musarrow.i}                                                
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-2 _DEFAULT-DISABLE
PROCEDURE disable_UI :
/* --------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
   -------------------------------------------------------------------- */
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U THEN DELETE WIDGET WINDOW-2.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-2 _DEFAULT-ENABLE
PROCEDURE enable_UI :
/* --------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
   -------------------------------------------------------------------- */
  ENABLE RECT-35 BTN_AVS BTN_SKRIV 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
  VIEW WINDOW-2.
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
         ASSIGN        
         SUBSTRING(tidut.UT,1) = "LISTA MATERIEL KOPPLAT TILL KONSTRUKTIONER".
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = STRING(TODAY)
         SUBSTRING(tidut.UT,20) = STRING(TIME,"HH:MM:SS").                                                               
         CREATE tidut.     
         CREATE tidut.                       
         ASSIGN  
         SUBSTRING(tidut.UT,1) = "E-NUMMER"
         SUBSTRING(tidut.UT,13) = "BEN?MNING"
         SUBSTRING(tidut.UT,44) = "ENHET".
         IF prisvar = 1 THEN ASSIGN SUBSTRING(tidut.UT,50) = "PRIS/ENHET".         
         CREATE tidut. 
         ASSIGN
          str =
"===========.==============================.=====.==========" 
          str2 =
"===========.==============================.=====".
         IF prisvar = 1 THEN DO:      
            SUBSTRING(tidut.UT,1) = str.                                                                                        
         END.
         ELSE DO:  
            SUBSTRING(tidut.UT,1) = str2.                                                                                        
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
   ASSIGN
   sumpris = 0
   sumantal = 0.  
    
   OPEN QUERY mq FOR EACH MTRLBER NO-LOCK.
   GET FIRST mq NO-LOCK.
   DO WHILE AVAILABLE(MTRLBER):
      FIND FIRST mtrl_temp WHERE mtrl_temp.ENR = MTRLBER.ENR USE-INDEX ENR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE mtrl_temp THEN DO:
         CREATE mtrl_temp.
         ASSIGN
         mtrl_temp.ENR = MTRLBER.ENR
         mtrl_temp.BENAMNING = MTRLBER.BENAMNING
         mtrl_temp.ENHET = MTRLBER.ENHET
         mtrl_temp.PRIS = MTRLBER.PRIS.         
      END.   
      GET NEXT mq NO-LOCK.
   END.   
          
   FOR EACH mtrl_temp USE-INDEX ENR:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = mtrl_temp.ENR              
      SUBSTRING(tidut.UT,13) = SUBSTRING(mtrl_temp.BENAMNING,1,30)     
      SUBSTRING(tidut.UT,44) = mtrl_temp.ENHET. 
      IF prisvar = 1 THEN ASSIGN SUBSTRING(tidut.UT,50) = STRING(mtrl_temp.PRIS).         
   END.     
   CREATE tidut.
   IF prisvar = 1 THEN DO:      
     SUBSTRING(tidut.UT,1) = str.                                                                                        
   END.
   ELSE DO:  
      SUBSTRING(tidut.UT,1) = str2.                                                                                        
   END. 
   CREATE tidut.   
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


&UNDEFINE BROWSE-NAME
&UNDEFINE FRAME-NAME
&UNDEFINE WINDOW-NAME
