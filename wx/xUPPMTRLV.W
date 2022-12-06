&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 10/15/96 -  1:50 pm

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
&Scoped-define NEW NEW 
&Scoped-define SHARED SHARED
DEFINE VARIABLE framesizeh AS HANDLE NO-UNDO.

/* DEFINE VARIABLE framesizedh AS HANDLE NO-UNDO. */
DEFINE NEW SHARED VARIABLE tthandle AS HANDLE NO-UNDO.
/*DEFINE SHARED VARIABLE hpApi AS HANDLE NO-UNDO.*/
DEFINE SHARED VARIABLE hpWinFunc AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE retvalkoll AS LOGICAL NO-UNDO.
DEFINE VARIABLE hcur AS INTEGER NO-UNDO.
DEFINE VARIABLE retval AS INTEGER NO-UNDO.
DEFINE VARIABLE brwproc AS HANDLE EXTENT 25 NO-UNDO.
DEFINE VARIABLE handproc AS HANDLE EXTENT 25 NO-UNDO.
DEFINE VARIABLE apphandass AS HANDLE NO-UNDO.
DEFINE VARIABLE apphandklar AS HANDLE NO-UNDO.
DEFINE VARIABLE appprogok AS LOGICAL NO-UNDO.
&GLOBAL-DEFINE LEFT-BROWSE 1
&GLOBAL-DEFINE RIGHT-BROWSE 2
&GLOBAL-DEFINE ARROWS 3

DEFINE VARIABLE ReturnValue AS INTEGER.

{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE mtrl_rowid AS ROWID NO-UNDO.

/*DEFINE NEW SHARED VARIABLE tth AS HANDLE NO-UNDO.*/
DEFINE VARIABLE sok AS LOGICAL NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE numrows AS INTEGER NO-UNDO.
DEFINE VARIABLE brwsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE multitid AS INTEGER NO-UNDO.
/*DEFINE VARIABLE multibrwsok AS CHARACTER NO-UNDO.*/
DEFINE VARIABLE anyprintquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE forstaenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE delquery1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE delquery2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE orgfraga AS CHARACTER NO-UNDO.
/*DEFINE VARIABLE rowvar AS ROWID NO-UNDO.*/
DEFINE VARIABLE kollenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE sprow AS ROWID      NO-UNDO.
DEFINE VARIABLE uppdat AS LOGICAL NO-UNDO.
/*{EGENBEN.I}*/
{DEFSOK.I}
{TRIGGINC.I}
&Scoped-define NEW NEW 
&Scoped-define SHARED SHARED
{MTRLTEMP.I}
{SOKDEF.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_HLEV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mtrltemp

/* Definitions for BROWSE BRW_HLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV 
&Scoped-define QUERY-STRING-BRW_HLEV FOR EACH mtrltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_HLEV OPEN QUERY BRW_HLEV FOR EACH mtrltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV mtrltemp


/* Definitions for BROWSE BRW_VLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.BPRIS mtrltemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VLEV mtrltemp.Enr ~
mtrltemp.Benamning mtrltemp.Enhet mtrltemp.BPRIS mtrltemp.NPRIS 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VLEV mtrltemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VLEV mtrltemp
&Scoped-define QUERY-STRING-BRW_VLEV FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = leverant ~
 AND mtrltemp.KALKNR = 0 NO-LOCK ~
    BY mtrltemp.Enr
&Scoped-define OPEN-QUERY-BRW_VLEV OPEN QUERY BRW_VLEV FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = leverant ~
 AND mtrltemp.KALKNR = 0 NO-LOCK ~
    BY mtrltemp.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_VLEV mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VLEV mtrltemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_VLEV 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BRW_HLEV-2 
       MENU-ITEM m_Visa_informationHLEV-2 LABEL "Visa information".


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_HLEV FOR 
      mtrltemp SCROLLING.

DEFINE QUERY BRW_VLEV FOR 
      mtrltemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV WINDOW-1 _STRUCTURED
  QUERY BRW_HLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(256)":U WIDTH 25
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 53.5 BY 10.33
         FONT 6
         TITLE "Materiel".

DEFINE BROWSE BRW_VLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VLEV WINDOW-1 _STRUCTURED
  QUERY BRW_VLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(40)":U
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.BPRIS FORMAT ">>>>99.99":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
  ENABLE
      mtrltemp.Enr
      mtrltemp.Benamning
      mtrltemp.Enhet
      mtrltemp.BPRIS
      mtrltemp.NPRIS
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 87.5 BY 11.92
         TITLE "Materiel för vald leverantör".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_HLEV AT ROW 1.25 COL 8
     BRW_VLEV AT ROW 13 COL 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110.13 BY 26.17.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Uppdatera materielkatalog"
         HEIGHT             = 26.17
         WIDTH              = 111.38
         MAX-HEIGHT         = 30.04
         MAX-WIDTH          = 128
         VIRTUAL-HEIGHT     = 30.04
         VIRTUAL-WIDTH      = 128
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* BROWSE-TAB BRW_HLEV 1 FRAME-A */
/* BROWSE-TAB BRW_VLEV BRW_HLEV FRAME-A */
/* SETTINGS FOR BROWSE BRW_HLEV IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BRW_HLEV:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_HLEV:POPUP-MENU IN FRAME FRAME-A             = MENU POPUP-MENU-BRW_HLEV-2:HANDLE
       BRW_HLEV:MAX-DATA-GUESS IN FRAME FRAME-A         = 1000
       BRW_HLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_HLEV:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BRW_VLEV:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_VLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEV
/* Query rebuild information for BROWSE BRW_HLEV
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.mtrltemp.Enr
     _FldNameList[2]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _FldNameList[3]   = Temp-Tables.mtrltemp.Enhet
     _FldNameList[4]   = Temp-Tables.mtrltemp.NPRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VLEV
/* Query rebuild information for BROWSE BRW_VLEV
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.mtrltemp.Enr|yes"
     _Where[1]         = "Temp-Tables.mtrltemp.LEVKOD = leverant
 AND Temp-Tables.mtrltemp.KALKNR = 0"
     _FldNameList[1]   > Temp-Tables.mtrltemp.Enr
"mtrltemp.Enr" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.mtrltemp.Enhet
"mtrltemp.Enhet" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.mtrltemp.BPRIS
"mtrltemp.BPRIS" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.mtrltemp.NPRIS
"mtrltemp.NPRIS" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VLEV */
&ANALYZE-RESUME

 

&Scoped-define BROWSE-NAME BRW_HLEV

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
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
   {ALLSTARTDYN.I}   
   RUN hmtmtrl_UI.
   RUN initbrowsers_UI.
   RUN enable_UI.   
   ENABLE brw_hlev WITH FRAME  {&FRAME-NAME}. 
      
   {musarrow.i}      
   {&WINDOW-NAME}:HIDDEN = FALSE.
{&WINDOW-NAME}:MOVE-TO-TOP ().
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initbrowsers_UI WINDOW-3 
PROCEDURE initbrowsers_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   
   
         EMPTY TEMP-TABLE mtrltemp NO-ERROR. 
         EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
         orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(1) + "' USE-INDEX ENR".
         CREATE valsoktemp.
         ASSIGN 
         valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/
         valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
         valsoktemp.SOKCHAR[3] = "mtrltemp" /*temptabell*/
         valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
         valsoktemp.SOKCHAR[5] = "ENR"      /*temptabells unika sökfaltnamn om flera skriv "aonr,delnr"*/
         valsoktemp.SOKINT[1] = 100         /*Hur många poster som ska laddas varje gång*/
         specbrw = 2.                      /*browser nummer allstart_ui*/
         tthandle = TEMP-TABLE mtrltemp:HANDLE.
         RUN nyladdabrw_UI IN brwproc[2] (INPUT TABLE valsoktemp).         
   RUN settriggerproc_UI IN brwproc[2] (INPUT 2,INPUT "anyprintable_UI").
   RUN settriggerproc_UI IN brwproc[2] (INPUT 4,INPUT "offend_UI").
   RUN settriggerproc_UI IN brwproc[2] (INPUT 5,INPUT "offhome_UI").
   RUN settriggerproc_UI IN brwproc[2] (INPUT 6,INPUT "end_UI").
   RUN settriggerproc_UI IN brwproc[2] (INPUT 7,INPUT "home_UI").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/       
   /*RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_VLEV:HANDLE IN FRAME {&FRAME-NAME}).*/
   RUN xDYNLADDBRW.P PERSISTENT SET brwproc[1]
         (INPUT BRW_VLEV:HANDLE IN FRAME {&FRAME-NAME}).       
   RUN xDYNLADDBRW.P PERSISTENT SET brwproc[2]
         (INPUT BRW_hLEV:HANDLE IN FRAME {&FRAME-NAME}).       
   RUN setdefaultcol_UI IN brwproc[1] (INPUT 1).
  RUN setcolindex_UI IN brwproc[2] (INPUT "LEVKOD by enr"). 
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
  THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1  _DEFAULT-ENABLE
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
  ENABLE BRW_VLEV 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hmtmtrl_UI WINDOW-1 
PROCEDURE hmtmtrl_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/     
   ASSIGN numrows = 100.
   /*Ladda mtrl för brw_hlev*/
   orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(1) + "' USE-INDEX ENR ".
   EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
   EMPTY TEMP-TABLE mtrltemp NO-ERROR. 
   CREATE valsoktemp.
   ASSIGN 
   valsoktemp.SOKCHAR[1] = "MTRL"
   valsoktemp.SOKCHAR[2] = orgfraga
   valsoktemp.SOKCHAR[3] = "mtrltemp"
   valsoktemp.SOKCHAR[4] = "MTRLROW"
   valsoktemp.SOKCHAR[5] = "ENR"      
   valsoktemp.SOKINT[1] = numrows   
   specbrw = 1.
   tthandle = TEMP-TABLE mtrltemp:HANDLE.
   RUN nyladdabrw_UI IN brwproc[specbrw] (INPUT TABLE valsoktemp).      
   RUN settriggerproc_UI IN brwproc[specbrw] (INPUT 2,INPUT "anyprintable_UI").
   RUN settriggerproc_UI IN brwproc[specbrw] (INPUT 4,INPUT "offend_UI").
   RUN settriggerproc_UI IN brwproc[specbrw] (INPUT 5,INPUT "offhome_UI").
   RUN settriggerproc_UI IN brwproc[specbrw] (INPUT 6,INPUT "end_UI").
   RUN settriggerproc_UI IN brwproc[specbrw] (INPUT 7,INPUT "home_UI").
    
   FIND FIRST mtrltemp WHERE mtrltemp.LEVKOD = string(1) AND mtrltemp.KALKNR = 0 NO-LOCK NO-ERROR.                   
   RUN setcolsortvar_UI IN brwproc[specbrw] (INPUT " WHERE KALKNR = 0 AND LEVKOD = '" + STRING(1) + "' USE-INDEX ENR ").
   RUN openbdynspec_UI IN brwproc[specbrw].
   IF AVAILABLE mtrltemp THEN DO:
      RUN setlastrowid_UI IN brwproc[specbrw] (INPUT ROWID(mtrltemp)).
      RUN lastselectdyn_UI IN brwproc[specbrw].
   END.   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI WINDOW-1 
PROCEDURE visa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   mtrltemp.BENAMNING:FORMAT IN BROWSE BRW_VLEV = "X(40)".
   DISPLAY
   mtrltemp.ENR  mtrltemp.BENAMNING
   mtrltemp.ENHET mtrltemp.BPRIS mtrltemp.NPRIS WITH BROWSE BRW_VLEV.   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

