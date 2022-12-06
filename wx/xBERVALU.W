&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-3


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-3 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 01/28/97 -  3:43 pm

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
/*ALLDEF.I*/


/* DEFINE VARIABLE framesizedh AS HANDLE NO-UNDO. */
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
DEFINE NEW SHARED VARIABLE tthandle AS HANDLE NO-UNDO.
DEFINE VARIABLE ReturnValue AS INTEGER.
DEFINE TEMP-TABLE valsoktemp NO-UNDO
   FIELD SOKVAL AS INTEGER 
   FIELD SOKCHAR AS CHARACTER EXTENT 10
   FIELD SOKDATE AS DATE EXTENT 10
   FIELD SOKINT AS INTEGER EXTENT 10
   FIELD SOKDECI AS DECIMAL EXTENT 10
   FIELD SOKLOG AS LOGICAL EXTENT 10.

&Scoped-define NEW NEW 

DEFINE NEW SHARED VARIABLE appcon AS LOGICAL NO-UNDO.


DEFINE {&NEW} SHARED VARIABLE varforetypval AS INTEGER EXTENT 100 NO-UNDO.     
DEFINE {&NEW} SHARED VARIABLE varforetypchar AS CHARACTER EXTENT 100 NO-UNDO.   
DEFINE {&NEW} SHARED TEMP-TABLE felmeddtemp 
  FIELD FELMEDD AS CHARACTER
  FIELD VAL AS INTEGER.


&Scoped-define SHARED SHARED
DEFINE {&NEW} {&SHARED} TEMP-TABLE mtrltemp NO-UNDO
   FIELD Enr     AS  CHARACTER  FORMAT "x(11)" LABEL "Enr"                         
   FIELD Benamning     AS CHARACTER FORMAT "x(40)" LABEL "Benämning"                            
   FIELD Bestkvant     AS  DECIMAL FORMAT "->>,>>9.99" INITIAL 0                          
   FIELD Enhet AS  CHARACTER    FORMAT "x(5)"                    
   FIELD BERKVANT    AS  DECIMAL FORMAT "->>,>>9.99" INITIAL 0
   FIELD SUMMA     AS  DECIMAL FORMAT ">>>>99.99" INITIAL 0
   FIELD LEVKOD     AS  CHARACTER FORMAT "x(4)"
   FIELD BPRIS     AS  DECIMAL FORMAT ">>>>99.99" INITIAL 0 LABEL "Bruttopris"
   FIELD NPRIS     AS  DECIMAL FORMAT ">>>>99.99" INITIAL 0 LABEL "Nettopris"
   FIELD KALKNR    AS  INTEGER FORMAT ">,>>>,>>9" INITIAL 0
   FIELD BERLEV     AS  CHARACTER  FORMAT "x(4)"                           
   FIELD OFFERT     AS  LOGICAL FORMAT "Ja/Nej" INITIAL NO
   FIELD KPRIS     AS  DECIMAL   FORMAT ">>>>99.99" INITIAL 0
   FIELD KUND     AS  LOGICAL  FORMAT "Ja/Nej" INITIAL NO
   FIELD MTRLROW AS ROWID
   INDEX BEN LEVKOD KALKNR BENAMNING
   INDEX Enr IS PRIMARY enr
   INDEX Lev LEVKOD KALKNR enr.

&Scoped-define NEW NEW
&Scoped-define SHARED SHARED




DEFINE VARIABLE brwsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE multitid AS INTEGER NO-UNDO.
DEFINE VARIABLE forstaenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE orgfraga AS CHARACTER NO-UNDO.
DEFINE VARIABLE setcolvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE spar_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE multibrwsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE specbrw AS INTEGER NO-UNDO.

PROCEDURE offend_UI :
   RUN hmtnasta_UI IN brwproc[specbrw].
END PROCEDURE.

PROCEDURE offhome_UI :
   RUN hmtforegaende_UI IN brwproc[specbrw].
END PROCEDURE.

PROCEDURE end_UI :
   RUN hmtsista_UI IN brwproc[specbrw].  
END PROCEDURE.

PROCEDURE home_UI :
   RUN hmtforsta_UI IN brwproc[specbrw].         
END PROCEDURE.

PROCEDURE anyprintable_UI :
   brwsok = LAST-EVENT:LABEL.
   IF ETIME - multitid <= 500 THEN DO:
      multibrwsok = multibrwsok + brwsok.
   END.
   ELSE multibrwsok = brwsok.
   RUN anyhmt_UI IN brwproc[specbrw] (INPUT multibrwsok).
   brwsok = "".
   multitid = ETIME.
END PROCEDURE.




/*MTRL*/
DEFINE NEW SHARED VARIABLE mtrl_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
/*MTRL*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-C
&Scoped-define BROWSE-NAME BRW_HLEV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mtrltemp

/* Definitions for BROWSE BRW_HLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.NPRIS mtrltemp.LEVKOD mtrltemp.KALKNR 
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


/* Definitions for FRAME FRAME-MTRL                                     */

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-3 AS WIDGET-HANDLE NO-UNDO.

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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV WINDOW-3 _STRUCTURED
  QUERY BRW_HLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(256)":U WIDTH 25
      mtrltemp.Enhet FORMAT "x(5)":U WIDTH 4.5
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
      mtrltemp.LEVKOD FORMAT "X(4)":U
      mtrltemp.KALKNR FORMAT ">,>>>,>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 105.5 BY 10.33
         FONT 6
         TITLE "Materiel".

DEFINE BROWSE BRW_VLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VLEV WINDOW-3 _STRUCTURED
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

DEFINE FRAME FRAME-C
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.63 BY 28.25.

DEFINE FRAME FRAME-MTRL
     BRW_VLEV AT ROW 1 COL 5.5
     BRW_HLEV AT ROW 14.75 COL 13
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4.5 ROW 2
         SIZE 120.5 BY 27.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: berfortemp T "?" NO-UNDO temp-db berfortemp
      TABLE: berlinjetemp T "?" NO-UNDO temp-db berlinjetemp
      TABLE: bernattemp T "?" NO-UNDO temp-db bernattemp
      TABLE: berstolptemp T "?" NO-UNDO temp-db berstolptemp
      TABLE: fastanmtemp T "?" NO-UNDO temp-db fastanmtemp
      TABLE: konstgrptemp T "?" NO-UNDO temp-db konstgrptemp
      TABLE: konsttemp T "?" NO-UNDO temp-db konsttemp
      TABLE: konstvaltemp T "?" NO-UNDO temp-db konstvaltemp
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
      TABLE: satstemp T "?" NO-UNDO temp-db satstemp
      TABLE: ? T "?" NO-UNDO temp-db kalk_temp
      TABLE: ? T "?" NO-UNDO temp-db list_mtrl
      TABLE: ? T "?" NO-UNDO temp-db kon_val
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-3 ASSIGN
         HIDDEN             = YES
         TITLE              = "Window 3"
         HEIGHT             = 28.42
         WIDTH              = 124.88
         MAX-HEIGHT         = 32.42
         MAX-WIDTH          = 127
         VIRTUAL-HEIGHT     = 32.42
         VIRTUAL-WIDTH      = 127
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
/* SETTINGS FOR WINDOW WINDOW-3
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-MTRL:FRAME = FRAME FRAME-C:HANDLE.

/* SETTINGS FOR FRAME FRAME-C
                                                                        */
/* SETTINGS FOR FRAME FRAME-MTRL
                                                                        */
/* BROWSE-TAB BRW_VLEV 1 FRAME-MTRL */
/* BROWSE-TAB BRW_HLEV BRW_VLEV FRAME-MTRL */
/* SETTINGS FOR BROWSE BRW_HLEV IN FRAME FRAME-MTRL
   NO-ENABLE                                                            */
ASSIGN 
       BRW_HLEV:HIDDEN  IN FRAME FRAME-MTRL                = TRUE
       BRW_HLEV:POPUP-MENU IN FRAME FRAME-MTRL             = MENU POPUP-MENU-BRW_HLEV-2:HANDLE
       BRW_HLEV:MAX-DATA-GUESS IN FRAME FRAME-MTRL         = 1000
       BRW_HLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-MTRL = TRUE
       BRW_HLEV:COLUMN-RESIZABLE IN FRAME FRAME-MTRL       = TRUE.

ASSIGN 
       BRW_VLEV:MAX-DATA-GUESS IN FRAME FRAME-MTRL         = 40000
       BRW_VLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-MTRL = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
THEN WINDOW-3:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEV
/* Query rebuild information for BROWSE BRW_HLEV
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.mtrltemp.Enr
     _FldNameList[2]   > Temp-Tables.mtrltemp.Benamning
"Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.mtrltemp.Enhet
"Enhet" ? ? "character" ? ? ? ? ? ? no ? no no "4.5" yes no no "U" "" ""
     _FldNameList[4]   = Temp-Tables.mtrltemp.NPRIS
     _FldNameList[5]   = Temp-Tables.mtrltemp.LEVKOD
     _FldNameList[6]   = Temp-Tables.mtrltemp.KALKNR
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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-C
/* Query rebuild information for FRAME FRAME-C
     _Query            is NOT OPENED
*/  /* FRAME FRAME-C */
&ANALYZE-RESUME

 

&Scoped-define BROWSE-NAME BRW_HLEV

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-3 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   
END.                                       
   

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:  
   &Scoped-define FRAME-NAME FRAME-C
          
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
   
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:  

   
   RUN allstartbrw_UI.
/*   RUN hmtmtrl_UI.*/

   
    
   RUN enable_UI. 
   FRAME FRAME-MTRL:HIDDEN = FALSE.
   ENABLE brw_hlev WITH FRAME frame-mtrl.
   {&WINDOW-NAME}:HIDDEN = FALSE.
   {&WINDOW-NAME}:MOVE-TO-TOP ().
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-3 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/ 
   RUN xDYNLADDBRW.P PERSISTENT SET brwproc[1]
        (INPUT BRW_VLEV:HANDLE IN FRAME FRAME-MTRL).       
  RUN setdefaultcol_UI IN brwproc[1] (INPUT 1).

   RUN xDYNLADDBRW.P PERSISTENT SET brwproc[22]
         (INPUT BRW_HLEV:HANDLE IN FRAME FRAME-MTRL).       
  /* RUN setcolindex_UI IN brwproc[22] (INPUT "LEVKOD").*/
   RUN initbrowsers_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-3  _DEFAULT-ENABLE
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
  VIEW FRAME FRAME-C IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-C}
  ENABLE BRW_VLEV 
      WITH FRAME FRAME-MTRL IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-MTRL}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hmtmtrl_UI WINDOW-3 
PROCEDURE hmtmtrl_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/     
   
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
   valsoktemp.SOKINT[1] = 100   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initbrowsers_UI WINDOW-3 
PROCEDURE initbrowsers_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   &Scoped-define FRAME-NAME FRAME-MTRL
   EMPTY TEMP-TABLE mtrltemp NO-ERROR. 
         EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
         orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(1) + " ' USE-INDEX lev ".
         CREATE valsoktemp.
         ASSIGN 
         valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/
         valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
         valsoktemp.SOKCHAR[3] = "mtrltemp" /*temptabell*/
         valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
         valsoktemp.SOKCHAR[5] = "ENR"      /*temptabells unika sökfaltnamn om flera skriv "aonr,delnr"*/
         valsoktemp.SOKINT[1] = 100         /*Hur många poster som ska laddas varje gång*/
         specbrw = 22.                      /*browser nummer allstart_ui*/
         tthandle = TEMP-TABLE mtrltemp:HANDLE.
         RUN nyladdabrw_UI IN brwproc[specbrw] (INPUT TABLE valsoktemp).         
   RUN settriggerproc_UI IN brwproc[specbrw] (INPUT 2,INPUT "anyprintable_UI").
   RUN settriggerproc_UI IN brwproc[specbrw] (INPUT 4,INPUT "offend_UI").
   RUN settriggerproc_UI IN brwproc[specbrw] (INPUT 5,INPUT "offhome_UI").
   RUN settriggerproc_UI IN brwproc[specbrw] (INPUT 6,INPUT "end_UI").
   RUN settriggerproc_UI IN brwproc[specbrw] (INPUT 7,INPUT "home_UI").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

