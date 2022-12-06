&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WFLEXTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WFLEXTID 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/04/06 -  4:27 pm

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

/*OBS!
ANYWHERE SKALL STÅ EFTER VARJE ON F3 OF FRAME FRAME-A ANYWHERE,
ON F4 OF FRAME FRAME-A ANYWHERE osv
  
ANYWHERE FÖRSVINNER VIA UIB






*/
   
   
DEFINE VARIABLE ocxvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE grundTimer  AS Guru.Timer              NO-UNDO.
DEFINE VARIABLE senasteTimer  AS Guru.Timer2              NO-UNDO.
ocxvar = TRUE.
DEFINE VARIABLE grundata AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chgrundata AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE senastetryck AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chsenastetryck AS COMPONENT-HANDLE NO-UNDO.



&Scoped-define NEW 
/*
DEFINE SHARED VARIABLE alltidmax AS LOGICAL NO-UNDO.
*/
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE flexrec AS RECID NO-UNDO.   
DEFINE NEW SHARED VARIABLE fldrec AS RECID NO-UNDO.  
DEFINE NEW SHARED VARIABLE flexpers AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE aonummer AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE delnummer AS INTEGER NO-UNDO.   
DEFINE NEW SHARED VARIABLE valaonrrec AS RECID NO-UNDO. 
DEFINE NEW SHARED VARIABLE ovut AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE ovkoll AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE regstart AS DECIMAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE regslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE onr AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE onr2 AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE dnr2 AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE dnr AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE tid AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE tra AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE typ AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE tpris AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE tid3 AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE manad AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchstarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE regtotalt AS DECIMAL NO-UNDO.   
DEFINE NEW SHARED VARIABLE regstartsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE regslutsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE bustart3 AS DECIMAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE vaxla AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE perssokrec AS RECID NO-UNDO. 
DEFINE NEW SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE tidtabrec2 AS RECID NO-UNDO.  
DEFINE NEW SHARED VARIABLE tidsedrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE tidsedlog AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE andvisrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE anvrec AS RECID NO-UNDO. 
DEFINE NEW SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE muszval AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE vartgamla AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE andvisvnr AS INTEGER FORMAT "999" NO-UNDO.  
DEFINE NEW SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE printer AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE printer1 AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE bilforare AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.
DEFINE NEW SHARED VARIABLE enflerdygns AS LOGICAL FORMAT "ENDAGS/FLERDYGNS" NO-UNDO.
DEFINE NEW SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE NEW SHARED VARIABLE klocka AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE valkalknr AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE kalkmtrl AS LOGICAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE kalkregvar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE florsak AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE flrec AS RECID NO-UNDO.

DEFINE NEW SHARED VARIABLE arnr AS INTEGER FORMAT "9999" NO-UNDO. 
DEFINE NEW SHARED VARIABLE manadnr AS INTEGER FORMAT "99" NO-UNDO.
DEFINE NEW SHARED VARIABLE manadnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE hamtkoll AS DATE NO-UNDO.
hamtkoll = TODAY.
DEFINE VARIABLE val AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE pkod AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE avb-mus AS LOGICAL NO-UNDO.
DEFINE VARIABLE overant AS INTEGER NO-UNDO.
DEFINE VARIABLE otim AS INTEGER NO-UNDO.
DEFINE VARIABLE otim2 AS INTEGER NO-UNDO.
DEFINE VARIABLE tiden AS INTEGER NO-UNDO.
DEFINE VARIABLE kollen AS INTEGER NO-UNDO.    
DEFINE VARIABLE kollen2 AS INTEGER NO-UNDO.
DEFINE VARIABLE kollen3 AS INTEGER NO-UNDO.
DEFINE VARIABLE flkoll AS INTEGER NO-UNDO.
DEFINE VARIABLE lunchut AS INTEGER NO-UNDO.
DEFINE VARIABLE tid2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE tid4 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE tid5 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE aonr3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dnr3 AS INTEGER NO-UNDO.    
DEFINE VARIABLE pltid AS LOGICAL NO-UNDO.
DEFINE VARIABLE ejtid AS LOGICAL INITIAL FALSE NO-UNDO.
DEFINE VARIABLE plus AS INTEGER NO-UNDO.
DEFINE VARIABLE flhjrec AS RECID NO-UNDO.   
DEFINE VARIABLE flsaldo AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO. 
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE flexkvst AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexkvsl AS DECIMAL NO-UNDO. 
DEFINE VARIABLE flexkvslspar AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexmost AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexmostspar AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexmosl AS DECIMAL NO-UNDO. 
DEFINE VARIABLE tidreg AS LOGICAL NO-UNDO.
DEFINE VARIABLE typ3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE tpris3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE tidslut AS DECIMAL NO-UNDO. 
DEFINE VARIABLE seku AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE sok1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sok3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok5 AS DECIMAL NO-UNDO.


DEFINE VARIABLE blobproch AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE dbconvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE conappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE gforetagvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE appkolltid AS INTEGER NO-UNDO.
DEFINE VARIABLE titlevar AS CHARACTER NO-UNDO.
DEFINE VARIABLE losenvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE sparomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE ovtbeord AS CHARACTER NO-UNDO.
DEFINE VARIABLE rmal AS CHARACTER NO-UNDO.
DEFINE VARIABLE antaldatbaser AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-46 RECT-47 FILL-IN-PKOD 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-PKOD 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WFLEXTID AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-46
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 98.5 BY .5
     BGCOLOR 1 .

DEFINE RECTANGLE RECT-47
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 98.5 BY .5
     BGCOLOR 1 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     FILL-IN-PKOD AT ROW 7.5 COL 11 COLON-ALIGNED
     RECT-46 AT ROW 5.08 COL 1
     RECT-47 AT ROW 14 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 99.38 BY 13.79.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WFLEXTID ASSIGN
         HIDDEN             = YES
         TITLE              = "Elpool i Umeå AB Flextid"
         HEIGHT             = 13.83
         WIDTH              = 99.5
         MAX-HEIGHT         = 27.38
         MAX-WIDTH          = 116
         VIRTUAL-HEIGHT     = 27.38
         VIRTUAL-WIDTH      = 116
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
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
/* SETTINGS FOR WINDOW WFLEXTID
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WFLEXTID)
THEN WFLEXTID:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME FRAME-A

&Scoped-define SELF-NAME FILL-IN-PKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PKOD WFLEXTID
ON LEAVE OF FILL-IN-PKOD IN FRAME FRAME-A /* Enhet/Sign */
DO:
   OUTPUT TO FLEXTID.txt APPEND.
     PUT UNFORMATTED "Check if it is working " TODAY " " STRING(TIME,"HH:MM:SS") " " grundTimer:TimerEventHandle() SKIP .
   OUTPUT CLOSE.
  
   IF grundTimer:TimerEventHandle() = ? OR senasteTimer:TimerEventHandle() = ? THEN DO:
      DELETE OBJECT grundTimer NO-ERROR.
      grundTimer = ?.
      DELETE OBJECT senasteTimer NO-ERROR.
      senasteTimer = ?. 
      RUN control_loadaFrame.
   END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WFLEXTID 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE
DO:
   IF VALID-OBJECT(grundTimer) THEN DELETE OBJECT grundTimer NO-ERROR.
   grundTimer = ?.
   IF VALID-OBJECT(senasteTimer) THEN DELETE OBJECT senasteTimer NO-ERROR.
   senasteTimer = ?.
   IF VALID-HANDLE(blobproch) THEN RUN deleteproc_UI IN blobproch.         
   IF VALID-HANDLE(blobproch) THEN DELETE PROCEDURE blobproch NO-ERROR.
   RUN disable_UI.
END.
   

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.) */
                  
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
   DEFINE VARIABLE sparfont AS INTEGER NO-UNDO.
  DEFINE VARIABLE movetotopp AS LOGICAL NO-UNDO.
   RUN enable_UI. 
   RUN control_loadaFrame.
   {&WINDOW-NAME}:HIDDEN = FALSE.
{&WINDOW-NAME}:ALWAYS-ON-TOP = TRUE.
movetotopp =  {&WINDOW-NAME}:MOVE-TO-TOP().
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_loadaFrame WFLEXTID 
PROCEDURE control_loadaFrame :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE intervalltime AS INTEGER NO-UNDO.
  
     OUTPUT TO FLEXTID.txt APPEND.
     PUT UNFORMATTED "restart of timers " TODAY " " STRING(TIME,"HH:MM:SS") SKIP .
     OUTPUT CLOSE.
     
        grundTimer = NEW Guru.Timer("grundata.PSTimer.Tick",THIS-PROCEDURE).
        intervalltime =  3500000.
       
        grundTimer:StartaTimer(intervalltime). /*Interval i milisekuder*/
        grundTimer:Timer:Tick:Subscribe(grundTimer:TimerEventRun).
         
        senasteTimer = NEW Guru.Timer2("senastetryck.PSTimer.Tick",THIS-PROCEDURE).
        intervalltime =  600000.
        
        senasteTimer:StartaTimer(intervalltime). /*Interval i milisekuder*/
        senasteTimer:Timer2:Tick:Subscribe(senasteTimer:TimerEventRun). 
     
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WFLEXTID  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WFLEXTID)
  THEN DELETE WIDGET WFLEXTID.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WFLEXTID  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-PKOD 
      WITH FRAME FRAME-A IN WINDOW WFLEXTID.
  ENABLE RECT-46 RECT-47 FILL-IN-PKOD 
      WITH FRAME FRAME-A IN WINDOW WFLEXTID.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE grundata.PSTimer.Tick WFLEXTID 
PROCEDURE grundata.PSTimer.Tick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   OUTPUT TO FLEXTID.txt APPEND.
   PUT UNFORMATTED "Timer1  " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE senastetryck.PSTimer.Tick WFLEXTID 
PROCEDURE senastetryck.PSTimer.Tick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   OUTPUT TO FLEXTID.txt APPEND.
   PUT UNFORMATTED "Timer2  " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sundao_UI WFLEXTID 
PROCEDURE sundao_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

