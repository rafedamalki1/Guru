&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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

DEFINE INPUT  PARAMETER planrvar AS LOGICAL NO-UNDO.
/* Local Variable Definitions ---                                       */  
{ALLDEF.I}
&SCOPED-DEFINE NEW
{GLOBVAR2DEL1.I}
{KALKALLTEMP.I}


  

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE brwh AS HANDLE NO-UNDO EXTENT 50.
DEFINE VARIABLE framekalk AS HANDLE NO-UNDO.
{REGVAR.I}

{HOPPSEK2W.I}
{EXECLIN2.I}

DEFINE NEW SHARED VARIABLE vkalproch AS HANDLE NO-UNDO. /* VISAFKALAPP.P */
{TIDUTTTNEW.I}
DEFINE TEMP-TABLE etidut NO-UNDO LIKE tidut
   FIELD ORDNING AS INTEGER
   INDEX ORDNING ORDNING.
{NATKALKTEMP.I}


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-NKALK

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_SKRIV BTN_EXCEL BTN_OK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_EXCEL 
     LABEL "Visa i excel":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "OK":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-NKALK
     BTN_SKRIV AT ROW 28.25 COL 36.63
     BTN_EXCEL AT ROW 28.25 COL 51.63
     BTN_OK AT ROW 28.25 COL 110.75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Följesedel"
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
/* SETTINGS FOR FRAME FRAME-NKALK
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-NKALK
/* Query rebuild information for FRAME FRAME-NKALK
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-NKALK */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXCEL WINDOW-2
ON CHOOSE OF BTN_EXCEL IN FRAME FRAME-NKALK /* Visa i excel */
DO:     
   {muswait.i}   
   RUN btnexcel_UI. 
   {musarrow.i}       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK WINDOW-2
ON CHOOSE OF BTN_OK IN FRAME FRAME-NKALK /* OK */
DO:
   
   {BORTBRWPROC.I}
    FOR EACH valdfasttemp WHERE valdfasttemp.SLUTVNR = 99:
       valdfasttemp.SLUTVNR = 0.
    END.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-NKALK /* Skriv ut */
DO:    
   RUN btnskriv_UI.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME FRAME-NKALK /* Skriv ut */
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
DO:
   IF VALID-HANDLE(vkalproch) THEN DELETE PROCEDURE vkalproch.
   {BORTBRWPROC.I}
   FOR EACH valdfasttemp WHERE valdfasttemp.SLUTVNR = 99:
      valdfasttemp.SLUTVNR = 0.
   END.
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
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
  
   {WIN_M_START.I}
   framekalk = FRAME FRAME-NKALK:HANDLE.
   {muswait.i}  
   RUN enable_UI.  
   {FRMSIZEF.I}
   
   {ALLSTARTDYN.I}  
   
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?.
   /*
   Guru.GlobalaVariabler:colrighth = BTN_OK:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   */
   Guru.GlobalaVariabler:colrighth = BTN_OK:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   ASSIGN
   Guru.GlobalaVariabler:colrighth = BTN_EXCEL:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   ASSIGN
   Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   {musarrow.i}               
   {WIN_M_SLUT.I}    
   FRAME FRAME-NKALK:HIDDEN = FALSE.     
    
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
   IF Guru.Konstanter:appcon THEN RUN VISAFNATAPP.P PERSISTENT SET vkalproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   ELSE RUN VISAFNATAPP.P PERSISTENT SET vkalproch. 
   EMPTY TEMP-TABLE natkalk_temp NO-ERROR. 
   FOR EACH valdfasttemp WHERE valdfasttemp.SLUTVNR = 99:
      CREATE natkalk_temp.
      BUFFER-COPY valdfasttemp TO natkalk_temp.
   END. 
   RUN visnatstart_UI IN vkalproch (INPUT planrvar, INPUT TABLE natkalk_temp,OUTPUT TABLE visnat).
   {&WINDOW-NAME}:TITLE = "Visning av Nätregleringskalkyl" . 
   /*
   FOR EACH visnat WHERE NO-LOCK:
        
      visnat.ANTAL1C = TRIM(visnat.ANTAL1C).  
     
   END.
  */
   FIND FIRST visnat NO-LOCK NO-ERROR.
   
   IF AVAILABLE visnat THEN DO:
      RUN skapabrw_UI (INPUT 1, TEMP-TABLE visnat:DEFAULT-BUFFER-HANDLE).
      RUN setcolindex_UI IN brwproc[1] (INPUT "ORDNING").
        
      RUN openbdynspec_UI IN brwproc[1].
      RUN columnstotempdyn_UI IN brwproc[1].
      FOR EACH visnat:
         RUN cellbrwrow_UI IN brwproc[1] (INPUT ROWID(visnat)).
         RUN rowdispnat_UI (INPUT TABLE coltemp, INPUT brwh[1]).
      END. 
      RUN openbdynspec_UI IN brwproc[1].
      brwh[1]:HIDDEN = FALSE.
   END.   
   ELSE MESSAGE "Inget att visa!"
        VIEW-AS ALERT-BOX. 
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnexcel_UI WINDOW-2 
PROCEDURE btnexcel_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/                
   
   {muswait.i}
   RUN visaexel_UI IN brwproc[1] (FALSE).
   RUN excel_UI (INPUT FALSE).
   {musarrow.i}     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnskriv_UI WINDOW-2 
PROCEDURE btnskriv_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/                
   RUN SKRIVVAL.W (INPUT TRUE).       
   IF musz = TRUE THEN DO:
      musz = FALSE. 
      RETURN NO-APPLY.
   END.
   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN.
   END.
   {muswait.i}
   RUN visaexel_UI IN brwproc[1] (FALSE).
   RUN excel_UI (INPUT TRUE).
   {musarrow.i}     
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
  ENABLE BTN_SKRIV BTN_EXCEL BTN_OK 
      WITH FRAME FRAME-NKALK IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-NKALK}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel_UI WINDOW-2 
PROCEDURE excel_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/                
   DEFINE INPUT PARAMETER saveas AS LOGICAL NO-UNDO.
   
   {AMERICANEUROPEAN.I}
   RUN exceldyn_UI IN brwproc[1] (INPUT saveas).    
   {EUROPEANAMERICAN.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispnat_UI WINDOW-2 
PROCEDURE rowdispnat_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER TABLE FOR coltemp.
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.             
   IF AVAILABLE visnat THEN DO:         
      IF visnat.PERIOD = 0  THEN DO:                     
         RUN fgcolor_UI IN brwproc[1] (INPUT "STATUSNIV", INPUT 4).
         RUN fgcolor_UI IN brwproc[1] (INPUT "BENAMNING", INPUT 4).
         RUN fgcolor_UI IN brwproc[1] (INPUT "OVRIGT1C", INPUT 4).
         IF visnat.BENAMNING = "" THEN RUN fgcolor_UI IN brwproc[1] (INPUT "OVRIGT1C", INPUT 1).
         IF visnat.ARTAL2 > 0 THEN DO:
            RUN fgcolor_UI IN brwproc[1] (INPUT "OVRIGT2C", INPUT 4).
         END.   
         IF visnat.ARTAL3 > 0 THEN DO:
            RUN fgcolor_UI IN brwproc[1] (INPUT "OVRIGT3C", INPUT 4).
         END.
         IF visnat.ARTAL4 > 0 THEN DO:
            RUN fgcolor_UI IN brwproc[1] (INPUT "OVRIGT4C", INPUT 4).
         END.
         IF visnat.ARTAL5 > 0 THEN DO:
            RUN fgcolor_UI IN brwproc[1] (INPUT "OVRIGT5C", INPUT 4).
         END.                       
      END.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapabrw_UI WINDOW-2 
PROCEDURE skapabrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   DEFINE INPUT  PARAMETER brvnr AS INTEGER NO-UNDO.  
   DEFINE INPUT  PARAMETER tthandlez AS HANDLE NO-UNDO.
   DEFINE VARIABLE brwantal AS INTEGER NO-UNDO.
   DEFINE VARIABLE x-multi AS DECIMAL NO-UNDO.
   DEFINE VARIABLE y-multi AS DECIMAL NO-UNDO. 
   DEFINE VARIABLE radvar AS DECIMAL NO-UNDO.
   DEFINE VARIABLE colvar AS DECIMAL NO-UNDO.     
   RUN DYNBRWINIT.P PERSISTENT SET brwproc[brvnr].
   RUN newbrw_UI IN brwproc[brvnr] (INPUT THIS-PROCEDURE).
   RUN brw_UI IN brwproc[brvnr] (OUTPUT brwh[brvnr]).
   /*FRAME-MTRL*/
   IF brvnr = 1 THEN DO:
      
      RUN brwegenskap_UI IN brwproc[brvnr] (INPUT tthandlez,INPUT "BRW_NAT", INPUT 24, INPUT 10, INPUT TRUE, INPUT TRUE, INPUT "Sammanställning av nätreglering", INPUT FRAME {&FRAME-NAME}:HANDLE).
      RUN getsizf_UI IN framesizeh (OUTPUT x-multi, OUTPUT y-multi).
      /*                                     bredd     höjd     kol      rad*/
      radvar = framekalk:WIDTH.
      radvar = radvar -(1). 
      RUN brwstorlek_UI IN brwproc[brvnr](INPUT radvar, INPUT 25, INPUT 1.5, INPUT 1.9, INPUT x-multi, INPUT y-multi).
     
      RUN fieldinit_UI IN brwproc[brvnr].              
      
      RUN createfields_UI IN brwproc[brvnr] (INPUT "PERIODC", INPUT "Period", INPUT 6, INPUT "X(6)", INPUT TRUE).
      /*
      RUN fgcolorcol_UI IN brwproc[brvnr] (INPUT "PERIODC", INPUT 13).
      */
      RUN createfields_UI IN brwproc[brvnr] (INPUT "STATUSNIV", INPUT "Status", INPUT 5, INPUT "X(6)", INPUT TRUE).
      
      IF visnat.ARTAL1 = 0 THEN 
      RUN createfields_UI IN brwproc[brvnr] (INPUT "OVRIGT1C", INPUT "Inget år bestämt", INPUT 11, INPUT "X(11)", INPUT TRUE).               
      ELSE RUN createfields_UI IN brwproc[brvnr] (INPUT "OVRIGT1C", INPUT STRING(visnat.ARTAL1) + " Kostnad", INPUT 11, INPUT "X(11)", INPUT TRUE).                                                                                                                              
      RUN createfields_UI IN brwproc[brvnr] (INPUT "ANTAL1C", INPUT "Antal", INPUT 9, INPUT "X(11)", INPUT TRUE).
      IF visnat.ARTAL2 > 0 THEN DO:                                                            
         RUN createfields_UI IN brwproc[brvnr] (INPUT "OVRIGT2C", INPUT STRING(visnat.ARTAL1) + " Kostnad", INPUT 11, INPUT "X(11)", INPUT TRUE).                                                                                                                              
         RUN createfields_UI IN brwproc[brvnr] (INPUT "ANTAL2C", INPUT "Antal", INPUT 9, INPUT "X(11)", INPUT TRUE).
      END.   
      IF visnat.ARTAL3 > 0 THEN DO:
         RUN createfields_UI IN brwproc[brvnr] (INPUT "OVRIGT3C", INPUT STRING(visnat.ARTAL1) + " Kostnad", INPUT 11, INPUT "X(11)", INPUT TRUE).                                                                                                                              
         RUN createfields_UI IN brwproc[brvnr] (INPUT "ANTAL3C", INPUT "Antal", INPUT 9, INPUT "X(11)", INPUT TRUE).
      END.
      IF visnat.ARTAL4 > 0 THEN DO:
         RUN createfields_UI IN brwproc[brvnr] (INPUT "OVRIGT4C", INPUT STRING(visnat.ARTAL1) + " Kostnad", INPUT 11, INPUT "X(11)", INPUT TRUE).                                                                                                                              
         RUN createfields_UI IN brwproc[brvnr] (INPUT "ANTAL4C", INPUT "Antal", INPUT 9, INPUT "X(11)", INPUT TRUE).
      END.
      IF visnat.ARTAL5 > 0 THEN DO:
         RUN createfields_UI IN brwproc[brvnr] (INPUT "OVRIGT5C", INPUT STRING(visnat.ARTAL1) + " Kostnad", INPUT 11, INPUT "X(11)", INPUT TRUE).                                                                                                                              
         RUN createfields_UI IN brwproc[brvnr] (INPUT "ANTAL5C", INPUT "Antal", INPUT 9, INPUT "X(11)", INPUT TRUE).
      END.
      RUN createfields_UI IN brwproc[brvnr] (INPUT "ARBKOD", INPUT "Kod", INPUT 3, INPUT "X(3)", INPUT TRUE).
      RUN createfields_UI IN brwproc[brvnr] (INPUT "LOPNRC", INPUT "Löpnr", INPUT 5, INPUT "X(4)", INPUT TRUE).
      RUN createfields_UI IN brwproc[brvnr] (INPUT "BENAMNING", INPUT "Benämning", INPUT 10, INPUT "X(256)", INPUT TRUE).
      
      RUN brwsetupstop_UI IN brwproc[brvnr] (INPUT 1).
        
      RUN dynprogextra IN brwproc[brvnr] (INPUT "rowdispnat_UI",INPUT THIS-PROCEDURE).
      RUN rowdispextrakor IN  brwproc[brvnr] (INPUT TRUE).
        
      brwh[brvnr]:FONT = 3.                   
   END.  
   RUN fieldslut_UI IN brwproc[brvnr].
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

