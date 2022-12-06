&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME whWebWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS whWebWin 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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

DEFINE VARIABLE DDDATop     AS INTEGER INITIAL 1.
DEFINE VARIABLE DDDABottom  AS INTEGER INITIAL 2.
DEFINE VARIABLE DDDALeft    AS INTEGER INITIAL 4.
DEFINE VARIABLE DDDARight   AS INTEGER INITIAL 8.
DEFINE VARIABLE DDDAFloat   AS INTEGER INITIAL 16.
DEFINE VARIABLE DDDAPopup   AS INTEGER INITIAL 32.
DEFINE VARIABLE iFw         AS INTEGER INITIAL 0.
DEFINE VARIABLE iBack       AS INTEGER INITIAL 0.
DEFINE VARIABLE barWeb      AS COM-HANDLE.
DEFINE VARIABLE wbBrowser   AS COM-HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME whWebFrame

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD chControl whWebWin 
FUNCTION chControl RETURNS COM-HANDLE
    ( input whFrame as WIDGET-HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR whWebWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE whBarWeb AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chwhBarWeb AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE whWbBrowser AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chwhWbBrowser AS COMPONENT-HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME whWebFrame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 138.4 BY 35.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW whWebWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Progress Software"
         COLUMN             = 27.6
         ROW                = 7
         HEIGHT             = 35
         WIDTH              = 138.4
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
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


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW whWebWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME whWebFrame
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(whWebWin)
THEN whWebWin:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME whWbBrowser ASSIGN
       FRAME        = FRAME whWebFrame:HANDLE
       ROW          = 1
       COLUMN       = 1
       HEIGHT       = 35
       WIDTH        = 138
       HIDDEN       = no
       SENSITIVE    = yes.

CREATE CONTROL-FRAME whBarWeb ASSIGN
       FRAME        = FRAME whWebFrame:HANDLE
       ROW          = 1.24
       COLUMN       = 73
       HEIGHT       = 1.52
       WIDTH        = 6.4
       HIDDEN       = yes
       SENSITIVE    = yes.
      whWbBrowser:NAME = "whWbBrowser":U .
/* whWbBrowser OCXINFO:CREATE-CONTROL from: {EAB22AC3-30C1-11CF-A7EB-0000C05BAE0B} type: wbBrowser */
      whBarWeb:NAME = "whBarWeb":U .
/* whBarWeb OCXINFO:CREATE-CONTROL from: {E4F874A0-56ED-11D0-9C43-00A0C90F29FC} type: barWeb */
      whBarWeb:MOVE-AFTER(whWbBrowser).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME whWebWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL whWebWin whWebWin
ON END-ERROR OF whWebWin /* Progress Software */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL whWebWin whWebWin
ON WINDOW-CLOSE OF whWebWin /* Progress Software */
DO:
  /* This event will close the window and terminate the procedure.  */
  RUN ShutdownActiveBar.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL whWebWin whWebWin
ON WINDOW-RESIZED OF whWebWin /* Progress Software */
DO:
    /*  This is an attempt to resize the frame and 
        WebBrowser control each time the window is
        resized.
    */
    
    DEFINE VARIABLE whFrame AS WIDGET-HANDLE.
 
    whFrame = whWbBrowser:frame.
    whFrame:WIDTH = SELF:WIDTH.
    whFrame:HEIGHT = SELF:HEIGHT.
    
    whWbBrowser:x = 0.
    whWbBrowser:y = 0.  

    whWbBrowser:WIDTH = SELF:WIDTH.
    whWbBrowser:HEIGHT = SELF:HEIGHT.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME whBarWeb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL whBarWeb whWebWin
PROCEDURE whBarWeb.barWeb.BandMove .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Band
  Notes:    This tells the WebBrowser control to adjust its size if the 
            any of the ActiveBar bands are moved.     
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Band AS COM-HANDLE NO-UNDO.

    whWbBrowser:WIDTH = SELF:WIDTH.
    whWbBrowser:HEIGHT = SELF:HEIGHT.
    
    RELEASE OBJECT p-Band.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL whBarWeb whWebWin
PROCEDURE whBarWeb.barWeb.BandResize .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Band
  Notes:    This tells the WebBrowser control to adjust its size if the 
            any of the ActiveBar bands are resized.     
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Band AS COM-HANDLE NO-UNDO.

    whWbBrowser:WIDTH = SELF:WIDTH.
    whWbBrowser:HEIGHT = SELF:HEIGHT.
    
    RELEASE OBJECT p-Band.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL whBarWeb whWebWin
PROCEDURE whBarWeb.barWeb.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Tool
  Notes:   This processes ActiveBar clicks and menu selections.    
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Tool AS COM-HANDLE NO-UNDO.

    DEFINE VARIABLE toolname  AS CHARACTER.
    DEFINE VARIABLE toolsNext AS COM-HANDLE.
    DEFINE VARIABLE toolsBack AS COM-HANDLE.

    toolname = p-Tool:Name.    
    toolsNext = barWeb:Tools:Item("tbNext").   
    toolsBack = barWeb:Tools:Item("tbBack").

    CASE toolname:
        WHEN "tbBack" THEN DO:
            wbBrowser:GoBack.
      
            iBack = iBack - 1.
            IF iBack = 0 THEN
                toolsBack:enabled = FALSE.
 
            toolsNext:enabled = TRUE.
            iFw = iFw + 1.            
        END.
        
        WHEN "tbNext" THEN DO:
            wbBrowser:GoForward.
 
            iFw = iFw - 1.
            if iFw = 0 THEN
                toolsNext:enabled = FALSE.
            
            toolsBack:enabled = TRUE.
            iBack = iBack + 1.
        END.
        
        WHEN "tbHome" THEN DO:
            wbBrowser:GoHome.
            toolsBack:enabled = TRUE.
            iBack = iBack + 1.
        END.
        
        WHEN "tbStop" THEN DO:
            wbBrowser:Stop.
            END.
            
        WHEN "tbRefresh" THEN
            wbBrowser:Refresh.
            
        WHEN "tbSearch" THEN DO:
            wbBrowser:GoSearch.
            toolsBack:enabled = TRUE.
            iBack = iBack + 1.
        END.
        
        WHEN "tbGo" THEN DO:
            DEFINE VARIABLE toolsAddress AS COM-HANDLE.
            toolsAddress = barWeb:Bands:Item("WebAddress"):Tools:Item("tcAddress").            
            wbBrowser:Navigate( toolsAddress:Text ).
            toolsBack:enabled = TRUE.
            iBack = iBack + 1.
            RELEASE OBJECT toolsAddress.
        END.    
        
        WHEN "tmExit" THEN DO:
            RUN ShutdownActiveBar.
            APPLY "CLOSE":U TO THIS-PROCEDURE.
        END.
    END CASE.
    
    IF VALID-HANDLE( toolsBack ) THEN
        RELEASE OBJECT toolsBack.
    IF VALID-HANDLE( toolsNext ) THEN
        RELEASE OBJECT toolsNext.
    IF VALID-HANDLE( p-Tool ) THEN
        RELEASE OBJECT p-Tool.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME whWbBrowser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL whWbBrowser whWebWin
PROCEDURE whWbBrowser.wbBrowser.NavigateComplete .
/*------------------------------------------------------------------------------
  Purpose: Do cleanup after navigation.   
  Parameters:  Required for OCX.
    URL
  Notes:    This puts the current URL into the ActiveBar's combo box if 
            it isn't already there.  
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-URL AS CHARACTER NO-UNDO.

    DEFINE VARIABLE sURL        AS CHARACTER.
    DEFINE VARIABLE sLocation   AS CHARACTER.
    DEFINE VARIABLE i           AS INTEGER.
    DEFINE VARIABLE bFound      AS LOGICAL.
    DEFINE VARIABLE toolsAddress AS COM-HANDLE.
     
    bFound = FALSE.
    sLocation = wbBrowser:LocationName.
    sURL = wbBrowser:LocationURL.
    toolsAddress = barWeb:Bands:Item("WebAddress"):Tools:Item("tcAddress").
    
    REPEAT i = 0 TO toolsAddress:CBList:Count - 1:
        IF bFound = FALSE THEN DO:
            IF toolsAddress:CBList:Item(i) = sURL THEN DO:
                bFound = TRUE.
                toolsAddress:CBListIndex = i.
            END.
         END.
    END.
    
    IF bFound = FALSE THEN 
        toolsAddress:CBList:AddItem( sURL ).
        
   toolsAddress:Text = sURL.
   
   RELEASE OBJECT toolsAddress.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK whWebWin 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  run SetupActiveBar.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load whWebWin _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "web.wrx":U ).

IF OCXFile <> ? THEN DO:
  ASSIGN
    chwhBarWeb = whBarWeb:COM-HANDLE
    UIB_S = chwhBarWeb:LoadControls( OCXFile, "whBarWeb":U)
    chwhWbBrowser = whWbBrowser:COM-HANDLE
    UIB_S = chwhWbBrowser:LoadControls( OCXFile, "whWbBrowser":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "The file, web.wrx, could not be found." skip
             "The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI whWebWin _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(whWebWin)
  THEN DELETE WIDGET whWebWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI whWebWin _DEFAULT-ENABLE
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
  RUN control_load.
  VIEW FRAME whWebFrame IN WINDOW whWebWin.
  {&OPEN-BROWSERS-IN-QUERY-whWebFrame}
  VIEW whWebWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetupActiveBar whWebWin 
PROCEDURE SetupActiveBar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

barWeb = chControl( whBarWeb ).
wbBrowser = chControl( whWbBrowser ).

barWeb:Detach().
barWeb:load( "web.tb", "GoMenu" ).
barWeb:load( "web.tb", "FileMenu" ).
barWeb:load( "web.tb", "WebMenu" ).
barWeb:load( "web.tb", "WebToolbar" ).
barWeb:load( "web.tb", "WebAddress" ).
barWeb:attachex( current-window:hwnd ).

wbBrowser:Navigate ( "http://www.progress.com" ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShutdownActiveBar whWebWin 
PROCEDURE ShutdownActiveBar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

barWeb:detach().

RELEASE OBJECT barWeb.
RELEASE OBJECT wbBrowser.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION chControl whWebWin 
FUNCTION chControl RETURNS COM-HANDLE
    ( input whFrame as WIDGET-HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  gets control's com-handle from control frame
    Notes:  Function Implementation.
------------------------------------------------------------------------------*/

DEFINE VARIABLE ch-Control AS COM-HANDLE.
DEFINE VARIABLE chFrame    AS COM-HANDLE.

chFrame = whFrame:com-handle.
ch-Control = chFrame:Controls:Item(1).
RETURN ch-Control.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


