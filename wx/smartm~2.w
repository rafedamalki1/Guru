&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
/* Procedure Description
"ADM SmartWindow Object Template

Use this template to create a new window which supports SmartObjects. Draw your SmartObjects on this container and establish the appropriate SmartLinks to connect them."
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Notes: 13-08-1998 GJvI  Open-Query tbv sort: Browser was empty
                          Modified local-create-objects
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

  DEFINE VARIABLE ghFolder  AS HANDLE  NO-UNDO.
  DEFINE VARIABLE ghBrowser AS HANDLE  NO-UNDO.
  DEFINE VARIABLE ghViewer1 AS HANDLE  NO-UNDO.
  DEFINE VARIABLE ghViewer2 AS HANDLE  NO-UNDO.
  DEFINE VARIABLE ghViewer3 AS HANDLE  NO-UNDO.
  DEFINE VARIABLE ghViewer4 AS HANDLE  NO-UNDO.
  DEFINE VARIABLE ghViewer5 AS HANDLE  NO-UNDO.
  DEFINE VARIABLE ghViewer6 AS HANDLE  NO-UNDO.
  DEFINE VARIABLE ghViewer7 AS HANDLE  NO-UNDO.

  DEFINE VARIABLE gcMaintBrowser LIKE bdk-maint.cMaintBrowser NO-UNDO.  
  DEFINE VARIABLE gcMaintViewer  LIKE bdk-maint.cMaintViewer  NO-UNDO.    
  DEFINE VARIABLE gcMaintLabel   LIKE bdk-maint.cMaintLabel   NO-UNDO.
  
  DEF VAR gcFunction LIKE bdk-maint.cFunction NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "XHelp" W-Win _INLINE
/* Actions: ? ? ? ? bdk/support/xhelp.p */
/* comma separated list of all widgets */
&SCOP ALL-WIDGETS 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RunSmartViewer W-Win 
FUNCTION RunSmartViewer RETURNS LOGICAL
  ( pnPage AS INT,
    INPUT-OUTPUT phViewer AS HANDLE,
    phGroupAssignSource AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 119.2 BY 21.1
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert SmartWindow title>"
         HEIGHT             = 21.1
         WIDTH              = 119.2
         MAX-HEIGHT         = 27.67
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 27.67
         VIRTUAL-WIDTH      = 160
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         FONT               = 4
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */
{src/bdk/method/smartdefaultwindow.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME        = FRAME F-Main:HANDLE
       ROW          = 2.67
       COLUMN       = 2
       HEIGHT       = 18.1
       WIDTH        = 117
       HIDDEN       = no
       SENSITIVE    = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {9ED94440-E5E8-101B-B9B5-444553540000} type: TabStrip */

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* <insert SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* <insert SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame W-Win
PROCEDURE CtrlFrame.TabStrip.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

RUN select-page( chCtrlFrame:TabStrip:SelectedItem:Index ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


&IF DEFINED(UIB_IS_RUNNING) > 0 &THEN
  RUN set-attribute-list('FunctionCode=Function':U).
&ENDIF

/* ***************************  Main Block  *************************** */
IF VALID-HANDLE({&WINDOW-NAME}) THEN DO:
    ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       {&WINDOW-NAME}:KEEP-FRAME-Z-ORDER = YES
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

    /* The CLOSE event can be used from inside or outside the procedure to  */
    /* terminate it.                                                        */
    ON CLOSE OF THIS-PROCEDURE 
       RUN dispatch IN THIS-PROCEDURE ('destroy':U).

&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
&ENDIF
    /* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
    MAIN-BLOCK:
    DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
       ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
 
    /* Now enable the interface and wait for the exit condition.            */
       RUN dispatch ('initialize':U).
       IF NOT THIS-PROCEDURE:PERSISTENT THEN
           WAIT-FOR CLOSE OF THIS-PROCEDURE.
    END.
&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
END.
&ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load W-Win _CONTROL-LOAD
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

OCXFile = SEARCH( "SmartMaintOcxWindow.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN DISPATCH IN THIS-PROCEDURE("initialize-controls":U) NO-ERROR.
END.
ELSE MESSAGE "SmartMaintOcxWindow.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win _DEFAULT-ENABLE
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
  VIEW FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-objects W-Win 
PROCEDURE local-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE nPage    AS INTEGER NO-UNDO.
  DEFINE VARIABLE cList    AS CHAR    NO-UNDO.
  DEFINE VARIABLE nX       AS DEC     NO-UNDO.
  DEFINE VARIABLE nY       AS DEC     NO-UNDO.
  
  RUN dispatch IN THIS-PROCEDURE ('create-objects':U).

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN nPage = INTEGER(RETURN-VALUE).
  CASE nPage: 

    WHEN 0 THEN DO:
       

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'bdk/objects/smrttool.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'BDK-FUNCTION = ':U + gcFunction + ',
                     BDK-NAVIGATION = yes,
                     BDK-SELECT = yes,
                     BDK-EXTRA-1 = ,
                     BDK-EXTRA-2 = ,
                     BDK-EXTRA-3 = ,
                     BDK-EXTRA-4 = ,
                     BDK-EXTRA-5 = ,
                     BDK-EXTRA-6 = ':U ,
             OUTPUT ghFolder ).
       RUN set-position IN ghFolder ( 1.00 , 1.00 ) NO-ERROR.

       /* Links to SmartMenu h_smrttool. */
       RUN add-link IN adm-broker-hdl ( ghFolder , 'bdk-Extra':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:

       /* run the browser */
       RUN init-object IN THIS-PROCEDURE (
             INPUT  gcMaintBrowser,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ,
                     OpenOninit = yes,
                     OpenOnSort = yes,   /* Open-Query tbv sort  13-08-1998 GJvI */
                     QueryWait  = 500,
                     Reposition = no':U ,
             OUTPUT ghBrowser ).
       RUN get-size-char IN ghBrowser( OUTPUT nX, OUTPUT nY ).
       RUN set-position IN ghBrowser ( ( FRAME {&FRAME-NAME}:HEIGHT-CHAR + 4.5 - nY ) / 2,
                                       ( FRAME {&FRAME-NAME}:WIDTH-CHAR - nX ) / 2 ) NO-ERROR.
       
       /* Links to Browser ghBrowser. */
       RUN add-link IN adm-broker-hdl ( ghFolder , 'Navigation':U , ghBrowser ).
       /*RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'Record':U , ghBrowser ).*/

    END. /* Page 1 */

    WHEN 2 THEN
    DO:
       /* run the viewer 1 */
       RunSmartViewer( 1, INPUT-OUTPUT ghViewer1, ? ).
       RUN add-link IN adm-broker-hdl ( ghFolder , 'TableIO':U , ghViewer1 ).
    END.
    
    WHEN 3 THEN
       /* run the viewer 2 */
       RunSmartViewer( 2, INPUT-OUTPUT ghViewer2, ghViewer1 ).

    WHEN 4 THEN
       /* run the viewer 3 */
       RunSmartViewer( 3, INPUT-OUTPUT ghViewer3, ghViewer2 ).

    WHEN 5 THEN
       /* run the viewer 4 */
       RunSmartViewer( 4, INPUT-OUTPUT ghViewer4, ghViewer3 ).

    WHEN 6 THEN
       /* run the viewer 5 */
       RunSmartViewer( 5, INPUT-OUTPUT ghViewer5, ghViewer4 ).
       
    WHEN 7 THEN
       /* run the viewer 6 */
       RunSmartViewer( 6, INPUT-OUTPUT ghViewer6, ghViewer5 ).

    WHEN 8 THEN
       /* run the viewer 7 */
       RunSmartViewer( 7, INPUT-OUTPUT ghViewer7, ghViewer6 ).

  END CASE.

  /* Select a Startup page. */
  IF nPage eq 0 THEN
    RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE nCount   AS INT     NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* get the function code */
  RUN get-attribute( "FunctionCode" ).
  gcFunction = RETURN-VALUE.
  IF gcFunction = "":U OR gcFunction = ? THEN
  DO:
    MESSAGE "No function defined" VIEW-AS ALERT-BOX ERROR TITLE "FunctionCode".
    RETURN "ADM-ERROR".
  END.
  ELSE
  DO:
    IF NOT getFunctionMaint( gcFunction ) THEN
    DO:
        MESSAGE "No bdk-maint defined" VIEW-AS ALERT-BOX ERROR TITLE "bdk-maint".
        RETURN "ADM-ERROR".
    END.
    ELSE
        ASSIGN gcMaintBrowser       = getMaintBrowser( gcFunction )
               gcMaintViewer        = REPLACE( getMaintViewer( gcFunction ), ",":U, "|":U )
               gcMaintLabel         = REPLACE( getMaintLabel( gcFunction ), ",":U, "|":U )
               {&WINDOW-NAME}:TITLE = getMaintTitle( gcFunction ).
  END.

  /* create the objects */
  RUN dispatch IN THIS-PROCEDURE ('create-objects':U).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  
  /* create the folder pages */
  DO nCount = 1 TO NUM-ENTRIES( gcMaintLabel, "|" ):
     chCtrlFrame:TabStrip:Tabs:Add( nCount,,ENTRY( nCount, gcMaintLabel, "|" ), ).
  END.
  
  CtrlFrame:MOVE-TO-BOTTOM().
  
  /* Code placed here will execute AFTER standard behavior.    */
  RUN init-pages(2).
  
  SESSION:SET-WAIT-STATE("":U).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RunSmartViewer W-Win 
FUNCTION RunSmartViewer RETURNS LOGICAL
  ( pnPage AS INT,
    INPUT-OUTPUT phViewer AS HANDLE,
    phGroupAssignSource AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cViewer  AS CHAR NO-UNDO.
  DEFINE VARIABLE nX       AS DEC     NO-UNDO.
  DEFINE VARIABLE nY       AS DEC     NO-UNDO.


   IF NUM-ENTRIES( gcMaintViewer, "|":U ) >= pnPage THEN
   DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  TRIM( ENTRY( pnPage, gcmaintViewer, "|" ) ),
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT phViewer ).
       RUN get-size-char IN phViewer( OUTPUT nX, OUTPUT nY ).
       RUN set-position IN phViewer ( ( FRAME {&FRAME-NAME}:HEIGHT-CHAR + 4.5 - nY ) / 2,
                                       ( FRAME {&FRAME-NAME}:WIDTH-CHAR - nX ) / 2 ) NO-ERROR.

       /* Links to SmartViewer phViewer */
       RUN add-link IN adm-broker-hdl ( ghBrowser , 'Record':U , phViewer ).
       IF VALID-HANDLE( phGroupAssignSource ) THEN
           RUN add-link IN adm-broker-hdl ( phGroupAssignSource, 'Group-Assign':U , phViewer ).

       /* Initialize other pages that this page requires. */
       IF pnPage > 1 THEN
           RUN init-pages IN THIS-PROCEDURE( STRING( pnPage - 1 ) ) NO-ERROR.
  END.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


