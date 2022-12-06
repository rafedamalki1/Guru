&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File:        objview.w  Progress V9 object viewer
               Rob den Boer  email rc.den.boer@hccnet.nl
  
  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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


&IF DEFINED(UIB_is_Running) NE 0 &THEN
 DEFINE VARIABLE               phroot AS HANDLE.
 ASSIGN phroot = SESSION.
&ELSE
/*   DEFINE INPUT        PARAMETER phroot AS HANDLE. */
 DEFINE VARIABLE               phroot AS HANDLE.
&ENDIF

   
DEF VAR chTreeRoot   AS COM-HANDLE NO-UNDO.
DEF VAR chLastColumn AS COM-HANDLE NO-UNDO.

DEF VAR vColumnType AS CHAR NO-UNDO.
DEF VAR vkey        AS CHAR NO-UNDO.

DEFINE VARIABLE iIteration AS INTEGER NO-UNDO.
DEFINE VARIABLE cAB AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSmallIcons AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLargeIcons AS CHARACTER NO-UNDO.

DEFINE VARIABLE lProps AS LOGICAL NO-UNDO.  /* attributes or properties in listview */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS ed-value btn-refresh btn-ok RECT-1 
&Scoped-Define DISPLAYED-OBJECTS fil-widget-type fil-widget-name ed-value 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-get-obj-descr wWin 
FUNCTION fn-get-obj-descr RETURNS CHARACTER
  ( INPUT phObj AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-get-smallicon wWin 
FUNCTION fn-get-smallicon RETURNS INTEGER
  ( INPUT phdl AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_File 
       MENU-ITEM m_Exit         LABEL "Exit"          .

DEFINE SUB-MENU m_Sort 
       MENU-ITEM m_Ascending    LABEL "Ascending"     
              TOGGLE-BOX
       MENU-ITEM m_Descending   LABEL "Descending"    
              TOGGLE-BOX.

DEFINE SUB-MENU m_View 
       MENU-ITEM m_Attributes   LABEL "Widget Attributes"
              TOGGLE-BOX
       MENU-ITEM m_Properties   LABEL "ADM Properties"
              TOGGLE-BOX
       RULE
       MENU-ITEM m_Large_Icons  LABEL "Large Icons"   
       MENU-ITEM m_Small_Icons  LABEL "Small Icons"   
       MENU-ITEM m_List         LABEL "List"          
       MENU-ITEM m_Report       LABEL "Report"        
       RULE
       SUB-MENU  m_Sort         LABEL "Sort"          
       RULE
       MENU-ITEM m_Value_Preview LABEL "Value Preview" 
              TOGGLE-BOX
       RULE
       MENU-ITEM m_Refresh      LABEL "Refresh"       .

DEFINE SUB-MENU m_Help 
       MENU-ITEM m_About        LABEL "About"         .

DEFINE MENU MENU-BAR-wWin MENUBAR
       SUB-MENU  m_File         LABEL "File"          
       SUB-MENU  m_View         LABEL "View"          
       SUB-MENU  m_Help         LABEL "Help"          .


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE ImageList AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chImageList AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE LargeImageList AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chLargeImageList AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE ListView AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chListView AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE TreeView AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chTreeView AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-ok 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-refresh 
     LABEL "Refresh" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE ed-value AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 75.6 BY 7.05
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fil-widget-name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 52.8 BY .86
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fil-widget-type AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 21.6 BY .86
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 131 BY 20.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fil-widget-type AT ROW 1.67 COL 54 COLON-ALIGNED NO-LABEL
     fil-widget-name AT ROW 1.67 COL 76.4 COLON-ALIGNED NO-LABEL
     ed-value AT ROW 14.62 COL 55.6 NO-LABEL
     btn-refresh AT ROW 22.38 COL 101.2
     btn-ok AT ROW 22.38 COL 118.2
     RECT-1 AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 133.2 BY 22.76
         DEFAULT-BUTTON btn-ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Object Viewer"
         HEIGHT             = 22.76
         WIDTH              = 133.2
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 160
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-wWin:HANDLE.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT wWin:LOAD-ICON("objview/objview":U) THEN
    MESSAGE "Unable to load icon: objview/objview"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
                                                                        */
ASSIGN 
       ed-value:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN fil-widget-name IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fil-widget-type IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME TreeView ASSIGN
       FRAME        = FRAME fMain:HANDLE
       ROW          = 1.67
       COLUMN       = 3.6
       HEIGHT       = 20
       WIDTH        = 51
       HIDDEN       = no
       SENSITIVE    = yes.

CREATE CONTROL-FRAME ListView ASSIGN
       FRAME        = FRAME fMain:HANDLE
       ROW          = 2.62
       COLUMN       = 55.6
       HEIGHT       = 11.91
       WIDTH        = 75.6
       HIDDEN       = no
       SENSITIVE    = yes.

CREATE CONTROL-FRAME ImageList ASSIGN
       FRAME        = FRAME fMain:HANDLE
       ROW          = 21.91
       COLUMN       = 4.2
       HEIGHT       = 1.81
       WIDTH        = 7.6
       HIDDEN       = yes
       SENSITIVE    = yes.

CREATE CONTROL-FRAME LargeImageList ASSIGN
       FRAME        = FRAME fMain:HANDLE
       ROW          = 21.91
       COLUMN       = 13.2
       HEIGHT       = 1.81
       WIDTH        = 7.6
       HIDDEN       = yes
       SENSITIVE    = yes.

PROCEDURE adm-create-controls:
      TreeView:NAME = "TreeView":U .
/* TreeView OCXINFO:CREATE-CONTROL from: {0713E8A2-850A-101B-AFC0-4210102A8DA7} type: TreeView */
      ListView:NAME = "ListView":U .
/* ListView OCXINFO:CREATE-CONTROL from: {58DA8D8A-9D6A-101B-AFC0-4210102A8DA7} type: ListView */
      ImageList:NAME = "ImageList":U .
/* ImageList OCXINFO:CREATE-CONTROL from: {58DA8D8F-9D6A-101B-AFC0-4210102A8DA7} type: ImageList */
      LargeImageList:NAME = "LargeImageList":U .
/* LargeImageList OCXINFO:CREATE-CONTROL from: {58DA8D8F-9D6A-101B-AFC0-4210102A8DA7} type: ImageList */
      ListView:MOVE-AFTER(fil-widget-name:HANDLE IN FRAME fMain).
      ImageList:MOVE-AFTER(ed-value:HANDLE IN FRAME fMain).
      LargeImageList:MOVE-AFTER(ImageList).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Object Viewer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Object Viewer */
DO:

  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok wWin
ON CHOOSE OF btn-ok IN FRAME fMain /* OK */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-refresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-refresh wWin
ON CHOOSE OF btn-refresh IN FRAME fMain /* Refresh */
DO:
  RUN ip-refresh.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-refresh wWin
ON F5 OF btn-refresh IN FRAME fMain /* Refresh */
DO:
  APPLY "CHOOSE":U TO btn-refresh.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ListView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ListView wWin OCX.ColumnClick
PROCEDURE ListView.ListView.ColumnClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    ColumnHeader
  Notes:       
------------------------------------------------------------------------------*/

 DEFINE INPUT PARAMETER p-ColumnHeader AS COM-HANDLE  NO-UNDO.
 
 IF NOT VALID-HANDLE(chLastColumn) OR chLastColumn NE p-ColumnHeader THEN
    ASSIGN chLastColumn = p-ColumnHeader.
 
 ELSE ASSIGN chListView:SortOrder = (IF chListView:SortOrder EQ 0 THEN 1 ELSE 0)
             MENU-ITEM m_Ascending:CHECKED IN MENU m_sort = IF chListView:SortOrder EQ 0 THEN YES ELSE NO
             MENU-ITEM m_Descending:CHECKED IN MENU m_sort = IF chListView:SortOrder EQ 0 THEN NO ELSE YES.
 
 ASSIGN chListView:Sorted  = yes
        chListView:SortKey = p-ColumnHeader:Index - 1.

 RELEASE OBJECT p-ColumnHeader.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ListView wWin OCX.DblClick
PROCEDURE ListView.ListView.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     doubleclick attribute runs attribute viewer dialog
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE hdl AS HANDLE.
  DEFINE VARIABLE cAttr AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.
  
  ASSIGN hdl   = WIDGET-HANDLE(ENTRY(3, chListView:SelectedItem:Key))
         cAttr = ENTRY(2, chListView:SelectedItem:Key).
  
         
  IF VALID-HANDLE(hdl)
  THEN RUN objview/attr.w(INPUT THIS-PROCEDURE:HANDLE, INPUT hdl, INPUT cAttr).
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ListView wWin OCX.ItemClick
PROCEDURE ListView.ListView.ItemClick .
/*------------------------------------------------------------------------------
  Purpose:     itemclick show attribute in preview pane
  Parameters:  Required for OCX.
    Item
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Item AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE hdl AS HANDLE.
DEFINE VARIABLE cAttr AS CHARACTER NO-UNDO.
DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.

    ASSIGN hdl   = WIDGET-HANDLE(ENTRY(3, p-Item:Key))
           cAttr = ENTRY(2, p-Item:Key).
    
    IF VALID-HANDLE(hdl)
    THEN DO:
          RUN ip-get-attr(INPUT hdl
                         ,INPUT-OUTPUT cAttr
                         ,INPUT-OUTPUT cValue).
          ASSIGN ed-value:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cValue.                     
    END. /* IF */                     
    RELEASE OBJECT p-Item.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_About
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_About wWin
ON CHOOSE OF MENU-ITEM m_About /* About */
DO:
  RUN objview/about.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Ascending
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Ascending wWin
ON CHOOSE OF MENU-ITEM m_Ascending /* Ascending */
DO:
 ASSIGN chListView:SortOrder = IF SELF:CHECKED THEN 0 ELSE 1
        MENU-ITEM m_Descending:CHECKED IN MENU m_sort = NOT SELF:CHECKED
        chListView:Sorted = YES.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Attributes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Attributes wWin
ON VALUE-CHANGED OF MENU-ITEM m_Attributes /* Widget Attributes */
DO:
  MENU-ITEM m_Properties:CHECKED IN MENU MENU-BAR-wWin = FALSE.
  SELF:CHECKED = TRUE.
  ASSIGN lProps = FALSE.
  RUN ip-view.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Descending
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Descending wWin
ON CHOOSE OF MENU-ITEM m_Descending /* Descending */
DO:
 ASSIGN chListView:SortOrder = IF SELF:CHECKED THEN 1 ELSE 0
        MENU-ITEM m_Ascending:CHECKED IN MENU m_sort = NOT SELF:CHECKED
        chListView:Sorted = YES.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Large_Icons
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Large_Icons wWin
ON CHOOSE OF MENU-ITEM m_Large_Icons /* Large Icons */
DO:
  ASSIGN chListView:View = 0.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_List
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_List wWin
ON CHOOSE OF MENU-ITEM m_List /* List */
DO:
    ASSIGN chListView:View = 2.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Properties
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Properties wWin
ON VALUE-CHANGED OF MENU-ITEM m_Properties /* ADM Properties */
DO:
  MENU-ITEM m_Attributes:CHECKED IN MENU MENU-BAR-wWin = FALSE.
  SELF:CHECKED = TRUE.
  ASSIGN lProps = TRUE.
  RUN ip-view.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Refresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Refresh wWin
ON CHOOSE OF MENU-ITEM m_Refresh /* Refresh */
DO:
  RUN ip-refresh.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Report
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Report wWin
ON CHOOSE OF MENU-ITEM m_Report /* Report */
DO:
  ASSIGN chListView:View = 3.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Small_Icons
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Small_Icons wWin
ON CHOOSE OF MENU-ITEM m_Small_Icons /* Small Icons */
DO:
 ASSIGN chListView:View = 1.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Value_Preview
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Value_Preview wWin
ON VALUE-CHANGED OF MENU-ITEM m_Value_Preview /* Value Preview */
DO:
  RUN ip-value-preview(INPUT SELF:CHECKED).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TreeView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TreeView wWin OCX.NodeClick
PROCEDURE TreeView.TreeView.NodeClick .
/*------------------------------------------------------------------------------
  Purpose:     nodeclick expands node
  Parameters:  Required for OCX.
    Node
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Node AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hdl AS HANDLE.
DEFINE VARIABLE cObjDescr AS CHARACTER NO-UNDO.

 IF VALID-HANDLE(p-Node)
 THEN DO:
  ASSIGN hdl = WIDGET-HANDLE(ENTRY(2,p-Node:Key)).
  IF VALID-HANDLE(hdl)
  THEN DO:
    RUN ip-get-obj-descr(INPUT hdl, INPUT-OUTPUT cObjDescr).  
    DO WITH FRAME {&FRAME-NAME}:
      ASSIGN fil-widget-name:SCREEN-VALUE = cObjDescr
             fil-widget-type:SCREEN-VALUE = hdl:TYPE.
    END. /* DO */             
  END. /* IF */      
 END. /* IF */               

 IF p-Node:Child EQ 0 THEN 
    RUN ip-expand-node (INPUT p-Node). 
                
/*  RUN ip-view-attrs(INPUT p-Node). */
 RUN ip-view.
 RELEASE OBJECT p-Node.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */


/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load wWin  _CONTROL-LOAD
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

OCXFile = SEARCH( "objview.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chImageList = ImageList:COM-HANDLE
    UIB_S = chImageList:LoadControls( OCXFile, "ImageList":U)
    chLargeImageList = LargeImageList:COM-HANDLE
    UIB_S = chLargeImageList:LoadControls( OCXFile, "LargeImageList":U)
    chListView = ListView:COM-HANDLE
    UIB_S = chListView:LoadControls( OCXFile, "ListView":U)
    chTreeView = TreeView:COM-HANDLE
    UIB_S = chTreeView:LoadControls( OCXFile, "TreeView":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "objview.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject wWin 
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
 
  /*--- release com handles ---*/
  
  IF VALID-HANDLE(chTreeView)
  THEN RELEASE OBJECT chTreeView.

  IF VALID-HANDLE(chListView)
  THEN RELEASE OBJECT chListView.

  IF VALID-HANDLE(chImageList)
  THEN RELEASE OBJECT chImageList.

  IF VALID-HANDLE(chLargeImageList)
  THEN RELEASE OBJECT chLargeImageList.

  IF VALID-HANDLE(chLastColumn)
  THEN RELEASE OBJECT chLastColumn.

  IF VALID-HANDLE(chTreeRoot)
  THEN RELEASE OBJECT chTreeRoot.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY fil-widget-type fil-widget-name ed-value 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE ed-value btn-refresh btn-ok RECT-1 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */


  /*--- if phroot is ? then take SESSION as startingpoint ---*/
  IF phroot = ?
  THEN ASSIGN phroot =  SESSION.
   
  RUN ip-initialize(INPUT phroot).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-add-to-tree wWin 
PROCEDURE ip-add-to-tree :
DEF INPUT PARAMETER ipNode    AS COM-HANDLE NO-UNDO.
 DEF INPUT PARAMETER phdl      AS HANDLE NO-UNDO.
 DEF INPUT PARAMETER pvKey     AS CHARACTER NO-UNDO.

 DEFINE VARIABLE ch    AS COM-HANDLE.
 IF VALID-HANDLE(phdl)
 THEN DO:
   ch = chTreeView:Nodes:Add(ipNode:Index,4,pvKey,fn-get-obj-descr(phdl),fn-get-smallicon(phdl),fn-get-smallicon(phdl)) NO-ERROR.
   RUN ip-expand-node(INPUT ch).
  END. /* IF */   
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-expand wWin 
PROCEDURE ip-expand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ipNode    AS COM-HANDLE NO-UNDO.

 DEFINE VARIABLE vKey AS CHAR NO-UNDO.
 DEFINE VARIABLE cObjDescr    AS CHARACTER NO-UNDO.
 DEFINE VARIABLE hdl   AS HANDLE.
 DEFINE VARIABLE hobj  AS HANDLE.
 DEFINE VARIABLE ch    AS COM-HANDLE.

  ASSIGN hobj = WIDGET-HANDLE(ENTRY(2,ipNode:Key)).
  IF CAN-QUERY(hobj, "POPUP-MENU")
  THEN DO:
    ASSIGN hdl = hobj:POPUP-MENU.
    RUN ip-add-to-tree(INPUT ipNode, INPUT hdl, INPUT "Object," + STRING(hdl)).  
  END. /* IF */

  ASSIGN hdl = hobj.
  IF VALID-HANDLE(hdl) AND 
     CAN-QUERY(hdl, "FIRST-CHILD")
  THEN DO:
    ASSIGN hdl = hdl:FIRST-CHILD.
    DO WHILE VALID-HANDLE(hdl):
      RUN ip-add-to-tree(INPUT ipNode, INPUT hdl, INPUT "Object," + STRING(hdl)).  
      ASSIGN hdl = hdl:NEXT-SIBLING.
    END. /* DO */
  END. /* IF */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-expand-browse wWin 
PROCEDURE ip-expand-browse :
/*------------------------------------------------------------------------------
  Purpose:     expands a BROWSE widget
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ipNode    AS COM-HANDLE NO-UNDO.

 DEFINE VARIABLE vKey AS CHAR NO-UNDO.
 DEFINE VARIABLE cObjDescr    AS CHARACTER NO-UNDO.
 DEFINE VARIABLE hdl       AS HANDLE.
 DEFINE VARIABLE hbrowse   AS HANDLE.
 DEFINE VARIABLE ch        AS COM-HANDLE.

  ASSIGN hbrowse = WIDGET-HANDLE(ENTRY(2,ipNode:Key)) NO-ERROR.

  ASSIGN hdl = hbrowse:POPUP-MENU NO-ERROR.  
  RUN ip-add-to-tree(INPUT ipNode, INPUT hdl, INPUT "Object," + STRING(hdl)).  
  
  ASSIGN hdl = hbrowse:FIRST-COLUMN NO-ERROR.
  DO WHILE VALID-HANDLE(hdl):
    RUN ip-add-to-tree(INPUT ipNode, INPUT hdl, INPUT "Object," + STRING(hdl)).  
    ASSIGN hdl = hdl:NEXT-COLUMN.
  END. /* DO */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-expand-frame wWin 
PROCEDURE ip-expand-frame :
/*------------------------------------------------------------------------------
  Purpose:     expands a frame widget
  Parameters:  <none> rect
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ipNode    AS COM-HANDLE NO-UNDO.

 DEFINE VARIABLE vKey AS CHAR NO-UNDO.
 DEFINE VARIABLE cObjDescr    AS CHARACTER NO-UNDO.
 DEFINE VARIABLE hdl    AS HANDLE.
 DEFINE VARIABLE hframe AS HANDLE.
 DEFINE VARIABLE ch     AS COM-HANDLE.

  ASSIGN hframe = WIDGET-HANDLE(ENTRY(2,ipNode:Key))
         hdl    = hframe:POPUP-MENU.
  RUN ip-add-to-tree(INPUT ipNode, INPUT hdl, INPUT "Object," + STRING(hdl)).  
  
  ASSIGN hdl = hframe:CURRENT-ITERATION 
         hdl = hdl:FIRST-CHILD NO-ERROR.
  RUN ip-add-to-tree(INPUT ipNode, INPUT hdl, INPUT "Object," + STRING(hdl)).  

           
  DO WHILE VALID-HANDLE(hdl):
    RUN ip-add-to-tree(INPUT ipNode, INPUT hdl, INPUT "Object," + STRING(hdl)).  
    ASSIGN hdl = hdl:NEXT-SIBLING.
  END. /* DO */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-expand-node wWin 
PROCEDURE ip-expand-node :
/*------------------------------------------------------------------------------
  Purpose:     workhorse proc for expanding nodes, 
               dispatches to ip-expand-WIDGET:TYPE
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ipNode    AS COM-HANDLE NO-UNDO.

 DEFINE VARIABLE vKey AS CHAR NO-UNDO.
 DEFINE VARIABLE cObjDescr    AS CHARACTER NO-UNDO.
 DEFINE VARIABLE hdl   AS HANDLE.
 DEFINE VARIABLE ch    AS COM-HANDLE.
 DEFINE VARIABLE cExpandProc AS CHARACTER.
 
 /* run ip-expand for the TYPE widget */
 
 IF VALID-HANDLE(ipNode)
 THEN DO:
  ASSIGN hdl = WIDGET-HANDLE(ENTRY(2,ipNode:Key)).
  IF VALID-HANDLE(hdl)
  THEN DO:
    DO WITH FRAME {&FRAME-NAME}:
        IF CAN-QUERY(hdl, "NAME")
        THEN ASSIGN fil-widget-name:SCREEN-VALUE = hdl:NAME.
        ASSIGN cExpandProc     = "ip-expand-" + LC(hdl:TYPE)
               fil-widget-type:SCREEN-VALUE = hdl:TYPE
                .
    END. /* DO */
     
    /*--- do we have a ip-expand-WIDGET:TYPE procedure ? ---*/               
    IF CAN-DO(THIS-PROCEDURE:INTERNAL-ENTRIES, cExpandProc)
    THEN   RUN VALUE(cExpandProc) (INPUT ipNode).
    ELSE   RUN ip-expand (INPUT ipNode).
  END. /* IF */  
 END. /* IF VALID-HANDLE(ipNode) */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-expand-procedure wWin 
PROCEDURE ip-expand-procedure :
/*------------------------------------------------------------------------------
  Purpose:     expands a procedure 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ipNode    AS COM-HANDLE NO-UNDO.

 DEFINE VARIABLE vKey AS CHAR NO-UNDO.
 DEFINE VARIABLE cObjDescr    AS CHARACTER NO-UNDO.
 DEFINE VARIABLE hdl   AS HANDLE.
 DEFINE VARIABLE ch    AS COM-HANDLE.


  ASSIGN hdl = WIDGET-HANDLE(ENTRY(2,ipNode:Key)).
         hdl = hdl:CURRENT-WINDOW NO-ERROR.
  DO WHILE VALID-HANDLE(hdl):
    RUN ip-add-to-tree(INPUT ipNode, INPUT hdl, INPUT "Object," + STRING(hdl)).  
    ASSIGN hdl = hdl:NEXT-SIBLING.
  END. /* DO */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-expand-pseudo-widget wWin 
PROCEDURE ip-expand-pseudo-widget :
/*------------------------------------------------------------------------------
  Purpose:     expands a pseudo-widget like SESSION
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ipNode    AS COM-HANDLE NO-UNDO.

 DEFINE VARIABLE vKey AS CHAR NO-UNDO.
 DEFINE VARIABLE cObjDescr    AS CHARACTER NO-UNDO.
 DEFINE VARIABLE hdl   AS HANDLE.
 DEFINE VARIABLE ch    AS COM-HANDLE.


  ASSIGN hdl = WIDGET-HANDLE(ENTRY(2,ipNode:Key)).
  IF VALID-HANDLE(hdl)
  THEN DO:       
    IF hdl = SESSION
    THEN ASSIGN hdl = SESSION:FIRST-PROCEDURE.
      DO WHILE VALID-HANDLE(hdl):
        RUN ip-add-to-tree(INPUT ipNode, INPUT hdl, INPUT "Object," + STRING(hdl)).  
        ASSIGN hdl = hdl:NEXT-SIBLING.
      END. /* DO */
  END. /* IF */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-expand-root wWin 
PROCEDURE ip-expand-root :
/*------------------------------------------------------------------------------
  Purpose:     expands the root entry of our treeview
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cRootDescr AS CHARACTER NO-UNDO.

    /* get description of root-object and start treeview */
    RUN ip-get-obj-descr(INPUT phroot, INPUT-OUTPUT cRootDescr).
       
    IF VALID-HANDLE(chTreeRoot)
    THEN RELEASE OBJECT chTreeRoot.
    
    ASSIGN chTreeRoot = chTreeView:Nodes:Add(,,"rootobject," + STRING(phroot),cRootDescr,fn-get-smallicon(phroot),fn-get-smallicon(phroot)). 
  
    RUN ip-expand-node(INPUT chTreeRoot).
    ASSIGN chTreeroot:expanded = yes. /* expand root node */
    RUN ip-view.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-expand-window wWin 
PROCEDURE ip-expand-window :
/*------------------------------------------------------------------------------
  Purpose:     expands a window widget
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ipNode    AS COM-HANDLE NO-UNDO.

 DEFINE VARIABLE hdl   AS HANDLE.
 DEFINE VARIABLE hwin  AS HANDLE.
 DEFINE VARIABLE ch    AS COM-HANDLE.

  ASSIGN hdl = WIDGET-HANDLE(ENTRY(2,ipNode:Key)).
  IF VALID-HANDLE(hdl)
  THEN DO:
  
    ASSIGN hwin = hdl:MENU-BAR.
    RUN ip-add-to-tree(INPUT ipNode, INPUT hwin, INPUT "Object," + STRING(hwin)).  
    ASSIGN hwin = hdl:POPUP-MENU.
    RUN ip-add-to-tree(INPUT ipNode, INPUT hwin, INPUT "Object," + STRING(hwin)).  
  
    ASSIGN hdl = hdl:FIRST-CHILD.
    DO WHILE VALID-HANDLE(hdl):
      RUN ip-add-to-tree(INPUT ipNode, INPUT hdl, INPUT "Object," + STRING(hdl)).  
      ASSIGN hdl = hdl:NEXT-SIBLING.
    END. /* DO */
  END. /* IF */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-get-attr wWin 
PROCEDURE ip-get-attr :
/*------------------------------------------------------------------------------
  Purpose:     gets attribute of widget, returns its value
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER        phObj  AS HANDLE.
DEFINE INPUT-OUTPUT PARAMETER pcAttr AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER pcVal  AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdl    AS HANDLE.
DEFINE VARIABLE cDescr AS CHARACTER NO-UNDO.

    ASSIGN pcVal  = "".
    
    CASE pcAttr:
     { objview/attributes.i &var = "pcVal" }
     OTHERWISE pcAttr = ?.
    END CASE.
    
    
    ASSIGN hdl = WIDGET-HANDLE(pcVal) NO-ERROR.
    IF VALID-HANDLE(hdl)
    THEN DO:
      RUN ip-get-obj-descr(INPUT hdl, INPUT-OUTPUT cDescr).
      ASSIGN pcVal = pcVal + ", " + hdl:TYPE + " " + cDescr.
    END. /* IF */                      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-get-obj-descr wWin 
PROCEDURE ip-get-obj-descr :
/*------------------------------------------------------------------------------
  Purpose:     gives description of object
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER phObj AS HANDLE.
  DEFINE INPUT-OUTPUT PARAMETER pcDescr AS CHARACTER NO-UNDO.

  CASE phObj:TYPE:
    WHEN 'PSEUDO-WIDGET'  
    THEN       ASSIGN pcDescr = (IF phObj = SESSION THEN "Session" ELSE "Pseudo").
    WHEN 'PROCEDURE'  
    THEN DO:
      IF phObj:FILE-NAME = "&{&UIB_is_Running}"
      THEN ASSIGN pcDescr = "This procedure".
      ELSE ASSIGN pcDescr = phObj:FILE-NAME.
    END.      
    WHEN 'WINDOW'  
    THEN       ASSIGN pcDescr = "Window".
    OTHERWISE  ASSIGN pcDescr = phObj:NAME.
  END CASE.    



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-get-widgetlist wWin 
PROCEDURE ip-get-widgetlist :
/*------------------------------------------------------------------------------
  Purpose:     gets list of supported attributes of widget type.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER phobj AS HANDLE.
DEFINE INPUT-OUTPUT PARAMETER pcList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cType AS CHARACTER NO-UNDO.

    IF phobj = SESSION
    THEN ASSIGN cType = "SESSION".
    ELSE ASSIGN cType = phobj:TYPE.
    
    
    CASE cType:
     { objview/others-list.i }  /* special cases for SESSION, PROCEDURE etc. */
     OTHERWISE
       ASSIGN pcList = LIST-QUERY-ATTRS(phobj).   /* regular widgets */
    END CASE. 

    ASSIGN pcList = LIST-QUERY-ATTRS(phobj).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-initialize wWin 
PROCEDURE ip-initialize :
/*------------------------------------------------------------------------------
  Purpose:     main init procedure
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER phroot AS HANDLE.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

/*
MESSAGE VALID-HANDLE(chTreeView) skip
        VALID-HANDLE(chListView) skip
        VALID-HANDLE(chImageList) skip
        VALID-HANDLE(chLargeImageList) skip
        VIEW-AS ALERT-BOX.
*/       

 IF VALID-HANDLE(chTreeView) AND
    VALID-HANDLE(chListView) AND
    VALID-HANDLE(chImageList)  AND
    VALID-HANDLE(chLargeImageList) 
 THEN DO:
    /* setup Treeview + listview + Imagelists */
    ASSIGN chTreeView            = chTreeView:Controls:Item(1)        /* get pointer to actual TreeView Control */
           chImageList           = chImageList:Controls:Item(1)       /* get pointer to actual ImageList Control */
           chlargeImageList      = chLargeImageList:Controls:Item(1)  /* get pointer to actual ImageList Control */
           chTreeView:ImageList  = chImageList                        /* set pictures for TreeView */
           chListView            = chListView:Controls:Item(1)        /* get pointer to actual ListView Control */
           chListView:SmallIcons = chImageList                        /* set pictures for ListView Small icons */
           chListView:Icons      = chLargeImageList.                  /* set pictures for ListView Large Icons */
  
    /* build icon lists */
    DO i = 1 TO NUM-ENTRIES(TRIM("{objview/widgets.i}")):
      ASSIGN cSmallIcons =  cSmallIcons 
                            + MIN(cSmallIcons, ",")
                            + ENTRY(i, TRIM("{objview/widgets.i}"))
                            .
    END.
  
    DO i = 1 TO NUM-ENTRIES(TRIM("{objview/non-widgets.i}")):
      ASSIGN cSmallIcons =  cSmallIcons 
                            + MIN(cSmallIcons, ",")
                            + ENTRY(i, TRIM("{objview/non-widgets.i}"))
                            .
    END.
  
    ASSIGN {&WINDOW-NAME}:NAME = "{&WINDOW-NAME}".
    
    MENU-ITEM m_Attributes:CHECKED IN MENU MENU-BAR-wWin = NOT(lProps).
    MENU-ITEM m_Properties:CHECKED IN MENU MENU-BAR-wWin = lProps.
    

    /*-- fill the treeview recursivly ---*/
    RUN ip-expand-root.
   
    /*--- default value preview on ---*/
    ASSIGN MENU-ITEM m_Value_Preview:CHECKED IN MENU MENU-BAR-wWin = TRUE.
    RUN ip-value-preview(INPUT MENU-ITEM m_Value_Preview:CHECKED IN MENU MENU-BAR-wWin).
    
  END. /* IF VALID-HANDLE ... */
  ELSE DO:
    /* i havent figured out this one ... happens when run persistent */
    MESSAGE "No initialized ActiveX controls." 
            VIEW-AS ALERT-BOX ERROR TITLE "ip-initialize".
  END. /* ELSE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-refresh wWin 
PROCEDURE ip-refresh :
/*------------------------------------------------------------------------------
  Purpose:     refreshes treeview and listview by rebuilding them both.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE lOK AS LOGICAL NO-UNDO.
 
  lOK = chTreeview:Nodes:Clear().
  lOK = chListView:ListItems:Clear().
  RUN ip-expand-root.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-set-attr wWin 
PROCEDURE ip-set-attr :
/*------------------------------------------------------------------------------
  Purpose:     set an attribute. not working yet.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER        phObj AS HANDLE.
DEFINE INPUT PARAMETER pcAttr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcVal  AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdl AS HANDLE.

/* welke TYPE is het attribute ?
CASE pcAttr:
 { objview/attributes-set.i &object = "phObj" &var = "pcVal" }
END CASE.

*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-value-preview wWin 
PROCEDURE ip-value-preview :
/*------------------------------------------------------------------------------
  Purpose:     turns on/off the attribute preview pane.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER plPreview AS LOGICAL NO-UNDO.

  IF plPreview
  THEN DO:
    DO WITH FRAME {&FRAME-NAME}:
      ASSIGN ListView:HEIGHT-PIXELS = 250
             ed-value:HIDDEN = FALSE.
    
    END. /* DO WITH FRAME ... */
  END. /* IF */
  ELSE DO:
    DO WITH FRAME {&FRAME-NAME}:
      ASSIGN ListView:HEIGHT-PIXELS = 400
             ed-value:HIDDEN = TRUE.
    END. /* DO WITH FRAME ... */
  END. /* ELSE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-view wWin 
PROCEDURE ip-view :
/*------------------------------------------------------------------------------
  Purpose:     views attributes or properties in listview.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 DEF VAR pnode AS COM-HANDLE.
 
 pnode = chTreeView:SelectedItem.

 IF VALID-HANDLE(pnode)
 THEN DO:
   IF lProps 
   THEN RUN ip-view-props(INPUT pnode).
   ELSE RUN ip-view-attrs(INPUT pnode). 
 END. /* IF */
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-view-attrs wWin 
PROCEDURE ip-view-attrs :
/*------------------------------------------------------------------------------
  Purpose:     views a widgets attributes in a listview
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ipNode    AS COM-HANDLE NO-UNDO.

 DEF VAR vOrder      AS INTEGER NO-UNDO.
 DEF VAR vNewItem    AS COM-HANDLE NO-UNDO.
 DEF VAR vTreeItem   AS COM-HANDLE NO-UNDO.
 DEF VAR ok          AS LOGICAL NO-UNDO.
 DEF VAR i           AS INTEGER NO-UNDO.
 DEF VAR vStart      AS INTEGER NO-UNDO.
 DEF VAR vEnd        AS INTEGER NO-UNDO.
 DEF VAR cAttr       AS CHARACTER NO-UNDO.
 DEF VAR cValue      AS CHARACTER NO-UNDO.
 DEF VAR hdl         AS HANDLE.
 DEF VAR cWidgetlist AS CHARACTER NO-UNDO. 

 IF vColumnType <> "Attribute":U    /* need to create new columns */
 THEN 
    ASSIGN ok = chListView:ColumnHeaders:Clear()
           ok = chListView:ColumnHeaders:Add(,,"Attribute")         
           ok = chListView:ColumnHeaders:Add(,,"Value")
           vColumnType = "Attribute":U.

 chListView:ListItems:Clear(). /* clear all existing listview entries */


  ASSIGN hdl = WIDGET-HANDLE(ENTRY(2,ipNode:Key)).
  IF VALID-HANDLE(hdl)
  THEN DO:
    RUN ip-get-widgetlist(INPUT hdl, INPUT-OUTPUT cWidgetlist). 
    DO i = 1 TO NUM-ENTRIES(cWidgetlist):
      ASSIGN cAttr = ENTRY(i, cWidgetlist).
      RUN ip-get-attr(INPUT hdl
                     ,INPUT-OUTPUT cAttr
                     ,INPUT-OUTPUT cValue).
        

        IF cAttr <> ? AND cAttr <> ""
        THEN DO:
          ASSIGN cValue = (IF cValue = ? THEN "?" ELSE cValue)
                 vNewItem = chListView:ListItems:Add(,"Attribute," + STRING(cAttr)
                                                     + "," + STRING(hdl)
                                                     ,cAttr,2,25).
      
          IF VALID-HANDLE(vNewItem)
          THEN   ASSIGN vNewItem:SubItems(1) = cValue
                        .
        END. /* IF */
    END. /* DO i = ... */
  END. /* IF VALID-HANDLE(hdl) */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-view-props wWin 
PROCEDURE ip-view-props :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipNode    AS COM-HANDLE NO-UNDO.

 DEFINE VARIABLE hdl         AS HANDLE.
 DEFINE VARIABLE hproc       AS HANDLE.
 DEFINE VARIABLE vNewItem    AS COM-HANDLE NO-UNDO.
 DEFINE VARIABLE clist       AS CHAR NO-UNDO.
 DEFINE VARIABLE i           AS INT NO-UNDO.
 DEFINE VARIABLE cProp       AS CHAR NO-UNDO.
 DEFINE VARIABLE cValue      AS CHAR NO-UNDO.
 DEFINE VARIABLE hAdmProp    AS HANDLE.
 DEFINE VARIABLE ok          AS LOGICAL NO-UNDO.
 DEFINE VARIABLE cPropsdone  AS CHARACTER NO-UNDO.

  /*--- retrieve bufferhandle to the Admprops temp-table. ---*/
  ASSIGN hproc    = WIDGET-HANDLE(ENTRY(2,ipnode:Key))
         hAdmProp = WIDGET-HANDLE(ENTRY(1, hproc:ADM-DATA, CHR(1))) NO-ERROR.
         
  IF VALID-HANDLE(hAdmProp)
  THEN DO:         
    
      IF vColumnType <> "Property":U    /* need to create new columns */
      THEN 
         ASSIGN ok = chListView:ColumnHeaders:Clear()
                ok = chListView:ColumnHeaders:Add(,,"ADM Property")         
                ok = chListView:ColumnHeaders:Add(,,"Value")
                vColumnType = "Property":U.
    
      chListView:ListItems:Clear(). /* clear all existing listview entries */
    
        /*--- display all fields in the admprops temp-table ---*/
        DO i = 1 TO hAdmProp:NUM-FIELDS:
          hdl = hAdmProp:BUFFER-FIELD(i).
          IF VALID-HANDLE(hdl)
          THEN DO: 
            ASSIGN cValue   = STRING(hdl:BUFFER-VALUE)
                   cValue   = (IF cValue = ? THEN "" ELSE cValue)
                   cProp    = hdl:NAME
                   vNewItem = chListView:ListItems:Add(,"Property," + STRING(cProp)
                                                      + "," + STRING(hproc)
                                                      ,cProp,2,27)
                   cPropsdone = cPropsdone + "," + cProp.
            
            IF VALID-HANDLE(vNewItem)
            THEN   ASSIGN vNewItem:SubItems(1) = cValue.
          END. /* IF */
          hdl = ?.
        END. /* DO */  
        
        /*--- do properties not in the temp-table by function ---*/
        DO i = 1 TO NUM-ENTRIES(hproc:INTERNAL-ENTRIES, ","):
          ASSIGN cProp = ENTRY(i, hproc:INTERNAL-ENTRIES, ",").
          IF cProp BEGINS "get"           /* property function ? */
          THEN DO:
            ASSIGN cProp = SUBSTRING(cProp, 4).
            IF INDEX(cPropsdone, cProp) = 0  /* already done ? */
            THEN DO:
              IF cProp <> "UserProperty"
              THEN DO:
                ASSIGN cValue = STRING( DYNAMIC-FUNCTION("get" + cProp IN hproc) )
                       cValue = (IF cValue = ? THEN "" ELSE cValue).
             
                     vNewItem = chListView:ListItems:Add(,"Property," + STRING(cProp)
                                                        + "," + STRING(hproc)
                                                        ,cProp,2,28).
                IF VALID-HANDLE(vNewItem)
                THEN   ASSIGN vNewItem:SubItems(1) = cValue.
              END. /* IF */                                            
            END. /* IF */
          END. /* IF */
        END. /* DO */
        
  END. /* IF */
  ELSE DO:    /* not a smartobject, display attributes instead. */
    MESSAGE "This is not a SmartObject." SKIP(0)
            "Returning to Attribute view mode."
            VIEW-AS ALERT-BOX INFORMATION.
    APPLY "VALUE-CHANGED":U TO MENU-ITEM m_Attributes IN MENU MENU-BAR-wWin.  
    RETURN NO-APPLY.
  END. /* ELSE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-get-obj-descr wWin 
FUNCTION fn-get-obj-descr RETURNS CHARACTER
  ( INPUT phObj AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:     gives description of object
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cDescr AS CHARACTER INITIAL "" NO-UNDO.

  IF VALID-HANDLE(phObj)
  THEN DO:
    CASE phObj:TYPE:
      WHEN 'PSEUDO-WIDGET'  
      THEN       ASSIGN cDescr = (IF phObj = SESSION THEN "Session" ELSE "Pseudo").
      WHEN 'PROCEDURE'  
      THEN DO:
        IF phObj:FILE-NAME = "&{&UIB_is_Running}"
        THEN ASSIGN cDescr = "This procedure".
        ELSE ASSIGN cDescr = phObj:FILE-NAME.
      END.      
      WHEN 'WINDOW'  
      THEN       ASSIGN cDescr = "Window".
      OTHERWISE  ASSIGN cDescr = phObj:NAME.
    END CASE.    
  END. /* IF */
  RETURN cDescr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-get-smallicon wWin 
FUNCTION fn-get-smallicon RETURNS INTEGER
  ( INPUT phdl AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  returns iconno in the list by WIDGET:TYPE
    Notes:  default is icon 23
------------------------------------------------------------------------------*/
DEFINE VARIABLE iIcon AS INTEGER.
 IF VALID-HANDLE(phdl)
 THEN DO:
    iIcon = LOOKUP(phdl:TYPE, cSmallIcons).
    IF iIcon = 0
    THEN RETURN 23.
    ELSE RETURN iIcon.
 END. /* IF */
 ELSE RETURN 23.
      
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

