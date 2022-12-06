&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/**-----------------------------------------------------------------------
Hand Made Software Splash Window - wSplash.w
Version 1.0

Disappears after 5 seconds or when user clicks anywhere on the Splash Window.

Copyright Hand Made Software 1997. HandMadeSoftware@theSea.com.

This program may be distributed freely without royalties. No warranties 
expressed or implied.

Splash.p and all associated programs that call Windows DLLs to make the
splash window are used by permission of Jurjen Dijkstra.

History
09/01/97 Initial Creation.
09/09/97 Given to Jurjen Dijkstra to distribute from his Web page.
         http://www.global-shared.com
09/13/97 JD: tiny improvements in the Definitions section
-----------------------------------------------------------------------**/


CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_running)=0 &THEN
  DEFINE INPUT PARAMETER ichBanner1     AS CHARACTER    NO-UNDO.
  DEFINE INPUT PARAMETER ichBanner2     AS CHARACTER    NO-UNDO.
  DEFINE INPUT PARAMETER ichBanner3     AS CHARACTER    NO-UNDO.
  DEFINE INPUT PARAMETER ichBanner4     AS CHARACTER    NO-UNDO.
  DEFINE INPUT PARAMETER ichBanner5     AS CHARACTER    NO-UNDO.
  DEFINE INPUT PARAMETER iinPause       AS INTEGER      NO-UNDO.
&ELSE  
  DEFINE VARIABLE ichBanner1  AS CHARACTER NO-UNDO INITIAL "Sample Splash Window".
  DEFINE VARIABLE ichBanner2  AS CHARACTER NO-UNDO INITIAL "".
  DEFINE VARIABLE ichBanner3  AS CHARACTER NO-UNDO INITIAL "".
  DEFINE VARIABLE ichBanner4  AS CHARACTER NO-UNDO INITIAL "".
  DEFINE VARIABLE ichBanner5  AS CHARACTER NO-UNDO INITIAL "".
  DEFINE VARIABLE iinPause    AS INTEGER   NO-UNDO INITIAL 5.
&ENDIF

IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
   MESSAGE "This procedure (wSplash.w) should be called persistent, for example from SplashDemo.p"
           VIEW-AS ALERT-BOX.
   RETURN.
END.


{windows.i}

&scop GWL_EXSTYLE         -20
&scop WS_EX_LAYERED       524288
&SCOPED-DEFINE LWA_COLORKEY 1
&scop LWA_ALPHA           2
&scop WS_EX_TRANSPARENT   32

DEFINE VARIABLE thisHWND AS INTEGER NO-UNDO.

PROCEDURE SetWindowLongA EXTERNAL "user32.dll":
  def INPUT PARAM HWND AS LONG.     
  def INPUT PARAM nIndex AS LONG.   
  def INPUT PARAM dwNewLong AS LONG.
  DEF RETURN PARAM stat AS LONG.
END.

PROCEDURE SetLayeredWindowAttributes EXTERNAL "user32.dll":
  def INPUT PARAM HWND AS LONG.
  def INPUT PARAM crKey AS LONG.
  def INPUT PARAM bAlpha AS SHORT.
  def INPUT PARAM dwFlagsas AS LONG.
  DEF RETURN PARAM stat AS SHORT.
END.

PROCEDURE GetWindowLongA EXTERNAL "user32.dll":
  def INPUT PARAM HWND AS LONG.
  def INPUT PARAM nIndex AS LONG.   
  DEF RETURN PARAM flgs AS LONG.
END.
           
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-1 finBanner1 finBanner2 finBanner3 ~
finBanner4 finBanner5 
&Scoped-Define DISPLAYED-OBJECTS finBanner1 finBanner2 finBanner3 ~
finBanner4 finBanner5 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE finBanner1 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 35.13 BY 1.25
     FGCOLOR 1 FONT 17 NO-UNDO.

DEFINE VARIABLE finBanner2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 54.75 BY .83
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE finBanner3 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 50.63 BY .83
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE finBanner4 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 50.63 BY .83
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE finBanner5 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 52.75 BY .75
     FONT 0 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "BILDER\elpool_vit.gif":U
     SIZE 23 BY 3.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     finBanner1 AT ROW 5.54 COL 15.5 COLON-ALIGNED NO-LABEL
     finBanner2 AT ROW 7.71 COL 3.25 NO-LABEL
     finBanner3 AT ROW 8.67 COL 5.38 COLON-ALIGNED NO-LABEL
     finBanner4 AT ROW 9.5 COL 5.5 COLON-ALIGNED NO-LABEL
     finBanner5 AT ROW 10.75 COL 4.38 NO-LABEL
     IMAGE-1 AT ROW 1.13 COL 1.63
    WITH 1 DOWN NO-BOX NO-HIDE KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 58.25 BY 10.67
         BGCOLOR 15 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 10.92
         WIDTH              = 59
         MAX-HEIGHT         = 23.08
         MAX-WIDTH          = 122.38
         VIRTUAL-HEIGHT     = 23.08
         VIRTUAL-WIDTH      = 122.38
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = 15
         FGCOLOR            = 15
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN finBanner2 IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN finBanner5 IN FRAME F-Main
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
     
IF VALID-HANDLE(THIS-PROCEDURE) THEN
    DELETE PROCEDURE THIS-PROCEDURE.
  

    RETURN NO-APPLY.
    

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

{src/adm/template/windowmn.i}
/* Now enable the interface and wait for the exit condition.            */
/** Added by me **/
RUN dispatch ('initialize':U).
ASSIGN finBanner1:SCREEN-VALUE = ichBanner1
       finBanner2:SCREEN-VALUE = ichBanner2
       finBanner3:SCREEN-VALUE = ichBanner3
       finBanner4:SCREEN-VALUE = ichBanner4
       finBanner5:SCREEN-VALUE = ichBanner5.

RUN MkSplash.p ({&WINDOW-NAME}:HWND, YES).

VIEW FRAME F-Main.
RUN GetParent IN Guru.Konstanter:hpApi (W-Win:HWND, OUTPUT thishwnd).

DEF VAR stat AS INTEGER NO-UNDO.
DEF VAR flgs AS INTEGER NO-UNDO.

RUN GetWindowLongA(thishwnd, {&GWL_EXSTYLE}, OUTPUT flgs) .
RUN SetWindowLongA(thishwnd, {&GWL_EXSTYLE}, flgs + {&WS_EX_LAYERED},
                   OUTPUT stat).
RUN MakeTransparent.


WAIT-FOR "MOUSE-SELECT-CLICK" OF IMAGE-1 PAUSE iinPause.
APPLY "MOUSE-SELECT-CLICK" TO IMAGE-1.
/* Inlagt 20060601 */
APPLY "CLOSE" TO THIS-PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY finBanner1 finBanner2 finBanner3 finBanner4 finBanner5 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE IMAGE-1 finBanner1 finBanner2 finBanner3 finBanner4 finBanner5 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MakeTransparent W-Win 
PROCEDURE MakeTransparent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR stat AS INTEGER NO-UNDO.
  DO WITH FRAME {&FRAME-NAME} :
  
  
  RUN SetLayeredWindowAttributes(thisHwnd, 
                                 color-table:GET-RGB-VALUE(FRAME {&FRAME-NAME}:BGCOLOR), 
                                 200, 
                                 2, 
                                 OUTPUT stat).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
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

