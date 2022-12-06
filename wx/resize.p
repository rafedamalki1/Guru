Main change in version 2.2: Better handling of adm-folders

Guidelines (see also presentation and examples of use in the JukeBox demo, <installdir>\winsrc):

Load the library as a super procedure:

DEF VAR hResizeLib AS HANDLE NO-UNDO.
RUN ResizeLib.p PERSIST SET hResizeLib.
SESSION:ADD-SUPER-PROC(hResizeLib).

Each window needs to be initialized for the resize function, f.ex like this:
DYNAMIC-FUNCTION("SetOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,150,100,200,0).

150: All widgets with x-coordinate >= 150 pixels should follow right edge of window as default
100: All widgets with y-coordinate >= 100 pixels should follow bottom edge of window as default
200: Min x size for window
0: Min y size is design size (recommended: To make windows smaller than design size sometimes make problems)

NB! If there are additional resize rules (under) these should (must) be set before "SetOrgWinSize" since this function will also restore the previous size/position. To keep these values invoke this section in the .ini file:
[ResizeLib]
SaveSettingsTo=.\resize
AutoSaveSettings=yes


The default values can be overridden like this:
DYNAMIC-FUNCITON("setNoMoveX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"WidgetName1,WidgetName 2,..").

Be aware that if f.ex browsers are side-by-side only one of them can be resized. The one that shouldn't be resized must be told so:

DYNAMIC-FUNCITON("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"BrowseName").

Other override function examples:
DYNAMIC-FUNCTION("setAddResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"RECT-1,RECT-2").
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"RECT-3,RECT-4").
DYNAMIC-FUNCTION("setAddResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"brwBalance").
DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"fi-fTotBal").
DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"fi-fTotDue").
DYNAMIC-FUNCTION("setNoMoveY", THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"brwDetails,fi-cDesc").
DYNAMIC-FUNCTION("setAddMoveY", THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"fi-dDueDate").

At the WINDOW-RESIZED trigger do the function call:

DYNAMIC-FUNCTION("SetWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").

A suppressed window needs an extra initialization to adjust to current window size:

DYNAMIC-FUNCTION("SetWidgetResize",FRAME {&FRAME-NAME}:FIRST-CHILD,THIS-PROCEDURE:CURRENT-WINDOW,"Adjust","").

Be aware that text widget positioning cannot be overridden cause the NAME attribute has unknown value (use a char var view-as text).

A good policy is to clean up window setting at window-close, cause the settings are kept in temp-tables:

DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).

Remember that all frames must be SCROLLABLE to allow change of virtual-size.

DictView.w is an example of simple use.

Example with ADM folder:

Function calls in container, local-initialize (f.ex - depending on the overrides you need to set) : DYNAMIC-FUNCTION("setOrgWinSize", CURRENT-WINDOW,600,400,0,0).
- Normal initialization of window (all to the rigth of 600 should follow right edge, all below 400 should follow bottom edge, don't allow smaller size than design size)

DYNAMIC-FUNCTION("setNoResizeY" ,THIS-PROCEDURE:CURRENT-WINDOW,DYNAMIC-FUNCTION("getFrameHandle" IN h_f-obje), "obj").
- "obj" is the name of the frame of the viewer that lays on top of the folders. The function makes sure that it doesn't get resized in the Y-direction

DYNAMIC-FUNCTION("setNoMoveY" , THIS-PROCEDURE:CURRENT-WINDOW, DYNAMIC-FUNCTION("getFrameHandle" IN h_f-obje), "gnr,bnr,fnr,w-Bruk-gnr").
- Also some of the fill-ins would try to move based on default Y (400) so we need to prevent that. Do similar overrides for the pages.

The "getFrameHandle" function must be added to smart.i:

&IF DEFINED (getFrameHandleDefined) = 0 &THEN

&SCOPED-DEFINE getFrameHandleDefined

FUNCTION getFrameHandle RETURNS WIDGET-HANDLE ():
&IF "{&FRAME-NAME}" NE "" &THEN
RETURN FRAME {&FRAME-NAME}:HANDLE. /* Function return value. */
&ELSE RETURN ?.
&ENDIF
END FUNCTION.

&ENDIF

In each folder instance put in local-initialize:

DYNAMIC-FUNCTION("SetWidgetResize",FRAME {&FRAME-NAME}:FIRST-CHILD,THIS-PROCEDURE:CURRENT-WINDOW,"Adjust","").


 


PROCEDURE InitializeObject:
DYNAMIC-FUNCTION("setOrgWinSize:U,
	     THIS-PROCEDURE:CURRENT-WINDOW,
	     600,400,0,0). /* limit X, limit Y, min X, min Y */
(0,0) sets design size as minimum for window. 


ON WINDOW-RESIZED:
DYNAMIC-FUNCTION("setWidgetResize":U,
	     THIS-PROCEDURE:CURRENT-WINDOW,
	     THIS-PROCEDURE:CURRENT-WINDOW,
	     "Resize":U,"").

ON CLOSE OF THIS-PROCEDURE
    DYNAMIC-FUNCTION("setCleanUpResize":U,
                   THIS-PROCEDURE:CURRENT-WINDOW).

DYNAMIC-FUNCTION("setNoResizeY":U,
		    THIS-PROCEDURE:CURRENT-WINDOW,
		    FRAME {&FRAME-NAME}:HANDLE,
		    "RECT-1":U).

Optionally make the search button stay with the fill-ins:
DYNAMIC-FUNCTION("setNoMoveX":U,
	THIS-PROCEDURE:CURRENT-WINDOW,
	FRAME {&FRAME-NAME}:HANDLE,
	"btnSearch":U). /* can be comma-separated list of widgets */

DYNAMIC-FUNCTION("setNoResizeX":U,
	THIS-PROCEDURE:CURRENT-WINDOW,
	FRAME {&FRAME-NAME}:HANDLE,
	"RECT-1":U).

PROCEDURE InitializeObject:
DYNAMIC-FUNCTION("setWidgetResize":U,
		     FRAME {&FRAME-NAME}:FIRST-CHILD,
	      	     THIS-PROCEDURE:CURRENT-WINDOW,
	           "Adjust":U,"").
DEF VAR hResizeLib AS HANDLE NO-UNDO.
RUN ResizeLib.p PERSIST SET hResizeLib.
SESSION:ADD-SUPER-PROC(hResizeLib).

- f.ex in the login routine)
DYNAMIC-FUNCTION("setNoResizeY":U,
		    THIS-PROCEDURE:CURRENT-WINDOW,
		    FRAME {&FRAME-NAME}:HANDLE,
		    "RECT-1,brwOrders":U).

DYNAMIC-FUNCTION("setAddMoveY":U,
	    THIS-PROCEDURE:CURRENT-WINDOW,
	    FRAME {&FRAME-NAME}:HANDLE,
	    "fill-in_OrderLabel,brwOrders":U).
DYNAMIC-FUNCTION("setSplitBarY",
	    THIS-PROCEDURE:CURRENT-WINDOW, 	  	    btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},
	    NO). /* */
Also we must give all handles to the widgets that should follow
the move:
DYNAMIC-FUNCTION("setFollowSplitBarY",
	    THIS-PROCEDURE:CURRENT-WINDOW,
               btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},
	     STRING(brwCustomer:HANDLE) + "," +  
	     STRING(fill-in_OrderLabel:HANDLE) + "," + 
	     STRING(brwOrders:HANDLE)).

DYNAMIC-FUNCTION("setSplitBarYlimits",
	     btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},
	     150,100). /* Min from top and bottom */
 

Before the suppressed window is run, pull the splitbar(s) back to the original (design) position:
	DYNAMIC-FUNCTION("resetSplitBarPos":U,
		         THIS-PROCEDURE:CURRENT-WINDOW,0).
DYNAMIC-FUNCTION("setFollowSplitBarX":U,
	        THIS-PROCEDURE:CURRENT-WINDOW, 
	        btnSplitBarX:HANDLE IN FRAME frSplitBarX, 
	        cFollowXbar). /* the list of handles */
DYNAMIC-FUNCTION("setWidgetResize":U,
	        hSuppressedFrame:FIRST-CHILD,
  	        THIS-PROCEDURE:CURRENT-WINDOW,
	        "Adjust":U,"").

In this example with double layer of tab-folders we must 
adjust the size of the splitbar (and it’s frame) to current 
window-size when the splitbar is re-created:
FRAME frSplitBarX:HEIGHT-PIXELS = 
	FRAME frSplitBarX:HEIGHT-PIXELS + 
	DYNAMIC-FUNCTION("getCurrentDeltaY":U).
btnSplitBarX:HEIGHT-PIXELS IN FRAME frSplitBarX = 
	FRAME frSplitBarX:HEIGHT-PIXELS - 2.   
DYNAMIC-FUNCTION("setCleanUpSplitBar":U,
	    THIS-PROCEDURE:CURRENT-WINDOW,
	    btnSplitBarX:HANDLE IN FRAME frSplitBarX).
  
smart.i:
&IF DEFINED (getFrameHandleDefined) = 0 &THEN
  &SCOPED-DEFINE getFrameHandleDefined

  FUNCTION getFrameHandle RETURNS WIDGET-HANDLE ():
    &IF "{&FRAME-NAME}" NE "" &THEN
      RETURN FRAME {&FRAME-NAME}:HANDLE.  
    &ELSE
      RETURN ?.
    &ENDIF  
  END FUNCTION.

&ENDIF
ADM2 has a built in function, "getContainerHandle"
PROCEDURE repositionSplitBar:
DEFINE VARIABLE dMinWidth  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dMinHeight AS DECIMAL    NO-UNDO.
DEFINE VARIABLE hFolder    AS HANDLE     NO-UNDO.
DEFINE VARIABLE hPanel     AS HANDLE     NO-UNDO.
DEFINE VARIABLE iPage      AS INTEGER    NO-UNDO.

&SCOPED-DEFINE PANEL_WIDTH 58

{fnarg lockWindow TRUE}.

{get MinWidth  dMinWidth h_dyntreeview} .

ASSIGN btnSplitBar:COL IN FRAME {&FRAME-NAME} = MAX(btnSplitBar:COL  ,dMinWidth)
       btnSplitBar:COL    = MIN(btnSplitBar:COL, {&WINDOW-NAME}:WIDTH * 4 / 5)
       btnSplitBar:ROW    = 1
       btnSplitBar:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT
       coModule:WIDTH     = MAX(1,btnSplitBar:COL - coModule:COL + .3)
       coObjType:WIDTH    = coModule:WIDTH
       NO-ERROR.
RUN resizeObject IN h_dyntreeview (INPUT {&WINDOW-NAME}:HEIGHT - FRAME {&FRAME-NAME}:ROW - 1.25,
                                    INPUT btnSplitBar:COL).

/* Check whether the folder width needs to be increased or decreased */
{get ContainerHandle hFolder h_folder}.
{get ContainerHandle hpanel h_pupdsav}.

IF hFolder:column <= btnSplitBar:COL + btnSplitBar:WIDTH THEN  /* SplitBar is being moved to the right */
DO:
     RUN resizeObject IN h_folder (INPUT {&WINDOW-NAME}:HEIGHT - FRAME {&FRAME-NAME}:ROW + .9,
                                       INPUT {&WINDOW-NAME}:WIDTH - btnSplitBar:COL - btnSplitBar:WIDTH ).
     RUN repositionObject IN h_folder (INPUT 1.00, btnSplitBar:COL + btnSplitBar:WIDTH).

     RUN resizeObject  IN  h_pupdsav (hPanel:HEIGHT,
                                      IF {&WINDOW-NAME}:WIDTH - btnSplitBar:COL - 1 < {&PANEL_WIDTH}
                                      THEN {&WINDOW-NAME}:WIDTH - btnSplitBar:COL - 2
                                      ELSE {&PANEL_WIDTH}).

     RUN repositionObject IN h_pupdsav (hPanel:ROW,
                                        btnSplitBar:COL + 1 + ({&WINDOW-NAME}:WIDTH - btnSplitBar:COL - hPanel:WIDTH - 1) / 2 ).

     IF VALID-HANDLE(h_gscicview) THEN
       RUN centerObject(INPUT h_gscicview, INPUT 1).
     IF VALID-HANDLE(h_gsmmsview) THEN
       RUN centerObject(INPUT h_gsmmsview, INPUT 1).
     IF VALID-HANDLE(h_gsmmiview) THEN
       RUN centerObject(INPUT h_gsmmiview, INPUT 1).
     IF VALID-HANDLE(h_gsmitview) THEN
       RUN centerObject(INPUT h_gsmitview, INPUT 1).
     IF VALID-HANDLE(h_gscobviewt) THEN
       RUN centerObject(INPUT h_gscobviewt, INPUT 1).
....

IF VALID-HANDLE(h_gsmomviewt) THEN
       RUN centerObject(INPUT h_gsmomviewt, INPUT 1).
      IF VALID-HANDLE(h_gsmtmview) THEN
       RUN centerObject(INPUT h_gsmtmview, INPUT 1).

    IF VALID-HANDLE(h_gsmombrow) THEN DO:
       RUN resizeObject IN h_gsmombrow(6.91,{&WINDOW-NAME}:WIDTH - btnSplitBar:COL - 7).
       RUN repositionObject IN h_gsmombrow(2.67, btnSplitBar:COL + 4).
    END.

END. /* End splitbar being moved to right */
ELSE DO: /* Splitbar being moved to left */
   RUN repositionObject IN h_folder (INPUT 1.00, btnSplitBar:COL + btnSplitBar:WIDTH  ).
   RUN resizeObject IN h_folder (INPUT {&WINDOW-NAME}:HEIGHT - FRAME {&FRAME-NAME}:ROW + .9,
                                       INPUT {&WINDOW-NAME}:WIDTH - btnSplitBar:COL - btnSplitBar:WIDTH ).
   RUN repositionObject IN h_pupdsav (hPanel:ROW,
                                      IF {&WINDOW-NAME}:WIDTH - btnSplitBar:COL - 1  > {&PANEL_WIDTH}
                                      THEN  btnSplitBar:COL + 1 + ({&WINDOW-NAME}:WIDTH - btnSplitBar:COL - {&PANEL_WIDTH} - 1) / 2
                                      ELSE  btnSplitBar:COL + 2).
   RUN resizeObject  IN  h_pupdsav (hPanel:HEIGHT,
                                   IF {&WINDOW-NAME}:WIDTH - btnSplitBar:COL - 1 < {&PANEL_WIDTH}
                                   THEN {&WINDOW-NAME}:WIDTH - btnSplitBar:COL - 3
                                   ELSE {&PANEL_WIDTH}).
   IF VALID-HANDLE(h_gscicview) THEN
      RUN centerObject(INPUT h_gscicview, INPUT 2).

   IF VALID-HANDLE(h_gsmmsview) THEN
       RUN centerObject(INPUT h_gsmmsview, INPUT 2).
   IF VALID-HANDLE(h_gsmmiview) THEN
       RUN centerObject(INPUT h_gsmmiview, INPUT 2).

IF VALID-HANDLE(h_gsmitview) THEN
       RUN centerObject(INPUT h_gsmitview, INPUT 2).
   IF VALID-HANDLE(h_gscobviewt) THEN
       RUN centerObject(INPUT h_gscobviewt, INPUT 2).
    IF VALID-HANDLE(h_gsmomviewt) THEN
       RUN centerObject(INPUT h_gsmomviewt, INPUT 2).
    IF VALID-HANDLE(h_gsmombrow) THEN DO:
       RUN repositionObject IN h_gsmombrow(2.67, btnSplitBar:COL + 4).
       RUN resizeObject IN h_gsmombrow(6.91,{&WINDOW-NAME}:WIDTH - btnSplitBar:COL - 7).
    END.
    IF VALID-HANDLE(h_gsmtmview) THEN
       RUN centerObject(INPUT h_gsmtmview, INPUT 2).

END.  /* End splitbar being moved to left */

 /* the calls to the folder is untabbing the tab folders. Reset tabs */
{get CurrentPage iPage}.
IF iPage > 2 THEN DO:
    {set CurrentPage 1}.
END.

RUN ShowCurrentPage IN h_folder (IF giLastTabPage = 2 THEN 2 ELSE 1). /* Select the first tab folder */
{set CurrentPage iPage}.

{fnarg lockWindow FALSE}.

END PROCEDURE.

Fill-ins are not resized by default. To resize:
DYNAMIC-FUNCTION("setAddResizeX":U,
	    THIS-PROCEDURE:CURRENT-WINDOW,
	    FRAME {&FRAME-NAME}:HANDLE,
	    "fi-cTotal":U). 
Function to lock window update:
DYNAMIC-FUNCTION("setLockWindowUpdate":U,TRUE).   
Function to display all widgets with names and types:
DYNAMIC-FUNCTION ("ShowMeTheWidgets":U,
	    hWidget,	/* handle to window, frame.. */ 
	    0,  		/* level, always 0  		  */  
	    "", 		/* try "bottom-up" 		  */
    	    "c:\temp\widgetlist.txt" /* result */
	    ). 
