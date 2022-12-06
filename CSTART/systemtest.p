/*systemtest.p*/
DEFINE VARIABLE cOutputFile AS CHARACTER  NO-UNDO INIT 'systemtest.txt'.
DEFINE VARIABLE cDefaultFixedFont AS CHARACTER  NO-UNDO FORMAT "X(35)".
DEFINE VARIABLE cDefaultFont AS CHARACTER  NO-UNDO FORMAT "X(35)".
DEFINE VARIABLE cDefaultV6UpdateFont AS CHARACTER   NO-UNDO FORMAT "X(35)".
DEFINE VARIABLE c AS CHARACTER   NO-UNDO FORMAT "X(35)".


DEFINE TEMP-TABLE TTSession 
    FIELD cAttrName AS CHARACTER 
    FIELD cAttrValue AS CHARACTER. 


FUNCTION GetComputerName RETURNS CHARACTER    
      ()  FORWARD.   
    
    OUTPUT TO VALUE(cOutputFile).
    
    PUT UNFORM "Date: " TODAY "   Time: " STRING(TIME,"HH:MM:SS")  '  Operating System: ' OPSYS SKIP.
    
    IF OPSYS = "WIN32" THEN 
        PUT UNFORM 'Machine name: ' GetComputerName() SKIP.
    
    PUT UNFORM 'Display type: ' SESSION:DISPLAY-TYPE SKIP.
    IF SESSION:BATCH-MODE THEN PUT UNFORM 'BATCH-MODE' SKIP.
    
    IF OPSYS = "WIN32" THEN 
        RUN GetScreenProprieties.
    RUN GetProgressVersion.
    RUN GetEnv.
    RUN GetStartupParameters. /* Hive results only in 9.1D07 and above*/
    RUN Get18N.
    RUN FontReport.    
    RUN GetSessionAttrs.    
    OUTPUT CLOSE.      

    file-info:file-name = cOutputFile.
    
/*     IF OPSYS = "WIN32" AND NOT session:BATCH-MODE THEN             */
/*         os-command no-wait notepad value(file-info:full-pathname). */
 

PROCEDURE FontReport:
    
    PUT UNFORM SKIP(2) "<<FONT REPORT 1>>" SKIP(2).

    /* Font characteristics */
    PUT UNFORM
     "SESSION:PIXELS-PER-ROW => " SESSION:PIXELS-PER-ROW  SKIP
     "SESSION:PIXELS-PER-COL => " SESSION:PIXELS-PER-COL SKIP.
     
    DEFINE VARIABLE i AS INTEGER    NO-UNDO.
    DO i = 0 TO 20:
        PUT UNFORM
         "FONT-TABLE:GET-TEXT-HEIGHT-PIXELS(" i ")) => "
           FONT-TABLE:GET-TEXT-HEIGHT-PIXELS(i) SKIP
         'FONT-TABLE:GET-TEXT-WIDTH-PIXELS("12345678901234567890",' i ') / 20  => '
           FONT-TABLE:GET-TEXT-WIDTH-PIXELS("12345678901234567890",i) / 20 SKIP
         'FONT-TABLE:GET-TEXT-WIDTH-PIXELS("12345678901234567890",' i ')  => '
           FONT-TABLE:GET-TEXT-WIDTH-PIXELS("12345678901234567890",i)  SKIP.
    END.
    
    GET-KEY-VALUE SECTION "startup" KEY "DefaultFixedFont" VALUE cDefaultFixedFont.
    GET-KEY-VALUE SECTION "startup" KEY "DefaultFont" VALUE cDefaultFont.
    GET-KEY-VALUE SECTION "startup" KEY "DefaultV6UpdateFont" VALUE cDefaultV6UpdateFont.
    
    PUT UNFORM skip(3) 
        'DefaultFixed font:' cDefaultFixedFont skip 
        'DefaultFont: ' cDefaultFont skip
        'DefaultV6UpdateFont ' cDefaultV6UpdateFont SKIP(3)
        .
    
    DO i = 0 TO 20:
        GET-KEY-VALUE SECTION "fonts" KEY "font" + STRING(i) VALUE c.
        PUT UNFORM "font" i "  => " c SKIP.
    END.
    
END PROCEDURE.


PROCEDURE GetComputerNameA EXTERNAL "KERNEL32.DLL":
 DEFINE OUTPUT PARAMETER ptrToString AS MEMPTR.
 DEFINE INPUT-OUTPUT PARAMETER intBufferSize AS LONG.
 DEFINE RETURN PARAMETER intResult AS SHORT.
END PROCEDURE.

FUNCTION GetComputerName RETURNS CHARACTER ():
 DEFINE VARIABLE chrComputerName AS CHARACTER NO-UNDO FORMAT "X(16)".
 DEFINE VARIABLE intBufferSize AS INTEGER NO-UNDO INITIAL 16.
 DEFINE VARIABLE intResult AS INTEGER NO-UNDO.
 DEFINE VARIABLE ptrToString AS MEMPTR NO-UNDO.


    SET-SIZE(ptrToString) = 16.

    RUN GetComputerNameA (
        OUTPUT ptrToString, 
        INPUT-OUTPUT intBufferSize,
        OUTPUT intResult
        ).

    IF intResult = 1 THEN
    DO:
        ASSIGN chrComputerName = GET-STRING(ptrToString,1).
        RETURN chrComputerName.
    END.
    ELSE
        RETURN "Getting the computer name - Buffer size is too small. Must be as least " +
        STRING(intBufferSize).

    SET-SIZE(ptrToString) = 0.
END.

PROCEDURE GetACP EXTERNAL "kernel32":        
   DEFINE RETURN PARAMETER  iCodePage AS LONG .
END PROCEDURE.




PROCEDURE GetDatabaseInfo:
DEFINE VARIABLE i AS INTEGER    NO-UNDO.
                                                
    PUT SKIP(2) "<< DATABASES >>: " NUM-DBS SKIP.

    REPEAT i = 1 TO NUM-DBS:  
        PUT UNFORM SKIP(2) 'DATABASE: ' LDBNAME(i) SKIP 
            '  DBTYPE:       ' DBTYPE(i) SKIP
            '  DBVERSION:    ' DBVERSION(i) SKIP
            '  Parameters:   ' DBPARAM(i) SKIP
            '  DBCODEPAGE:   ' DBCODEPAGE(i) SKIP
            '  DBCOLLATION:  ' DBCOLLATION(i) SKIP
        .
    END.

END.


PROCEDURE Get18N:
 DEFINE VARIABLE iCodePage AS INTEGER NO-UNDO.

    PUT UNFORM SKIP(2) "<< IN18N >>" SKIP(2).

    /*Get Windows codepage */
    IF OPSYS = "WIN32" THEN 
        RUN GetACP(OUTPUT iCodePage).
    
    
    /* I18N Progress session parameters */
    PUT UNFORM
        'Windows codepage: ' iCodePage  SKIP
        'cpinternal: ' session:cpinternal  SKIP
        'cpstream: ' SESSION:CPSTREAM SKIP
        'cpprint: ' SESSION:CPPRINT SKIP
        'cpcase: ' SESSION:CPCASE SKIP
        'cpcoll: ' SESSION:CPCOLL SKIP
        'cplog: ' SESSION:CPLOG SKIP
        'cprcodein: ' SESSION:CPRCODEIN SKIP
        'cprcodeout: ' SESSION:CPRCODEOUT SKIP
        'cpterm: ' SESSION:CPTERM                               
        . 
     
    /*Get databases codepage */
    
        RUN GetDatabaseInfo.
END.


PROCEDURE GetProgressVersion:
DEFINE VARIABLE cinp      AS CHARACTER           NO-UNDO. /* hold 1st line of version file */
DEF VAR dlcValue AS CHAR NO-UNDO.
DEF VAR patchLevel AS CHAR NO-UNDO.
DEF VAR i AS INT NO-UNDO.

  IF OPSYS = "Win32":U THEN /* Get DLC from Registry */
    GET-KEY-VALUE SECTION "Startup":U KEY "DLC":U VALUE dlcValue.

  IF (dlcValue = "" OR dlcValue = ?) THEN DO:
    ASSIGN dlcValue = OS-GETENV("DLC":U). /* Get DLC from environment */
      IF (dlcValue = "" OR dlcValue = ?) THEN DO: /* Still nothing? */
        ASSIGN patchLevel = "".
        RETURN.
      END.
  END.
  FILE-INFO:FILE-NAME = dlcValue + "/version":U.
  IF FILE-INFO:FULL-PATHNAME NE ? THEN DO: /* Read the version file */
    INPUT FROM VALUE(FILE-INFO:FULL-PATHNAME).
      IMPORT UNFORMATTED cinp. /* Get the first line */
    INPUT CLOSE.
    /*
     * As of version 9.1D just append everything from the version file
     * after the version from PROVERSION property
     */
    LEVEL:
    DO i = 2 TO NUM-ENTRIES(cinp," ":U):
      IF ENTRY(i,cinp," ") BEGINS PROVERSION THEN DO:
        ASSIGN patchLevel = REPLACE(ENTRY(i,cinp," "),PROVERSION,"").
        IF patchLevel = '' THEN patchLevel = 'NO PATCH INSTALLED'. 
	LEAVE LEVEL.
      END.
    END.
  END.
  PUT UNFORM 'Progress Version:' PROVERSION patchlevel SKIP (2).

END PROCEDURE.      


/*--------------------------------------------------------*/


PROCEDURE GetStartupParameters:
  IF NOT AVAILABLE TTSession THEN
      RUN SessionAttrs.
  FIND FIRST TTSession WHERE 
      TTSession.cAttrName = 'STARTUP-PARAMETERS' NO-ERROR.  /*could not refere the SESSION:STARTUP-PARAMETERS directly as is not valid prior to 9.1D07*/

  IF AVAILABLE TTSession THEN 
    PUT UNFORM SKIP 'Session startup parameters: '  SKIP TTSession.cAttrValue.
  ELSE PUT UNFORM 'Could not retrieve the session startup parameters'.
END.

PROCEDURE GetSessionAttrs:
  PUT UNFORM SKIP(2) 'SESSION ATTRIBUTES:  ' SKIP.

  IF NOT AVAILABLE TTSession THEN
      RUN SessionAttrs.
  
  FOR EACH TTSession:  
      PUT UNFORM '     ' TTSession.cAttrName ': ' TTSession.cAttrValue SKIP.
  END.

END.


PROCEDURE SessionAttrs: 
    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    DEFINE VARIABLE hCall AS HANDLE      NO-UNDO.

    FOR EACH TTSession:
        DELETE TTSession.
    END.

    CREATE CALL hCall.
    DO i = 1 TO NUM-ENTRIES(LIST-QUERY-ATTRS (SESSION)) TRANSACTION:
        CREATE TTSession.
        
        cAttrName = ENTRY(i, LIST-QUERY-ATTRS(SESSION)).
        hCall:CLEAR(). 
        hCall:IN-HANDLE = SESSION.
        hCall:CALL-TYPE = GET-ATTR-CALL-TYPE.
        hCall:CALL-NAME = cAttrName.
        hCall:INVOKE() NO-ERROR.
        
        IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
            IF ERROR-STATUS:GET-NUMBER(ERROR-STATUS:NUM-MESSAGES) = 4065 THEN
                cAttrValue = ' << Method with parameters>>'.
             ELSE cAttrValue = 'ERROR retriving the value'.
        END.
        ELSE cAttrValue = string(hCall:RETURN-VALUE)  .        
        
    END.
    
    DELETE OBJECT hCall.
END PROCEDURE.


/* &GLOBAL-DEFINE SM_CXVIRTUALSCREEN      78 */
/* &GLOBAL-DEFINE SM_CYVIRTUALSCREEN      79 */
/* {GetSystemMetrics.i}. */
/*---------------------------------------------------------------
File Name: GetSystemMetrics.i
Purpose: Define and document the numerous system configurations and settings that may be accessed by invoking the WINAPI GetSystemMetrics function.
Syntax of invoking the GetSystemMetrics API: RUN GetSystemMetrics(INPUT piSetting , OUTPUT piResult). 
Parameters:
INPUT piSetting AS INTEGER * The system metric or configuration setting to be retrieved.
RETURN piResult AS INTEGER * A dimention in PIXELS or an INTEGER return value.

Note that the system metrics can vary from display to display.  If the function succeeds, the return value is the requested system metric or configuration setting.  If the function fails, the return value is 0. GetLastError does not provide extended error information.  The INPUT parameter can be one of the following values. Note that all SM_CX* values are widths and all SM_CY* values are heights.  Also note that all settings designed to return Boolean data represent TRUE as any nonzero value, and FALSE as a zero value.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_ARRANGE   56
/*---------------------------------------------------------------
The SM_ARRANGE setting specifies how the system arranges minimized windows, and consists of a starting position and a direction.  The starting position can be one of the following values:
0 ARW_BOTTOMLEFT  Start at the lower*left corner of the work area. 
1 ARW_BOTTOMRIGHT Start at the lower*right corner of the work area.
2 ARW_TOPLEFT     Start at the upper*left corner of the work area. 
3 ARW_TOPRIGHT    Start at the upper*right corner of the work area.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CLEANBOOT   67
/*---------------------------------------------------------------
The SM_CLEANBOOT setting specifies how the system is started:
0 Normal boot
1 Fail*safe boot
2 Fail*safe with network boot
A fail*safe boot (also called SafeBoot, Safe Mode, or Clean Boot)
bypasses the user startup files.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CMONITORS   80
/*---------------------------------------------------------------
The SM_CMONITORS setting specifies the number of display monitors on a desktop. Note that GetSystemMetrics(SM_CMONITORS) counts only visible display monitors. The EnumDisplayMonitors WINAPI counts both visible display monitors and invisible pseudo*monitors that are associated with mirroring drivers. An invisible pseudo*monitor is associated with a pseudo*device used to mirror application drawing for remoting or other purposes.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CMOUSEBUTTONS   43
/*---------------------------------------------------------------
The SM_CMOUSEBUTTONS setting specifies the number of buttons on a mouse, or zero if no mouse is installed.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXBORDER   5
/*---------------------------------------------------------------
The SM_CXBORDER setting specifies the width of a window border in pixels. This is equivalent to the SM_CXEDGE value for windows with the 3*D look.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXCURSOR   13
/*---------------------------------------------------------------
The SM_CXCURSOR setting specifies the width of a cursor, in pixels.  The system cannot create cursors of other sizes.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXDLGFRAME   7
&GLOBAL-DEFINE SM_CXFIXEDFRAME   7
/*---------------------------------------------------------------
The SM_CXDLGFRAME or the SM_CXFIXEDFRAME setting specifies The thickness, in pixels, of the frame around the perimeter of a window that has a caption but is not sizable. SM_CXFIXEDFRAME is the height of the horizontal border, and SM_CYFIXEDFRAME is the width of the vertical border.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXDOUBLECLK   36
/*---------------------------------------------------------------
The SM_CXDOUBLECLK setting specifies the width of the rectangle around the location of a first click in a double*click sequence in pixels. The second click must occur within the rectangle that is defined by SM_CXDOUBLECLK and SM_CYDOUBLECLK for the system to consider the two clicks a double*click. The two clicks must also occur within a given time.  To set the width of the double*click rectangle, call SystemParametersInfo with SPI_SETDOUBLECLKWIDTH.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXDRAG   68
/*---------------------------------------------------------------
The SM_CXDRAG setting specifies the number of pixels on either side of a mouse*down point that the mouse pointer can move before a drag operation begins. This allows the user to click and release the mouse button easily without unintentionally starting a drag operation. If this value is negative, it is subtracted from the left of the mouse*down point and added to the right of it.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXEDGE   45
/*---------------------------------------------------------------
The SM_CXEDGE setting specifies the width of a 3*D border in pixels.  This metric is the 3*D counterpart of SM_CXBORDER.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXFOCUSBORDER   83
/*---------------------------------------------------------------
The SM_CXFOCUSBORDER setting specifies the width. in pixels, of the left and right edges of the focus rectangle that the DrawFocusRect draws.  This System Metric value is not supported on Windows 2000.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXFRAME   32
&GLOBAL-DEFINE SM_CXSIZEFRAME   32
/*---------------------------------------------------------------
The SM_CXFRAME or the SM_CXSIZEFRAME setting specifies the thickness, in pixels, of the sizing border around the perimeter of a window that can be resized. SM_CXSIZEFRAME is the width of the horizontal border, and SM_CYSIZEFRAME is the height of the vertical border.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXFULLSCREEN   16
/*---------------------------------------------------------------
The SM_CXFULLSCREEN setting specifies the width, in pixels, of the client area for a full*screen window on the primary display monitor.  To get the coordinates of the portion of the screen that is not obscured by the system taskbar or by application desktop toolbars, call the SystemParametersInfo function with the SPI_GETWORKAREA value.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXHSCROLL   21
/*---------------------------------------------------------------
The SM_CXHSCROLL setting specifies the width, in pixels, of the arrow bitmap on a horizontal scroll bar.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXHTHUMB   10
/*---------------------------------------------------------------
The SM_CXHTHUMB setting specifies the width, in pixels, of the thumb box in a horizontal scroll bar.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXICON   11
/*---------------------------------------------------------------
The SM_CXICON setting specifies the default width, in pixels, of an icon.  The LoadIcon function can load only icons with the dimensions that SM_CXICON and SM_CYICON specifies.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXICONSPACING   38
/*---------------------------------------------------------------
The SM_CXICONSPACING setting specifies the width, in pixels, of a grid cell for items in large icon view.  Each item fits into a rectangle of size SM_CXICONSPACING by SM_CYICONSPACING when arranged. This value is always greater than or equal to SM_CXICON.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXMAXIMIZED   61
/*---------------------------------------------------------------
The SM_CXMAXIMIZED setting specifies the default width, in pixels, of a maximized top*level window on the primary display monitor.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXMAXTRACK   59
/*---------------------------------------------------------------
The SM_CXMAXTRACK setting specifies the default maximum width, in pixels, of a window that has a caption and sizing borders. This metric refers to the entire desktop. The user cannot drag the window frame to a size larger than these dimensions. A window can override this value by processing the WM_GETMINMAXINFO message.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXMENUCHECK   71
/*---------------------------------------------------------------
The SM_CXMENUCHECK setting specifies the width, in pixels, of the default menu check*mark bitmap.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXMENUSIZE   54
/*---------------------------------------------------------------
The SM_CXMENUSIZE setting specifies the width, in pixels, of menu bar buttons, such as the child window close button that is used in the multiple document interface.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXMIN   28
/*---------------------------------------------------------------
The SM_CXMIN setting specifies the minimum width, in pixels, of a window.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXMINIMIZED   57
/*---------------------------------------------------------------
The SM_CXMINIMIZED setting specifies the width of a minimized window in pixels.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXMINSPACING   47
/*---------------------------------------------------------------
The SM_CXMINSPACING setting specifies the width, in pixels, of a grid cell for a minimized window. Each minimized window fits into a rectangle this size when arranged. This value is always greater than or equal to SM_CXMINIMIZED.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXMINTRACK   34
/*---------------------------------------------------------------
The SM_CXMINTRACK setting specifies the minimum tracking width of a window, in pixels. The user cannot drag the window frame to a size smaller than these dimensions. A window can override this value by processing the WM_GETMINMAXINFO message.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXPADDEDBORDER   92
/*---------------------------------------------------------------
The SM_CXPADDEDBORDER setting specifies the amount of border padding for captioned windows, in pixels.  This value is not supported on Windows XP/2000.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXSCREEN   0
/*---------------------------------------------------------------
The SM_CXSCREEN setting specifies the width of the screen of the primary display monitor, in pixels. This is the same value obtained by calling GetDeviceCaps as follows: GetDeviceCaps( hdcPrimaryMonitor, HORZRES).
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXSIZE   30
/*---------------------------------------------------------------
The SM_CXSCREEN setting specifies the width of a button in a window caption or title bar, in pixels.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXSMICON   49
/*---------------------------------------------------------------
The SM_CXSMICON setting specifies the recommended width of a small icon, in pixels. Small icons typically appear in window captions and in small icon view.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXSMSIZE   52
/*---------------------------------------------------------------
The SM_CXSMSIZE setting specifies the width of small caption buttons, in pixels.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXVIRTUALSCREEN   78
/*---------------------------------------------------------------
The SM_CXVIRTUALSCREEN setting specifies the width of the virtual screen, in pixels. The virtual screen is the bounding rectangle of all display monitors. The SM_XVIRTUALSCREEN metric is the coordinates for the left side of the virtual screen.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CXVSCROLL   2
/*---------------------------------------------------------------
The SM_CXVSCROLL setting specifies the width of a vertical scroll bar, in pixels.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYBORDER   6
/*---------------------------------------------------------------
The SM_CYBORDER setting specifies the height of a window border, in pixels. This is equivalent to the SM_CYEDGE value for windows with the 3*D look.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYCAPTION   4
/*---------------------------------------------------------------
The SM_CYCAPTION setting specifies the height of a caption area, in pixels.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYCURSOR   14
/*---------------------------------------------------------------
The SM_CYCURSOR setting specifies the height of a cursor, in pixels. The system cannot create cursors of other sizes.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYDLGFRAME   8
&GLOBAL-DEFINE SM_CYFIXEDFRAME   8
/*---------------------------------------------------------------
The SM_CYDLGFRAME or the SM_CYFIXEDFRAME setting specifies the thickness of the frame around the perimeter of a window that has a caption but is not sizable, in pixels. SM_CXFIXEDFRAME is the height of the horizontal border, and SM_CYFIXEDFRAME is the width of the vertical border.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYDOUBLECLK   37
/*---------------------------------------------------------------
The SM_CYDOUBLECLK setting specifies the height of the rectangle around the location of a first click in a double*click sequence, in pixels. The second click must occur within the rectangle defined by SM_CXDOUBLECLK and SM_CYDOUBLECLK for the system to consider the two clicks a double*click. The two clicks must also occur within a specified time.  To set the height of the double*click rectangle, call SystemParametersInfo with SPI_SETDOUBLECLKHEIGHT. 
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYDRAG   69
/*---------------------------------------------------------------
The SM_CYDRAG setting specifies the number of pixels above and below a mouse*down point that the mouse pointer can move before a drag operation begins. This allows the user to click and release the mouse button easily without unintentionally starting a drag operation. If this value is negative, it is subtracted from above the mouse*down point and added below it.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYEDGE   46
/*---------------------------------------------------------------
The SM_CYEDGE setting specifies the height height of a 3*D border, in pixels. This is the 3*D counterpart of SM_CYBORDER.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYFOCUSBORDER   84
/*---------------------------------------------------------------
The SM_CYFOCUSBORDER setting specifies the height of the top and bottom edges of the focus rectangle drawn by DrawFocusRect. This value is in pixels.  This value is not supported on Windows 2000.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYFRAME   33
&GLOBAL-DEFINE SM_CYSIZEFRAME   33
/*---------------------------------------------------------------
The SM_CYCAPTION or the SM_CYSIZEFRAME setting specifies the thickness of the sizing border around the perimeter of a window that can be resized, in pixels. SM_CXSIZEFRAME is the width of the horizontal border, and SM_CYSIZEFRAME is the height of the vertical border.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYFULLSCREEN   17
/*---------------------------------------------------------------
The SM_CYFULLSCREEN setting specifies the height of the client area for a full*screen window on the primary display monitor, in pixels. To get the coordinates of the portion of the screen not obscured by the system taskbar or by application desktop toolbars, call the SystemParametersInfo function with the SPI_GETWORKAREA value.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYHSCROLL   3
/*---------------------------------------------------------------
The SM_CYHSCROLL setting specifies the height of a horizontal scroll bar, in pixels.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYICON   12
/*---------------------------------------------------------------
The SSM_CYICON setting specifies the efault height of an icon, in pixels. The LoadIcon function can load only icons with the dimensions SM_CXICON and SM_CYICON.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYCAPTION   39
/*---------------------------------------------------------------
The SM_CYICONSPACING setting specifies the height of a grid cell for items in large icon view, in pixels. Each item fits into a rectangle of size SM_CXICONSPACING by SM_CYICONSPACING when arranged. This value is always greater than or equal to SM_CYICON.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYKANJIWINDOW   18
/*---------------------------------------------------------------
The SM_CYKANJIWINDOW setting specifies the height of the Kanji window at the bottom of the screen, in pixels for double byte character set versions of the system.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYMAXIMIZED   62
/*---------------------------------------------------------------
The SM_CYMAXIMIZED setting specifies the default height, in pixels, of a maximized top*level window on the primary display monitor.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYMAXTRACK   60
/*---------------------------------------------------------------
The SM_CYMAXTRACK setting specifies the default maximum height of a window that has a caption and sizing borders, in pixels. This metric refers to the entire desktop. The user cannot drag the window frame to a size larger than these dimensions. A window can override this value by processing the WM_GETMINMAXINFO message.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYMENU   15
/*---------------------------------------------------------------
The SM_CYMENU setting specifies the height of a single*line menu bar, in pixels.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYMENUCHECK   72
/*---------------------------------------------------------------
The SM_CYMENUCHECK setting specifies the height of the default menu check*mark bitmap, in pixels.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYMENUSIZE   55
/*---------------------------------------------------------------
The SM_CYMENUSIZE setting specifies the height of menu bar buttons, such as the child window close button that is used in the multiple document interface, in pixels.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYMIN   29
/*---------------------------------------------------------------
The SM_CYMIN setting specifies the  minimum height of a window, in pixels. 
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYMINIMIZED   58
/*---------------------------------------------------------------
The SM_CYMINIMIZED setting specifies the height of a minimized window, in pixels.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYMINSPACING   48
/*---------------------------------------------------------------
The SM_CYMINSPACING setting specifies the height of a grid cell for a minimized window, in pixels. Each minimized window fits into a rectangle this size when arranged. This value is always greater than or equal to SM_CYMINIMIZED.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYMINTRACK   35
/*---------------------------------------------------------------
The SM_CYMINSPACING setting specifies the minimum tracking height of a window, in pixels. The user cannot drag the window frame to a size smaller than these dimensions. A window can override this value by processing the WM_GETMINMAXINFO message.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYSCREEN   1
/*---------------------------------------------------------------
The SM_CYSCREEN setting specifies the height of the screen of the primary display monitor, in pixels. This is the same value obtained by calling GetDeviceCaps as follows: GetDeviceCaps( hdcPrimaryMonitor, VERTRES).
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYSIZE   31
/*---------------------------------------------------------------
The SM_CYSIZE setting specifies the height of a button in a window caption or title bar, in pixels.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYSMCAPTION   51
/*---------------------------------------------------------------
The SM_CYSMCAPTION setting specifies the height of a small caption, in pixels.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYSMICON   50
/*---------------------------------------------------------------
The SM_CYSMICON setting specifies the recommended height of a small icon, in pixels. Small icons typically appear in window captions and in small icon view.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYSMSIZE   53
/*---------------------------------------------------------------
The SM_CYSMSIZE setting specifies the height of small caption buttons, in pixels.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYVIRTUALSCREEN   79
/*---------------------------------------------------------------
The SM_CYVIRTUALSCREEN setting specifies the height of the virtual screen, in pixels. The virtual screen is the bounding rectangle of all display monitors. The SM_YVIRTUALSCREEN metric is the coordinates for the top of the virtual screen.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYVSCROLL   20
/*---------------------------------------------------------------
The SM_CYVSCROLL setting specifies the height of the arrow bitmap on a vertical scroll bar, in pixels.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_CYVTHUMB   9
/*---------------------------------------------------------------
The SM_CYVTHUMB setting specifies the height of the thumb box in a vertical scroll bar, in pixels.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_DBCSENABLED   42
/*---------------------------------------------------------------
The SM_DBCSENABLED setting returns nonzero value if User32.dll supports Double Byte Character Sets ( DBCS); otherwise, 0.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_DEBUG   22
/*---------------------------------------------------------------
The SM_DEBUG setting returns nonzero value if the debug version of User.exe is installed; otherwise, 0.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_DIGITIZER   94
/*---------------------------------------------------------------
The SM_CYCAPTION setting returns nonzero value if the current operating system is Windows 7 or Windows Server 2008 R2 and the Tablet PC Input service is started; otherwise, 0. The return value is a bit mask that specifies the type of digitizer input supported by the device. The return value specifies one or more of the following values:
0x01 NID_INTEGRATED_TOUCH The device has an integrated touch digitizer.
0x02 NID_EXTERNAL_TOUCH   The device has an external touch digitizer.
0x04 NID_INTEGRATED_PEN   The device has an integrated pen digitizer.
0x08 NID_EXTERNAL_PEN     The device has an external pen digitizer.
0x40 NID_MULTI_INPUT      The device supports multiple sources of digitizer input.
0x80 NID_READY            The device is ready to receive digitizer input.
This value is not supported on Windows Server 2008, Windows Vista, and Windows XP/2000.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_IMMENABLED   82
/*---------------------------------------------------------------
The SM_IMMENABLED setting setting returns nonzero if Input Method Manager/Input Method Editor features are enabled; otherwise, 0.  SM_IMMENABLED indicates whether the system is ready to use a Unicode-based IME on a Unicode application. To ensure that a language-dependent IME works, check SM_DBCSENABLED and the system ANSI code page. Otherwise the ANSI-to-Unicode conversion may not be performed correctly, or some components like fonts or registry settings may not be present.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_MAXIMUMTOUCHES   95
/*---------------------------------------------------------------
The SM_MAXIMUMTOUCHES setting returns nonzero if there are digitizers in the system; otherwise, 0. SM_MAXIMUMTOUCHES returns the aggregate maximum of the maximum number of contacts supported by every digitizer in the system. If the system has only single*touch digitizers, the return value is 1. If the system has multi*touch digitizers, the return value is the number of simultaneous contacts the hardware can provide.  This value is not supported on Windows Server 2008, Windows Vista, and Windows XP/2000. 
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_MEDIACENTER   87
/*---------------------------------------------------------------
The SM_MEDIACENTER setting returns nonzero if the current operating system is the Windows XP, Media Center Edition; otherwise, 0.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_MENUDROPALIGNMENT   40
/*---------------------------------------------------------------
The SM_MENUDROPALIGNMENT setting returns nonzero if drop*down menus are right*aligned with the corresponding menu*bar item; 0 if the menus are left*aligned.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_MIDEASTENABLED   74
/*---------------------------------------------------------------
The SM_MIDEASTENABLED setting returns nonzero if the system is enabled for Hebrew and Arabic languages; otherwise, 0.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_MOUSEPRESENT   19
/*---------------------------------------------------------------
The SM_MOUSEPRESENT setting returns nonzero if a mouse is installed; otherwise, 0. This value is rarely zero, because of support for virtual mice and because some systems detect the presence of the port instead of the presence of a mouse.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_MOUSEHORIZONTALWHEELPRESENT   91
/*---------------------------------------------------------------
The SM_MOUSEHORIZONTALWHEELPRESENT setting returns nonzero if a mouse with a horizontal scroll wheel is installed; otherwise 0.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_MOUSEWHEELPRESENT   75
/*---------------------------------------------------------------
The SM_MOUSEWHEELPRESENT setting returns nonzero if a mouse with a vertical scroll wheel is installed; otherwise 0.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_NETWORK   63
/*---------------------------------------------------------------
The SM_NETWORK setting returns the least significant bit is set if a network is present; otherwise, it is cleared. The other bits are reserved for future use.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_PENWINDOWS   41
/*---------------------------------------------------------------
The SM_PENWINDOWS setting returns nonzero if the Microsoft Windows for Pen computing extensions are installed; zero otherwise.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_REMOTECONTROL   8193
/*---------------------------------------------------------------
The SM_REMOTECONTROL (Hex 0x2001) setting is used in a Terminal Services environment. Its value is nonzero if the current session is remotely controlled; otherwise, 0. 
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_REMOTESESSION   4096
/*---------------------------------------------------------------
The SM_REMOTESESSION (Hex 0x1000) setting is used in a Terminal Services environment. If the calling process is associated with a Terminal Services client session, the return value is nonzero. If the calling process is associated with the Terminal Services console session, the return value is 0.  Windows Server 2003 and Windows XP:  The console session is not necessarily the physical console. For more information, see WTSGetActiveConsoleSessionId.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_SAMEDISPLAYFORMAT   81
/*---------------------------------------------------------------
The SM_SAMEDISPLAYFORMAT setting returns nonzero if all the display monitors have the same color format, otherwise, 0. Two displays can have the same bit depth, but different color formats. For example, the red, green, and blue pixels can be encoded with different numbers of bits, or those bits can be located in different places in a pixel color value. 
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_SECUR   44
/*---------------------------------------------------------------
The SM_SECUR system metric should be ignored; it always returns 0.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_SERVERR2   89
/*---------------------------------------------------------------
The SM_SERVERR2 setting returns the build number if the system is Windows Server 2003 R2; otherwise, 0. 
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_SHOWSOUNDS   70
/*---------------------------------------------------------------
The SM_SHOWSOUNDS setting returns nonzero if the user requires an application to present information visually in situations where it would otherwise present the information only in audible form; otherwise, 0.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_SHUTTINGDOWN   8192
/*---------------------------------------------------------------
The SM_SHUTTINGDOWN (Hex 0x2000) setting returns nonzero if the current session is shutting down; otherwise, 0.  This value is not supported on Windows 2000.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_SLOWMACHINE   73
/*---------------------------------------------------------------
The SM_SLOWMACHINE setting returns nonzero if the computer has a low*end (slow) processor; otherwise, 0.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_STARTER   88
/*---------------------------------------------------------------
The SM_STARTER setting returns nonzero if the current operating system is Windows 7 Starter Edition, Windows Vista Starter, or Windows XP Starter Edition; otherwise, 0.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_SWAPBUTTON   23
/*---------------------------------------------------------------
The SM_SWAPBUTTON setting returns nonzero if the meanings of the left and right mouse buttons are swapped; otherwise, 0.
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_TABLETPC   86
/*---------------------------------------------------------------
The SM_TABLETPC setting returns nonzero if the current operating system is the Windows XP Tablet PC edition or if the current operating system is Windows Vista or Windows 7 and the Tablet PC Input service is started; otherwise, 0. 
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_XVIRTUALSCREEN   76
/*---------------------------------------------------------------
The SM_XVIRTUALSCREEN setting returns the coordinates for the left side of the virtual screen. The virtual screen is the bounding rectangle of all display monitors. The SM_CXVIRTUALSCREEN metric is the width of the virtual screen. 
---------------------------------------------------------------*/
&GLOBAL-DEFINE SM_YVIRTUALSCREEN   77
/*---------------------------------------------------------------
The SM_YVIRTUALSCREEN setting returns the coordinates for the top of the virtual screen. The virtual screen is the bounding rectangle of all display monitors. The SM_CYVIRTUALSCREEN metric is the height of the virtual screen.
---------------------------------------------------------------*/


PROCEDURE GetScreenProprieties: 
DEF VAR VirtWidth AS INTEGER.
DEF VAR VirtHeight AS INTEGER.
/* PC */
DEFINE VARIABLE iNumberOfMonitors        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iWidthOfPrimaryMonitor   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iHeightOfPrimaryMonitor   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iWidthOfBothMonitors     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iWidthOfSecondaryMonitor AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSMArrange AS INTEGER     NO-UNDO.
DEFINE VARIABLE iWidthOfFullScreen AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSMCYFULLSCREEN AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSMCYMAXIMIZED AS INTEGER     NO-UNDO.
DEFINE VARIABLE iHeightOfVirtualScreen AS INTEGER     NO-UNDO.

/* Get the number of Monitors connected to the desktop */
RUN GetSystemMetrics({&SM_CMONITORS}, OUTPUT iNumberOfMonitors).
/* Get the width of the Primary Monitor in pixels */
RUN GetSystemMetrics ({&SM_CXSCREEN}, OUTPUT iWidthOfPrimaryMonitor).
RUN GetSystemMetrics ({&SM_CYSCREEN}, OUTPUT iHeightOfPrimaryMonitor).
/* Get the width of the Primary Monitor in pixels */
RUN GetSystemMetrics ({&SM_CXFULLSCREEN}, OUTPUT iWidthOfFullScreen).
/* Get the width of the Secondary Monitor in pixels */
ASSIGN
    iWidthOfSecondaryMonitor = iWidthOfBothMonitors - iWidthOfPrimaryMonitor.

RUN GetSystemMetrics ({&SM_ARRANGE}, OUTPUT iSMArrange).
RUN GetSystemMetrics ({&SM_CYFULLSCREEN}, OUTPUT iSMCYFULLSCREEN).
RUN GetSystemMetrics ({&SM_CYMAXIMIZED}, OUTPUT iSMCYMAXIMIZED).


/* Get the size of the virtual screen from Windows */
RUN GetSystemMetrics({&SM_CXVIRTUALSCREEN}, OUTPUT VirtWidth).
RUN GetSystemMetrics({&SM_CYVIRTUALSCREEN}, OUTPUT VirtHeight).

    PUT UNFORM   /* gives info about screen resolution*/
     "Screen Virtual Width: " VirtWidth SKIP
     "Screen Virtual Height: " VirtHeight SKIP (2).

    PUT UNFORM   /* gives info about screen resolution*/
    "Number of the desktop monitors :~t" iNumberOfMonitors "~n"
    "Height of the primary monitor in pixels:~t" iHeightOfPrimaryMonitor "~n"
    "Width of the primary monitor in pixels:~t" iWidthOfPrimaryMonitor "~n"
    "Width of the both monitor in pixels:~t" iWidthOfBothMonitors "~n"
    "Height of the virtual screen in pixels:~t" iHeightOfVirtualScreen "~n"
    "Width of the secondary monitor in pixels:~t" iWidthOfSecondaryMonitor "~n"
    "Width of screen in pixels:~t" iWidthOfFullScreen "~n"
    "how the system arranges minimized windows:~t" iSMArrange "~n"
    "height of client area for a full screen window:~t" iSMCYFULLSCREEN "~n"
    "default hieght of maximized full screen window:~t" iSMCYMAXIMIZED "~n" SKIP (2).
    
END. 

PROCEDURE GetSystemMetrics EXTERNAL "USER32.DLL":

    DEFINE INPUT  PARAMETER nIndex AS LONG.

    DEFINE RETURN PARAMETER nRet   AS LONG.

END PROCEDURE.

PROCEDURE GetEnv:
    PUT UNFORM 'ORACLE_SID: ' OS-GETENV("ORACLE_SID":U) SKIP.
    PUT UNFORM 'ORACLE_HOME: ' OS-GETENV("ORACLE_HOME":U) SKIP.
    PUT UNFORM 'NLS_LANG: ' OS-GETENV("NLS_LANG":U) SKIP.
    PUT UNFORM 'NLS_CHARACTER_SET: ' OS-GETENV("NLS_CHARACTER_SET":U) SKIP.

END PROCEDURE.


