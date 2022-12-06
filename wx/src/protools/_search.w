&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: _search.w

  Description: Finds all copies of a file in the PROGRESS PROPATH 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: David Lee

  Created: 5/12/95
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

define variable foundCount as integer no-undo.

&IF "{&WINDOW-SYSTEM}" BEGINS "MS-WIN" &THEN
{adecomm/fileinfo.i}

DEFINE VARIABLE file-year AS INTEGER.
DEFINE VARIABLE file-mon AS INTEGER.
DEFINE VARIABLE file-day AS INTEGER.
DEFINE VARIABLE file-hour AS INTEGER.
DEFINE VARIABLE file-min AS INTEGER.
DEFINE VARIABLE file-sec AS INTEGER.
DEFINE VARIABLE file-size AS INTEGER.    
DEFINE VARIABLE error AS INTEGER.

&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS whichFile doIt Btn_Close foundFile 
&Scoped-Define DISPLAYED-OBJECTS whichFile foundFile 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Close AUTO-GO 
     LABEL "&Close" 
     SIZE 12 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON doIt 
     LABEL "&Search" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE whichFile AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1.1 NO-UNDO.

DEFINE VARIABLE foundFile AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     SIZE 64 BY 4.1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     whichFile AT ROW 2 COL 2 NO-LABEL
     doIt AT ROW 2.1 COL 53
     Btn_Close AT ROW 3.43 COL 53
     foundFile AT ROW 5.05 COL 2 NO-LABEL
     "Search for File:" VIEW-AS TEXT
          SIZE 18 BY .67 AT ROW 1.29 COL 2
     "Can be Found In:" VIEW-AS TEXT
          SIZE 18 BY .67 AT ROW 4.24 COL 2
     SPACE(47.28) SKIP(4.46)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Propath File Search"
         DEFAULT-BUTTON Btn_Close.

 

/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   Default                                                              */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN whichFile IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Propath File Search */
DO:
  run adecomm/_setcurs.p("WAIT"). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Propath File Search */
DO:
  run adecomm/_setcurs.p(""). 
 APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close Dialog-Frame
ON CHOOSE OF Btn_Close IN FRAME Dialog-Frame /* Close */
DO:
 Apply "CLOSE" to FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME doIt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL doIt Dialog-Frame
ON CHOOSE OF doIt IN FRAME Dialog-Frame /* Search */
DO:
    define variable s   as logical   no-undo.
    define variable str as character no-undo.
    define variable rel as logical   no-undo.
        
    ASSIGN whichfile.
    foundFile:LIST-ITEMS = "".
    foundCount = 1.
        
    if length(whichFile) > 1 then do:
        run adecomm/_setcurs.p("WAIT").
       
        /* 
         * Full path name, don't use propath
         */        
        if index(":", whichFile) > 0 then run addFile(search(whichFile)).
        else do:
            define variable i         as integer   no-undo.
            define variable rCodeName as character no-undo.
            define variable ext       as character no-undo.


            do i = 1 to num-entries(propath):

                /*
                 * Don't want a slash if starting from
                 * a local directory
                 */
                 
                if entry(i, propath) = "" then
                    assign
                        str = whichFile
                        rel = yes.
                else
                    assign
                        str = entry(i, propath) + "~/" + whichFile
                        rel = no
                    .
                
                /*
                 * Replace all the slashes
                 */
                
                &IF "{&WINDOW-SYSTEM}" BEGINS "MS-WIN" &THEN
                
                   str = replace(str, "~/", "~\").   
                    
                &ELSE
                     str = replace(str, "~\", "~/").               
                &ENDIF 
                
                /*
                 * Look for r-code if a.p or .w
                 */
            
                ext = SUBSTRING(str, LENGTH(str,"CHARACTER":U) - 1, -1, "CHARACTER":U).
                IF ext = ".p" OR ext = ".w" THEN DO:                
                  run makeRCodeName(str, output rCodeName).
                  if rCodeName <> ? then run addFile(rCodeName).              
                END.
                                                                           
                file-info:file-name = str.
                
                /*
                 * If we're working with a relative file then
                 * the check is different
                 */

                if rel then do:
                
                    if search(str) = str then
                           run addFile(str).                
                end.
                else do: 
                    if     file-info:pathname <> ? 
                       and file-info:pathname = str then
                           run addFile(str).    
                end.
            end.
        end.
        /* Report if nothing were found. */
        IF foundFile:NUM-ITEMS eq 0 THEN foundFile:LIST-ITEMS = "[No files found]".
        run adecomm/_setcurs.p ("").
    end.
    else
        foundFile:SCREEN-VALUE = "".    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME whichFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL whichFile Dialog-Frame
ON LEAVE OF whichFile IN FRAME Dialog-Frame
DO:
    if whichFile = whichFile:SCREEN-VALUE then return.
    
    whichFIle = whichFile:SCREEN-VALUE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL whichFile Dialog-Frame
ON RETURN OF whichFile IN FRAME Dialog-Frame
DO:
    if whichFile = whichFile:SCREEN-VALUE then return.
    
    whichFile = whichFile:SCREEN-VALUE.
    apply "CHOOSE" to doIt.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.
  
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  
  /*
   * Load the file information
   */
  &IF "{&WINDOW-SYSTEM}" BEGINS "MS-WIN" &THEN
      run LoadFileInfo.
  &ENDIF.

  run adecomm/_setcurs.p("").
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
  run adecomm/_setcurs.p("").
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addFile Dialog-Frame 
PROCEDURE addFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter fName as character no-undo.

define variable str     as character no-undo.
define variable s       as logical   no-undo.
define variable dateStr as character no-undo initial "".
define variable modDate as date      no-undo.
define variable modTime as character no-undo.
define variable preStr  as character no-undo.

if fName = ? then return.
if length(fName) < 0 then return.

preStr = string(foundCount) + ". " + fName.

do with frame {&FRAME-NAME}:
    &IF "{&WINDOW-SYSTEM}" BEGINS "MS-WIN" &THEN
        IF OPSYS = "WIN32" THEN
         run file_info (fName, output file-year,
                               output file-mon,
                               output file-day,
                               output file-hour,
                               output file-min,
                               output file-sec,
                               output file-size,
                               output error).
        ELSE
         run file_info16 (fName, output file-year,
                                 output file-mon,
                                 output file-day,
                                 output file-hour,
                                 output file-min,
                                 output file-sec,
                                 output file-size,
                                 output error).                             
        assign
            modDate = date(string(file-mon,"99")
                    + "/"
                    + string(file-day,"99")
                    + "/"
                    + substr(string(file-year),3,2,"fixed"))
                     
            modTime = string(file-hour,"99")
                    + ":"
                    + string(file-min,"99")
                    + ":"
                    + string(file-sec,"99")

            dateStr = string(modDate) + " " + modTime
        .
        
        /*
         * Try to get the dates/teims to lineup. But never
         * allow a filename to be clipped!
         */
         
        if length(preStr) < 55 then preStr = string(preStr, "X(55)").
    &ENDIF
    assign
        str = preStr + " " + dateStr
        s = foundFile:ADD-LAST(str)
        foundCount = foundCount + 1
    .
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame _DEFAULT-ENABLE
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
  DISPLAY whichFile foundFile 
      WITH FRAME Dialog-Frame.
  ENABLE whichFile doIt Btn_Close foundFile 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE makeRCodeName Dialog-Frame 
PROCEDURE makeRCodeName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input  parameter fName as character no-undo.
define output parameter rName as character no-undo initial ?.

define variable str as character no-undo.

if num-entries(fName, ".") > 1 then

     str = substr(fName, 1, r-index(fName, ".") - 1) + ".r".
else
     str = fName + ".r". 

file-info:file-name = str.

if     file-info:pathname <> ? 
   and file-info:pathname = str then rName = str.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


