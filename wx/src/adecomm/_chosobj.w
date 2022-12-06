&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME d_openso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS d_openso 
/*------------------------------------------------------------------------

  File: _chosobj.w

  Description: Choose dialog for custom objects 

  Input Parameters:
     p_mode (char) - defines mode for dialog: {&WT-CONTROL} OR ""
                     (i.e. OCX or non-OCX (SmartObjects)
     p_cst-attr (char) - chr(10) delimited list of options from .cst file:
                   DIRECTORY-LIST dir1,dir2,...       /* list of dirs to search */
                   FILTERS        filter1,filter2,... /* file filters to use */
                   TITLE          <string>            /* title for this dialog */
     p_newTemplate (char) - template designated for a new object of this type
                            (e.g. from .cst file: NEW-TEMPLATE <templ-name>)
     p_showOptions (char) - buttons to enable in the dialog-box in a CDL:
                            BROWSE, NEW, PREVIEW
     
  
  Output Parameters:
     p_fileChosen (char) - filename of object to draw in a window or dialog
     p_otherThing (char) - OCX chosen to draw (unused for SmartObjects)
     p_cancelled  (log)  - True if the user cancelled out of the dialog

  Author: Gerry Seidl 

  Created: 02/12/95 - 10:11 pm

  Modified on 7/31/95 - Moved to adecomm
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
&GLOBAL-DEFINE WIN95-BTN YES
{adecomm/commeng.i}   /* Help String Definitions                 */

/* ***************************  Definitions  ************************** */
&GLOBAL-DEFINE WT-CONTROL               OCX
&GLOBAL-DEFINE WL-CONTROL               OCX-Controls

/* Parameters Definitions ---   */
  DEFINE INPUT  PARAMETER p_mode        AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER p_cst-attr    AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER p_newTemplate AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER p_showOptions AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER p_fileChosen  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER p_otherThing  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER p_cancelled   AS LOGICAL   NO-UNDO INITIAL FALSE. 

  DEFINE VARIABLE filters   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dirs      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dtitle    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ldummy    AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE rc        AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE tdirs     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE browseone AS LOGICAL   NO-UNDO INITIAL no.
  DEFINE VARIABLE newone    AS LOGICAL   NO-UNDO INITIAL no.
  DEFINE VARIABLE i         AS INTEGER   NO-UNDO.
  DEFINE VARIABLE helpFile  AS INTEGER   NO-UNDO.
  
  DEFINE VARIABLE DOS-SLASH  AS CHARACTER NO-UNDO INITIAL "~\":u.
  DEFINE VARIABLE UNIX-SLASH AS CHARACTER NO-UNDO INITIAL "/":u.
  DEFINE VARIABLE OS-SLASH   AS CHARACTER NO-UNDO.
         /* Initialized in main block. */

/*
** Patrick Tullmann's Dir DLL
*/
{adecomm/dirsrch.i}
  DEFINE VARIABLE list-mem    AS MEMPTR            NO-UNDO.
  DEFINE VARIABLE list-char   AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE list-size   AS INTEGER INIT 8000 NO-UNDO. 
  DEFINE VARIABLE missed-file AS INTEGER           NO-UNDO.
  DEFINE VARIABLE DirError    AS INTEGER           NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME d_openso

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS listlbl filename s_files cb_filters cb_dirs 
&Scoped-Define DISPLAYED-OBJECTS listlbl filename s_files cb_filters ~
cb_dirs 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON b_Browse 
     LABEL "&Browse..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON b_New 
     LABEL "&New..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON b_Preview 
     LABEL "&Preview..." 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cb_dirs AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","" 
     SIZE 35 BY 1.1 NO-UNDO.

DEFINE VARIABLE cb_filters AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","" 
     SIZE 35 BY 1.1 NO-UNDO.

DEFINE VARIABLE filename AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE listlbl AS CHARACTER FORMAT "X(256)":U INITIAL "Master File:" 
      VIEW-AS TEXT 
     SIZE 16 BY .62 NO-UNDO.

DEFINE VARIABLE s_files AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SORT SCROLLBAR-VERTICAL 
     SIZE 33 BY 5 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME d_openso
     listlbl AT ROW 1.29 COL 1 COLON-ALIGNED NO-LABEL
     filename AT ROW 2 COL 3 NO-LABEL
     s_files AT ROW 3 COL 5 NO-LABEL
     b_Preview AT ROW 3 COL 39
     b_Browse AT ROW 4.52 COL 39
     b_New AT ROW 6.91 COL 39
     cb_filters AT ROW 9.14 COL 3 NO-LABEL
     cb_dirs AT ROW 11 COL 3 NO-LABEL
     "File Filter:" VIEW-AS TEXT
          SIZE 14 BY .81 AT ROW 8.29 COL 3
     "Directory:" VIEW-AS TEXT
          SIZE 12 BY .71 AT ROW 10.19 COL 3
     SPACE(39.59) SKIP(1.19)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Open SmartObject".

 

/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX d_openso
   L-To-R                                                               */
ASSIGN 
       FRAME d_openso:SCROLLABLE       = FALSE
       FRAME d_openso:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON b_Browse IN FRAME d_openso
   NO-ENABLE                                                            */
ASSIGN 
       b_Browse:HIDDEN IN FRAME d_openso           = TRUE.

/* SETTINGS FOR BUTTON b_New IN FRAME d_openso
   NO-ENABLE                                                            */
ASSIGN 
       b_New:HIDDEN IN FRAME d_openso           = TRUE.

/* SETTINGS FOR BUTTON b_Preview IN FRAME d_openso
   NO-ENABLE                                                            */
ASSIGN 
       b_Preview:HIDDEN IN FRAME d_openso           = TRUE.

/* SETTINGS FOR COMBO-BOX cb_dirs IN FRAME d_openso
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX cb_filters IN FRAME d_openso
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN filename IN FRAME d_openso
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME d_openso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL d_openso d_openso
ON ENDKEY OF FRAME d_openso /* Open SmartObject */
OR END-ERROR OF FRAME d_openso
DO:
  ASSIGN
    p_fileChosen = "":U
    p_otherThing = "":U
    p_cancelled = TRUE
  .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL d_openso d_openso
ON GO OF FRAME d_openso /* Open SmartObject */
DO:
  DEFINE VARIABLE ok_choice AS LOGICAL NO-UNDO.
  
  /*
   * Make sure that the contents of the fill-in
   * are legal. This traps the problem if the
   * user types a bad filter and goes to OK
   * without hitting CR
   */
  IF NOT browseone THEN DO:
    run changeFilter(output ok_choice).
    if not ok_choice then return no-apply.
  END.
  
  /* See if there is a valid choice? */

  RUN Check_Filechosen (OUTPUT ok_choice). 
  IF NOT ok_choice THEN RETURN NO-APPLY.

  /* Make sure slashes are correct for the platform */
  IF LOOKUP(OPSYS , "MSDOS,WIN32":U) > 0 THEN
    p_fileChosen = REPLACE(p_fileChosen, UNIX-SLASH, DOS-SLASH).
  ELSE IF OPSYS = "UNIX":U THEN            
    p_fileChosen = REPLACE(p_fileChosen, DOS-SLASH, UNIX-SLASH).
  
  /* Lowercase the filename to make it easier to port code to Unix */
  p_fileChosen = LC(p_fileChosen).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b_Browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_Browse d_openso
ON CHOOSE OF b_Browse IN FRAME d_openso /* Browse... */
DO:
  DEFINE VARIABLE lOK AS LOGICAL NO-UNDO.
  
  /* Open another file */
  SYSTEM-DIALOG GET-FILE p_fileChosen
      TITLE    "Open an object"
      FILTERS  "UIB files (*.w)"    "*.w",
               "R-code files (*.r)" "*.r",
               "All Files (*.*)"    "*.*"
      USE-FILENAME
      MUST-EXIST
      UPDATE   lOK.
  /* Did we cancel at this point? */
  IF NOT lOK THEN RETURN NO-APPLY.
  ELSE DO:
    /* Set the flag that indicates we just browsed a file (so that
       the "GO" action won't look at the file name in the selection list) */
    browseone = YES.
    /* See if it can be located in the PROPATH */
    DO i = 1 to NUM-ENTRIES(PROPATH):
      FILE-INFO:FILE-NAME = TRIM(ENTRY(i,PROPATH)).
      IF p_fileChosen BEGINS FILE-INFO:FULL-PATHNAME AND
        FILE-INFO:FULL-PATHNAME NE ? THEN DO:
        /* If it's there, chop off the leading part */
        p_fileChosen = SUBSTRING(p_fileChosen, LENGTH(FILE-INFO:FULL-PATHNAME) + 2, -1, "CHARACTER":U).
        LEAVE.
      END.
    END.
    APPLY "GO" TO FRAME {&FRAME-NAME}. /* got it */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b_New
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_New d_openso
ON CHOOSE OF b_New IN FRAME d_openso /* New... */
DO:
  IF p_newTemplate <> "" OR p_newTemplate = ? THEN DO:
    FILE-INFO:FILE-NAME = p_newTemplate.
    IF FILE-INFO:FULL-PATHNAME NE ? THEN DO:
      ASSIGN FRAME {&FRAME-NAME}:HIDDEN = YES.
      RUN adeuib/_open-w.p (FILE-INFO:FULL-PATHNAME, "UNTITLED"). /* open from template in UIB */
      ASSIGN newone  = YES.
      APPLY "U1" TO FRAME {&FRAME-NAME}. /* direct path out */
    END.
  END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b_Preview
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_Preview d_openso
ON CHOOSE OF b_Preview IN FRAME d_openso /* Preview... */
DO:
  DEFINE VARIABLE ok_choice AS LOGICAL NO-UNDO.
  
  run adecomm/_setcurs.p("WAIT":u).
  /* See if there is a valid choice? */
  RUN Check_Filechosen (OUTPUT ok_choice).
 
  /* If so, preview the current object in a dialog-box */
  IF ok_choice THEN 
    if p_mode = "{&WT-CONTROL}":U then run adeuib/_prvcont.w(p_fileChosen, p_otherThing).
                                  else run adeuib/_so-prvw.w(p_fileChosen).
  run adecomm/_setcurs.p("":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb_dirs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb_dirs d_openso
ON VALUE-CHANGED OF cb_dirs IN FRAME d_openso
DO:

  if cb_dirs = cb_dirs:SCREEN-VALUE then return.
  run adecomm/_setcurs.p("WAIT":U).
  
  RUN BuildFileList. 
  RUN Set_First.
  
  cb_dirs = cb_dirs:SCREEN-VALUE.
  run adecomm/_setcurs.p("":U). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb_filters
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb_filters d_openso
ON VALUE-CHANGED OF cb_filters IN FRAME d_openso
DO:

  if cb_filters = cb_filters:SCREEN-VALUE then return. 
  run adecomm/_setcurs.p("WAIT":U).
  
  RUN BuildFileList.
  RUN Set_First.
  
  cb_filters = cb_filters:SCREEN-VALUE.
  run adecomm/_setcurs.p("":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filename
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filename d_openso
ON RETURN OF filename IN FRAME d_openso
DO:
define variable p_ok as logical no-undo.
  if filename = filename:SCREEN-VALUE then return. 

  run changeFilter(output p_OK).
  if not p_OK then return no-apply.
  
  /* Change sensitivity of various buttons */
  RUN Set_Sensitivity.
  
  filename = filename:SCREEN-VALUE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME s_files
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL s_files d_openso
ON DEFAULT-ACTION OF s_files IN FRAME d_openso
DO:
  APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL s_files d_openso
ON ENTRY OF s_files IN FRAME d_openso
DO:
  IF SELF:SCREEN-VALUE <> "":U AND
     SELF:SCREEN-VALUE <> ?  AND
     SELF:SCREEN-VALUE <> "?" AND
     SELF:SCREEN-VALUE <> "<None>" THEN
       ASSIGN filename:SCREEN-VALUE = SELF:SCREEN-VALUE.
     ELSE filename:SCREEN-VALUE = "".
     
  filename = filename:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL s_files d_openso
ON VALUE-CHANGED OF s_files IN FRAME d_openso
DO:
  IF SELF:SCREEN-VALUE <> "":U AND
     SELF:SCREEN-VALUE <> ?  AND
     SELF:SCREEN-VALUE <> "?" AND
     SELF:SCREEN-VALUE <> "<None>" THEN
       ASSIGN filename:SCREEN-VALUE = SELF:SCREEN-VALUE.
     ELSE filename:SCREEN-VALUE = "":U.
  
  /*
   * Reset the user's choices
   */
  
  assign   
      filename = filename:SCREEN-VALUE
      p_fileChosen = "":U
      p_otherThing = "":U
  .
     
  RUN Set_Sensitivity.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK d_openso 


/* ***************************  Main Block  *************************** */

IF p_mode = "{&WT-CONTROL}":U THEN helpFile = {&Choose_VBX_Controls_Dlg_Box}.
                              ELSE helpFile = {&Choose_SmartObject_Dlg_Box}.

IF LOOKUP(OPSYS, "MSDOS,WIN32":u) > 0 THEN
  ASSIGN OS-SLASH = "~\":u.
ELSE
  ASSIGN OS-SLASH = "/":u.

{ adecomm/okbar.i &TOOL = "COMM"
                  &CONTEXT = helpFile
                 }
                 
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN initGUI.
  RUN Setup(OUTPUT rc).
  

  IF NOT rc THEN DO:
    MESSAGE "Invalid parameters defined for" p_mode + "."
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
  END.
  RUN enable_UI.
  RUN Set_First.
  
  
  RUN adecomm/_setcurs.p("":U).
  WAIT-FOR GO, U1 OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildFileList d_openso 
PROCEDURE BuildFileList :
/* -----------------------------------------------------------
  Purpose:     Build the file listing in the selection list
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE VARIABLE file_list AS CHARACTER NO-UNDO INITIAL "".
  DEFINE VARIABLE file_base AS CHARACTER NO-UNDO FORMAT "X(64)".
  DEFINE VARIABLE file_abs  AS CHARACTER NO-UNDO FORMAT "X(64)".
  DEFINE VARIABLE fileattr AS CHARACTER NO-UNDO FORMAT "X(10)".
  DEFINE VARIABLE pos       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE CurDir    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE CurFilter AS CHARACTER NO-UNDO.
  DEFINE VARIABLE t         AS CHARACTER NO-UNDO INITIAL "":U.
  DEFINE VARIABLE i         AS INTEGER   NO-UNDO.
  DEFINE VARIABLE Use_DLL   AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE DirPrefix AS CHARACTER NO-UNDO.
  DEFINE VARIABLE DirBase   AS CHARACTER NO-UNDO.
    
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN filename:SCREEN-VALUE  = "":U
           filename               = "":U
           s_files:LIST-ITEMS     = "":U
           pos = cb_dirs:LOOKUP(cb_dirs:SCREEN-VALUE)
           FILE-INFO:FILENAME     = ENTRY(pos,tdirs)
           CurDir                 = LC(file-info:full-pathname).
             
    /* The DLL does not return any files if the directory passed is
     * the Root directory (e.g. "C:\"), so in that case, we'll use
     * the "slow" way.
     */  
    IF LOOKUP(OPSYS, "MSDOS,WIN32":u) > 0 THEN
    DO:
        RUN adecomm/_osprefx.p
            (INPUT CurDir, OUTPUT DirPrefix, OUTPUT DirBase).
        /* If the directory path is not the same as the directory prefix,
           then we do not have a root drive, so use the DLL. */
        ASSIGN Use_DLL = (CurDir <> DirPrefix).
    END.
    
    IF Use_DLL THEN 
    DO: /* use Windows .DLL instead of OS-DIR */
      RUN adecomm/_setcurs.p ("WAIT":U).
      ASSIGN
        SET-SIZE(list-mem)         = list-size
        CurFilter                  = "":U
        CurFilter                  = cb_filters:SCREEN-VALUE.

      RUN file_search (CurDir,CurFilter,INPUT-OUTPUT list-mem,list-size,
        OUTPUT missed-file, OUTPUT DirError).
      
      /* Error checking */
      IF DirError <> 0 THEN DO:
        RUN adecomm/_setcurs.p ("":U).
        SET-SIZE(list-mem) = 0.
        MESSAGE "Error in directory search." VIEW-AS ALERT-BOX ERROR.
        RETURN.
      END.
      IF missed-file > 0 THEN 
        MESSAGE "Too many files in your directory." skip
                "The file list may not be inclusive."
          VIEW-AS ALERT-BOX INFORMATION. 
      
      ASSIGN 
        file_list          = LC(GET-STRING(list-mem,1))
        SET-SIZE(list-mem) = 0.

    END. /* WINDOWS .DLL version */
    ELSE DO: /* other O/S's use the slow 4GL method */  
      RUN adecomm/_setcurs.p ("WAIT":U).
      INPUT FROM OS-DIR (ENTRY(pos,tdirs)) NO-ECHO.
      REPEAT:
        IMPORT file_base file_abs fileattr.
        IF file_base MATCHES REPLACE(cb_filters:SCREEN-VALUE,".","~~.") AND INDEX(fileattr,"D") = 0 THEN
            file_list = file_list + (IF file_list NE "" THEN "," ELSE "") + file_base.
      END.
      INPUT CLOSE.
      IF LOOKUP(OPSYS , "MSDOS,WIN32":u) > 0 THEN file_list = LC(file_list).
    END.
    
    IF file_list NE "" THEN DO: 
      s_files:LIST-ITEMS   = file_list. 
      s_files:SCREEN-VALUE = s_files:ENTRY(1).
    END.
    ELSE ldummy = s_files:ADD-FIRST("<none>").
    RUN adecomm/_setcurs.p ("":U).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChangeFilter d_openso 
PROCEDURE ChangeFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER p_OK AS LOGICAL NO-UNDO INITIAL yes.
  
  DEFINE VARIABLE pos       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE newpath   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE newfilter AS CHARACTER NO-UNDO.
  DEFINE VARIABLE l         AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE chdir     AS LOGICAL   NO-UNDO INITIAL NO.
  DEFINE VARIABLE chfilter  AS LOGICAL   NO-UNDO INITIAL NO.
  DEFINE VARIABLE chfile    AS LOGICAL   NO-UNDO INITIAL NO.
  DEFINE VARIABLE tfname    AS CHARACTER NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN tfname = (IF LOOKUP(OPSYS , "MSDOS,WIN32":U) > 0
                   THEN REPLACE(filename:SCREEN-VALUE, UNIX-SLASH, DOS-SLASH) 
                   ELSE REPLACE(filename:SCREEN-VALUE, DOS-SLASH, UNIX-SLASH) ).
    IF INDEX(tfname, OS-SLASH) = 0 THEN /* add path from filter */
        FILE-INFO:FILE-NAME = cb_dirs:SCREEN-VALUE + OS-SLASH + tfname.
    ELSE
        FILE-INFO:FILE-NAME = tfname. /* assume that user is specifying dir and filename */
    
    IF FILE-INFO:FULL-PATHNAME <> ? THEN DO:
    
        IF INDEX(FILE-INFO:FILE-TYPE,"D") > 0 THEN DO: /* directory */
            ASSIGN p_OK = NO.
            IF LOOKUP(FILE-INFO:FULL-PATHNAME, tdirs) > 0 THEN
              ASSIGN cb_dirs:SCREEN-VALUE = cb_dirs:ENTRY(LOOKUP(FILE-INFO:FULL-PATHNAME, tdirs)). /* existing dir */
            ELSE DO: /* New dir */
              IF cb_dirs:LOOKUP(FILE-INFO:PATHNAME) = 0 THEN l = cb_dirs:ADD-FIRST(FILE-INFO:PATHNAME).
              ASSIGN cb_dirs:SCREEN-VALUE = FILE-INFO:PATHNAME
                     dirs = cb_dirs:LIST-ITEMS
                     tdirs = "".
              RUN Check_Dirs.  
            END.
            ASSIGN chdir = YES.
        END.        
        ELSE /* not a directory */
           ASSIGN
             p_fileChosen = FILE-INFO:PATHNAME
             chfile     = YES
           .
           
    END.
    ELSE DO:
      /* Bad filename, but might be a new filter and/or dir */
      IF INDEX(tfname, OS-SLASH) > 0 OR INDEX(tfname, "*") > 0 THEN DO:  /* change dir and/or filter */
        ASSIGN p_OK = NO.
        IF INDEX(tfname, OS-SLASH) > 0 THEN DO:
          /* If there is no wildcard here, it's really a bad filename */
          IF INDEX(tfname, "*") = 0 THEN DO:
            MESSAGE "Invalid directory path or filename." VIEW-AS ALERT-BOX ERROR.
            RETURN.
          END.
          /* It's got a slash, extract new search dir */
          ASSIGN newpath = SUBSTRING(tfname, 1, R-INDEX(tfname,OS-SLASH) - 1, "CHARACTER":U).
          FILE-INFO:FILE-NAME = newpath.
          IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
            MESSAGE "Invalid pathname: " newpath VIEW-AS ALERT-BOX ERROR.
            RETURN.
          END. 
          ELSE DO:
            IF LOOKUP(FILE-INFO:FULL-PATHNAME, tdirs) > 0 THEN
              ASSIGN cb_dirs:SCREEN-VALUE = cb_dirs:ENTRY(LOOKUP(FILE-INFO:FULL-PATHNAME, tdirs)). /* existing dir */
            ELSE DO:
              IF cb_dirs:LOOKUP(newpath) = 0 THEN l = cb_dirs:ADD-FIRST(newpath).
              ASSIGN cb_dirs:SCREEN-VALUE = newpath
                     dirs = cb_dirs:LIST-ITEMS
                     tdirs = "".
              RUN Check_Dirs.
            END.
            ASSIGN chdir = YES.
          END.
        END.
        IF INDEX(tfname, "*") > 0 THEN DO: /* wildcard */
          IF INDEX(tfname,OS-SLASH) > 0 THEN /* contains a slash? */
            IF R-INDEX(tfname,OS-SLASH) < R-INDEX(tfname, "*") THEN DO:
              /* The slash comes before the wildcard - good */
              ASSIGN newfilter = SUBSTRING(tfname, R-INDEX(tfname,OS-SLASH) + 1,-1,"CHARACTER":U).
              IF cb_filters:LOOKUP(newfilter) = 0 THEN l = cb_filters:ADD-FIRST(newfilter).
              ASSIGN cb_filters:SCREEN-VALUE = newfilter.
            END.
            ELSE DO: /* The '*' should not come before the last slash! */
              MESSAGE "Invalid wildcard entry." VIEW-AS ALERT-BOX ERROR.
              chfilter = NO.
              p_OK = NO.
            END.
          ELSE DO: /* No slash */
            ASSIGN newfilter = filename:SCREEN-VALUE.
            IF cb_filters:LOOKUP(newfilter) = 0 THEN l = cb_filters:ADD-FIRST(newfilter).
            ASSIGN cb_filters:SCREEN-VALUE = newfilter
                   chfilter = YES.         
          END.
        END. 
      END. /* slash or '*' */
      ELSE DO: /* really a bad filename! */
        MESSAGE "File: " + filename:SCREEN-VALUE + " was not found." 
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        p_OK = NO.
      END.         
    END.
    
    IF chfilter THEN APPLY "VALUE-CHANGED" TO cb_filters.
    IF chdir    THEN APPLY "VALUE-CHANGED" TO cb_dirs.
    IF chfile  AND
      s_files:LOOKUP(filename:SCREEN-VALUE) > 0 THEN 
                     s_files:SCREEN-VALUE = filename:SCREEN-VALUE.

END.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Check_Dirs d_openso 
PROCEDURE Check_Dirs :
/* -----------------------------------------------------------
  Purpose:     Check to see if we can locate the dirs in the list
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE VARIABLE i     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE dirs2 AS CHARACTER NO-UNDO.
  DO i = 1 TO NUM-ENTRIES(dirs):
      ASSIGN FILE-INFO:FILE-NAME = ENTRY(i,dirs).
      IF INDEX(FILE-INFO:FILE-TYPE,"D") > 0 THEN /* must be a directory */
          ASSIGN tdirs  = tdirs  + "," + FILE-INFO:FULL-PATHNAME
                 dirs2  = dirs2  + "," + ENTRY(i,dirs).
      ELSE IF ENTRY(i,dirs) NE "" AND 
              SUBSTRING(ENTRY(i,dirs),LENGTH(ENTRY(i,dirs)) - 1,2,"CHARACTER":U) 
              NE "PL" AND p_mode NE "{&WT-CONTROL}":U THEN
                /* Skip .pl's and null entries */
                MESSAGE "Directory: " + ENTRY(i,dirs) + " is defined for this object" skip
                  "but does not currently exist on this system." skip(1)
                  "This directory name will be ignored."
                  VIEW-AS ALERT-BOX WARNING BUTTONS OK.
  END.
  ASSIGN dirs  = SUBSTRING(dirs2,2,-1,"CHARACTER")
         tdirs = SUBSTRING(tdirs,2,-1,"CHARACTER").
           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Check_FileChosen d_openso 
PROCEDURE Check_FileChosen :
/*------------------------------------------------------------------------------
  Purpose: Check the value of the variable fileChosen and make sure it is valid.
           Reset filechose based on the value in the filling.    
  Parameters:  p_OK - FALSE if the file is not good
  Notes:       
------------------------------------------------------------------------------ */  
  DEFINE OUTPUT PARAMETER p_OK AS LOGICAL NO-UNDO INITIAL yes.
  DEFINE VARIABLE ocxStatus    AS INTEGER NO-UNDO.
  DEFINE VARIABLE dispMessage  AS LOGICAL NO-UNDO INITIAL no.
  DO WITH FRAME {&FRAME-NAME}: 
    IF NOT newone AND NOT browseone THEN DO:

        if p_mode = "{&WT-CONTROL}" then do:
            run processOCX(output ocxStatus).
            IF ocxStatus = 0 then
                assign
                    p_OK = no
                    p_otherThing = ""
                    dispMessage = yes
                .
            else if ocxStatus = -1 then
                assign
                    p_OK = no
                    p_otherThing = ""
                    dispMessage = no
                . 
        end.
        else do: 
            run processObj(output p_OK).
            if not p_OK then dispMessage = true.
        end.
 
        IF dispMessage then
            MESSAGE "Please select a file." VIEW-AS ALERT-BOX WARNING. 
    END.    
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI d_openso _DEFAULT-DISABLE
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
  HIDE FRAME d_openso.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI d_openso _DEFAULT-ENABLE
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
  DISPLAY listlbl filename s_files cb_filters cb_dirs 
      WITH FRAME d_openso.
  ENABLE listlbl filename s_files cb_filters cb_dirs 
      WITH FRAME d_openso.
  VIEW FRAME d_openso.
  {&OPEN-BROWSERS-IN-QUERY-d_openso}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initAdmGUI d_openso 
PROCEDURE initAdmGUI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
----------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:
  IF p_newTemplate = "" OR p_newTemplate = ? THEN
      b_New:HIDDEN = TRUE. /* turn it off */
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initGUI d_openso 
PROCEDURE initGUI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
 * Do the common things first ...
 */
DO WITH FRAME {&FRAME-NAME}:
    do i = 1 to num-entries(p_showOptions):
        case entry(i, p_showOptions):
            when "BROWSE"  then enable b_browse.
            when "NEW"     then enable b_new .
            when "PREVIEW" then enable b_preview.
        end.
end.

if p_mode = "{&WT-CONTROL}" then run initOCXGUI.
                          else run initAdmGUI.

END.                          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initOCXGUI d_openso 
PROCEDURE initOCXGUI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processObj d_openso 
PROCEDURE processObj :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER p_OK AS LOGICAL NO-UNDO INITIAL yes.

DO WITH FRAME {&FRAME-NAME}:

  ASSIGN p_fileChosen = cb_dirs:SCREEN-VALUE + OS-SLASH + s_files:SCREEN-VALUE.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processOCX d_openso 
PROCEDURE processOCX :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER OCXStatus AS INTEGER NO-UNDO INITIAL 1.

DEFINE VARIABLE pos      AS INTEGER   NO-UNDO.
define variable s        as integer   no-undo.
define variable cList    as character no-undo.
define variable fullName as character no-undo.

do with frame {&FRAME-NAME}:

    /*
     * If there is already a p_fileChosen and p_otherThing that
     * means that the user has come through here before (probably
     * did a ppreview). Return with the same values. This is
     * needed if there is a OCX dll with multiple OCXs. If
     * we go through the code again the user will be forced to
     * pick the dialog box again.
     */

    if p_fileChosen <> "" and p_otherThing <> "" then return.
    
    assign
        fullName = cb_dirs:SCREEN-VALUE + OS-SLASH + s_files:SCREEN-VALUE
        p_fileChosen = s_files:SCREEN-VALUE 
      .
    /*
     * The user supplied a OCX DLL. Go get the control..
     */
    
    /* Call to GetControlsOfLib moved to _getctrl.p */    
    RUN adeuib/_getctrl.p (INPUT fullName, OUTPUT cList, OUTPUT s).
    
    if s <> 0 then do:
       assign
           p_fileChosen = ""
           OCXStatus = -1
       .

        return.
    end.
    /*
     * IF there are more than 1 control in the OCX then present the
     * user with a choice.
     */
   
    if num-entries(cList, chr(10)) = 0 then do:
    
       /*
        * There is a problem. 
        */
        
       message "A problem was detected trying to access the" skip
               fullname " control." skip

               view-as alert-box.
       assign
           p_fileChosen = ""
           OCXStatus = -1
       .
       return.
    end.

    if num-entries(cList, chr(10)) > 1 then do:

        run adecomm/_setcurs.p("WAIT").
        run adeuib/_mulcont.w("Choose control from " + p_fileChosen,
                                          replace(cList,chr(10), ","),
                              entry(1, cList, chr(10)),
                              output p_otherThing,
                              output lDummy).
                              
        if lDummy = true then do:
           assign
               p_fileChosen = ""
               OCXStatus = -1.
           .
           return.
        end.
    end.
    else do:

        p_otherThing = entry(1, cList, CHR(10)).
    end.        
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Setup d_openso 
PROCEDURE Setup :
/* -----------------------------------------------------------
  Purpose:     Setup dialog
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER rc AS LOGICAL INITIAL yes.
DEFINE VARIABLE         i  AS INTEGER NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF p_cst-attr <> "" THEN DO:
    DO i = 1 TO NUM-ENTRIES(p_cst-attr,CHR(10)):
        IF ENTRY(i,p_cst-attr,CHR(10)) BEGINS "DIRECTORY-LIST" THEN
            dirs = TRIM(SUBSTRING(TRIM(ENTRY(i,p_cst-attr,CHR(10))),15,-1,"CHARACTER")).
        ELSE IF ENTRY(i,p_cst-attr,CHR(10)) BEGINS "FILTER" THEN
            filters = TRIM(SUBSTRING(TRIM(ENTRY(i,p_cst-attr,CHR(10))),7,-1,"CHARACTER")).
        ELSE IF ENTRY(i,p_cst-attr,CHR(10)) BEGINS "TITLE" THEN
            dtitle = TRIM(SUBSTRING(TRIM(ENTRY(i,p_cst-attr,CHR(10))),6,-1,"CHARACTER")).
    END.
    IF dirs = "" OR filters = "" THEN DO:
        rc = no.
        RETURN.
    END.
    IF dtitle = "" THEN DO:
      IF p_mode = "{&WT-CONTROL}" THEN dTitle = "Choose {&WL-CONTROL}".
                                ELSE dtitle = "Choose Object".
    END.
    RUN Check_Dirs.
    ASSIGN cb_filters:LIST-ITEMS     = filters
           cb_dirs:LIST-ITEMS        = dirs
           FRAME {&FRAME-NAME}:TITLE = dtitle
           cb_filters:SCREEN-VALUE   = cb_filters:ENTRY(1)
           cb_dirs:SCREEN-VALUE      = cb_dirs:ENTRY(1)
           cb_dirs                   = cb_dirs:SCREEN-VALUE
           cb_filters                = cb_filters:SCREEN-VALUE
           filename                  = ""
           p_otherThing              = ""
        .

    RUN BuildFileList.
    IF p_mode = "{&WT-CONTROL}" THEN listlbl = "{&WL-CONTROL}" + ":".
  END.
  ELSE rc = no.
END.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set_First d_openso 
PROCEDURE Set_First :
/* -----------------------------------------------------------
  Purpose:     Selects the first file in the list.
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF s_files:NUM-ITEMS > 0 AND s_files:ENTRY(1) NE "<None>" THEN
      ASSIGN s_files:SCREEN-VALUE = s_files:ENTRY(1).
  APPLY "VALUE-CHANGED" TO s_files IN FRAME {&FRAME-NAME}.
  APPLY "ENTRY" TO s_files IN FRAME {&FRAME-NAME}.

END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set_Sensitivity d_openso 
PROCEDURE Set_Sensitivity :
/*------------------------------------------------------------------------------
  Purpose:     Sets the Sensitivity of various items in the dialog-box based
               on the current situation. 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    /* We cannot preview if there are no files selected. */
    b_Preview:SENSITIVE = (filename:SCREEN-VALUE ne "").
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


