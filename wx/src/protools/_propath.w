&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME f
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS f 
/*------------------------------------------------------------------------

  File: _propath.w

  Description: Let the user view and modify the PROPATH attribute.

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Wm.T.Wood 

  Created: 08/03/93 - 12:59 pm
  
  Modified by Gerry Seidl

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
{ protools/ptlshlp.i } /* help definitions */
{ adecomm/_adetool.i }

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS path-list Btn_OK Btn_Cancel b_add b_delete ~
b_search b_move_up b_move_down BUTTON-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel":L 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK":L 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-3 
     LABEL "&Help" 
     SIZE 15 BY 1.14.

DEFINE BUTTON b_add 
     LABEL "&Add...":L 
     SIZE 15 BY 1.14.

DEFINE BUTTON b_delete 
     LABEL "&Remove":L 
     SIZE 15 BY 1.14.

DEFINE BUTTON b_move_down 
     LABEL "Move &Down":L 
     SIZE 15 BY 1.14.

DEFINE BUTTON b_move_up 
     LABEL "Move &Up":L 
     SIZE 15 BY 1.14.

DEFINE BUTTON b_search 
     LABEL "&Search..." 
     SIZE 15 BY 1.14.

DEFINE VARIABLE path-list AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     SIZE 34 BY 10.19 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f
     path-list AT ROW 1.57 COL 3 NO-LABEL
     Btn_OK AT ROW 1.57 COL 39
     Btn_Cancel AT ROW 2.86 COL 39
     b_add AT ROW 4.33 COL 39
     b_delete AT ROW 5.57 COL 39
     b_search AT ROW 6.81 COL 39
     b_move_up AT ROW 8.1 COL 39
     b_move_down AT ROW 9.33 COL 39
     BUTTON-3 AT ROW 10.57 COL 39
     SPACE(1.39) SKIP(0.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS THREE-D  SCROLLABLE 
         TITLE "Propath Editor":L
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.

 

/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX f
   UNDERLINE                                                            */
ASSIGN 
       FRAME f:SCROLLABLE       = FALSE.

/* SETTINGS FOR BUTTON Btn_Cancel IN FRAME f
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON Btn_OK IN FRAME f
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON b_add IN FRAME f
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON b_delete IN FRAME f
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON b_move_down IN FRAME f
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON b_move_up IN FRAME f
   NO-DISPLAY                                                           */
/* SETTINGS FOR SELECTION-LIST path-list IN FRAME f
   NO-DISPLAY                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 f
ON CHOOSE OF BUTTON-3 IN FRAME f /* Help */
DO:
  RUN adecomm/_adehelp.p ( "ptls", "CONTEXT", {&PROPATH_Editor}, ? ).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b_add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_add f
ON CHOOSE OF b_add IN FRAME f /* Add... */
DO:
  DEFINE VAR l_ok           AS LOGICAL   NO-UNDO.
  DEFINE VAR Invalid_IsOK   AS LOGICAL   NO-UNDO INIT NO.
  DEFINE VAR file_ext       AS CHARACTER NO-UNDO.
  
  DEFINE FRAME f_add
         "Directory:" VIEW-AS TEXT AT ROW 1.5 COL 2
         new_dir    AS CHAR FORMAT "X(132)" VIEW-AS FILL-IN SIZE 30 BY 1.08
                    AT 2
         btn_OK     AT ROW 1.5 COL 34
         SKIP (0.1)
         btn_cancel AT 34 SPACE (1)
         SKIP (0.5)
         WITH SIDE-LABELS NO-LABELS THREE-D
              DEFAULT-BUTTON btn_OK 
              VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
                      TITLE "Add Directory to Propath".
  
  /* Parent the dialog-box to same window as the "parent" dialog.   */
  FRAME f_add:PARENT = FRAME {&FRAME-NAME}:PARENT.
  ON WINDOW-CLOSE OF FRAME f_add APPLY "END-ERROR" TO SELF.

  l_ok = NO.
  MAIN_LOOP:
  DO WHILE NOT l_ok
     ON STOP UNDO, LEAVE  ON ENDKEY UNDO, LEAVE:
    RUN adecomm/_setcurs.p (INPUT "":U).
    ENABLE new_dir btn_OK btn_cancel WITH FRAME f_add.
    WAIT-FOR GO OF FRAME f_add.
    RUN adecomm/_setcurs.p (INPUT "WAIT":U).
    ASSIGN new_dir:SCREEN-VALUE = TRIM(new_dir:SCREEN-VALUE)
           new_dir:SCREEN-VALUE = "." WHEN new_dir:SCREEN-VALUE = ""
           .

    /* Is the choice a valid directory? i.e. It is blank (shorthand for 
       the current directory)  OR it is a real directory. */
    ASSIGN FILE-INFO:FILENAME = new_dir:SCREEN-VALUE
           Invalid_IsOK       = NO.
    IF ( FILE-INFO:FULL-PATHNAME = ? ) THEN
    DO:
        RUN adecomm/_osfext.p (INPUT new_dir:SCREEN-VALUE, OUTPUT file_ext).
        MESSAGE new_dir:SCREEN-VALUE SKIP
                IF file_ext <> ".pl" THEN
                    "Cannot find this directory."
                ELSE
                    "Cannot find this r-code library."
                SKIP(1)
                "Do you want to continue?"
                VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO 
                        UPDATE Invalid_IsOK .
        IF ( Invalid_IsOK = NO ) THEN NEXT MAIN_LOOP.
    END.
    /* If its not a directory, it must be an r-code lib. Otherwise, invalid. */
    IF ( NOT FILE-INFO:FILE-TYPE BEGINS "D" ) THEN
    DO:
        RUN adecomm/_osfext.p (INPUT new_dir:SCREEN-VALUE, OUTPUT file_ext).
        IF file_ext <> ".pl":U THEN
        DO ON STOP UNDO, RETRY:
          IF NOT RETRY THEN
          MESSAGE new_dir:SCREEN-VALUE SKIP
                  "This is not a valid directory or r-code library."
                  VIEW-AS ALERT-BOX WARNING.
          NEXT MAIN_LOOP.
        END.
    END.
    ASSIGN l_ok = YES.
    ASSIGN l_ok = path-list:ADD-FIRST ("  0. " + (IF FILE-INFO:FILENAME = "." THEN "[current directory]" ELSE FILE-INFO:FILENAME)).
    IF l_ok THEN
    DO:
        RUN number_path.
        path-list:SCREEN-VALUE = path-list:ENTRY(1).
    END.
  END.
  RUN adecomm/_setcurs.p (INPUT "":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b_delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_delete f
ON CHOOSE OF b_delete IN FRAME f /* Remove */
DO:
  DEFINE VAR i      AS INTEGER NO-UNDO.
  DEFINE VAR choice AS CHAR    NO-UNDO.
  DEFINE VAR l_ok   AS LOGICAL NO-UNDO.
  DEFINE VAR adedir AS CHAR    NO-UNDO.
  DEFINE VAR dlcdir AS CHAR    NO-UNDO.
  
  ASSIGN dlcdir = OS-GETENV("DLC")
         adedir = dlcdir + (IF LOOKUP(OPSYS, "MSDOS,WIN32") > 0 THEN "~\gui" ELSE "/gui").
  
  choice = path-list:SCREEN-VALUE.
  IF INDEX(choice,adedir) > 0 OR INDEX(choice,dlcdir) > 0 THEN DO:
      MESSAGE "You cannot remove the ADE or DLC directory from the Propath!"
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN NO-APPLY.
  END.
  DO i = 1 TO NUM-ENTRIES(choice):
    l_ok = path-list:DELETE(ENTRY(i,choice)).
  END.
  /* Renumber the path */
  RUN number_path.
  path-list:SCREEN-VALUE = path-list:ENTRY(1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b_move_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_move_down f
ON CHOOSE OF b_move_down IN FRAME f /* Move Down */
DO:
  DEFINE VAR i           AS INTEGER NO-UNDO.
  DEFINE VAR ipos        AS INTEGER NO-UNDO.
  DEFINE VAR choice      AS CHAR NO-UNDO.
  DEFINE VAR swap-item   AS CHAR NO-UNDO.
  DEFINE VAR choice-item AS CHAR NO-UNDO.
  DEFINE VAR new-swap    AS CHAR NO-UNDO.
  DEFINE VAR new-choice  AS CHAR NO-UNDO.
  DEFINE VAR l_ok        AS LOGICAL NO-UNDO.
  
  ASSIGN choice = path-list:SCREEN-VALUE.
  DO i = NUM-ENTRIES(choice) TO 1 BY -1:
    ASSIGN choice-item = ENTRY (i,choice) 
           ipos        = INTEGER (SUBSTRING(choice-item,1,3)).
    IF ipos < path-list:NUM-ITEMS THEN DO:
      /* Get the item above the current choice and swap it with the current choice.
         Change the line numbers of the choice to the swapped line number (this
         is the first 3 characters). */
      swap-item  = path-list:ENTRY(ipos + 1).
      IF LOOKUP(swap-item,choice) eq 0
      THEN ASSIGN
             new-choice = choice-item
             new-swap   = swap-item
             SUBSTRING(new-swap,1,3)   = SUBSTRING(choice-item,1,3)
             SUBSTRING(new-choice,1,3) = SUBSTRING(swap-item,1,3)
             l_ok = path-list:REPLACE( new-swap, choice-item)
             l_ok = path-list:REPLACE( new-choice, swap-item)
             ENTRY(i,choice) = new-choice.
    END.
  END.
  /* reset the value of the choice. */
  path-list:SCREEN-VALUE = choice.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b_move_up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_move_up f
ON CHOOSE OF b_move_up IN FRAME f /* Move Up */
DO:
  DEFINE VAR i           AS INTEGER NO-UNDO.
  DEFINE VAR ipos        AS INTEGER NO-UNDO.
  DEFINE VAR choice      AS CHAR NO-UNDO.
  DEFINE VAR swap-item   AS CHAR NO-UNDO.
  DEFINE VAR choice-item AS CHAR NO-UNDO.
  DEFINE VAR new-swap    AS CHAR NO-UNDO.
  DEFINE VAR new-choice  AS CHAR NO-UNDO.
  DEFINE VAR l_ok        AS LOGICAL NO-UNDO.
  
  ASSIGN choice = path-list:SCREEN-VALUE.
  DO i = 1 TO NUM-ENTRIES(choice):
    ASSIGN choice-item = ENTRY (i,choice) 
           ipos        = INTEGER (SUBSTRING(choice-item,1,3)).
    IF ipos > 1 THEN DO:
      /* Get the item above the current choice and swap it with the current choice.
         Change the line numbers of the choice to the swapped line number (this
         is the first 3 characters). */
      swap-item  = path-list:ENTRY(ipos - 1).
      IF LOOKUP(swap-item,choice) eq 0
      THEN ASSIGN
             new-choice = choice-item
             new-swap   = swap-item
             SUBSTRING(new-swap,1,3)   = SUBSTRING(choice-item,1,3)
             SUBSTRING(new-choice,1,3) = SUBSTRING(swap-item,1,3)
             l_ok = path-list:REPLACE( new-swap, choice-item)
             l_ok = path-list:REPLACE( new-choice, swap-item)
             ENTRY(i,choice) = new-choice.
    END.
  END.
  /* reset the value of the choice. */
  path-list:SCREEN-VALUE = choice.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b_search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b_search f
ON CHOOSE OF b_search IN FRAME f /* Search... */
DO:
  RUN protools/_search.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK f 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Populate the selection list */
RUN get_path.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
  /* Reset PROPATH based on any changes */
  RUN set_propath.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI f _DEFAULT-DISABLE
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
  HIDE FRAME f.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI f _DEFAULT-ENABLE
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
  ENABLE path-list Btn_OK Btn_Cancel b_add b_delete b_search b_move_up 
         b_move_down BUTTON-3 
      WITH FRAME f.
  {&OPEN-BROWSERS-IN-QUERY-f}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get_path f 
PROCEDURE get_path :
/* -----------------------------------------------------------
  Purpose:     Fill the selection list with the contents of 
               PROPATH.  Note that we put line numbers on each line 
               (and we support up to 999 items in the propath).
  Parameters:  <none>
 -------------------------------------------------------------*/
  DEFINE VAR plist AS CHAR    NO-UNDO.
  DEFINE VAR i     AS INTEGER NO-UNDO.
  DEFINE VAR cnt   AS INTEGER NO-UNDO.
  
  cnt = NUM-ENTRIES(PROPATH).
  IF cnt > 999 THEN DO:
    MESSAGE "Only first 999 items in the path will be displayed."
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
    cnt = 999.
  END.
  /* Add a number to the front of each line (this will allow us to have unique entries). */
  DO i = 1 TO cnt:
    IF entry(i,PROPATH) = "." THEN 
         plist = (IF i eq 1 THEN "" ELSE plist + ",") + STRING(i,">>9") + ". " + "[current directory]".
    ELSE plist = (IF i eq 1 THEN "" ELSE plist + ",") + STRING(i,">>9") + ". " + ENTRY(i,PROPATH).
  END.
  path-list:LIST-ITEMS IN FRAME {&FRAME-NAME} = plist.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE number_path f 
PROCEDURE number_path :
/* -----------------------------------------------------------
  Purpose:     Change the first 3 characters of a line in path-list
               to be the line numbe (in format ">>9").
  Parameters:  <none>
 -------------------------------------------------------------*/
  DEFINE VAR plist AS CHAR    NO-UNDO.
  DEFINE VAR i     AS INTEGER NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    /* Remove a number from each line.  */
    DO i = 1 TO path-list:NUM-ITEMS:
      plist = (IF i eq 1 THEN "" ELSE plist + ",") + 
              STRING(i,">>9") + SUBSTRING(path-list:ENTRY(i),4).
    END.
    path-list:LIST-ITEMS = plist.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set_propath f 
PROCEDURE set_propath :
/* -----------------------------------------------------------
  Purpose:     Copy the contents of the path-list to PROPATH. 
               Note that we need to remove the line numbers from
               each line (in format ">>9. ") by taking the substring
               of each line.
  Parameters:  <none>
 -------------------------------------------------------------*/
  DEFINE VAR path   AS CHAR    CASE-SENSITIVE  NO-UNDO.
  DEFINE VAR i      AS INTEGER                 NO-UNDO.
  DEFINE VAR choice AS LOGICAL                 NO-UNDO.
  DEFINE VAR dlcdir AS CHAR                    NO-UNDO.
  DEFINE VAR oldpp  AS CHAR    FORMAT "X(4096)" NO-UNDO. /* save to INI file */
  
  DO WITH FRAME {&FRAME-NAME}:
    /* Remove a number from each line.  */
    DO i = 1 TO path-list:NUM-ITEMS:
      path = (IF i eq 1 THEN "" ELSE path + ",") + SUBSTRING(path-list:ENTRY(i),6).
    END.
  END.
  ASSIGN i = LOOKUP("[current directory]",path).
  IF i > 0 THEN
      ENTRY(i,path) = ".".
  /* Store PROPATH if it has changed */
  IF PROPATH NE path THEN DO:
      ASSIGN PROPATH = path.
      MESSAGE "Propath was changed only for this session. " skip
          "Do you want to save it to your startup file?"  skip
          "(the previous value will be saved to OLD-PROPATH)"
          VIEW-AS ALERT-BOX QUESTION BUTTONS yes-no UPDATE choice.
      IF choice THEN DO:
          /* Since %DLC%\GUI, %DLC% & %DLC%\BIN are automatically added, we will not write
           * them out */
          ASSIGN dlcdir = OS-GETENV("DLC") + (IF LOOKUP(OPSYS, "MSDOS,WIN32") > 0 THEN "~\gui" ELSE "/gui").
          i = LOOKUP(dlcdir + (IF LOOKUP(OPSYS, "MSDOS,WIN32") > 0 THEN "~\gui" ELSE "/gui"),PATH).
          IF i > 0 THEN ENTRY(i,PATH) = "".
          i = LOOKUP(dlcdir + (IF LOOKUP(OPSYS, "MSDOS,WIN32") > 0 THEN "~\bin" ELSE "/bin"),PATH).
          IF i > 0 THEN ENTRY(i,PATH) = "".
          i = LOOKUP(dlcdir,PATH).
          IF i > 0 THEN ENTRY(i,PATH) = "".
          PATH = REPLACE(PATH,",,",","). /* remove null entries */
          IF LENGTH(PATH) > 4096 THEN DO:
              MESSAGE "Propath exceeds 4096 bytes. It cannot be saved to your startup file"
                  VIEW-AS ALERT-BOX WARNING BUTTONS OK.
              RETURN.
          END.      
          GET-KEY-VALUE SECTION "Startup" KEY "PROPATH" VALUE oldpp.
          IF LENGTH(oldpp) > 0 THEN /* save off old ProPath value */
              PUT-KEY-VALUE SECTION "Startup" KEY "OLD-PROPATH" VALUE oldpp NO-ERROR.
          /* write out new propath */
          PUT-KEY-VALUE SECTION "Startup" KEY "PROPATH" VALUE path NO-ERROR.
          IF ERROR-STATUS:ERROR THEN RUN adeshar/_puterr.p ("Propath Editor", CURRENT-WINDOW).
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


