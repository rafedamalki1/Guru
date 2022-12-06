&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
/* Parameters Definitions ---                                           */
{KONSTRMTRL.I}
DEFINE INPUT PARAMETER flabel    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER brwulabel AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER brwvlabel AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR ukonsttemp.
DEFINE INPUT  PARAMETER valdkod AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER valdben AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR vkonsttemp.

/* Local Variable Definitions ---                                       */
&Scoped-define NEW 
&Scoped-define SHARED SHARED

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_UKON

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ukonsttemp vkonsttemp

/* Definitions for BROWSE BRW_UKON                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_UKON ukonsttemp.KTYPKOD ~
ukonsttemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UKON 
&Scoped-define QUERY-STRING-BRW_UKON FOR EACH ukonsttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UKON OPEN QUERY BRW_UKON FOR EACH ukonsttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UKON ukonsttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UKON ukonsttemp


/* Definitions for BROWSE BRW_VKON                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VKON vkonsttemp.KTYPKOD ~
vkonsttemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VKON 
&Scoped-define QUERY-STRING-BRW_VKON FOR EACH vkonsttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VKON OPEN QUERY BRW_VKON FOR EACH vkonsttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VKON vkonsttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VKON vkonsttemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_UKON BRW_VKON BTN_OVER BTN_BACK Btn_OK ~
Btn_AVB FILL-IN-VALTXT 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-VALTXT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_AVB AUTO-GO 
     LABEL "aVBRYT" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u.bmp":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE VARIABLE FILL-IN-VALTXT AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 117.5 BY 1.21
     FONT 17 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_UKON FOR 
      ukonsttemp SCROLLING.

DEFINE QUERY BRW_VKON FOR 
      vkonsttemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UKON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UKON Dialog-Frame _STRUCTURED
  QUERY BRW_UKON NO-LOCK DISPLAY
      ukonsttemp.KTYPKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
      ukonsttemp.BENAMNING FORMAT "X(256)":U WIDTH 22
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH MULTIPLE SIZE 40 BY 12
         TITLE "Browse 1".

DEFINE BROWSE BRW_VKON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VKON Dialog-Frame _STRUCTURED
  QUERY BRW_VKON NO-LOCK DISPLAY
      vkonsttemp.KTYPKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
      vkonsttemp.BENAMNING FORMAT "X(256)":U WIDTH 22
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH MULTIPLE SIZE 40 BY 12
         TITLE "Browse 1".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BRW_UKON AT ROW 3 COL 1.5
     BRW_VKON AT ROW 3 COL 47.5
     BTN_OVER AT ROW 6 COL 42.38
     BTN_BACK AT ROW 8.5 COL 42.5
     Btn_OK AT ROW 15.17 COL 90.88
     Btn_AVB AT ROW 15.17 COL 106.88
     FILL-IN-VALTXT AT ROW 1.29 COL 2.5 NO-LABEL
     SPACE(1.99) SKIP(13.82)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_AVB.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: ukonsttemp T "?" NO-UNDO temp-db ukonsttemp
      TABLE: vkonsttemp T "?" NO-UNDO temp-db vkonsttemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_UKON 1 Dialog-Frame */
/* BROWSE-TAB BRW_VKON BRW_UKON Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BTN_BACK:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       BTN_OVER:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VALTXT IN FRAME Dialog-Frame
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-VALTXT:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UKON
/* Query rebuild information for BROWSE BRW_UKON
     _TblList          = "Temp-Tables.ukonsttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.ukonsttemp.KTYPKOD
"ukonsttemp.KTYPKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ukonsttemp.BENAMNING
"ukonsttemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_UKON */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VKON
/* Query rebuild information for BROWSE BRW_VKON
     _TblList          = "Temp-Tables.vkonsttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.vkonsttemp.KTYPKOD
"vkonsttemp.KTYPKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.vkonsttemp.BENAMNING
"vkonsttemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VKON */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
   EMPTY TEMP-TABLE vkonsttemp NO-ERROR. 
   {BORTBRWPROC.I}
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_AVB Dialog-Frame
ON CHOOSE OF Btn_AVB IN FRAME Dialog-Frame /* aVBRYT */
DO:
   EMPTY TEMP-TABLE vkonsttemp NO-ERROR. 
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
   
   RETURN.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UKON
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
   {DIA_M_START.I}
   {muswait.i}  
   {ALLSTARTDYN.I} 
   ASSIGN
   FILL-IN-VALTXT = "Kopplad materiel för " + valdkod + " " + valdben + " i gruppen " + brwvlabel + ", kommer att kopplas till valda konstruktioner"
   BRW_UKON:TITLE = brwulabel  
   BRW_VKON:TITLE = brwulabel.
   RUN openbdynspec_UI IN brwproc[1].
   RUN enable_UI.
   {FRMSIZED.I}  
   {musarrow.i}        
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI Dialog-Frame 
PROCEDURE allstartbrw_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_UKON:HANDLE IN FRAME {&FRAME-NAME}).   
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_VKON:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNARROW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_UKON:HANDLE, INPUT BRW_VKON:HANDLE ,
       INPUT INPUT BTN_OVER:HANDLE, INPUT ? , INPUT ?, INPUT INPUT BTN_BACK:HANDLE).
   RUN setcolindex_UI IN brwproc[1] (INPUT "ORDNING").    
   RUN setcolindex_UI IN brwproc[2] (INPUT "ORDNING").    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-VALTXT 
      WITH FRAME Dialog-Frame.
  ENABLE BRW_UKON BRW_VKON BTN_OVER BTN_BACK Btn_OK Btn_AVB FILL-IN-VALTXT 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

