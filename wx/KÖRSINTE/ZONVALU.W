&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

&Scoped-define SHARED SHARED 
{DIRDEF.I}
{AONRDEF.I}
{OMRTEMPW.I}
{AVTAONRTEMP.I}
{AUTOMREGTEMP.I}
{ANSPROJBER.I}
DEFINE SHARED VARIABLE omrvar AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE zonapph AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_ZON

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES zontemp

/* Definitions for BROWSE BRW_ZON                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_ZON zontemp.ZONNR zontemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ZON zontemp.ZONNR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_ZON zontemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_ZON zontemp
&Scoped-define QUERY-STRING-BRW_ZON FOR EACH zontemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ZON OPEN QUERY BRW_ZON FOR EACH zontemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ZON zontemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ZON zontemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_ZON BTN_OK BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ZON FOR 
      zontemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ZON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ZON Dialog-Frame _STRUCTURED
  QUERY BRW_ZON NO-LOCK DISPLAY
      zontemp.ZONNR COLUMN-LABEL "Zon" FORMAT "x(6)":U
      zontemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "x(15)":U
            WIDTH 21.5
  ENABLE
      zontemp.ZONNR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 31.5 BY 8
         TITLE "Zoner under området".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BRW_ZON AT ROW 1.5 COL 1.5
     BTN_OK AT ROW 9.92 COL 4
     BTN_AVB AT ROW 9.92 COL 19
     SPACE(0.74) SKIP(0.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Zon".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: zontemp T "?" NO-UNDO temp-db zontemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_ZON 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ZON
/* Query rebuild information for BROWSE BRW_ZON
     _TblList          = "Temp-Tables.zontemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.zontemp.ZONNR
"zontemp.ZONNR" "Zon" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.zontemp.BENAMNING
"zontemp.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no "21.5" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ZON */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR,ENDKEY OF FRAME Dialog-Frame /* Zon */
DO:
   {BORTBRWPROC.I}
   musz = TRUE.
   IF VALID-HANDLE(zonapph) THEN DELETE PROCEDURE zonapph.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Zon */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:     
   ASSIGN
   antal_valda = BRW_ZON:NUM-SELECTED-ROWS
   antal_raknare = 1.   
   IF antal_valda NE 0 THEN DO:
      DO WHILE antal_raknare LE antal_valda:
         status-ok = BRW_ZON:FETCH-SELECTED-ROW(antal_raknare).    
         CREATE zonvaltemp.
         ASSIGN
         zonvaltemp.OMRADE = zontemp.OMRADE
         zonvaltemp.ZONNR = zontemp.ZONNR.
         antal_raknare = antal_raknare + 1.
      END.
   END.
   ELSE DO:
      MESSAGE "Ingen zon vald!"
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      RETURN NO-APPLY.
   END.      
   {BORTBRWPROC.I}
   IF VALID-HANDLE(zonapph) THEN DELETE PROCEDURE zonapph.
   APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ZON
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
   {ALLSTARTDYN.I} 
   FIND FIRST omrtemp WHERE omrtemp.OMRADE = omrvar USE-INDEX OMR
   NO-LOCK NO-ERROR.    
   FRAME {&FRAME-NAME}:TITLE = "Zoner under " + LC(Guru.Konstanter:gomrl) + ":" + omrtemp.NAMN.   
   RUN zonhmt IN zonapph (INPUT omrvar,OUTPUT TABLE zontemp).

   OPEN QUERY BRW_ZON FOR EACH zontemp WHERE zontemp.OMRADE = omrtemp.OMRADE
   USE-INDEX OMR NO-LOCK.  
   RUN enable_UI.       
   {FRMSIZED.I} 
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI Dialog-Frame 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   ASSIGN
   zontemp.ZONNR:READ-ONLY IN BROWSE BRW_ZON = TRUE.
   
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_ZON:HANDLE IN FRAME {&FRAME-NAME}).
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN ZONAPP.P PERSISTENT SET zonapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN ZONAPP.P PERSISTENT SET zonapph.
   END.
  
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
  ENABLE BRW_ZON BTN_OK BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

