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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
{PERSONALTEMP.I}
DEFINE VARIABLE teletapph AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_VPERS

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES personaltemp

/* Definitions for BROWSE BRW_VPERS                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VPERS personaltemp.FORNAMN ~
personaltemp.EFTERNAMN personaltemp.TELEFON personaltemp.MOBILTEL ~
personaltemp.GATUADRESS personaltemp.POSTNUMMER personaltemp.POSTADRESS ~
personaltemp.BOXEN personaltemp.PERSONSOK personaltemp.PERSONALKOD ~
personaltemp.OMRADE personaltemp.GODKAND 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VPERS 
&Scoped-define QUERY-STRING-BRW_VPERS FOR EACH personaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VPERS OPEN QUERY BRW_VPERS FOR EACH personaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VPERS personaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VPERS personaltemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_VPERS Btn_OK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 14 BY 1
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_VPERS FOR 
      personaltemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_VPERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VPERS Dialog-Frame _STRUCTURED
  QUERY BRW_VPERS NO-LOCK DISPLAY
      personaltemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 13
      personaltemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 15
      personaltemp.TELEFON COLUMN-LABEL "Telefon" FORMAT "x(15)":U
            WIDTH 12
      personaltemp.MOBILTEL COLUMN-LABEL "Mobiltele" FORMAT "x(15)":U
            WIDTH 12
      personaltemp.GATUADRESS COLUMN-LABEL "Gatuadress" FORMAT "X(25)":U
            WIDTH 20
      personaltemp.POSTNUMMER COLUMN-LABEL "P-nr" FORMAT "XXX XX":U
            WIDTH 8
      personaltemp.POSTADRESS COLUMN-LABEL "Postadress" FORMAT "X(20)":U
      personaltemp.BOXEN COLUMN-LABEL "Box" FORMAT "x(256)":U WIDTH 7
      personaltemp.PERSONSOK COLUMN-LABEL "E-post" FORMAT "x(256)":U
            WIDTH 20
      personaltemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
            WIDTH 7
      personaltemp.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      personaltemp.GODKAND COLUMN-LABEL "Användare" FORMAT "X(30)":U
            WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 122.5 BY 26
         FONT 4
         TITLE "Telefon och Epost lista".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BRW_VPERS AT ROW 1.5 COL 1.38
     Btn_OK AT ROW 27.63 COL 111
     SPACE(1.00) SKIP(0.25)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Telefonlista"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: personaltemp T "?" NO-UNDO temp-db personaltemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_VPERS 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BRW_VPERS:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE
       BRW_VPERS:COLUMN-RESIZABLE IN FRAME Dialog-Frame       = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VPERS
/* Query rebuild information for BROWSE BRW_VPERS
     _TblList          = "Temp-Tables.personaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.personaltemp.FORNAMN
"personaltemp.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.personaltemp.EFTERNAMN
"personaltemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.personaltemp.TELEFON
"personaltemp.TELEFON" "Telefon" "x(15)" "character" ? ? ? ? ? ? no "" no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.personaltemp.MOBILTEL
"personaltemp.MOBILTEL" "Mobiltele" "x(15)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.personaltemp.GATUADRESS
"personaltemp.GATUADRESS" "Gatuadress" ? "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.personaltemp.POSTNUMMER
"personaltemp.POSTNUMMER" "P-nr" ? "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.personaltemp.POSTADRESS
"personaltemp.POSTADRESS" "Postadress" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.personaltemp.BOXEN
"personaltemp.BOXEN" "Box" "x(256)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.personaltemp.PERSONSOK
"personaltemp.PERSONSOK" "E-post" "x(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.personaltemp.PERSONALKOD
"personaltemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.personaltemp.OMRADE
"personaltemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.personaltemp.GODKAND
"personaltemp.GODKAND" "Användare" "X(30)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VPERS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Telefonlista */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VPERS
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
   RUN enable_UI.       
   {FRMSIZED.I}
   {musarrow.i}
   {DIA_M_SLUT.I}
   RUN openbdyn_UI IN brwproc[1] (INPUT "").  
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
   DEBUGGER:SET-BREAK().
   RUN DYNBRW.P PERSISTENT SET brwproc[1] (INPUT BRW_VPERS:HANDLE IN FRAME {&FRAME-NAME}).       
   IF Guru.Konstanter:appcon THEN DO:
      RUN TELELAPP.P PERSISTENT SET teletapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT (OUTPUT TABLE personaltemp). 
   END.
   ELSE DO:      
      RUN TELELAPP.P PERSISTENT SET teletapph (OUTPUT TABLE personaltemp). 
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
  ENABLE BRW_VPERS Btn_OK 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

