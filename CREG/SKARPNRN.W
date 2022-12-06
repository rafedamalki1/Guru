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
{GLOBVAR2DEL1.I}

DEFINE NEW SHARED VARIABLE skarpproch AS HANDLE NO-UNDO. /* SKARPAPP.P */
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.   
DEFINE VARIABLE vilkenbrw AS INTEGER NO-UNDO.
DEFINE VARIABLE nyttid AS INTEGER NO-UNDO.
DEFINE VARIABLE valfaktrec AS RECID NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE vjid AS CHARACTER NO-UNDO.

&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED 
{SKARPTEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_PRE

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES nrprefistemp faktskarptemp nrstyrtemp

/* Definitions for BROWSE BRW_PRE                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_PRE nrprefistemp.PREFIX ~
nrprefistemp.NRTEXT nrprefistemp.VIJUDID 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PRE nrprefistemp.PREFIX ~
nrprefistemp.NRTEXT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_PRE nrprefistemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_PRE nrprefistemp
&Scoped-define QUERY-STRING-BRW_PRE FOR EACH nrprefistemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PRE OPEN QUERY BRW_PRE FOR EACH nrprefistemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PRE nrprefistemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PRE nrprefistemp


/* Definitions for BROWSE BRW_SKARP                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_SKARP faktskarptemp.AVDELNINGNR ~
faktskarptemp.OMRADE faktskarptemp.ARTAL faktskarptemp.ARKIVSTALLE ~
faktskarptemp.LOPNR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SKARP 
&Scoped-define QUERY-STRING-BRW_SKARP FOR EACH faktskarptemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_SKARP OPEN QUERY BRW_SKARP FOR EACH faktskarptemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_SKARP faktskarptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SKARP faktskarptemp


/* Definitions for BROWSE BRW_STYR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_STYR nrstyrtemp.PREFIX nrstyrtemp.NRTEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_STYR nrstyrtemp.PREFIX ~
nrstyrtemp.NRTEXT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_STYR nrstyrtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_STYR nrstyrtemp
&Scoped-define QUERY-STRING-BRW_STYR FOR EACH nrstyrtemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_STYR OPEN QUERY BRW_STYR FOR EACH nrstyrtemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_STYR nrstyrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_STYR nrstyrtemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_PRE BRW_PRE BRW_SKARP BRW_STYR BTN_NY ~
BTN_AND BTN_BORT BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_PRE 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE VARIABLE RAD_PRE AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Nr.serier för aonr", 2,
"Nr.serier för fakturering", 1
     SIZE 56 BY 1.08 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_PRE FOR 
      nrprefistemp SCROLLING.

DEFINE QUERY BRW_SKARP FOR 
      faktskarptemp SCROLLING.

DEFINE QUERY BRW_STYR FOR 
      nrstyrtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_PRE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PRE Dialog-Frame _STRUCTURED
  QUERY BRW_PRE NO-LOCK DISPLAY
      nrprefistemp.PREFIX COLUMN-LABEL "Prefix" FORMAT "X(2)":U
      nrprefistemp.NRTEXT COLUMN-LABEL "Text för prefix" FORMAT "X(256)":U
            WIDTH 30
      nrprefistemp.VIJUDID COLUMN-LABEL "Jur pers" FORMAT "X(8)":U
  ENABLE
      nrprefistemp.PREFIX
      nrprefistemp.NRTEXT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 52.5 BY 9.25
         TITLE "Prefix".

DEFINE BROWSE BRW_SKARP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SKARP Dialog-Frame _STRUCTURED
  QUERY BRW_SKARP NO-LOCK DISPLAY
      faktskarptemp.AVDELNINGNR COLUMN-LABEL "Avdelning" FORMAT ">>>>9":U
      faktskarptemp.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      faktskarptemp.ARTAL COLUMN-LABEL "Årtal" FORMAT "9999":U
      faktskarptemp.ARKIVSTALLE COLUMN-LABEL "Arkivställe" FORMAT "X(8)":U
      faktskarptemp.LOPNR COLUMN-LABEL "Löpnummer" FORMAT "->>>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 45.5 BY 9.25
         TITLE "Nr.serier för fakturering".

DEFINE BROWSE BRW_STYR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_STYR Dialog-Frame _STRUCTURED
  QUERY BRW_STYR NO-LOCK DISPLAY
      nrstyrtemp.PREFIX COLUMN-LABEL "Styrfält" FORMAT "X(1)":U
      nrstyrtemp.NRTEXT COLUMN-LABEL "Text" FORMAT "X(256)":U WIDTH 30
  ENABLE
      nrstyrtemp.PREFIX
      nrstyrtemp.NRTEXT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 44 BY 9.25
         TITLE "Styrfält".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     RAD_PRE AT ROW 1.46 COL 1.5 NO-LABEL
     BRW_PRE AT ROW 2.67 COL 1.5
     BRW_SKARP AT ROW 2.67 COL 1.5
     BRW_STYR AT ROW 2.75 COL 56
     BTN_NY AT ROW 12.33 COL 3.5
     BTN_AND AT ROW 12.33 COL 18
     BTN_BORT AT ROW 12.33 COL 32
     BTN_AVB AT ROW 12.33 COL 86
     SPACE(1.49) SKIP(0.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Nr.serier".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: faktskarptemp T "?" NO-UNDO temp-db faktskarptemp
      TABLE: nrprefistemp T "?" NO-UNDO temp-db nrprefistemp
      TABLE: nrstyrtemp T "?" NO-UNDO temp-db nrstyrtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_PRE RAD_PRE Dialog-Frame */
/* BROWSE-TAB BRW_SKARP BRW_PRE Dialog-Frame */
/* BROWSE-TAB BRW_STYR BRW_SKARP Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BRW_SKARP:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PRE
/* Query rebuild information for BROWSE BRW_PRE
     _TblList          = "Temp-Tables.nrprefistemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.nrprefistemp.PREFIX
"nrprefistemp.PREFIX" "Prefix" "X(2)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.nrprefistemp.NRTEXT
"nrprefistemp.NRTEXT" "Text för prefix" "X(256)" "character" ? ? ? ? ? ? yes ? no no "30" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.nrprefistemp.VIJUDID
"nrprefistemp.VIJUDID" "Jur pers" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PRE */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SKARP
/* Query rebuild information for BROWSE BRW_SKARP
     _TblList          = "Temp-Tables.faktskarptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.faktskarptemp.AVDELNINGNR
"faktskarptemp.AVDELNINGNR" "Avdelning" ">>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.faktskarptemp.OMRADE
"faktskarptemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.faktskarptemp.ARTAL
"faktskarptemp.ARTAL" "Årtal" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.faktskarptemp.ARKIVSTALLE
"faktskarptemp.ARKIVSTALLE" "Arkivställe" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.faktskarptemp.LOPNR
"faktskarptemp.LOPNR" "Löpnummer" "->>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_SKARP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_STYR
/* Query rebuild information for BROWSE BRW_STYR
     _TblList          = "Temp-Tables.nrstyrtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.nrstyrtemp.PREFIX
"nrstyrtemp.PREFIX" "Styrfält" "X(1)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.nrstyrtemp.NRTEXT
"nrstyrtemp.NRTEXT" "Text" "X(256)" "character" ? ? ? ? ? ? yes ? no no "30" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_STYR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* Nr.serier */
DO:
   RUN sparaserie_UI IN skarpproch (INPUT TABLE nrprefistemp, INPUT TABLE nrstyrtemp).
   IF VALID-HANDLE(skarpproch) THEN DELETE PROCEDURE skarpproch. 
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* Nr.serier */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Nr.serier */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_PRE
&Scoped-define SELF-NAME BRW_PRE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PRE Dialog-Frame
ON LEAVE OF BRW_PRE IN FRAME Dialog-Frame /* Prefix */
DO:
   IF AVAILABLE nrprefistemp THEN DISPLAY nrprefistemp.NRTEXT WITH BROWSE BRW_PRE.
   vilkenbrw = 2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PRE Dialog-Frame
ON ROW-LEAVE OF BRW_PRE IN FRAME Dialog-Frame /* Prefix */
DO:
   IF AVAILABLE nrprefistemp THEN DO:
      DISPLAY nrprefistemp.PREFIX nrprefistemp.NRTEXT WITH BROWSE BRW_PRE.
      ASSIGN
      nrprefistemp.PREFIX = INPUT BROWSE BRW_PRE nrprefistemp.PREFIX
      nrprefistemp.NRTEXT = INPUT BROWSE BRW_PRE nrprefistemp.NRTEXT.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PRE Dialog-Frame
ON VALUE-CHANGED OF BRW_PRE IN FRAME Dialog-Frame /* Prefix */
DO:
   /*ccc*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME nrprefistemp.PREFIX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nrprefistemp.PREFIX BRW_PRE _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF nrprefistemp.PREFIX IN BROWSE BRW_PRE /* Prefix */
DO:
   IF AVAILABLE nrprefistemp THEN 
   DISPLAY nrprefistemp.PREFIX WITH BROWSE BRW_PRE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nrprefistemp.PREFIX BRW_PRE _BROWSE-COLUMN Dialog-Frame
ON LEAVE OF nrprefistemp.PREFIX IN BROWSE BRW_PRE /* Prefix */
DO:
   IF AVAILABLE nrprefistemp THEN DO: 
      nrprefistemp.PREFIX = INPUT BROWSE BRW_PRE nrprefistemp.PREFIX.
      DISPLAY nrprefistemp.PREFIX WITH BROWSE BRW_PRE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME nrprefistemp.NRTEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nrprefistemp.NRTEXT BRW_PRE _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF nrprefistemp.NRTEXT IN BROWSE BRW_PRE /* Text för prefix */
DO:
   IF AVAILABLE nrprefistemp THEN 
   DISPLAY nrprefistemp.NRTEXT WITH BROWSE BRW_PRE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nrprefistemp.NRTEXT BRW_PRE _BROWSE-COLUMN Dialog-Frame
ON LEAVE OF nrprefistemp.NRTEXT IN BROWSE BRW_PRE /* Text för prefix */
DO:
   IF AVAILABLE nrprefistemp THEN DO:
      nrprefistemp.NRTEXT = INPUT BROWSE BRW_PRE nrprefistemp.NRTEXT.
      DISPLAY nrprefistemp.NRTEXT WITH BROWSE BRW_PRE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_STYR
&Scoped-define SELF-NAME BRW_STYR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_STYR Dialog-Frame
ON LEAVE OF BRW_STYR IN FRAME Dialog-Frame /* Styrfält */
DO:
   IF AVAILABLE nrstyrtemp THEN DISPLAY nrstyrtemp.NRTEXT WITH BROWSE BRW_STYR.
   vilkenbrw = 3.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_STYR Dialog-Frame
ON ROW-LEAVE OF BRW_STYR IN FRAME Dialog-Frame /* Styrfält */
DO:
   IF AVAILABLE nrstyrtemp THEN DO:
      DISPLAY nrstyrtemp.PREFIX nrstyrtemp.NRTEXT WITH BROWSE BRW_STYR.
      ASSIGN
      nrstyrtemp.PREFIX = INPUT BROWSE BRW_STYR nrstyrtemp.PREFIX
      nrstyrtemp.NRTEXT = INPUT BROWSE BRW_STYR nrstyrtemp.NRTEXT.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_STYR Dialog-Frame
ON VALUE-CHANGED OF BRW_STYR IN FRAME Dialog-Frame /* Styrfält */
DO:
   /*ccc*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME nrstyrtemp.PREFIX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nrstyrtemp.PREFIX BRW_STYR _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF nrstyrtemp.PREFIX IN BROWSE BRW_STYR /* Styrfält */
DO:
   IF AVAILABLE nrstyrtemp THEN 
   DISPLAY nrstyrtemp.PREFIX WITH BROWSE BRW_STYR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nrstyrtemp.PREFIX BRW_STYR _BROWSE-COLUMN Dialog-Frame
ON LEAVE OF nrstyrtemp.PREFIX IN BROWSE BRW_STYR /* Styrfält */
DO:
   IF AVAILABLE nrstyrtemp THEN DO:
      nrstyrtemp.PREFIX = INPUT BROWSE BRW_STYR nrstyrtemp.PREFIX.
      DISPLAY nrstyrtemp.PREFIX WITH BROWSE BRW_STYR.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME nrstyrtemp.NRTEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nrstyrtemp.NRTEXT BRW_STYR _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF nrstyrtemp.NRTEXT IN BROWSE BRW_STYR /* Text */
DO:
   IF AVAILABLE nrstyrtemp THEN
   DISPLAY nrstyrtemp.NRTEXT WITH BROWSE BRW_STYR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nrstyrtemp.NRTEXT BRW_STYR _BROWSE-COLUMN Dialog-Frame
ON LEAVE OF nrstyrtemp.NRTEXT IN BROWSE BRW_STYR /* Text */
DO:
   IF AVAILABLE nrstyrtemp THEN DO:
      nrstyrtemp.NRTEXT = INPUT BROWSE BRW_STYR nrstyrtemp.NRTEXT.
      DISPLAY nrstyrtemp.NRTEXT WITH BROWSE BRW_STYR.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND Dialog-Frame
ON CHOOSE OF BTN_AND IN FRAME Dialog-Frame /* Ändra */
DO:
   RUN andra_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avsluta */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT Dialog-Frame
ON CHOOSE OF BTN_BORT IN FRAME Dialog-Frame /* Ta bort */
DO:
   RUN bort_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY Dialog-Frame
ON CHOOSE OF BTN_NY IN FRAME Dialog-Frame /* Ny */
DO:
   RUN ny_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_PRE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_PRE Dialog-Frame
ON VALUE-CHANGED OF RAD_PRE IN FRAME Dialog-Frame
DO:
   RAD_PRE = INPUT RAD_PRE.
   vilkenbrw = RAD_PRE.
   ASSIGN 
   BRW_PRE:HIDDEN = TRUE 
   BRW_STYR:HIDDEN = TRUE
   BTN_AND:HIDDEN = TRUE
   BRW_SKARP:HIDDEN = TRUE. 
   IF RAD_PRE = 1 THEN DO:
      ASSIGN
      BTN_AND:HIDDEN = FALSE
      BRW_SKARP:HIDDEN = FALSE. 
      /*FILL-IN-TEXT = "Faktura nr.serie:". */
   END.
   ELSE DO:
      ASSIGN
      BRW_PRE:HIDDEN = FALSE 
      BRW_STYR:HIDDEN = FALSE.
      /*FILL-IN-TEXT = Guru.Konstanter:gaok + " nr.serie:".*/
   END.
   /*IF Guru.Konstanter:varforetypval[45] = 1 THEN DO:
       BRW_STYR:HIDDEN = TRUE.
   END.*/
   /*DISPLAY FILL-IN-TEXT WITH FRAME {&FRAME-NAME}.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_PRE
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
   /*
   
   */
   {ALLSTARTDYN.I}
   
   RAD_PRE:DELETE("Nr.serier för aonr").
   RAD_PRE:DELETE("Nr.serier för fakturering").
   RAD_PRE:ADD-LAST("Nr.serier för " + LC(Guru.Konstanter:gaok), 2).
   IF Guru.Konstanter:varforetypval[3] >= 1 AND Guru.Konstanter:varforetypval[3] < 4 THEN RAD_PRE:ADD-LAST("Nr.serier för fakturering", 1).
   RAD_PRE = 2.
   IF Guru.Konstanter:varforetypval[18] = 1 THEN DO:   
      nrprefistemp.VIJUDID:VISIBLE IN BROWSE BRW_PRE = TRUE.      
   END.
   ELSE DO:
      nrprefistemp.VIJUDID:VISIBLE IN BROWSE BRW_PRE = FALSE.
   END.
      
  
   RUN enable_UI.
   /*
    faktskarptemp.LOPNR:FORMAT IN BROWSE BRW_SKARP = "->>>>>>9".
    */       
   {FRMSIZED.I} 
   RUN openbdynspec_UI IN brwproc[1].
   RUN openbdynspec_UI IN brwproc[2].
   RUN openbdynspec_UI IN brwproc[3].
   IF Guru.Konstanter:varforetypval[3] = 0 OR  Guru.Konstanter:varforetypval[3] = 4 THEN RAD_PRE:HIDDEN = TRUE.
   /*LULE STYR*/
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "CLULE" THEN DO:
      RAD_PRE:SCREEN-VALUE = STRING(2).
      APPLY "VALUE-CHANGED" TO RAD_PRE.
       /*BRW_STYR:HIDDEN = TRUE. */
      musz = musz.
      APPLY "HOME" TO BRW_STYR.
      APPLY "HOME" TO BRW_PRE.
   END.
   ELSE DO:
      RAD_PRE = 1.
      APPLY "VALUE-CHANGED" TO RAD_PRE.
      GET FIRST BRW_SKARP NO-LOCK.
      IF NOT AVAILABLE faktskarptemp THEN DO:
         ASSIGN
         BTN_AND:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END.
      faktskarptemp.AVDELNINGNR:LABEL IN BROWSE BRW_SKARP= Guru.Konstanter:gavdk.      
   END.
    IF Guru.Konstanter:varforetypval[45] = 1 THEN DO:
       BRW_STYR:HIDDEN = TRUE.
    END.   
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
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_SKARP:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_PRE:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_STYR:HANDLE IN FRAME {&FRAME-NAME}).
   IF Guru.Konstanter:appcon THEN DO:
      RUN SKARPAPP.P PERSISTENT SET skarpproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.         
   END.
   ELSE DO:
      RUN SKARPAPP.P PERSISTENT SET skarpproch.         
   END.   
   
   RUN laddaskarp_UI IN skarpproch 
      (OUTPUT TABLE nrprefistemp, OUTPUT TABLE nrstyrtemp, OUTPUT TABLE faktskarptemp).
      
    RUN dynprogextra IN brwproc[1]  (INPUT "rdformat_UI",INPUT THIS-PROCEDURE).
    RUN rowdispextrakor IN brwproc[1]  (INPUT TRUE).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI Dialog-Frame 
PROCEDURE andra_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF AVAILABLE faktskarptemp THEN DO:
      valfaktrec = faktskarptemp.FAKTREC.
      
      RUN SKARPNY.W (INPUT-OUTPUT valfaktrec). 
      
      IF musz = FALSE THEN DO:
         RUN openbdynspec_UI IN brwproc[1].
         FIND FIRST faktskarptemp WHERE faktskarptemp.FAKTREC = valfaktrec NO-ERROR.
         IF AVAILABLE faktskarptemp THEN RUN selectbyrowid_UI IN brwproc[1] (INPUT ROWID(faktskarptemp)).
      END.      
   END.
   musz = FALSE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI Dialog-Frame 
PROCEDURE bort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF vilkenbrw = 1 THEN DO:
      IF AVAILABLE faktskarptemp THEN DO:
         MESSAGE "Vill du ta bort denna post ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE STRING(faktskarptemp.AVDELNINGNR) + " " + faktskarptemp.OMRADE
         UPDATE answer1 AS LOGICAL.
         IF answer1 THEN DO:
            RUN tabortfaktskarp_UI IN skarpproch (INPUT faktskarptemp.FAKTREC). 
            DELETE faktskarptemp.
            RUN selnextprevrow_UI IN brwproc[1].
            RUN openbdynspec_UI IN brwproc[1].
            RUN lastselectdyn_UI IN brwproc[1].
            IF NOT AVAILABLE faktskarptemp THEN DO:
               ASSIGN
               BTN_AND:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
               BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
            END.
         END.
      END.
   END.
   ELSE IF vilkenbrw = 2 THEN DO:
      IF AVAILABLE nrprefistemp THEN DO:
         MESSAGE "Vill du ta bort denna post ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE STRING(nrprefistemp.NRTEXT)
         UPDATE answer2 AS LOGICAL.
         IF answer2 THEN DO:            
            RUN tabortserie_UI IN skarpproch (INPUT 2, INPUT nrprefistemp.IDNRP).
            DELETE nrprefistemp.
            RUN selnextprevrow_UI IN brwproc[2].
            RUN openbdynspec_UI IN brwproc[2].
            RUN lastselectdyn_UI IN brwproc[2].            
            IF NOT AVAILABLE nrprefistemp THEN DO:
               BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
            END.
            
         END.
      END.
   END.
   ELSE IF vilkenbrw = 3 THEN DO:
      IF AVAILABLE nrstyrtemp THEN DO:
         MESSAGE "Vill du ta bort denna post ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE STRING(nrstyrtemp.NRTEXT)
         UPDATE answer3 AS LOGICAL.
         IF answer3 THEN DO:
            RUN tabortserie_UI IN skarpproch (INPUT 3, INPUT nrstyrtemp.IDNRS).
            DELETE nrstyrtemp.
            RUN selnextprevrow_UI IN brwproc[3].
            RUN openbdynspec_UI IN brwproc[3].
            RUN lastselectdyn_UI IN brwproc[3].
            IF NOT AVAILABLE nrstyrtemp THEN DO:
               BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
            END.
         END.
      END.
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
  DISPLAY RAD_PRE 
      WITH FRAME Dialog-Frame.
  ENABLE RAD_PRE BRW_PRE BRW_SKARP BRW_STYR BTN_NY BTN_AND BTN_BORT BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny_UI Dialog-Frame 
PROCEDURE ny_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF vilkenbrw = 1 THEN DO:      
      valfaktrec = ?.
      
      RUN SKARPNY.W (INPUT-OUTPUT valfaktrec). 
      
      IF musz = FALSE THEN DO:
         RUN openbdynspec_UI IN brwproc[1].
         FIND FIRST faktskarptemp WHERE faktskarptemp.FAKTREC = valfaktrec NO-ERROR.
         IF AVAILABLE faktskarptemp THEN DO: 
            RUN selectbyrowid_UI IN brwproc[1] (INPUT ROWID(faktskarptemp)).
            ASSIGN
            BTN_AND:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
            BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         END.
      END.      
      musz = FALSE.    
   END.
   ELSE IF vilkenbrw = 2 THEN DO:
      RUN skapaserie_UI IN skarpproch (INPUT 2, OUTPUT nyttid).
      CREATE nrprefistemp.
      ASSIGN
      nrprefistemp.IDNRP = nyttid
      nrprefistemp.PREFIX = "00".
      IF Guru.Konstanter:varforetypval[18] = 1 THEN DO: 
         RUN NRSENY.W (INPUT nyttid, OUTPUT jid , OUTPUT vjid).
      END.
      FIND FIRST nrprefistemp WHERE nrprefistemp.IDNRP = nyttid NO-LOCK NO-ERROR.
      ASSIGN nrprefistemp.JUDID = jid
      nrprefistemp.VIJUDID = vjid.
      RUN openbdynspec_UI IN brwproc[2].
      FIND FIRST nrprefistemp WHERE nrprefistemp.IDNRP = nyttid NO-LOCK NO-ERROR.
      IF AVAILABLE nrprefistemp THEN DO: 
         RUN selectbyrowid_UI IN brwproc[2] (INPUT ROWID(nrprefistemp)).
         BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
   END.
   ELSE IF vilkenbrw = 3 THEN DO:            
      RUN skapaserie_UI IN skarpproch (INPUT 3, OUTPUT nyttid).
      CREATE nrstyrtemp.
      ASSIGN
      nrstyrtemp.IDNRS = nyttid
      nrstyrtemp.PREFIX = "0".
      nrstyrtemp.ORGINALNRTEXT = nrstyrtemp.NRTEXT.
      RUN openbdynspec_UI IN brwproc[3].
      FIND FIRST nrstyrtemp WHERE nrstyrtemp.IDNRS = nyttid NO-LOCK NO-ERROR.
      IF AVAILABLE nrstyrtemp THEN DO: 
         RUN selectbyrowid_UI IN brwproc[3] (INPUT ROWID(nrstyrtemp)).
         BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rdformat_UI Dialog-Frame 
PROCEDURE rdformat_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER TABLE FOR coltemp.
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.
   
   faktskarptemp.LOPNR:FORMAT IN BROWSE BRW_SKARP = "->>>>>>9".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
