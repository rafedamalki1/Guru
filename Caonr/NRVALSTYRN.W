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
DEFINE OUTPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER aonrrec AS RECID NO-UNDO.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{NRSERIETEMPN.I}
{GLOBVAR2DEL1.I}
{OMRTEMPW.I}

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE nrvalapph AS HANDLE NO-UNDO.
DEFINE VARIABLE queh AS HANDLE NO-UNDO.
DEFINE VARIABLE tempcolh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynbrwh AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE valtemp NO-UNDO
   FIELD VALNR AS INTEGER 
   FIELD VALTEXT AS CHARACTER.

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
&Scoped-define INTERNAL-TABLES nrprefistemp nrstyrtemp

/* Definitions for BROWSE BRW_PRE                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_PRE nrprefistemp.PREFIX ~
nrprefistemp.NRTEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PRE nrprefistemp.PREFIX 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_PRE nrprefistemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_PRE nrprefistemp
&Scoped-define QUERY-STRING-BRW_PRE FOR EACH nrprefistemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PRE OPEN QUERY BRW_PRE FOR EACH nrprefistemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PRE nrprefistemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PRE nrprefistemp


/* Definitions for BROWSE BRW_STYR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_STYR nrstyrtemp.PREFIX nrstyrtemp.NRTEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_STYR nrstyrtemp.PREFIX 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_STYR nrstyrtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_STYR nrstyrtemp
&Scoped-define QUERY-STRING-BRW_STYR FOR EACH nrstyrtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_STYR OPEN QUERY BRW_STYR FOR EACH nrstyrtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_STYR nrstyrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_STYR nrstyrtemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BRW_PRE}~
    ~{&OPEN-QUERY-BRW_STYR}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_FAST TOG_VAL BRW_PRE BRW_STYR BRW_VAL ~
FILL-IN_AONR BTN_KLAR BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_FAST TOG_VAL FILL-IN_AONR 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_KLAR AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(7)":U 
     LABEL "AONR" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE TOG_VAL AS LOGICAL INITIAL no 
     LABEL "Val fritt nr" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_PRE FOR 
      nrprefistemp SCROLLING.

DEFINE QUERY BRW_STYR FOR 
      nrstyrtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_PRE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PRE Dialog-Frame _STRUCTURED
  QUERY BRW_PRE NO-LOCK DISPLAY
      nrprefistemp.PREFIX COLUMN-LABEL "Prefix" FORMAT "X(2)":U
      nrprefistemp.NRTEXT COLUMN-LABEL "Text för prefix" FORMAT "X(256)":U
            WIDTH 32.88
  ENABLE
      nrprefistemp.PREFIX
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 44 BY 9.25
         TITLE "Prefix".

DEFINE BROWSE BRW_STYR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_STYR Dialog-Frame _STRUCTURED
  QUERY BRW_STYR NO-LOCK DISPLAY
      nrstyrtemp.PREFIX COLUMN-LABEL "Styrfält" FORMAT "X(1)":U
            WIDTH 11.63
      nrstyrtemp.NRTEXT COLUMN-LABEL "Text" FORMAT "X(256)":U WIDTH 27.25
  ENABLE
      nrstyrtemp.PREFIX
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 44 BY 9.25
         TITLE "Styrfält".

DEFINE BROWSE BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAL Dialog-Frame _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 44 BY 9.25
         TITLE "" EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     RAD_FAST AT ROW 1.38 COL 1.5 NO-LABEL
     TOG_VAL AT ROW 2.38 COL 1.5
     BRW_PRE AT ROW 3.42 COL 1.5
     BRW_STYR AT ROW 3.42 COL 47.5
     BRW_VAL AT ROW 3.5 COL 47.5
     FILL-IN_AONR AT ROW 13.25 COL 6 COLON-ALIGNED
     BTN_KLAR AT ROW 13.25 COL 62.5
     BTN_AVB AT ROW 13.25 COL 77.5
     SPACE(0.62) SKIP(0.07)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Välj typ av arbetsorder serie".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: nrprefistemp T "?" NO-UNDO temp-db nrprefistemp
      TABLE: nrstyrtemp T "?" NO-UNDO temp-db nrstyrtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_PRE TOG_VAL Dialog-Frame */
/* BROWSE-TAB BRW_STYR BRW_PRE Dialog-Frame */
/* BROWSE-TAB BRW_VAL BRW_STYR Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PRE
/* Query rebuild information for BROWSE BRW_PRE
     _TblList          = "Temp-Tables.nrprefistemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.nrprefistemp.PREFIX
"nrprefistemp.PREFIX" "Prefix" "X(2)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.nrprefistemp.NRTEXT
"nrprefistemp.NRTEXT" "Text för prefix" "X(256)" "character" ? ? ? ? ? ? no ? no no "32.88" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_PRE */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_STYR
/* Query rebuild information for BROWSE BRW_STYR
     _TblList          = "Temp-Tables.nrstyrtemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.nrstyrtemp.PREFIX
"nrstyrtemp.PREFIX" "Styrfält" "X(1)" "character" ? ? ? ? ? ? yes ? no no "11.63" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.nrstyrtemp.NRTEXT
"nrstyrtemp.NRTEXT" "Text" "X(256)" "character" ? ? ? ? ? ? no ? no no "27.25" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_STYR */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* Välj typ av arbetsorder serie */
DO:
   musz = TRUE.
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nrvalapph) THEN DELETE PROCEDURE nrvalapph.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* Välj typ av arbetsorder serie */
DO:
   APPLY "END-ERROR" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Välj typ av arbetsorder serie */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_PRE
&Scoped-define SELF-NAME BRW_PRE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PRE Dialog-Frame
ON LEAVE OF BRW_PRE IN FRAME Dialog-Frame /* Prefix */
DO:
   IF AVAILABLE nrprefistemp THEN 
   DISPLAY nrprefistemp.NRTEXT WITH BROWSE BRW_PRE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PRE Dialog-Frame
ON VALUE-CHANGED OF BRW_PRE IN FRAME Dialog-Frame /* Prefix */
DO:
   RUN fastnr_UI (INPUT 1).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_STYR
&Scoped-define SELF-NAME BRW_STYR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_STYR Dialog-Frame
ON LEAVE OF BRW_STYR IN FRAME Dialog-Frame /* Styrfält */
DO:
   IF AVAILABLE nrstyrtemp THEN 
   DISPLAY nrstyrtemp.NRTEXT WITH BROWSE BRW_STYR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_STYR Dialog-Frame
ON VALUE-CHANGED OF BRW_STYR IN FRAME Dialog-Frame /* Styrfält */
DO:
   RUN fastnr_UI (INPUT 2).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VAL
&Scoped-define SELF-NAME BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAL Dialog-Frame
ON VALUE-CHANGED OF BRW_VAL IN FRAME Dialog-Frame
DO:
  RUN fastnr_UI (INPUT 3).  
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


&Scoped-define SELF-NAME BTN_KLAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KLAR Dialog-Frame
ON CHOOSE OF BTN_KLAR IN FRAME Dialog-Frame /* Ok */
DO:
   
   aonrvar = "".
   IF Guru.Konstanter:globforetag = "CELPA" OR Guru.Konstanter:globforetag = "GRAN" THEN DO:
      IF AVAILABLE nrprefistemp THEN DO:
         IF nrprefistemp.PREFIX = STRING(36) THEN DO:
            aonrvar = nrprefistemp.PREFIX + STRING(valtemp.VALNR).
         END.
      END.
      IF AVAILABLE nrprefistemp THEN DO:
         IF AVAILABLE nrstyrtemp THEN DO:
            aonrvar = nrprefistemp.PREFIX + nrstyrtemp.PREFIX.
         END.
      END.
   END.
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      IF AVAILABLE nrprefistemp THEN DO:
         IF nrprefistemp.PREFIX = STRING(38) THEN DO:
            IF NOT AVAILABLE nrstyrtemp THEN DO:
               MESSAGE "Du måste välja en styrkod!" VIEW-AS ALERT-BOX.
               RETURN NO-APPLY.
            END.
            ELSE DO:
               IF nrstyrtemp.PREFIX = "6" OR nrstyrtemp.PREFIX = "7" OR nrstyrtemp.PREFIX = "8" THEN.
               ELSE DO:
                  MESSAGE "Felaktig styrkod!" VIEW-AS ALERT-BOX.
                  RETURN NO-APPLY.
               END.
            END.
         END.
         IF nrprefistemp.PREFIX = STRING(36) THEN DO:
            IF NOT AVAILABLE nrstyrtemp THEN DO:
               MESSAGE "Du måste välja en styrkod!" VIEW-AS ALERT-BOX.
               RETURN NO-APPLY.
            END.
            ELSE DO:
               IF nrstyrtemp.PREFIX = "0" OR nrstyrtemp.PREFIX = "1" OR nrstyrtemp.PREFIX = "2" OR  nrstyrtemp.PREFIX = "3" OR  nrstyrtemp.PREFIX = "4" OR  nrstyrtemp.PREFIX = "5"  THEN.
               ELSE DO:
                  MESSAGE "Felaktig styrkod!" VIEW-AS ALERT-BOX.
                  RETURN NO-APPLY.
               END.
            END.
         END.
         IF nrprefistemp.PREFIX = STRING(46) THEN DO:
            IF NOT AVAILABLE nrstyrtemp THEN DO:
               MESSAGE "Du måste välja en styrkod!" VIEW-AS ALERT-BOX.
               RETURN NO-APPLY.
            END.
            ELSE DO:
               IF nrstyrtemp.PREFIX = "0" OR nrstyrtemp.PREFIX = "1" OR nrstyrtemp.PREFIX = "2"  THEN.
               ELSE DO:                
                  MESSAGE "Felaktig styrkod!" VIEW-AS ALERT-BOX.
                  RETURN NO-APPLY.
               END.
            END.
         END.
         IF nrprefistemp.PREFIX = STRING(63) THEN DO:
            IF NOT AVAILABLE nrstyrtemp THEN DO:
               MESSAGE "Du måste välja en styrkod!" VIEW-AS ALERT-BOX.
               RETURN NO-APPLY.
            END.
            ELSE DO:
               IF nrstyrtemp.PREFIX = "2" OR nrstyrtemp.PREFIX = "3" OR nrstyrtemp.PREFIX = "4" OR nrstyrtemp.PREFIX = "5" OR nrstyrtemp.PREFIX = "6"  THEN.
               ELSE DO:                
                  MESSAGE "Felaktig styrkod!" VIEW-AS ALERT-BOX.
                  RETURN NO-APPLY.
               END.
            END.
         END.
         
      END.
   END.
   
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      IF AVAILABLE nrprefistemp THEN DO:
         IF nrprefistemp.PREFIX = STRING(38) THEN DO:
            IF AVAILABLE nrstyrtemp THEN DO:
               aonrvar = nrprefistemp.PREFIX + nrstyrtemp.PREFIX.
            END.
         END.
         ELSE IF nrprefistemp.PREFIX = STRING(36) THEN DO:
            IF AVAILABLE nrstyrtemp THEN DO:
               aonrvar = nrprefistemp.PREFIX + nrstyrtemp.PREFIX.
            END.
         END.
         ELSE IF nrprefistemp.PREFIX = STRING(46) THEN DO:
            IF AVAILABLE nrstyrtemp THEN DO:
               aonrvar = nrprefistemp.PREFIX + nrstyrtemp.PREFIX.
            END.
         END.
         ELSE IF nrprefistemp.PREFIX = STRING(63) THEN DO:
            IF AVAILABLE nrstyrtemp THEN DO:
               aonrvar = nrprefistemp.PREFIX + nrstyrtemp.PREFIX.
            END.
         END.
         ELSE DO:
            aonrvar = nrprefistemp.PREFIX.
         END.         
      END.
   END.
   FILL-IN_AONR = INPUT FILL-IN_AONR.
   IF FILL-IN_AONR NE "" THEN DO:      
      IF FILL-IN_AONR NE aonrvar THEN aonrvar = "F" + FILL-IN_AONR.      
   END.
   IF aonrvar = "" OR aonrvar = "F" THEN DO:
      IF NOT AVAILABLE nrprefistemp THEN DO:
         MESSAGE "Du måste välja ett prefix!" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.
   IF aonrvar = "" OR aonrvar = "F" THEN DO:
      MESSAGE "Du måste välja något!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   FILL-IN_AONR = TRIM(FILL-IN_AONR).
   DISPLAY FILL-IN_AONR WITH FRAME {&FRAME-NAME}.
   RUN AONRNAMNKOLL.P (INPUT 1, INPUT FILL-IN_AONR,OUTPUT musz).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.   
    RUN AONRNAMNKOLL.P (INPUT 2, INPUT FILL-IN_AONR,OUTPUT musz).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "elpa" THEN DO: 
      IF AVAILABLE nrprefistemp THEN DO:         
         IF nrprefistemp.PREFIX = STRING(38) OR nrprefistemp.PREFIX = STRING(36) OR nrprefistemp.PREFIX = STRING(46) OR nrprefistemp.PREFIX = STRING(63) THEN DO:
             IF AVAILABLE nrstyrtemp THEN DO:
                MESSAGE "Du har valt prefix" nrprefistemp.PREFIX + " " + nrprefistemp.NRTEXT + " styrkod " + nrstyrtemp.PREFIX + " " + nrstyrtemp.NRTEXT +
               " är detta korrekt?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val3 AS LOGICAL.
               CASE val3:         
                  WHEN FALSE THEN DO:
                     RETURN NO-APPLY.
                  END.      
               END CASE.   
             END.
         END.
         ELSE DO:         
            MESSAGE "Du har valt prefix" nrprefistemp.PREFIX + " " + nrprefistemp.NRTEXT +
            " är detta korrekt?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
            CASE val:         
               WHEN FALSE THEN DO:
                  RETURN NO-APPLY.
               END.      
            END CASE.   
         END.
      END.

   END.   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "elpa" THEN DO: 
      IF AVAILABLE nrprefistemp THEN DO:
         IF Guru.Konstanter:globforetag = "elpa" THEN DO: 
            IF nrprefistemp.JUDID = "BYGG" THEN DO:
               aonrvar = "S" + aonrvar.
            END.
         END.
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO: 
            IF nrprefistemp.JUDID = "SEAB" THEN DO:
               aonrvar = "S" + aonrvar.
            END.
         END.
      END.         
   END.   
   IF Guru.Konstanter:appcon THEN DO:
      RUN NRAOAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT Guru.Konstanter:globomr,INPUT RAD_FAST,INPUT-OUTPUT aonrvar,OUTPUT aonrrec).
   END.
   ELSE DO:
      RUN NRAOAPP.P
      (INPUT Guru.Konstanter:globomr,INPUT RAD_FAST,INPUT-OUTPUT aonrvar,OUTPUT aonrrec).
   END.
   IF aonrvar = "XXXXXX" THEN DO:
      MESSAGE "Nummerserien är full eller nummret är redan upplagt! Välj ett annat nummer!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   APPLY "GO" TO BTN_KLAR IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KLAR Dialog-Frame
ON GO OF BTN_KLAR IN FRAME Dialog-Frame /* Ok */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nrvalapph) THEN DELETE PROCEDURE nrvalapph.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_FAST Dialog-Frame
ON VALUE-CHANGED OF RAD_FAST IN FRAME Dialog-Frame
DO:
   RAD_FAST = INPUT RAD_FAST.
   RUN fastnr_UI (INPUT 0).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_VAL Dialog-Frame
ON VALUE-CHANGED OF TOG_VAL IN FRAME Dialog-Frame /* Val fritt nr */
DO:
   TOG_VAL = INPUT TOG_VAL.
   RUN fastnr_UI (INPUT 0).  
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
   {ALLSTARTDYN.I}
   RUN nrseriehmt IN nrvalapph(INPUT Guru.Konstanter:globanv,OUTPUT TABLE nrprefistemp,
                               OUTPUT TABLE nrstyrtemp).
   {TILLFAST.I}
   IF RAD_FAST = ? THEN RAD_FAST = FALSE.
   &Scoped-define FORMATNAMN FILL-IN_AONR   
   {AOFORMAT3.I}      
   RUN enable_UI.       
   IF Guru.Konstanter:globforetag = "sund"  THEN DO:
      ASSIGN RAD_FAST = FALSE.
      RAD_FAST:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      TOG_VAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      FILL-IN_AONR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      IF Guru.Konstanter:globforetag = "SUND" THEN DO:
         FILL-IN_AONR:HIDDEN = FALSE.
         DISPLAY FILL-IN_AONR WITH FRAME {&FRAME-NAME}.
         FILL-IN_AONR:LABEL = Guru.Konstanter:gaok.
         ENABLE FILL-IN_AONR WITH FRAME {&FRAME-NAME}.
      END.      
   END.
   ELSE DO:   
      APPLY "VALUE-CHANGED" TO RAD_FAST.
   END.
   
   ASSIGN FRAME {&FRAME-NAME}:TITLE = "Välj typ av " + LC(Guru.Konstanter:gaol) + "serie...".
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   RUN openbdyn_UI IN brwproc[2] (INPUT "").
   ASSIGN BRW_VAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   {FRMSIZED.I}
   {DIA_M_SLUT.I}
   RELEASE nrprefistemp NO-ERROR. 
   RELEASE nrstyrtemp NO-ERROR. 
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
   nrprefistemp.PREFIX:READ-ONLY IN BROWSE BRW_PRE = TRUE
   nrstyrtemp.PREFIX:READ-ONLY IN BROWSE BRW_STYR = TRUE.
   
   CREATE QUERY queh.
   queh:SET-BUFFERS(BUFFER valtemp:HANDLE).
   BRW_VAL:QUERY IN FRAME {&FRAME-NAME} = queh.
   BRW_VAL:ADD-COLUMNS-FROM (BUFFER valtemp:HANDLE) IN FRAME {&FRAME-NAME}.
   BRW_VAL:ALLOW-COLUMN-SEARCHING = TRUE.

   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_PRE:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_STYR:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_VAL:HANDLE IN FRAME {&FRAME-NAME}).  
   IF Guru.Konstanter:appcon THEN DO:
      RUN NRVALAPPN.P PERSISTENT SET nrvalapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NRVALAPPN.P PERSISTENT SET nrvalapph.
   END.
   ENABLE BRW_VAL WITH FRAME {&FRAME-NAME}.
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
  DISPLAY RAD_FAST TOG_VAL FILL-IN_AONR 
      WITH FRAME Dialog-Frame.
  ENABLE RAD_FAST TOG_VAL BRW_PRE BRW_STYR FILL-IN_AONR BTN_KLAR BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fastnr_UI Dialog-Frame 
PROCEDURE fastnr_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   DEFINE VARIABLE nrlangd AS INTEGER NO-UNDO.
   FILL-IN_AONR = INPUT FRAME {&FRAME-NAME} FILL-IN_AONR.
   IF vad = 1 THEN DO:
      IF Guru.Konstanter:globforetag = "CELPA" OR Guru.Konstanter:globforetag = "GRAN" THEN DO:
         /*ElektroSandberg Syd och ElektroSandberg Nord*/
         IF nrprefistemp.PREFIX = STRING(36) THEN DO:
            EMPTY TEMP-TABLE valtemp NO-ERROR. 
            CREATE valtemp.
            ASSIGN 
            valtemp.VALNR = 1
            valtemp.VALTEXT = "Syd".
            CREATE valtemp.
            ASSIGN 
            valtemp.VALNR = 5
            valtemp.VALTEXT = "Nord".
            dynbrwh = BRW_VAL:HANDLE.
            dynbrwh:TITLE = "Område". 
            tempcolh = dynbrwh:GET-BROWSE-COLUMN(1).    
            tempcolh:LABEL = "Nr".
            tempcolh = dynbrwh:GET-BROWSE-COLUMN(2).    
            tempcolh:LABEL = "Val".
            RUN openbdynspec_UI IN brwproc[3].
            ASSIGN 
            BRW_STYR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            BRW_VAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.                       
         END.
         ELSE DO:
            ASSIGN 
            BRW_STYR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
            BRW_VAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
            FILL-IN_AONR = nrprefistemp.PREFIX + SUBSTRING(FILL-IN_AONR,LENGTH(nrprefistemp.PREFIX) + 1).
         END.        
      END.     
      ELSE DO:
         RUN setcolsortvar_UI IN brwproc[2] (INPUT "").
         FILL-IN_AONR = nrprefistemp.PREFIX + SUBSTRING(FILL-IN_AONR,LENGTH(nrprefistemp.PREFIX) + 1).
         FIND FIRST nrstyrtemp  WHERE nrstyrtemp.PREFIX = "0" NO-ERROR.
         IF AVAILABLE nrstyrtemp THEN DO:
            IF nrprefistemp.PREFIX = "46" THEN DO:          
               nrstyrtemp.NRTEXT = "H-sand 2010".         
               RUN setcolsortvar_UI IN brwproc[2] (INPUT "PREFIX > '0'").         
            END.
            ELSE nrstyrtemp.NRTEXT = nrstyrtemp.ORGINALNRTEXT.                    
         END.
         FIND FIRST nrstyrtemp  WHERE nrstyrtemp.PREFIX = "1" NO-ERROR.
         IF AVAILABLE nrstyrtemp THEN DO:
            IF nrprefistemp.PREFIX = "46" THEN DO:          
               nrstyrtemp.NRTEXT = "H-sand 2012".          
               RUN setcolsortvar_UI IN brwproc[2] (INPUT "PREFIX > '0'").           
            END.
            ELSE DO: 
               nrstyrtemp.NRTEXT = nrstyrtemp.ORGINALNRTEXT.                 
            END.                        
         END.
         FIND FIRST nrstyrtemp  WHERE nrstyrtemp.PREFIX = "2" NO-ERROR.
         IF AVAILABLE nrstyrtemp THEN DO:
            IF nrprefistemp.PREFIX = "63" THEN DO:          
               nrstyrtemp.NRTEXT = "Nyinvestering Elnät".           
            END.            
            IF nrprefistemp.PREFIX = "46" THEN DO:                         
               nrstyrtemp.NRTEXT = "H-sand 2012".
               RUN setcolsortvar_UI IN brwproc[2] (INPUT "PREFIX > '0'").           
            END.            
            ELSE DO: 
               FIND FIRST nrstyrtemp  WHERE nrstyrtemp.PREFIX = "2" NO-ERROR.
               nrstyrtemp.NRTEXT = nrstyrtemp.ORGINALNRTEXT.
               FIND FIRST nrstyrtemp  WHERE nrstyrtemp.PREFIX = "3" NO-ERROR.
               IF AVAILABLE nrstyrtemp THEN DO:
                  nrstyrtemp.NRTEXT = nrstyrtemp.ORGINALNRTEXT.
               END.   
            END.                        
         END.         
         FIND FIRST nrstyrtemp  WHERE nrstyrtemp.PREFIX = "5" NO-ERROR.
         IF AVAILABLE nrstyrtemp THEN DO:
            IF nrprefistemp.PREFIX = "36" THEN DO:          
               nrstyrtemp.NRTEXT = "Bredband".               
               RUN setcolsortvar_UI IN brwproc[2] (INPUT "PREFIX > '4'").           
            END.
         END.
         FIND FIRST nrstyrtemp  WHERE nrstyrtemp.PREFIX = "6" NO-ERROR.
         IF AVAILABLE nrstyrtemp THEN DO:
            IF nrprefistemp.PREFIX = "63" THEN DO:          
               nrstyrtemp.NRTEXT = "Nyinvestering Elnät  2012".           
            END.                                   
         END.         
         RUN openbdynspec_UI IN brwproc[2].
      END.     
   END.
   IF vad = 2 THEN DO:
      IF NOT AVAILABLE nrprefistemp THEN DO:
         MESSAGE "Välj ett prefix!" VIEW-AS ALERT-BOX.
         RETURN.
      END.
      FILL-IN_AONR =  nrprefistemp.PREFIX + nrstyrtemp.PREFIX + SUBSTRING(FILL-IN_AONR,LENGTH(nrprefistemp.PREFIX) + LENGTH(nrstyrtemp.PREFIX) + 1).
   END.
   IF vad = 3 THEN DO:
      FILL-IN_AONR =  nrprefistemp.PREFIX + STRING(valtemp.VALNR) + SUBSTRING(FILL-IN_AONR,LENGTH(nrprefistemp.PREFIX) + 1).      
   END.   
   IF TOG_VAL = TRUE THEN DO:
      DISPLAY FILL-IN_AONR WITH FRAME {&FRAME-NAME}.
      FILL-IN_AONR:LABEL = Guru.Konstanter:gaok.
      ENABLE FILL-IN_AONR WITH FRAME {&FRAME-NAME}.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

