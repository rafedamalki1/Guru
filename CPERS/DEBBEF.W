&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/06/28 - 12:36 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER omrbefvar AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW  
&Scoped-define SHARED SHARED
{AUTOMTEMP.I}
{OMRTEMPW.I}
{DEBPRITAB.I} 
{BEFTEMP.I}
{SOKDEF.I}
DEFINE SHARED VARIABLE timkostrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE pris1 AS LOGICAL NO-UNDO.    
DEFINE VARIABLE pris2 AS LOGICAL NO-UNDO.   
DEFINE VARIABLE pris3 AS LOGICAL NO-UNDO.   
DEFINE VARIABLE pris4 AS LOGICAL NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_BEF

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES befattningstemp omrtemp

/* Definitions for BROWSE BRW_BEF                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_BEF befattningstemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BEF 
&Scoped-define QUERY-STRING-BRW_BEF FOR EACH befattningstemp NO-LOCK ~
    BY befattningstemp.NAMN
&Scoped-define OPEN-QUERY-BRW_BEF OPEN QUERY BRW_BEF FOR EACH befattningstemp NO-LOCK ~
    BY befattningstemp.NAMN.
&Scoped-define TABLES-IN-QUERY-BRW_BEF befattningstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BEF befattningstemp


/* Definitions for BROWSE BRW_OMR                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_OMR omrtemp.OMRADE omrtemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OMR 
&Scoped-define QUERY-STRING-BRW_OMR FOR EACH omrtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_OMR OPEN QUERY BRW_OMR FOR EACH omrtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_OMR omrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OMR omrtemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DIALOG-1 ~
    ~{&OPEN-QUERY-BRW_BEF}~
    ~{&OPEN-QUERY-BRW_OMR}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_OMR BRW_BEF FILL-IN-PRIS-1 ~
FILL-IN-PRIS-2 FILL-IN-PRIS-3 FILL-IN-PRIS-4 BTN_UPP BTN_OK 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-BEF-2 FILL-IN-DEB-1 FILL-IN-DEB-2 ~
FILL-IN-DEB-3 FILL-IN-DEB-4 FILL-IN-PRIS-1 FILL-IN-PRIS-2 FILL-IN-PRIS-3 ~
FILL-IN-PRIS-4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UPP 
     LABEL "ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-BEF AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEF-2 AS CHARACTER FORMAT "X(20)":U 
     LABEL "Nu ändrar du priset för befattning" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-DEB-1 AS CHARACTER FORMAT "X(9)":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-DEB-2 AS CHARACTER FORMAT "X(9)":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-DEB-3 AS CHARACTER FORMAT "X(9)":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-DEB-4 AS CHARACTER FORMAT "X(9)":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS-1 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS-2 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS-3 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS-4 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BEF FOR 
      befattningstemp SCROLLING.

DEFINE QUERY BRW_OMR FOR 
      omrtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BEF DIALOG-1 _STRUCTURED
  QUERY BRW_BEF NO-LOCK DISPLAY
      befattningstemp.NAMN COLUMN-LABEL "Övertid för" FORMAT "x(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS SIZE 23.88 BY 11.17
         TITLE "Befattningar".

DEFINE BROWSE BRW_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OMR DIALOG-1 _STRUCTURED
  QUERY BRW_OMR DISPLAY
      omrtemp.OMRADE COLUMN-LABEL "Område" FORMAT "X(6)":U
      omrtemp.NAMN COLUMN-LABEL "Benämning" FORMAT "X(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 28.38 BY 11.17.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-BEF AT ROW 1.5 COL 53.38 COLON-ALIGNED NO-LABEL
     FILL-IN-BEF-2 AT ROW 1.5 COL 53.38 COLON-ALIGNED
     BRW_OMR AT ROW 2.83 COL 1.5
     BRW_BEF AT ROW 2.83 COL 2.63
     FILL-IN-DEB-1 AT ROW 5.13 COL 29.38 COLON-ALIGNED NO-LABEL
     FILL-IN-DEB-2 AT ROW 5.13 COL 41.38 COLON-ALIGNED NO-LABEL
     FILL-IN-DEB-3 AT ROW 5.13 COL 53.38 COLON-ALIGNED NO-LABEL
     FILL-IN-DEB-4 AT ROW 5.13 COL 65.38 COLON-ALIGNED NO-LABEL
     FILL-IN-PRIS-1 AT ROW 6.63 COL 29.38 COLON-ALIGNED NO-LABEL
     FILL-IN-PRIS-2 AT ROW 6.63 COL 41.38 COLON-ALIGNED NO-LABEL
     FILL-IN-PRIS-3 AT ROW 6.63 COL 53.38 COLON-ALIGNED NO-LABEL
     FILL-IN-PRIS-4 AT ROW 6.63 COL 65.38 COLON-ALIGNED NO-LABEL
     BTN_UPP AT ROW 13 COL 48.38
     BTN_OK AT ROW 13 COL 63.38
     SPACE(0.74) SKIP(0.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändrig av priser för vald befattning":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: befattningstemp T "?" NO-UNDO temp-db befattningstemp
      TABLE: omrtemp T "?" NO-UNDO temp-db omrtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_OMR FILL-IN-BEF-2 DIALOG-1 */
/* BROWSE-TAB BRW_BEF BRW_OMR DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

ASSIGN 
       BRW_OMR:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-1 = TRUE
       BRW_OMR:COLUMN-RESIZABLE IN FRAME DIALOG-1       = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-BEF IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-BEF:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-BEF-2 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-DEB-1 IN FRAME DIALOG-1
   SHARED NO-ENABLE                                                     */
/* SETTINGS FOR FILL-IN FILL-IN-DEB-2 IN FRAME DIALOG-1
   SHARED NO-ENABLE                                                     */
/* SETTINGS FOR FILL-IN FILL-IN-DEB-3 IN FRAME DIALOG-1
   SHARED NO-ENABLE                                                     */
/* SETTINGS FOR FILL-IN FILL-IN-DEB-4 IN FRAME DIALOG-1
   SHARED NO-ENABLE                                                     */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BEF
/* Query rebuild information for BROWSE BRW_BEF
     _TblList          = "TEMP-DB.befattningstemp"
     _Options          = "NO-LOCK"
     _OrdList          = "TEMP-DB.befattningstemp.NAMN|yes"
     _FldNameList[1]   > Temp-Tables.befattningstemp.NAMN
"befattningstemp.NAMN" "Övertid för" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_BEF */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OMR
/* Query rebuild information for BROWSE BRW_OMR
     _TblList          = "TEMP-DB.omrtemp"
     _FldNameList[1]   > Temp-Tables.omrtemp.OMRADE
"omrtemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.omrtemp.NAMN
"omrtemp.NAMN" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_OMR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_BEF
&Scoped-define SELF-NAME BRW_BEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BEF DIALOG-1
ON VALUE-CHANGED OF BRW_BEF IN FRAME DIALOG-1 /* Befattningar */
DO:
   IF omrbefvar = 1 THEN DO:
      &Scoped-define BROWSE-NAME BRW_BEF            
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
      APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
      ASSIGN
      FILL-IN-BEF-2 = befattningstemp.NAMN
      FILL-IN-BEF = befattningstemp.BEFATTNING. 
      FIND FIRST debpris WHERE debpris.BEFATTNING = FILL-IN-BEF 
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   END.
   IF AVAILABLE debpris THEN DO:    
      ASSIGN
      FILL-IN-PRIS-1 = debpris.PRISA   
      FILL-IN-PRIS-2 = debpris.PRISA-2   
      FILL-IN-PRIS-3 = debpris.PRISA-3  
      FILL-IN-PRIS-4 = debpris.PRISA-4.
   END.
   ELSE DO: 
      ASSIGN
      FILL-IN-PRIS-1 = 0
      FILL-IN-PRIS-2 = 0
      FILL-IN-PRIS-3 = 0
      FILL-IN-PRIS-4 = 0.
   END.
   DISPLAY 
   FILL-IN-BEF-2 FILL-IN-PRIS-1 FILL-IN-PRIS-2 FILL-IN-PRIS-3 FILL-IN-PRIS-4 
   WITH FRAME DIALOG-1.
   IF FILL-IN-DEB-1 = "" THEN FILL-IN-PRIS-1:HIDDEN = TRUE.  
   IF FILL-IN-DEB-2 = "" THEN FILL-IN-PRIS-2:HIDDEN = TRUE.  
   IF FILL-IN-DEB-3 = "" THEN FILL-IN-PRIS-3:HIDDEN = TRUE.  
   IF FILL-IN-DEB-4 = "" THEN FILL-IN-PRIS-4:HIDDEN = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_OMR
&Scoped-define SELF-NAME BRW_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_OMR DIALOG-1
ON VALUE-CHANGED OF BRW_OMR IN FRAME DIALOG-1
DO:
   
   &Scoped-define BROWSE-NAME BRW_OMR     
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   ASSIGN
   FILL-IN-BEF-2 = omrtemp.NAMN
   FILL-IN-BEF = omrtemp.OMRADE. 
   FIND FIRST debpris WHERE debpris.OMRADE = FILL-IN-BEF 
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF AVAILABLE debpris THEN DO:    
      ASSIGN
      FILL-IN-PRIS-1 = debpris.PRISA   
      FILL-IN-PRIS-2 = debpris.PRISA-2   
      FILL-IN-PRIS-3 = debpris.PRISA-3  
      FILL-IN-PRIS-4 = debpris.PRISA-4.
   END.
   ELSE DO: 
      ASSIGN
      FILL-IN-PRIS-1 = 0
      FILL-IN-PRIS-2 = 0
      FILL-IN-PRIS-3 = 0
      FILL-IN-PRIS-4 = 0.
   END.
   DISPLAY 
   FILL-IN-BEF-2 FILL-IN-PRIS-1 FILL-IN-PRIS-2 FILL-IN-PRIS-3 FILL-IN-PRIS-4 
   WITH FRAME DIALOG-1.
   IF FILL-IN-DEB-1 = "" THEN FILL-IN-PRIS-1:HIDDEN = TRUE.  
   IF FILL-IN-DEB-2 = "" THEN FILL-IN-PRIS-2:HIDDEN = TRUE.  
   IF FILL-IN-DEB-3 = "" THEN FILL-IN-PRIS-3:HIDDEN = TRUE.  
   IF FILL-IN-DEB-4 = "" THEN FILL-IN-PRIS-4:HIDDEN = TRUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Avsluta */
DO:
   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP DIALOG-1
ON CHOOSE OF BTN_UPP IN FRAME DIALOG-1 /* ok */
DO:
  
   ASSIGN
   FILL-IN-PRIS-1 = INPUT FILL-IN-PRIS-1
   FILL-IN-PRIS-2 = INPUT FILL-IN-PRIS-2
   FILL-IN-PRIS-3 = INPUT FILL-IN-PRIS-3 
   FILL-IN-PRIS-4 = INPUT FILL-IN-PRIS-4.   
   IF omrbefvar = 1 THEN DO: 
      IF FILL-IN-DEB-1 = "" THEN pris1 = FALSE.
      ELSE DO:
         MESSAGE "Vill du att alla med befattning" + " " + FILL-IN-BEF-2 " ska ha" SKIP
         SKIP
         CAPS(Guru.Konstanter:gdebk) + " " +  FILL-IN-DEB-1 "="  FILL-IN-PRIS-1 "kr ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
         CASE val1:
            WHEN TRUE THEN DO:
               pris1 = TRUE.          
            END.
            WHEN FALSE THEN DO:
               pris1 = FALSE. 
            END.       
         END CASE. 
      END.
      IF FILL-IN-DEB-2 = "" THEN pris2 = FALSE.
      ELSE DO:  
         MESSAGE "Vill du att alla med befattning" + " " +  FILL-IN-BEF-2 " ska ha"
         SKIP
         CAPS(Guru.Konstanter:gdebk) + " " +  FILL-IN-DEB-2 "="  FILL-IN-PRIS-2 "kr ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val2 AS LOGICAL.
         CASE val2:
            WHEN TRUE THEN DO:
               pris2 = TRUE.          
            END.
            WHEN FALSE THEN DO:
               pris2 = FALSE. 
            END.       
         END CASE.
      END.
      IF FILL-IN-DEB-3 = "" THEN pris3 = FALSE.
      ELSE DO: 
         MESSAGE "Vill du att alla med befattning" + " " +  FILL-IN-BEF-2 " ska ha"
         SKIP
         CAPS(Guru.Konstanter:gdebk) + " " +  FILL-IN-DEB-3 "="  FILL-IN-PRIS-3 "kr ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val3 AS LOGICAL.
         CASE val3:
            WHEN TRUE THEN DO:
               pris3 = TRUE.          
            END.
            WHEN FALSE THEN DO:
               pris3 = FALSE. 
            END.       
         END CASE. 
      END.
      IF FILL-IN-DEB-4 = "" THEN pris4 = FALSE.
      ELSE DO:   
         MESSAGE "Vill du att alla med befattning" + " " + FILL-IN-BEF-2 " ska ha"
         SKIP
         CAPS(Guru.Konstanter:gdebk) + " " +  FILL-IN-DEB-4 "="  FILL-IN-PRIS-4 "kr ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val4 AS LOGICAL.
         CASE val4:
            WHEN TRUE THEN DO:
               pris4 = TRUE.          
            END.
            WHEN FALSE THEN DO:
               pris4 = FALSE. 
            END.       
         END CASE.
      END.
      FOR EACH valsoktemp:
         DELETE valsoktemp.
      END.   
      CREATE valsoktemp.
      ASSIGN
      valsoktemp.SOKCHAR[1] = FILL-IN-DEB-1
      valsoktemp.SOKCHAR[2] = FILL-IN-DEB-2
      valsoktemp.SOKCHAR[3] = FILL-IN-DEB-3
      valsoktemp.SOKCHAR[4] = FILL-IN-DEB-4
      valsoktemp.SOKCHAR[5] = FILL-IN-BEF.
      IF pris1 = TRUE THEN DO:
         ASSIGN valsoktemp.SOKINT[1] = FILL-IN-PRIS-1.
      END.
      ELSE valsoktemp.SOKCHAR[1] = "XXX". 
      IF pris2 = TRUE THEN DO:
         ASSIGN valsoktemp.SOKINT[2] = FILL-IN-PRIS-2.
      END.
      ELSE valsoktemp.SOKCHAR[2] = "XXX".
      IF pris3 = TRUE THEN DO:   
         ASSIGN valsoktemp.SOKINT[3] = FILL-IN-PRIS-3.
      END.
      ELSE valsoktemp.SOKCHAR[3] = "XXX".
      IF pris4 = TRUE THEN DO:   
         ASSIGN valsoktemp.SOKINT[4] = FILL-IN-PRIS-4.
      END.
      ELSE valsoktemp.SOKCHAR[4] = "XXX".              
      ASSIGN
      valsoktemp.SOKINT[1] = FILL-IN-PRIS-1      
      valsoktemp.SOKINT[2] = FILL-IN-PRIS-2
      valsoktemp.SOKINT[3] = FILL-IN-PRIS-3
      valsoktemp.SOKINT[4] = FILL-IN-PRIS-4.
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN FLRDEB.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 3,INPUT TABLE valsoktemp,INPUT-OUTPUT TABLE debpris).
      END.
      ELSE DO:
         RUN FLRDEB.P 
         (INPUT 3,INPUT TABLE valsoktemp,INPUT-OUTPUT TABLE debpris).
      END.
     
   END.
   IF omrbefvar = 2 THEN DO: 
      IF FILL-IN-DEB-1 = "" THEN pris1 = FALSE.
      ELSE DO:
         MESSAGE "Vill du att alla med " + LC(Guru.Konstanter:gomrk) + " " + FILL-IN-BEF-2 " ska ha" SKIP
         SKIP
         CAPS(Guru.Konstanter:gdebk) + " " +  FILL-IN-DEB-1 "="  FILL-IN-PRIS-1 "kr ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val5 AS LOGICAL.
         CASE val5:
            WHEN TRUE THEN DO:
               pris1 = TRUE.          
            END.
            WHEN FALSE THEN DO:
               pris1 = FALSE. 
            END.       
         END CASE. 
      END.
      IF FILL-IN-DEB-2 = "" THEN pris2 = FALSE.
      ELSE DO:  
         MESSAGE "Vill du att alla med " + LC(Guru.Konstanter:gomrk) + " " +  FILL-IN-BEF-2 " ska ha"
         SKIP
         CAPS(Guru.Konstanter:gdebk) + " " +  FILL-IN-DEB-2 "="  FILL-IN-PRIS-2 "kr ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val6 AS LOGICAL.
         CASE val6:
            WHEN TRUE THEN DO:
               pris2 = TRUE.          
            END.
            WHEN FALSE THEN DO:
               pris2 = FALSE. 
            END.       
         END CASE.
      END.
      IF FILL-IN-DEB-3 = "" THEN pris3 = FALSE.
      ELSE DO: 
         MESSAGE "Vill du att alla med " + LC(Guru.Konstanter:gomrk) + " " +  FILL-IN-BEF-2 " ska ha"
         SKIP
         CAPS(Guru.Konstanter:gdebk) + " " +  FILL-IN-DEB-3 "="  FILL-IN-PRIS-3 "kr ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val7 AS LOGICAL.
         CASE val7:
            WHEN TRUE THEN DO:
               pris3 = TRUE.          
            END.
            WHEN FALSE THEN DO:
               pris3 = FALSE. 
            END.       
         END CASE. 
      END.
      IF FILL-IN-DEB-4 = "" THEN pris4 = FALSE.
      ELSE DO:   
         MESSAGE "Vill du att alla med " + LC(Guru.Konstanter:gomrk) + " " +  FILL-IN-BEF-2 " ska ha"
         SKIP
         CAPS(Guru.Konstanter:gdebk) + " " +  FILL-IN-DEB-4 "="  FILL-IN-PRIS-4 "kr ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val8 AS LOGICAL.
         CASE val8:
            WHEN TRUE THEN DO:
               pris4 = TRUE.          
            END.
            WHEN FALSE THEN DO:
               pris4 = FALSE. 
            END.       
         END CASE.
      END.
      {muswait.i}
      FOR EACH valsoktemp:
         DELETE valsoktemp.
      END.   
      CREATE valsoktemp.
      ASSIGN
      valsoktemp.SOKCHAR[1] = FILL-IN-DEB-1
      valsoktemp.SOKCHAR[2] = FILL-IN-DEB-2
      valsoktemp.SOKCHAR[3] = FILL-IN-DEB-3
      valsoktemp.SOKCHAR[4] = FILL-IN-DEB-4
      valsoktemp.SOKCHAR[5] = FILL-IN-BEF.
      IF pris1 = TRUE THEN DO:
         ASSIGN valsoktemp.SOKINT[1] = FILL-IN-PRIS-1.
      END.
      ELSE valsoktemp.SOKCHAR[1] = "XXX". 
      IF pris2 = TRUE THEN DO:
         ASSIGN valsoktemp.SOKINT[2] = FILL-IN-PRIS-2.
      END.
      ELSE valsoktemp.SOKCHAR[2] = "XXX".
      IF pris3 = TRUE THEN DO:   
         ASSIGN valsoktemp.SOKINT[3] = FILL-IN-PRIS-3.
      END.
      ELSE valsoktemp.SOKCHAR[3] = "XXX".
      IF pris4 = TRUE THEN DO:   
         ASSIGN valsoktemp.SOKINT[4] = FILL-IN-PRIS-4.
      END.
      ELSE valsoktemp.SOKCHAR[4] = "XXX".              
      ASSIGN
      valsoktemp.SOKINT[1] = FILL-IN-PRIS-1      
      valsoktemp.SOKINT[2] = FILL-IN-PRIS-2
      valsoktemp.SOKINT[3] = FILL-IN-PRIS-3
      valsoktemp.SOKINT[4] = FILL-IN-PRIS-4.
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN FLRDEB.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 4,INPUT TABLE valsoktemp,INPUT-OUTPUT TABLE debpris).
      END.
      ELSE DO:
         RUN FLRDEB.P 
         (INPUT 4,INPUT TABLE valsoktemp,INPUT-OUTPUT TABLE debpris).
      END.
   END.
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BEF
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


/* ***************************  Main Block  *************************** */

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
   {DIA_M_START.I}

   FIND FIRST automregtemp WHERE automregtemp.PRISTYP = FILL-IN-DEB-1
   USE-INDEX PRISTYP NO-LOCK NO-ERROR.
   IF NOT AVAILABLE automregtemp THEN musz = musz.
   ELSE
   FILL-IN-PRIS-1 = automregtemp.PRISA.
   FIND FIRST automregtemp WHERE automregtemp.PRISTYP = FILL-IN-DEB-2
   USE-INDEX PRISTYP NO-LOCK NO-ERROR.
   IF NOT AVAILABLE automregtemp THEN musz = musz.
   ELSE
   FILL-IN-PRIS-2 = automregtemp.PRISA.
   FIND FIRST automregtemp WHERE automregtemp.PRISTYP = FILL-IN-DEB-3
   USE-INDEX PRISTYP NO-LOCK NO-ERROR.
   IF NOT AVAILABLE automregtemp THEN musz = musz.
   ELSE
   FILL-IN-PRIS-3 = automregtemp.PRISA.
   FIND FIRST automregtemp WHERE automregtemp.PRISTYP = FILL-IN-DEB-4
   USE-INDEX PRISTYP NO-LOCK NO-ERROR.
   IF NOT AVAILABLE automregtemp THEN musz = musz.
   ELSE
   FILL-IN-PRIS-4 = automregtemp.PRISA.
   RUN enable_UI.       
   {FRMSIZED.I}
   IF FILL-IN-DEB-1 = "" THEN FILL-IN-PRIS-1:HIDDEN = TRUE.  
   IF FILL-IN-DEB-2 = "" THEN FILL-IN-PRIS-2:HIDDEN = TRUE.  
   IF FILL-IN-DEB-3 = "" THEN FILL-IN-PRIS-3:HIDDEN = TRUE.  
   IF FILL-IN-DEB-4 = "" THEN FILL-IN-PRIS-4:HIDDEN = TRUE.
   RUN goma_UI.
   
   omrtemp.OMRADE:LABEL IN BROWSE BRW_OMR = Guru.Konstanter:gomrk.  
   DISPLAY FILL-IN-BEF-2 WITH FRAME {&FRAME-NAME}.
   {musarrow.i} 

   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-BEF-2 FILL-IN-DEB-1 FILL-IN-DEB-2 FILL-IN-DEB-3 FILL-IN-DEB-4 
          FILL-IN-PRIS-1 FILL-IN-PRIS-2 FILL-IN-PRIS-3 FILL-IN-PRIS-4 
      WITH FRAME DIALOG-1.
  ENABLE BRW_OMR BRW_BEF FILL-IN-PRIS-1 FILL-IN-PRIS-2 FILL-IN-PRIS-3 
         FILL-IN-PRIS-4 BTN_UPP BTN_OK 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goma_UI DIALOG-1 
PROCEDURE goma_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF omrbefvar = 1 THEN DO:
      &Scoped-define BROWSE-NAME BRW_BEF
      APPLY "HOME" TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().   
      ASSIGN
      FILL-IN-BEF-2 = befattningstemp.NAMN
      FILL-IN-BEF = befattningstemp.BEFATTNING
      {&BROWSE-NAME}:HIDDEN = FALSE.
      BRW_OMR:HIDDEN = TRUE.
   END.
   ELSE DO:
      &Scoped-define BROWSE-NAME BRW_OMR
      APPLY "HOME" TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().   
      ASSIGN
      FILL-IN-BEF-2 = omrtemp.NAMN
      FILL-IN-BEF = omrtemp.OMRADE.
      {&BROWSE-NAME}:HIDDEN = FALSE.            
      BRW_BEF:HIDDEN = TRUE.
      FILL-IN-BEF-2:LABEL = "Nu ändrar du priset för " + LC(Guru.Konstanter:gomrk).
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ändrig av priser för vald befattning".
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

