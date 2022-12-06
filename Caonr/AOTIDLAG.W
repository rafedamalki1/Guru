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

  Created: 95/05/10 -  1:37 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 

{FAKTTYPDEF.I}
{GLOBVAR2DEL1.I}

&Scoped-define SHARED SHARED 
{DIRDEF.I}
{AONRDEF.I}
{AVTAONRTEMP.I}
DEFINE SHARED     VARIABLE aonrrec       AS RECID     NO-UNDO.
DEFINE SHARED     VARIABLE musz          AS LOGICAL   NO-UNDO.
DEFINE NEW SHARED VARIABLE tidlagproch   AS HANDLE    NO-UNDO.
DEFINE            VARIABLE status-ok     AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE antal_valda   AS INTEGER   NO-UNDO.
DEFINE            VARIABLE antal_raknare AS INTEGER   NO-UNDO.
DEFINE            VARIABLE brec          AS RECID     NO-UNDO.
DEFINE            VARIABLE aotidlagapph  AS HANDLE    NO-UNDO.
DEFINE            VARIABLE idtidlage     AS CHARACTER NO-UNDO.
DEFINE VARIABLE fardigs AS LOGICAL NO-UNDO.
{HOPPSEK2W.I}
 


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_AOTID

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES aotidslagtemp tidslagtemp

/* Definitions for BROWSE BRW_AOTID                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_AOTID aotidslagtemp.TIDLAGE ~
aotidslagtemp.AKTIVITET1 aotidslagtemp.DAT1 aotidslagtemp.ANVANDARE1 ~
aotidslagtemp.AKTIVITET2 aotidslagtemp.DAT2 aotidslagtemp.ANVANDARE2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AOTID aotidslagtemp.TIDLAGE 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_AOTID aotidslagtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_AOTID aotidslagtemp
&Scoped-define QUERY-STRING-BRW_AOTID FOR EACH aotidslagtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AOTID OPEN QUERY BRW_AOTID FOR EACH aotidslagtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AOTID aotidslagtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AOTID aotidslagtemp


/* Definitions for BROWSE BRW_TIDLAGEN                                  */
&Scoped-define FIELDS-IN-QUERY-BRW_TIDLAGEN tidslagtemp.TIDLAGE ~
tidslagtemp.AKTIVITET1 tidslagtemp.AKTIVITET2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TIDLAGEN tidslagtemp.TIDLAGE 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_TIDLAGEN tidslagtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_TIDLAGEN tidslagtemp
&Scoped-define QUERY-STRING-BRW_TIDLAGEN FOR EACH tidslagtemp NO-LOCK ~
    BY tidslagtemp.TIDLAGE
&Scoped-define OPEN-QUERY-BRW_TIDLAGEN OPEN QUERY BRW_TIDLAGEN FOR EACH tidslagtemp NO-LOCK ~
    BY tidslagtemp.TIDLAGE.
&Scoped-define TABLES-IN-QUERY-BRW_TIDLAGEN tidslagtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TIDLAGEN tidslagtemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DIALOG-1 ~
    ~{&OPEN-QUERY-BRW_TIDLAGEN}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_AOTID BTN_TIDBORT BTN_TIDTILL ~
BRW_TIDLAGEN BTN_NY BTN_OK 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_TIDLAGEN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_NY 
    LABEL "Ny" 
    SIZE 12 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
    LABEL "Ok":L 
    SIZE 14 BY 1.

DEFINE BUTTON BTN_TIDBORT 
    IMAGE-UP FILE "BILDER/redner-u.bmp":U
    LABEL "NER":L 
    SIZE 4 BY 1.5.

DEFINE BUTTON BTN_TIDTILL 
    IMAGE-UP FILE "BILDER\upp-u":U
    LABEL "UPP":L 
    SIZE 4 BY 1.5.

DEFINE VARIABLE FILL-IN_TIDLAGEN AS CHARACTER FORMAT "X(256)":U INITIAL "Tidlägen" 
    VIEW-AS TEXT 
    SIZE 9.5 BY 1.21
    FONT 17 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AOTID FOR 
    aotidslagtemp SCROLLING.

DEFINE QUERY BRW_TIDLAGEN FOR 
    tidslagtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AOTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AOTID DIALOG-1 _STRUCTURED
    QUERY BRW_AOTID NO-LOCK DISPLAY
    aotidslagtemp.TIDLAGE COLUMN-LABEL "Tidläge" FORMAT "X(256)":U
    WIDTH 25
    aotidslagtemp.AKTIVITET1 COLUMN-LABEL "Aktivitet" FORMAT "X(20)":U
    aotidslagtemp.DAT1 FORMAT "99/99/99":U
    aotidslagtemp.ANVANDARE1 COLUMN-LABEL "Ändrad av" FORMAT "x(13)":U
    WIDTH 14
    aotidslagtemp.AKTIVITET2 COLUMN-LABEL "Aktivitet" FORMAT "X(20)":U
    aotidslagtemp.DAT2 FORMAT "99/99/99":U
    aotidslagtemp.ANVANDARE2 COLUMN-LABEL "Ändrad av" FORMAT "x(13)":U
    WIDTH 14
  ENABLE
      aotidslagtemp.TIDLAGE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 116.88 BY 6.63
         TITLE "Valda tidlägen".

DEFINE BROWSE BRW_TIDLAGEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TIDLAGEN DIALOG-1 _STRUCTURED
    QUERY BRW_TIDLAGEN NO-LOCK DISPLAY
    tidslagtemp.TIDLAGE COLUMN-LABEL "Tidläge" FORMAT "X(256)":U
    WIDTH 25
    tidslagtemp.AKTIVITET1 COLUMN-LABEL "Aktivitet" FORMAT "X(15)":U
    tidslagtemp.AKTIVITET2 COLUMN-LABEL "Aktivitet" FORMAT "X(15)":U
  ENABLE
      tidslagtemp.TIDLAGE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 61.75 BY 12.75
         TITLE "Tidlägen".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
    BRW_AOTID AT ROW 3.92 COL 1.5
    BTN_TIDBORT AT ROW 11 COL 46.75
    BTN_TIDTILL AT ROW 11 COL 70.75
    BRW_TIDLAGEN AT ROW 13.33 COL 29.13
    BTN_NY AT ROW 26.33 COL 54
    BTN_OK AT ROW 26.33 COL 104.38
    FILL-IN_TIDLAGEN AT ROW 1.13 COL 1.5 NO-LABEL
    "~"Dubbel klicka~" för ändring." VIEW-AS TEXT
    SIZE 59.38 BY 1.21 AT ROW 2.54 COL 1.5
    FONT 17
    SPACE(57.99) SKIP(23.62)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    TITLE "Tidlägen":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: aotidslagtemp T "?" NO-UNDO temp-db aotidslagtemp
      TABLE: tidslagtemp T "?" NO-UNDO temp-db tidslagtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_AOTID TEXT-1 DIALOG-1 */
/* BROWSE-TAB BRW_TIDLAGEN BTN_TIDTILL DIALOG-1 */
ASSIGN 
    FRAME DIALOG-1:SCROLLABLE = FALSE
    FRAME DIALOG-1:HIDDEN     = TRUE.

ASSIGN 
    BRW_AOTID:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-1 = TRUE
    BRW_AOTID:COLUMN-RESIZABLE IN FRAME DIALOG-1       = TRUE.

ASSIGN 
    BRW_TIDLAGEN:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-1 = TRUE
    BRW_TIDLAGEN:COLUMN-RESIZABLE IN FRAME DIALOG-1       = TRUE.

ASSIGN 
    BTN_NY:HIDDEN IN FRAME DIALOG-1 = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_TIDLAGEN IN FRAME DIALOG-1
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
    FILL-IN_TIDLAGEN:READ-ONLY IN FRAME DIALOG-1 = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AOTID
/* Query rebuild information for BROWSE BRW_AOTID
     _TblList          = "Temp-Tables.aotidslagtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.aotidslagtemp.TIDLAGE
"aotidslagtemp.TIDLAGE" "Tidläge" "X(256)" "character" ? ? ? ? ? ? yes ? no no "25" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.aotidslagtemp.AKTIVITET1
"aotidslagtemp.AKTIVITET1" "Aktivitet" "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   = Temp-Tables.aotidslagtemp.DAT1
     _FldNameList[4]   > Temp-Tables.aotidslagtemp.ANVANDARE1
"aotidslagtemp.ANVANDARE1" "Ändrad av" "x(13)" "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.aotidslagtemp.AKTIVITET2
"aotidslagtemp.AKTIVITET2" "Aktivitet" "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   = Temp-Tables.aotidslagtemp.DAT2
     _FldNameList[7]   > Temp-Tables.aotidslagtemp.ANVANDARE2
"aotidslagtemp.ANVANDARE2" "Ändrad av" "x(13)" "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AOTID */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TIDLAGEN
/* Query rebuild information for BROWSE BRW_TIDLAGEN
     _TblList          = "Temp-Tables.tidslagtemp"
     _Options          = "NO-LOCK "
     _OrdList          = "Temp-Tables.tidslagtemp.TIDLAGE|yes"
     _FldNameList[1]   > Temp-Tables.tidslagtemp.TIDLAGE
"tidslagtemp.TIDLAGE" "Tidläge" "X(256)" "character" ? ? ? ? ? ? yes ? no no "25" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.tidslagtemp.AKTIVITET1
"tidslagtemp.AKTIVITET1" "Aktivitet" "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.tidslagtemp.AKTIVITET2
"tidslagtemp.AKTIVITET2" "Aktivitet" "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_TIDLAGEN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Tidlägen */
    DO:
        {muswait.i}
        FRAME DIALOG-1:HIDDEN = TRUE.   
        IF VALID-HANDLE(aotidlagapph) THEN DELETE PROCEDURE aotidlagapph.   
        IF VALID-HANDLE(tidlagproch) THEN DELETE PROCEDURE tidlagproch.
        {BORTBRWPROC.I}
        RETURN.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Tidlägen */
    DO:
        {muswait.i}
        FRAME DIALOG-1:HIDDEN = TRUE.   
        IF VALID-HANDLE(aotidlagapph) THEN DELETE PROCEDURE aotidlagapph.  
        IF VALID-HANDLE(tidlagproch) THEN DELETE PROCEDURE tidlagproch.
        {BORTBRWPROC.I}
        RETURN.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AOTID
&Scoped-define SELF-NAME BRW_AOTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AOTID DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF BRW_AOTID IN FRAME DIALOG-1 /* Valda tidlägen */
    DO:
        RUN andra_UI.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY DIALOG-1
ON CHOOSE OF BTN_NY IN FRAME DIALOG-1 /* Ny */
    DO:
        RUN nytid_UI.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
    DO: 
        {muswait.i}    
        DEFINE VARIABLE numrader AS INTEGER NO-UNDO.
        FRAME DIALOG-1:HIDDEN = TRUE.
        IF VALID-HANDLE(aotidlagapph) THEN DELETE PROCEDURE aotidlagapph.   
        IF VALID-HANDLE(tidlagproch) THEN DELETE PROCEDURE tidlagproch.
        {BORTBRWPROC.I}
        RETURN.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TIDBORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TIDBORT DIALOG-1
ON CHOOSE OF BTN_TIDBORT IN FRAME DIALOG-1 /* NER */
    DO:
        RUN bort_UI.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TIDTILL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TIDTILL DIALOG-1
ON CHOOSE OF BTN_TIDTILL IN FRAME DIALOG-1 /* UPP */
    DO:
        RUN ny_UI.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} 
    APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    {DIA_M_START.I}
    {musarrow.i}
    {ALLSTARTDYN.I} 
    
    RUN laddatid IN aotidlagapph (INPUT aonrvar,INPUT delnrvar,
        OUTPUT TABLE aotidslagtemp,
        OUTPUT TABLE tidslagtemp).
   
    ASSIGN 
    FILL-IN_TIDLAGEN = Guru.Konstanter:gtidlk.
    aotidslagtemp.TIDLAGE:LABEL IN BROWSE BRW_AOTID = Guru.Konstanter:gtidlk.
    FRAME DIALOG-1:TITLE = Guru.Konstanter:gtidlk. 
   &Scoped-define BROWSE-NAME BRW_AOTID
    BRW_AOTID:TITLE = "Valda " + LC(Guru.Konstanter:gtidlk).
   &Scoped-define BROWSE-NAME BRW_TIDLAGEN
    BRW_TIDLAGEN:TITLE = Guru.Konstanter:gtidlk.
    FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND  uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
    IF AVAILABLE uppaonrtemp THEN DO:
        IF Guru.Konstanter:globforetag = "CELPA" OR Guru.Konstanter:globforetag = "GKAL" THEN   DO:
                OPEN QUERY BRW_AOTID FOR EACH aotidslagtemp
                    WHERE aotidslagtemp.AONR = uppaonrtemp.AONR AND
                    aotidslagtemp.DELNR = uppaonrtemp.DELNR NO-LOCK 
                    BY aotidslagtemp.DAT1 DESCENDING.
        END.
        ELSE DO:         
                OPEN QUERY BRW_AOTID FOR EACH aotidslagtemp
                    WHERE aotidslagtemp.AONR = uppaonrtemp.AONR AND
                    aotidslagtemp.DELNR = uppaonrtemp.DELNR NO-LOCK BY aotidslagtemp.TIDLAGE.
        END.      
    END.  
    RUN enable_UI.       
    {FRMSIZED.I}  
    IF Guru.Konstanter:regsekvar[10] = FALSE THEN DO:
        ASSIGN 
        BTN_NY:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
        DISABLE BTN_NY WITH FRAME {&FRAME-NAME}.
    END.
    {DIA_M_SLUT.I}
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-1 
PROCEDURE allstartbrw_UI :
    /* -----------------------------------------------------------
      Purpose: Changing screen-value for combo-box CMB_OMR     
      Parameters:  Input = Screen-value for CMB_FOR
      Notes:       
    -------------------------------------------------------------*/    
    ASSIGN
        aotidslagtemp.TIDLAGE:READ-ONLY IN BROWSE BRW_AOTID  = TRUE
        tidslagtemp.TIDLAGE:READ-ONLY IN BROWSE BRW_TIDLAGEN = TRUE.

    RUN DYNBRW.P PERSISTENT SET brwproc[1]
        (INPUT BRW_AOTID:HANDLE IN FRAME {&FRAME-NAME}).
    RUN DYNBRW.P PERSISTENT SET brwproc[2]
        (INPUT BRW_TIDLAGEN:HANDLE IN FRAME {&FRAME-NAME}).
 
    IF Guru.Konstanter:appcon THEN DO:
        RUN AOTIDLAGAPP.P PERSISTENT SET aotidlagapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
        RUN TIDLAGAPP.P PERSISTENT SET tidlagproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
    END.
    ELSE DO:
        RUN AOTIDLAGAPP.P PERSISTENT SET aotidlagapph.
        RUN TIDLAGAPP.P PERSISTENT SET tidlagproch.
    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI DIALOG-1 
PROCEDURE andra_UI :
    /* -----------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/
    status-ok = BRW_AOTID:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
    brec = RECID(aotidslagtemp).
    IF status-ok = FALSE THEN RETURN.
    IF aotidslagtemp.IDTIDLAG = "MALDATUM" THEN status-ok = status-ok.
    ELSE IF aotidslagtemp.ANVANDARE2 = "AUTOMATISKT" THEN    DO:
            MESSAGE "Ett automatiskt tidläge går ej att ändra" VIEW-AS ALERT-BOX TITLE "Meddelande".
            RETURN.
        END.
    {muswait.i}
    RUN ANDTIDLAG.W (INPUT aotidslagtemp.AONRTIDLREC).
    IF Guru.Konstanter:globforetag = "CELPA" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
            OPEN QUERY BRW_AOTID FOR EACH aotidslagtemp
                WHERE aotidslagtemp.AONR = uppaonrtemp.AONR AND
                aotidslagtemp.DELNR = uppaonrtemp.DELNR NO-LOCK 
                BY aotidslagtemp.DAT1 DESCENDING.
    END.
    ELSE DO:         
            OPEN QUERY BRW_AOTID FOR EACH aotidslagtemp
            WHERE aotidslagtemp.AONR = uppaonrtemp.AONR AND
            aotidslagtemp.DELNR = uppaonrtemp.DELNR NO-LOCK BY aotidslagtemp.TIDLAGE.
    END. 
    FIND aotidslagtemp WHERE RECID(aotidslagtemp) = brec NO-ERROR.
    IF AVAILABLE aotidslagtemp THEN   DO:
        RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(aotidslagtemp)).
        RUN lastselectdyn_UI IN brwproc[1].
    END.
             
   
    {musarrow.i}             
    musz = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI DIALOG-1 
PROCEDURE bort_UI :
    /* -----------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/
    status-ok = BRW_AOTID:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
    IF status-ok = FALSE THEN RETURN.
    brec = aotidslagtemp.AONRTIDLREC.
    IF aotidslagtemp.ANVANDARE2 = "AUTOMATISKT" THEN   DO:
        MESSAGE "Ett automatiskt tidläge går ej att ta bort" VIEW-AS ALERT-BOX TITLE "Meddelande".
        RETURN.
    END.
    MESSAGE "Vill du verkligen ta bort detta " LC(Guru.Konstanter:gtidlk) "?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE tidslagtemp.TIDLAGE
        UPDATE answer AS LOGICAL.
    IF answer THEN  DO TRANSACTION:
        {muswait.i}
        FIND FIRST aotidslagtemp WHERE aotidslagtemp.AONRTIDLREC = brec NO-LOCK NO-ERROR.
        RUN bort IN aotidlagapph (INPUT aotidslagtemp.AONR,INPUT aotidslagtemp.DELNR,
            INPUT aotidslagtemp.AONRTIDLREC).
        FIND FIRST aotidslagtemp WHERE aotidslagtemp.AONRTIDLREC = brec NO-LOCK NO-ERROR.
        FIND FIRST aotidkopp WHERE aotidkopp.AONR = aotidslagtemp.AONR AND
         aotidkopp.DELNR = aotidslagtemp.DELNR AND aotidkopp.IDTIDLAG = aotidslagtemp.IDTIDLAG
         USE-INDEX AONR NO-LOCK NO-ERROR.
        IF AVAILABLE aotidkopp THEN DELETE aotidkopp.
        DELETE aotidslagtemp.
        RUN selnextprevrow_UI IN brwproc[1].
        IF Guru.Konstanter:globforetag = "CELPA" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
                OPEN QUERY BRW_AOTID FOR EACH aotidslagtemp
                 WHERE aotidslagtemp.AONR = uppaonrtemp.AONR AND
                 aotidslagtemp.DELNR = uppaonrtemp.DELNR NO-LOCK 
                 BY aotidslagtemp.DAT1 DESCENDING.
        END.
        ELSE DO:         
                OPEN QUERY BRW_AOTID FOR EACH aotidslagtemp
                    WHERE aotidslagtemp.AONR = uppaonrtemp.AONR AND
                    aotidslagtemp.DELNR = uppaonrtemp.DELNR NO-LOCK BY aotidslagtemp.TIDLAGE.
        END. 
        RUN lastselectdyn_UI IN brwproc[1].     
    END.
    {musarrow.i}       
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
    DISPLAY FILL-IN_TIDLAGEN 
        WITH FRAME DIALOG-1.
    ENABLE BRW_AOTID BTN_TIDBORT BTN_TIDTILL BRW_TIDLAGEN BTN_NY BTN_OK 
        WITH FRAME DIALOG-1.
    {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nytid_UI DIALOG-1 
PROCEDURE nytid_UI :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    {muswait.i}   
    idtidlage = "".
    RUN TLAGENY.W (INPUT-OUTPUT idtidlage).   
    {musarrow.i}
   
    IF musz = FALSE THEN DO: 
        RUN openbdynspec_UI IN brwproc[2].
        FIND FIRST tidslagtemp WHERE tidslagtemp.IDTIDLAG = idtidlage NO-LOCK NO-ERROR.
        RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(tidslagtemp)).
        RUN lastselectdyn_UI IN brwproc[2].      
    END.
    musz = FALSE.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny_UI DIALOG-1 
PROCEDURE ny_UI :
    /* -----------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/
    {muswait.i}     
    DEFINE VARIABLE idvar AS CHARACTER NO-UNDO.
    antal_valda = BRW_TIDLAGEN:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
    antal_raknare = 1.
    DO WHILE antal_raknare LE antal_valda:                                   
        status-ok = BRW_TIDLAGEN:FETCH-SELECTED-ROW(antal_raknare).        
        fardigs = FALSE.
        IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
           IF uppaonrtemp.DELNR NE 0 THEN DO:
              IF tidslagtemp.IDTIDLAG BEGINS "Färdigställande" THEN fardigs = TRUE.                            
           END.   
        END.       
              
        FIND FIRST aotidslagtemp WHERE aotidslagtemp.IDTIDLAG = tidslagtemp.IDTIDLAG AND 
        aotidslagtemp.AONR = uppaonrtemp.AONR AND aotidslagtemp.DELNR = uppaonrtemp.DELNR NO-LOCK NO-ERROR.
        IF NOT AVAILABLE aotidslagtemp THEN   DO:
           IF fardigs = TRUE THEN DO:
              MESSAGE "Färdigställande läggs enbart på delnr 0!" VIEW-AS ALERT-BOX TITLE "Meddelande".
           END.
           ELSE DO:    
              RUN ny IN aotidlagapph (INPUT aonrvar,INPUT delnrvar,INPUT Guru.Konstanter:globanv,
                   INPUT Guru.Konstanter:globforetag,INPUT tidslagtemp.IDTIDLAG,
                   INPUT-OUTPUT TABLE aotidslagtemp).
           END.        
        END.
        ELSE DO:
            MESSAGE "Det går bara att lägga upp ett tidläge en gång!" VIEW-AS ALERT-BOX TITLE "Meddelande".
        END.
        ASSIGN
            idvar         = tidslagtemp.IDTIDLAG
            antal_raknare = antal_raknare + 1.
    END. 
    status-ok = BRW_TIDLAGEN:DESELECT-ROWS().  
    {musarrow.i}
    IF Guru.Konstanter:globforetag = "CELPA" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
            OPEN QUERY BRW_AOTID FOR EACH aotidslagtemp
                WHERE aotidslagtemp.AONR = uppaonrtemp.AONR AND
                aotidslagtemp.DELNR = uppaonrtemp.DELNR NO-LOCK 
                BY aotidslagtemp.DAT1 DESCENDING.
    END.
    ELSE DO:         
            OPEN QUERY BRW_AOTID FOR EACH aotidslagtemp
                WHERE aotidslagtemp.AONR = uppaonrtemp.AONR AND
                aotidslagtemp.DELNR = uppaonrtemp.DELNR NO-LOCK BY aotidslagtemp.TIDLAGE.
    END.         
    IF antal_valda >= 1 THEN DO:
        FIND FIRST aotidslagtemp WHERE aotidslagtemp.AONR = uppaonrtemp.AONR AND
            aotidslagtemp.DELNR = uppaonrtemp.DELNR AND aotidslagtemp.IDTIDLAG = idvar
            NO-LOCK NO-ERROR.
        IF AVAILABLE aotidslagtemp THEN  DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(aotidslagtemp)).
            RUN lastselectdyn_UI IN brwproc[1].
        END.
    END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

