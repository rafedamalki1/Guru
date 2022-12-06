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

  Description: OBS VERSION 7 SÄTT IN NO-LABELS VID DEF AV BROWSEN!!

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/06/27 -  7:37 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW NEW 
&Scoped-define SHARED SHARED
{DEBPRITAB.I} 
{SOKDEF.I}
{AUTOMTEMP.I}
{BEFTEMP.I}
DEFINE NEW SHARED VARIABLE timkostrec AS RECID NO-UNDO. 
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE sidnr AS INTEGER NO-UNDO.
DEFINE VARIABLE raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE radvar AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE timraknare AS INTEGER NO-UNDO.
DEFINE VARIABLE maxraknare AS INTEGER NO-UNDO.
DEFINE VARIABLE kollraknare AS INTEGER NO-UNDO.
DEFINE VARIABLE timtyp AS CHARACTER EXTENT 50 FORMAT "X(9)" NO-UNDO.
DEFINE VARIABLE grundpris AS DECIMAL EXTENT 50 FORMAT "->>>>>.99" NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE forsta AS LOGICAL NO-UNDO.
DEFINE VARIABLE visstart AS INTEGER NO-UNDO.
DEFINE VARIABLE visslut AS INTEGER NO-UNDO.
DEFINE VARIABLE pris1 AS LOGICAL NO-UNDO.    
DEFINE VARIABLE pris2 AS LOGICAL NO-UNDO.   
DEFINE VARIABLE pris3 AS LOGICAL NO-UNDO.   
DEFINE VARIABLE pris4 AS LOGICAL NO-UNDO.
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



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_DEBPR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES debpris

/* Definitions for BROWSE BRW_DEBPR                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_DEBPR debpris.PERSONALKOD ~
debpris.EFTERNAMN debpris.PRISA debpris.PRISA-2 debpris.PRISA-3 ~
debpris.PRISA-4 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_DEBPR 
&Scoped-define QUERY-STRING-BRW_DEBPR FOR EACH debpris NO-LOCK
&Scoped-define OPEN-QUERY-BRW_DEBPR OPEN QUERY BRW_DEBPR FOR EACH debpris NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_DEBPR debpris
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_DEBPR debpris


/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DIALOG-1 ~
    ~{&OPEN-QUERY-BRW_DEBPR}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-21 RAD_SORT FBTN_FLPR FBTN_SKR ~
BTN_ANDRA BTN_ANDRA-2 BTN_ANDRA-3 BTN_ANDRA-4 FILL-IN_SPERSONALKOD ~
FILL-IN_SEFTERNAMN BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_SORT FILL-IN_SPERSONALKOD ~
FILL-IN_SEFTERNAMN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ANDRA 
     LABEL "Markerade":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_ANDRA-2 
     LABEL "Alla":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_ANDRA-3 
     LABEL "Befattning":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_ANDRA-4 
     LABEL "Område":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_FLPR 
     LABEL "Fler priser":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKR 
     LABEL "Skriv ut" 
     SIZE 14 BY 1.

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

DEFINE {&NEW} SHARED VARIABLE FILL-IN-ENAMN AS CHARACTER FORMAT "X(10)":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-PER AS CHARACTER FORMAT "X(5)":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_SEFTERNAMN AS CHARACTER FORMAT "x(25)" 
     LABEL "Efternamn" 
     VIEW-AS FILL-IN 
     SIZE 20.63 BY .83.

DEFINE VARIABLE FILL-IN_SPERSONALKOD AS CHARACTER FORMAT "x(5)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .83.

DEFINE VARIABLE RAD_SORT AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Enhet/Sign", 1,
"Befattning", 2
     SIZE 30.5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 73.13 BY 1.25
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_DEBPR FOR 
      debpris SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_DEBPR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_DEBPR DIALOG-1 _STRUCTURED
  QUERY BRW_DEBPR NO-LOCK DISPLAY
      debpris.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "X(5)":U
      debpris.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "X(256)":U
            WIDTH 25
      debpris.PRISA COLUMN-LABEL "Pris" FORMAT "->>>>9.99":U
      debpris.PRISA-2 COLUMN-LABEL "Pris" FORMAT "->>>>9.99":U
      debpris.PRISA-3 COLUMN-LABEL "Pris" FORMAT "->>>>9.99":U
      debpris.PRISA-4 COLUMN-LABEL "Pris" FORMAT "->>>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH MULTIPLE SIZE 73.13 BY 13.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     RAD_SORT AT ROW 1.75 COL 60.13 NO-LABEL
     FILL-IN-PER AT ROW 5.08 COL 1.5 NO-LABEL
     FILL-IN-ENAMN AT ROW 5.08 COL 7.13 COLON-ALIGNED NO-LABEL
     FILL-IN-DEB-1 AT ROW 5.08 COL 29.75 COLON-ALIGNED NO-LABEL
     FILL-IN-DEB-2 AT ROW 5.08 COL 40.25 COLON-ALIGNED NO-LABEL
     FILL-IN-DEB-3 AT ROW 5.08 COL 50.75 COLON-ALIGNED NO-LABEL
     FILL-IN-DEB-4 AT ROW 5.08 COL 61.25 COLON-ALIGNED NO-LABEL
     BRW_DEBPR AT ROW 6.58 COL 1.5
     FBTN_FLPR AT ROW 8 COL 77.5
     FBTN_SKR AT ROW 9.08 COL 77.5
     BTN_ANDRA AT ROW 19.71 COL 11.5
     BTN_ANDRA-2 AT ROW 19.71 COL 25.5
     BTN_ANDRA-3 AT ROW 19.71 COL 39.5
     BTN_ANDRA-4 AT ROW 19.71 COL 53.5
     FILL-IN_SPERSONALKOD AT ROW 21.38 COL 23.88 COLON-ALIGNED
     FILL-IN_SEFTERNAMN AT ROW 21.38 COL 44.38 COLON-ALIGNED
     BTN_AVB AT ROW 21.42 COL 77.5
     "Uppdatering av kostnader för tjänster i kr/tim:" VIEW-AS TEXT
          SIZE 64.63 BY 1.5 AT ROW 3.04 COL 1.5
          FONT 17
     "Vid utskrift sorterad på:" VIEW-AS TEXT
          SIZE 18.88 BY .92 AT ROW 1.75 COL 41.25
     "Sök på:" VIEW-AS TEXT
          SIZE 8.38 BY .83 AT ROW 21.46 COL 4
     RECT-21 AT ROW 21.17 COL 1.5
     SPACE(17.49) SKIP(0.20)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Debiteringspriser":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db debpris
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_DEBPR FILL-IN-DEB-4 DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_DEBPR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_DEBPR:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_DEBPR:MAX-DATA-GUESS IN FRAME DIALOG-1         = 300
       BRW_DEBPR:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-1 = TRUE
       BRW_DEBPR:COLUMN-RESIZABLE IN FRAME DIALOG-1       = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DEB-1 IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN-DEB-1:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DEB-2 IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN-DEB-2:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DEB-3 IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN-DEB-3:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DEB-4 IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN-DEB-4:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-ENAMN IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN-ENAMN:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-PER IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE ALIGN-L                                  */
ASSIGN 
       FILL-IN-PER:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_DEBPR
/* Query rebuild information for BROWSE BRW_DEBPR
     _TblList          = "Temp-Tables.debpris"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.debpris.PERSONALKOD
"debpris.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.debpris.EFTERNAMN
"debpris.EFTERNAMN" "Efternamn" "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.debpris.PRISA
"debpris.PRISA" "Pris" "->>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.debpris.PRISA-2
"debpris.PRISA-2" "Pris" "->>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.debpris.PRISA-3
"debpris.PRISA-3" "Pris" "->>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.debpris.PRISA-4
"debpris.PRISA-4" "Pris" "->>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_DEBPR */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_DEBPR
&Scoped-define SELF-NAME BRW_DEBPR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_DEBPR DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF BRW_DEBPR IN FRAME DIALOG-1
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   timkostrec = RECID(debpris).
   {muswait.i}
   
   RUN DEBMARK.W.
   
   {musarrow.i} 
   IF musz = TRUE THEN DO:     
      musz = FALSE.
    
   END.
   ELSE DO:
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 63
      soktemp.SOKCHAR[1] = FILL-IN-DEB-1
      soktemp.SOKCHAR[2] = FILL-IN-DEB-2
      soktemp.SOKCHAR[3] = FILL-IN-DEB-3
      soktemp.SOKCHAR[4] = FILL-IN-DEB-4
      soktemp.SOKCHAR[5] = debpris.PERSONALKOD
      soktemp.SOKINT[1] = debpris.PRISA      
      soktemp.SOKINT[2] = debpris.PRISA-2
      soktemp.SOKINT[3] = debpris.PRISA-3
      soktemp.SOKINT[4] = debpris.PRISA-4.
      {SOKANROP.I}      
   END.
   {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}  
   RUN repo_UI (INPUT timkostrec). 
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.  
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ANDRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDRA DIALOG-1
ON CHOOSE OF BTN_ANDRA IN FRAME DIALOG-1 /* Markerade */
DO:
   antal_valda = {&BROWSE-NAME}:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   MESSAGE "Vill du att alla markerade ska ha samma priser ?"   
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE val1 AS LOGICAL.
   CASE val1:
      WHEN FALSE THEN DO:        
         DO WHILE antal_raknare LE antal_valda :
            status-ok = {&BROWSE-NAME}:FETCH-SELECTED-ROW(antal_raknare).
            timkostrec = RECID(debpris).
            {muswait.i}
            
            RUN DEBMARK.W.
            
            {musarrow.i} 
            IF musz = TRUE THEN DO:      
               musz = FALSE.        
            END.
            ELSE DO:
               {SOKSTART.I}
               ASSIGN
               soktemp.SOKVAL = 63
               soktemp.SOKCHAR[1] = FILL-IN-DEB-1
               soktemp.SOKCHAR[2] = FILL-IN-DEB-2
               soktemp.SOKCHAR[3] = FILL-IN-DEB-3
               soktemp.SOKCHAR[4] = FILL-IN-DEB-4
               soktemp.SOKCHAR[5] = debpris.PERSONALKOD
               soktemp.SOKINT[1] = debpris.PRISA      
               soktemp.SOKINT[2] = debpris.PRISA-2
               soktemp.SOKINT[3] = debpris.PRISA-3
               soktemp.SOKINT[4] = debpris.PRISA-4.
               {SOKANROP.I}                     
            END.      
            antal_raknare = antal_raknare + 1.  
         END.
      END.
      WHEN TRUE THEN DO:
         
         RUN DEBMARKA.W (OUTPUT pris1, OUTPUT pris2, OUTPUT pris3, OUTPUT pris4,
         OUTPUT FILL-IN-PRIS-1, OUTPUT FILL-IN-PRIS-2, OUTPUT FILL-IN-PRIS-3, 
         OUTPUT FILL-IN-PRIS-4).         
         
         IF musz = TRUE THEN DO:      
            ASSIGN
            pris1 = FALSE
            pris2 = FALSE
            pris3 = FALSE
            pris4 = FALSE
            musz = FALSE.        
         END.    
         ELSE DO:
            DO WHILE antal_raknare LE antal_valda :
               status-ok = {&BROWSE-NAME}:FETCH-SELECTED-ROW(antal_raknare).
               {SOKSTART.I}
               ASSIGN
               soktemp.SOKVAL = 63
               soktemp.SOKCHAR[1] = FILL-IN-DEB-1
               soktemp.SOKCHAR[2] = FILL-IN-DEB-2
               soktemp.SOKCHAR[3] = FILL-IN-DEB-3
               soktemp.SOKCHAR[4] = FILL-IN-DEB-4.
               IF pris1 = TRUE THEN DO TRANSACTION:
                  ASSIGN debpris.PRISA = FILL-IN-PRIS-1.
               END.
               ELSE soktemp.SOKCHAR[1] = "XXX". 
               IF pris2 = TRUE THEN DO TRANSACTION:
                  ASSIGN debpris.PRISA-2 = FILL-IN-PRIS-2.
               END.
               ELSE soktemp.SOKCHAR[2] = "XXX".
               IF pris3 = TRUE THEN DO TRANSACTION:   
                  ASSIGN debpris.PRISA-3 = FILL-IN-PRIS-3.
               END.
               ELSE soktemp.SOKCHAR[3] = "XXX".
               IF pris4 = TRUE THEN DO TRANSACTION:   
                  ASSIGN debpris.PRISA-4 = FILL-IN-PRIS-4.
               END.
               ELSE soktemp.SOKCHAR[4] = "XXX".              
               ASSIGN
               soktemp.SOKCHAR[5] = debpris.PERSONALKOD
               soktemp.SOKINT[1] = debpris.PRISA      
               soktemp.SOKINT[2] = debpris.PRISA-2
               soktemp.SOKINT[3] = debpris.PRISA-3
               soktemp.SOKINT[4] = debpris.PRISA-4.
               {SOKANROP.I}                     
               antal_raknare = antal_raknare + 1.
            END.   
            ASSIGN
            pris1 = FALSE
            pris2 = FALSE
            pris3 = FALSE
            pris4 = FALSE
            musz = FALSE.
         END. 
      END.
      OTHERWISE DO:
         RETURN NO-APPLY. 
      END.         
   END CASE.
   
   OPEN QUERY BRW_DEBPR FOR EACH debpris NO-LOCK .
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ANDRA-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDRA-2 DIALOG-1
ON CHOOSE OF BTN_ANDRA-2 IN FRAME DIALOG-1 /* Alla */
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   timkostrec = RECID(debpris).
   {muswait.i}
   RUN DEBALLA.W.
   
   
   {musarrow.i} 
   {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ANDRA-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDRA-3 DIALOG-1
ON CHOOSE OF BTN_ANDRA-3 IN FRAME DIALOG-1 /* Befattning */
DO:    
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   timkostrec = RECID(debpris).   
   {muswait.i}
   
   RUN DEBBEF.W (INPUT 1).
   
   {musarrow.i} 
   
   OPEN QUERY BRW_DEBPR FOR EACH debpris NO-LOCK .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ANDRA-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDRA-4 DIALOG-1
ON CHOOSE OF BTN_ANDRA-4 IN FRAME DIALOG-1 /* Område */
DO:    
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   timkostrec = RECID(debpris).   
   {muswait.i}
   
   RUN DEBBEF.W (INPUT 2).
   
   {musarrow.i} 
   
   OPEN QUERY BRW_DEBPR FOR EACH debpris NO-LOCK .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avsluta */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_FLPR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_FLPR DIALOG-1
ON CHOOSE OF FBTN_FLPR IN FRAME DIALOG-1 /* Fler priser */
DO:
   radvar = CURRENT-RESULT-ROW("{&BROWSE-NAME}").
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   timkostrec = RECID(debpris).      
   kollraknare = kollraknare + 4.
   forsta = FALSE.
   IF timtyp[kollraknare + 1] = "" THEN DO:
      MESSAGE "Det finns inga flera priser att visa!  Visningen starta om från början!" 
      VIEW-AS ALERT-BOX.
      {muswait.i}
      ASSIGN
      timkostrec = RECID(debpris) 
      timraknare = 1
      kollraknare = 0         
      visstart = 1
      visslut = 4.
      OPEN QUERY aq FOR EACH automregtemp USE-INDEX PRISTYP NO-LOCK.
      GET FIRST aq NO-LOCK.
      DO WHILE AVAILABLE(automregtemp):
         ASSIGN
         timtyp[timraknare] = automregtemp.PRISTYP
         grundpris[timraknare] = automregtemp.PRISA.
         maxraknare = timraknare.
         timraknare = timraknare + 1. 
         GET NEXT aq NO-LOCK.
      END.
      {DEBPRIIN.I}       
      RUN label_UI.            
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}  
      RUN repor_UI (INPUT radvar).         
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
      APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.      
   END.
   ELSE DO:             
      status-mus2 = SESSION:SET-WAIT-STATE("GENERAL").                          
      timraknare = 1.     
      REPEAT:
         timtyp[timraknare] = timtyp[timraknare + kollraknare].
         grundpris[timraknare] = grundpris[timraknare + kollraknare].
         IF timraknare = 4 THEN LEAVE.
         timraknare = timraknare + 1.
      END.   
      FOR EACH valsoktemp:
         DELETE valsoktemp.
      END.   
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN FLRDEB.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 1,INPUT TABLE valsoktemp,INPUT-OUTPUT TABLE debpris).
      END.
      ELSE DO:
         RUN FLRDEB.P 
         (INPUT 1,INPUT TABLE valsoktemp,INPUT-OUTPUT TABLE debpris).
      END.
      RUN label_UI.     
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}  
      RUN repor_UI (INPUT radvar).   
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
      APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.      
   END.
   status-mus2 = SESSION:SET-WAIT-STATE("").
   {musarrow.i}     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR DIALOG-1
ON CHOOSE OF FBTN_SKR IN FRAME DIALOG-1 /* Skriv ut */
DO:
   RUN SKRIVVAL.W (INPUT FALSE).          
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:    
      RUN ut_UI.      
   END.
   {musarrow.i}      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR DIALOG-1
ON MOUSE-MENU-CLICK OF FBTN_SKR IN FRAME DIALOG-1 /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SEFTERNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SEFTERNAMN DIALOG-1
ON ANY-KEY OF FILL-IN_SEFTERNAMN IN FRAME DIALOG-1 /* Efternamn */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SEFTERNAMN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SEFTERNAMN DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SEFTERNAMN IN FRAME DIALOG-1 /* Efternamn */
DO:
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.   
   FILL-IN_SEFTERNAMN = INPUT FILL-IN_SEFTERNAMN.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   persrec = RECID(debpris).
   IF FILL-IN_SEFTERNAMN = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SEFTERNAMN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   posok = '*' + FILL-IN_SEFTERNAMN + '*'.
   FIND debpris WHERE RECID(debpris) = persrec NO-LOCK NO-ERROR.
   FIND NEXT debpris WHERE debpris.EFTERNAMN MATCHES posok       
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.    
   IF NOT AVAILABLE debpris THEN DO:
      FIND FIRST debpris WHERE debpris.EFTERNAMN MATCHES posok       
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE debpris THEN DO:
         MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_SEFTERNAMN IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   IF AVAILABLE debpris THEN DO:
      RUN repo_UI (INPUT RECID(debpris)).
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SPERSONALKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERSONALKOD DIALOG-1
ON ANY-KEY OF FILL-IN_SPERSONALKOD IN FRAME DIALOG-1 /* Enhet/Sign */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERSONALKOD DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SPERSONALKOD IN FRAME DIALOG-1 /* Enhet/Sign */
DO:
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.
   FILL-IN_SPERSONALKOD = INPUT FILL-IN_SPERSONALKOD.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   persrec = RECID(debpris).
   IF FILL-IN_SPERSONALKOD = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   posok = '*' + FILL-IN_SPERSONALKOD + '*'.
   FIND debpris WHERE RECID(debpris) = persrec NO-LOCK NO-ERROR.
   FIND NEXT debpris WHERE debpris.PERSONALKOD MATCHES posok       
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE debpris THEN DO:
      FIND FIRST debpris WHERE debpris.PERSONALKOD MATCHES posok       
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE debpris THEN DO:
         MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   IF AVAILABLE debpris THEN DO:
      RUN repo_UI (INPUT RECID(debpris)).
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_SORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_SORT DIALOG-1
ON VALUE-CHANGED OF RAD_SORT IN FRAME DIALOG-1
DO:
   RAD_SORT = INPUT RAD_SORT.    
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
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   IF Guru.Konstanter:appcon THEN DO: 
      RUN AUOTOMRHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (OUTPUT TABLE automregtemp).
   END.
   ELSE DO:
      RUN AUOTOMRHMT.P 
      (OUTPUT TABLE automregtemp).
   END.
   IF Guru.Konstanter:appcon THEN DO: 
      RUN BEFHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (OUTPUT TABLE befattningstemp).
   END.
   ELSE DO:
      RUN BEFHMT.P
      (OUTPUT TABLE befattningstemp).
   END.   
   FOR EACH debpris USE-INDEX PERSONALKOD:
      DELETE debpris.
   END.
   forsta = TRUE.
   timraknare = 1.
   kollraknare = 0.    
   OPEN QUERY aq FOR EACH automregtemp USE-INDEX PRISTYP NO-LOCK.
   GET FIRST aq NO-LOCK.
   DO WHILE AVAILABLE(automregtemp):
      ASSIGN
      timtyp[timraknare] = automregtemp.PRISTYP
      grundpris[timraknare] = automregtemp.PRISA.
      maxraknare = timraknare.
      timraknare = timraknare + 1. 
      GET NEXT aq NO-LOCK.
   END.      
   {DEBPRIIN.I}                                  
   RUN label_UI.   
   RAD_SORT = 1.           
   RUN enable_UI.       
   {FRMSIZED.I}
   
   BTN_ANDRA-4:LABEL = Guru.Konstanter:gomrk.
   APPLY "HOME" TO {&BROWSE-NAME}.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
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
  DISPLAY RAD_SORT FILL-IN_SPERSONALKOD FILL-IN_SEFTERNAMN 
      WITH FRAME DIALOG-1.
  ENABLE RECT-21 RAD_SORT FBTN_FLPR FBTN_SKR BTN_ANDRA BTN_ANDRA-2 BTN_ANDRA-3 
         BTN_ANDRA-4 FILL-IN_SPERSONALKOD FILL-IN_SEFTERNAMN BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE label_UI DIALOG-1 
PROCEDURE label_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/                   
   {DEPRIS.I}           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repor_UI DIALOG-1 
PROCEDURE repor_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER browrec AS INTEGER NO-UNDO.
   {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
   REPOSITION {&BROWSE-NAME} TO ROW browrec NO-ERROR.  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo_UI DIALOG-1 
PROCEDURE repo_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER browrec AS RECID NO-UNDO.
   {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
   REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI DIALOG-1 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   {muswait.i}
   {PRINTSTAENDE.I}  
   PUT SKIP (2).
   ASSIGN
   sidnr = 1
   raknare = 4. 
   PUT sidnr AT 80 SKIP.              /*RAD 3*/           
   IF RAD_SORT = 1 THEN DO:
       PUT  TODAY AT  6
       SKIP.
       raknare = raknare + 1.
       PUT       
       "ENHET/" AT 6     
       "DEBITERINGSPRISER :" AT 44 
       SKIP.
       raknare = raknare + 1.
       PUT  
       "SIGN" AT 6               
       "NAMN" AT 12
       "BEFATTNING" AT 23       
       FILL-IN-DEB-1 AT 44
       FILL-IN-DEB-2 AT 54
       FILL-IN-DEB-3 AT 64 
       FILL-IN-DEB-4 AT 74
       SKIP. 
       raknare = raknare + 1.
       PUT                       
       "=====.==========.====================.=========.=========.=========.=========."  
       AT 6
       SKIP.
       raknare = raknare + 1.
       FOR EACH debpris USE-INDEX PERSONALKOD NO-LOCK:
          FIND FIRST befattningstemp WHERE 
          befattningstemp.BEFATTNING = debpris.BEFATTNING NO-LOCK NO-ERROR.
          PUT          
          debpris.PERSONALKOD AT 6
          debpris.FORNAMN FORMAT "X(1)" AT 12
          debpris.EFTERNAMN FORMAT "x(8)" AT 14  
          befattningstemp.NAMN AT 23           
          debpris.PRISA FORMAT ">>>>>>>>9" AT 44
          debpris.PRISA-2 FORMAT ">>>>>>>>9" AT 54
          debpris.PRISA-3 FORMAT ">>>>>>>>9" AT 64
          debpris.PRISA-4 FORMAT ">>>>>>>>9" AT 74
          SKIP.
          IF raknare < Guru.GlobalaVariabler:globsids THEN DO:  
              raknare = raknare + 1.
          END.  
          ELSE DO:
             PUT SKIP (2).
             ASSIGN         
             raknare = 4
             sidnr = sidnr + 1.
             PUT sidnr AT 80    /*RAD 1*/
             SKIP.
          END.
       END.
    END.  
    IF RAD_SORT = 2 THEN DO:
       PUT  TODAY AT  6
       SKIP.
       raknare = raknare + 1.
       PUT       
       "ENHET/" AT 27     
       "DEBITERINGSPRISER :" AT 44 
       SKIP.
       raknare = raknare + 1.
       PUT "BEFATTNING" AT 6 
       "SIGN" AT 27               
       "NAMN" AT 33
       FILL-IN-DEB-1 AT 44
       FILL-IN-DEB-2 AT 54
       FILL-IN-DEB-3 AT 64 
       FILL-IN-DEB-4 AT 74
       SKIP. 
       raknare = raknare + 1.
       PUT      
       "====================.=====.==========.=========.=========.=========.=========."  
       AT 6
       SKIP.
       FOR EACH debpris USE-INDEX BEF NO-LOCK:
          FIND FIRST befattningstemp WHERE 
          befattningstemp.BEFATTNING = debpris.BEFATTNING NO-LOCK NO-ERROR.
          PUT
          befattningstemp.NAMN AT 6
          debpris.PERSONALKOD AT 27
          debpris.FORNAMN FORMAT "X(1)" AT 33
          debpris.EFTERNAMN FORMAT "x(8)" AT 35             
          debpris.PRISA FORMAT ">>>>>>>>9" AT 44
          debpris.PRISA-2 FORMAT ">>>>>>>>9" AT 54
          debpris.PRISA-3 FORMAT ">>>>>>>>9" AT 64
          debpris.PRISA-4 FORMAT ">>>>>>>>9" AT 74
          SKIP.
          IF raknare < Guru.GlobalaVariabler:globsids THEN DO:  
              raknare = raknare + 1.
          END.  
          ELSE DO:
             PUT SKIP (2).
             ASSIGN         
             raknare = 4
             sidnr = sidnr + 1.
             PUT sidnr AT 80    /*RAD 1*/
             SKIP.
          END.
       END.
    END.           
   OUTPUT CLOSE.
   {musarrow.i}    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

