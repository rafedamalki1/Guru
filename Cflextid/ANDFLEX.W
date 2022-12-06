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

  Created: 95/07/05 - 10:41 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&Scoped-define NEW    
&Scoped-define SHARED 
{AVDTEMP.I}
{FLEXTAB.I}
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR extratemp.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}
DEFINE NEW SHARED VARIABLE bustart3 AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE flrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE flrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE fldrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE klocka AS DECIMAL NO-UNDO. 
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE sparomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE flexkvst AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexkvsl AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexkvslspar AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexmost AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexmostspar AS DECIMAL NO-UNDO.
DEFINE VARIABLE flexmosl AS DECIMAL NO-UNDO.     
DEFINE VARIABLE seku AS INTEGER NO-UNDO.
DEFINE VARIABLE pkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sok3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE nyttaoapph AS HANDLE NO-UNDO.                     /* NYTTAOAPP.P */
DEFINE VARIABLE omravdand AS INTEGER NO-UNDO.   /*används i NYCOL.I*/
DEFINE VARIABLE flexomrade AS CHARACTER NO-UNDO.
{OMRTEMPW.I}
&Scoped-define NEW   
&Scoped-define SHARED
{ANVPERS.I}
&Scoped-define NEW   
&Scoped-define SHARED SHARED
{DIRDEF.I}
{AVDELNINGTEMPT.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_AONR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES utsokaonr

/* Definitions for BROWSE BRW_AONR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_AONR utsokaonr.OMRADE utsokaonr.AONR ~
utsokaonr.DELNR utsokaonr.ORT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AONR utsokaonr.OMRADE 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_AONR utsokaonr
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_AONR utsokaonr
&Scoped-define QUERY-STRING-BRW_AONR FOR EACH utsokaonr NO-LOCK ~
    BY utsokaonr.OMRADE ~
       BY utsokaonr.AONR ~
        BY utsokaonr.DELNR
&Scoped-define OPEN-QUERY-BRW_AONR OPEN QUERY BRW_AONR FOR EACH utsokaonr NO-LOCK ~
    BY utsokaonr.OMRADE ~
       BY utsokaonr.AONR ~
        BY utsokaonr.DELNR.
&Scoped-define TABLES-IN-QUERY-BRW_AONR utsokaonr
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AONR utsokaonr


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN_TID CMB_KNAPP FILL-IN_AONR ~
FILL-IN_DELNR FILL-IN_ORSAK BTN_REG BTN_AVB CMB_AVD CMB_OMR RECT-22 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_TID CMB_KNAPP FILL-IN_AONR ~
FILL-IN_DELNR FILL-IN_ORSAK FILL-IN-TEXT CMB_AVD CMB_OMR ~
FILL-IN_PERSONALKOD FILL-IN_DATUM FILL-IN-SKP 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_REG 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_KNAPP AS CHARACTER FORMAT "X(256)":U 
     LABEL "Knapp" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "In","Ut","Lunch ut","Lunch in","Övertid ut","Övertid in","Annat in","Annat ut","Flex in","Flex ut" 
     DROP-DOWN-LIST
     SIZE 13.38 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SKP AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Visa aonr för:" 
     VIEW-AS FILL-IN 
     SIZE 22.75 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_AONRS AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_DATUM AS DATE FORMAT "99/99/99" 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT "999" INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_ORSAK AS CHARACTER FORMAT "X(68)" 
     LABEL "Orsak" 
     VIEW-AS FILL-IN 
     SIZE 27.13 BY 1.

DEFINE VARIABLE FILL-IN_ORTS AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_PERSONALKOD AS CHARACTER FORMAT "x(5)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_TID AS DECIMAL FORMAT "99.99" INITIAL 0 
     LABEL "Tid" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

DEFINE VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 34 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 51 BY 2.42
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      utsokaonr SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AONR DIALOG-1 _STRUCTURED
  QUERY BRW_AONR NO-LOCK DISPLAY
      utsokaonr.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      utsokaonr.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      utsokaonr.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      utsokaonr.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(40)":U
  ENABLE
      utsokaonr.OMRADE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 51 BY 11.83
         TITLE "Aktiva arbetsordernummer".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_TID AT ROW 8.83 COL 11.88 COLON-ALIGNED
     CMB_KNAPP AT ROW 10.5 COL 11.88 COLON-ALIGNED
     FILL-IN_AONR AT ROW 12.17 COL 11.88 COLON-ALIGNED
     FILL-IN_DELNR AT ROW 13.79 COL 11.88 COLON-ALIGNED
     FILL-IN_ORSAK AT ROW 15.42 COL 11.88 COLON-ALIGNED
     BTN_REG AT ROW 20.5 COL 63.75
     BTN_AVB AT ROW 20.5 COL 78.75
     FILL-IN_AONRS AT ROW 17.88 COL 65.5 COLON-ALIGNED
     FILL-IN_ORTS AT ROW 18.83 COL 65.5 COLON-ALIGNED
     FILL-IN-TEXT AT ROW 1.54 COL 68 COLON-ALIGNED NO-LABEL
     CMB_AVD AT ROW 2.5 COL 68.25 COLON-ALIGNED NO-LABEL
     RAD_FAST AT ROW 3.67 COL 34.75 NO-LABEL
     CMB_OMR AT ROW 3.67 COL 68.25 COLON-ALIGNED NO-LABEL
     BRW_AONR AT ROW 4.92 COL 41.75
     FILL-IN_PERSONALKOD AT ROW 5.58 COL 11.88 COLON-ALIGNED
     FILL-IN_DATUM AT ROW 7.21 COL 11.88 COLON-ALIGNED
     FILL-IN-SKP AT ROW 17.75 COL 40.88 COLON-ALIGNED NO-LABEL
     RECT-22 AT ROW 17.46 COL 41.75
     SPACE(1.74) SKIP(2.11)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändra eller registrera flextid":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: utsokaonr T "?" NO-UNDO temp-db utsokaonr
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE Custom                                                   */
/* BROWSE-TAB BRW_AONR CMB_OMR DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_AONR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_AONR:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_AONR:MAX-DATA-GUESS IN FRAME DIALOG-1         = 1000.

/* SETTINGS FOR FILL-IN FILL-IN-SKP IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-TEXT IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_AONRS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_AONRS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_DATUM IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_ORTS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_ORTS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_PERSONALKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_FAST:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AONR
/* Query rebuild information for BROWSE BRW_AONR
     _TblList          = "Temp-Tables.utsokaonr"
     _Options          = "NO-LOCK "
     _OrdList          = "Temp-Tables.utsokaonr.OMRADE|yes,Temp-Tables.utsokaonr.AONR|yes,Temp-Tables.utsokaonr.DELNR|yes"
     _FldNameList[1]   > Temp-Tables.utsokaonr.OMRADE
"utsokaonr.OMRADE" "Område" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.utsokaonr.AONR
"utsokaonr.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.utsokaonr.DELNR
"utsokaonr.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.utsokaonr.ORT
"utsokaonr.ORT" "Ort/Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AONR */
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
ON END-ERROR OF FRAME DIALOG-1 /* Ändra eller registrera flextid */
DO:
   musz = TRUE.
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Ändra eller registrera flextid */
DO:     
   RETURN.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AONR
&Scoped-define SELF-NAME BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AONR DIALOG-1
ON VALUE-CHANGED OF BRW_AONR IN FRAME DIALOG-1 /* Aktiva arbetsordernummer */
DO:
   IF musz = FALSE THEN DO: 
      IF AVAILABLE utsokaonr THEN DO:
         ASSIGN
         FILL-IN_AONR = utsokaonr.AONR
         FILL-IN_DELNR = utsokaonr.DELNR.
         DISPLAY  FILL-IN_AONR FILL-IN_DELNR WITH FRAME {&FRAME-NAME}.    
      END.
   END.
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   musz = TRUE.
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:

   {muswait.i}     
   ASSIGN
   FILL-IN_DATUM = INPUT FILL-IN_DATUM
   FILL-IN_TID = INPUT FILL-IN_TID  
   CMB_KNAPP = INPUT CMB_KNAPP
   FILL-IN_AONR = INPUT FILL-IN_AONR
   FILL-IN_DELNR = INPUT FILL-IN_DELNR
   FILL-IN_ORSAK = INPUT FILL-IN_ORSAK.  
   regdatum = FILL-IN_DATUM.
   IF FILL-IN_TID > 24.00 THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.   
   IF ( FILL-IN_TID - TRUNCATE(FILL-IN_TID,0)) > 0.59 THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.
   IF FILL-IN_TID = 00.00 THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END. 
   RUN REGDAG.P.
   RUN REGVEC.P.
   {SLFLARBW.I}.   
   sok4 = STRING(regdatum).
   IF Guru.Konstanter:appcon THEN DO: 
      RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 9,INPUT-OUTPUT personaltemp.PERSONALKOD,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.
   ELSE DO:
      RUN FLEXTIDH.P 
      (INPUT 9,INPUT-OUTPUT personaltemp.PERSONALKOD,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5). 
   END.
   IF sok4 = "" THEN sok4 = sok4.
   ELSE IF regdatum LE DATE(sok4) THEN DO:
      MESSAGE "Tidsedeln är godkänd/färdigrapporterad tom"  DATE(sok4) VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN_DATUM IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.      
   END.

   IF Guru.Konstanter:appcon THEN DO:                           
      RUN GODKOLLA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT pkod,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN GODKOLLA.P  
      (INPUT pkod,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
      RETURN NO-APPLY.      
   END.             
   FIND FIRST flexregtemp WHERE flexregtemp.KOD = flexavttemp.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.
   IF AVAILABLE flexregtemp THEN DO:
      flexkvst = flexregtemp.KVSTART.
      flexkvsl = flexregtemp.KVSLUT.
      IF MONTH(regdatum) > MONTH(flexregtemp.SOMMARST) AND 
         MONTH(regdatum) < MONTH(flexregtemp.SOMMARSL) THEN DO:
         flexkvst = flexregtemp.KVSOST.
         flexkvsl = flexregtemp.KVSOSL.
      END.
      ELSE IF MONTH(regdatum) = MONTH(flexregtemp.SOMMARSL) AND 
         DAY(regdatum) <= DAY(flexregtemp.SOMMARSL) THEN DO:
         flexkvst = flexregtemp.KVSOST.
         flexkvsl = flexregtemp.KVSOSL.
      END.
      ELSE IF MONTH(regdatum) = MONTH(flexregtemp.SOMMARST) AND 
         DAY(regdatum) >= DAY(flexregtemp.SOMMARST) THEN DO: 
         flexkvst = flexregtemp.KVSOST.
         flexkvsl = flexregtemp.KVSOSL.
      END.
      ASSIGN
      flexmost = flexregtemp.MOSTART
      flexmosl = flexregtemp.MOSLUT.
      /*hårdkodning för att slippa ändra flexreg.MOSLUT efter sommartid OBS- måste ändras till vintertid en gång till*/
      IF MONTH(TODAY) > MONTH(flexregtemp.SOMMARST) AND MONTH(TODAY) < MONTH(flexregtemp.SOMMARSL) THEN DO:
         IF flexavttemp.FLEXKOD = "K" THEN regdatum = regdatum.               
         ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.
         /*ELSE IF flexavttemp.FLEXKOD = "KV" AND flexmosl = 8 THEN ASSIGN flexmosl = 7.30.*/       
      END.
      ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARSL) AND DAY(TODAY) <= DAY(flexregtemp.SOMMARSL) THEN DO:
         IF flexavttemp.FLEXKOD = "K" THEN regdatum = regdatum.
         ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.         
      END.
      ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARST) AND DAY(TODAY) >= DAY(flexregtemp.SOMMARST) THEN DO: 
         IF flexavttemp.FLEXKOD = "K" THEN regdatum = regdatum.
         ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.         
      END.
      ELSE DO:
         IF flexavttemp.FLEXKOD = "K" THEN regdatum = regdatum.
         ELSE  IF flexmosl = 8.30 THEN ASSIGN flexmosl = 9.00.                                 
      END.  
      IF personaltemp.DELTID = TRUE THEN DO:
         sok1 = personaltemp.ANSTALLNING.
         IF Guru.Konstanter:appcon THEN DO: 
            RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT 6,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
            INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
         END.
         ELSE DO:
            RUN FLEXTIDH.P 
            (INPUT 6,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
            INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
         END.   
         IF sok2 = 1 THEN sok2 = sok2.
         ELSE DO:
            musz = FALSE.
            FIND FIRST flexregtemp WHERE flexregtemp.KOD = flexavttemp.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.
            IF AVAILABLE flexregtemp THEN DO:            
               IF MONTH(TODAY) > MONTH(flexregtemp.SOMMARST) AND 
               MONTH(TODAY) < MONTH(flexregtemp.SOMMARSL) THEN musz = TRUE.
               ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARSL) AND 
               DAY(TODAY) <= DAY(flexregtemp.SOMMARSL) THEN musz = TRUE.               
               ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARST) AND 
               DAY(TODAY) >= DAY(flexregtemp.SOMMARST) THEN musz = TRUE.
            END.            
            IF musz = TRUE THEN DO:
               musz = FALSE.
               IF sok4 NE "" THEN DO:
                  nytid = DECIMAL(sok4).
               END.
               ELSE DO:
                  sekunder = sok5.
                  RUN SEKTIM.P.
               END.
            END.
            ELSE DO:         
               sekunder = sok5.
               RUN SEKTIM.P.
            END.                        
            IF nytid > regslut THEN DO:                
               flexkvslspar = flexkvsl.
               nytid = flexkvsl.
               RUN TIMSEK.P.
               seku = sekunder.
               nytid = regslut.
               RUN TIMSEK.P.
               sekunder = seku - sok5 + sekunder.
               RUN SEKTIM.P.
               flexkvsl = nytid.
               nytid = flexkvst.
               RUN TIMSEK.P.
               seku = sekunder.
               nytid = regslut.
               RUN TIMSEK.P.
               sekunder = seku - sok5 + sekunder.
               RUN SEKTIM.P.
               flexkvst = nytid.
               IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
                  flexkvsl = flexkvslspar.                  
               END.
            END.
            sok1 = personaltemp.ANSTALLNING.
            IF Guru.Konstanter:appcon THEN DO: 
               RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT 5,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
               INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
            END.
            ELSE DO:
               RUN FLEXTIDH.P 
               (INPUT 5,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
               INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
            END.   
            IF sok2 = 1 THEN sok2 = sok2.
            ELSE DO:
               sekunder = sok5.
               RUN SEKTIM.P.               
               IF nytid < regstart THEN DO:           
                  flexmostspar = flexmost.
                  nytid = flexmosl.
                  RUN TIMSEK.P.
                  seku = sekunder.
                  nytid = regstart.
                  RUN TIMSEK.P.
                  sekunder = seku - sok5 + sekunder.
                  RUN SEKTIM.P.
                  flexmosl = nytid.
                  nytid = flexmost.
                  RUN TIMSEK.P.
                  seku = sekunder.
                  nytid = regstart.
                  RUN TIMSEK.P.
                  sekunder = seku - sok5 + sekunder.
                  RUN SEKTIM.P.
                  flexmost = nytid.
                  IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:               
                     IF personaltemp.PERSONALKOD = "SEMGN" THEN DO:
                        flexmost = flexmostspar.
                     END.
                  END.
               END.               
            END.
         END.        
      END.
      ASSIGN
      sok1 = ansttemp.KOD
      sok4 = STRING(regdatum).
      IF Guru.Konstanter:appcon THEN DO: 
         RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 7,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.
      ELSE DO:
         RUN FLEXTIDH.P 
         (INPUT 7,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.
      IF sok2 = 1 THEN DO:
         flexkvst = regslut.
      END.    
   END.    
   IF CMB_KNAPP = "IN" THEN DO:       
      IF AVAILABLE flexregtemp THEN DO:        
         IF FILL-IN_TID > flexmosl THEN DO:                                                         
            MESSAGE "Du är utanför flexramen använd knappen Flex in istället" flexmosl VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.            
         END.          
         IF FILL-IN_TID < flexmost THEN DO:                       
            MESSAGE "Du är inte inom flexramen! Vill du registrera ändå?" VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO UPDATE val3 AS LOGICAL.
            CASE val3:             
               WHEN FALSE THEN DO:
                  APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
                  APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.                                            
               END.   
            END CASE.
         END.      
      END.   
   END.   
   IF CMB_KNAPP = "UT" THEN DO:       
      IF AVAILABLE flexregtemp THEN DO:
         IF FILL-IN_TID < flexkvst THEN DO:                                                         
            MESSAGE "Du är inte inom flexramen! Vill du registrera ändå?" VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO UPDATE val2 AS LOGICAL.
            CASE val2:             
               WHEN FALSE THEN DO:
                  APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
                  APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.            
               END.   
            END CASE. 
         END.            
      END.   
   END.   
   IF CMB_KNAPP = "Flex in" OR CMB_KNAPP = "Annat in" THEN DO:       
      IF AVAILABLE flexregtemp THEN DO:        
         IF FILL-IN_TID < regstart THEN DO:                                                         
            MESSAGE "Före ordinarie arbetstid använd knappen In istället" VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.            
         END.                   
      END.   
   END.   
   IF CMB_KNAPP = "Flex ut" OR CMB_KNAPP = "Annat ut" THEN DO:       
      IF AVAILABLE flexregtemp THEN DO:                 
         IF FILL-IN_TID > regslut THEN DO:                                                         
            MESSAGE "Efter ordinarie arbetstid använd knappen Ut istället" VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.            
         END.                  
      END.   
   END.
   IF CMB_KNAPP = "UT" OR CMB_KNAPP = "LUNCH UT" THEN DO:       
      RELEASE utsokaonr.
   END.
   ELSE IF CMB_KNAPP = "FLEX UT" THEN DO:
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = "155" AND 
      utsokaonr.DELNR = 0 NO-LOCK NO-ERROR.  
      IF NOT AVAILABLE utsokaonr THEN DO:
         MESSAGE Guru.Konstanter:gaok FILL-IN_AONR STRING(FILL-IN_DELNR,Guru.Konstanter:varforetypchar[1]) "finns inte." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_AONR IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.
      ELSE DO:                          
         {AOKOLLERS.I}             
         IF utsokaonr.AONRAVDATUM = 01/01/1991 OR
         utsokaonr.AONRAVDATUM >= regdatum THEN FILL-IN_DELNR = FILL-IN_DELNR.
         ELSE DO:
            MESSAGE Guru.Konstanter:gaok FILL-IN_AONR STRING(FILL-IN_DELNR,Guru.Konstanter:varforetypchar[1]) "är redan avslutat." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN_AONR IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
         END.            
      END.
   END.
   ELSE DO:
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN_AONR AND 
      utsokaonr.DELNR = FILL-IN_DELNR NO-LOCK NO-ERROR.  
      IF NOT AVAILABLE utsokaonr THEN DO:
         MESSAGE Guru.Konstanter:gaok FILL-IN_AONR STRING(FILL-IN_DELNR,Guru.Konstanter:varforetypchar[1]) "finns inte." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_AONR IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.
      ELSE DO:                          
         {AOKOLLERS.I}             
         IF utsokaonr.AONRAVDATUM = 01/01/1991 OR
         utsokaonr.AONRAVDATUM >= regdatum THEN FILL-IN_DELNR = FILL-IN_DELNR.
         ELSE DO:
            MESSAGE Guru.Konstanter:gaok FILL-IN_AONR STRING(FILL-IN_DELNR,Guru.Konstanter:varforetypchar[1]) "är redan avslutat." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN_AONR IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
         END.
      END.
   END.
   IF AVAILABLE utsokaonr THEN DO:
      ASSIGN sok1 = utsokaonr.AONR
      sok2 = utsokaonr.DELNR.
   END.
   ELSE ASSIGN sok1 = "" sok2 = 0.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN ANDFLKOLL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 1,INPUT personaltemp.PERSONALKOD,INPUT regdatum,INPUT CMB_KNAPP,
      INPUT sok1,INPUT sok2,INPUT FILL-IN_TID,INPUT FILL-IN_ORSAK,
      INPUT Guru.Konstanter:globanv,INPUT regstart,INPUT regslut,
      INPUT regdagnamn, INPUT regvnr ,OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN ANDFLKOLL.P  
      (INPUT 1,INPUT personaltemp.PERSONALKOD,INPUT regdatum,INPUT CMB_KNAPP,
      INPUT sok1,INPUT sok2,INPUT FILL-IN_TID,INPUT FILL-IN_ORSAK,
      INPUT Guru.Konstanter:globanv,INPUT regstart,INPUT regslut,
      INPUT regdagnamn, INPUT regvnr ,OUTPUT TABLE felmeddtemp).
   END.      
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO UPDATE val4 AS LOGICAL.
      CASE val4:             
         WHEN FALSE THEN DO:
            APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.            
         END.   
      END CASE. 
   END.            

   IF Guru.Konstanter:appcon THEN DO:                           
      RUN ANDFLEXA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT vart,INPUT personaltemp.PERSONALKOD,INPUT regdatum,INPUT CMB_KNAPP,
      INPUT sok1,INPUT sok2,INPUT FILL-IN_TID,INPUT FILL-IN_ORSAK,
      INPUT flexkvst,INPUT flexkvsl,INPUT Guru.Konstanter:globanv,INPUT regstart,INPUT regslut,
      INPUT regdagnamn, INPUT regvnr ,OUTPUT TABLE felmeddtemp,INPUT-OUTPUT TABLE extratemp).
   END.
   ELSE DO:
      RUN ANDFLEXA.P  
      (INPUT vart,INPUT personaltemp.PERSONALKOD,INPUT regdatum,INPUT CMB_KNAPP,
      INPUT sok1,INPUT sok2,INPUT FILL-IN_TID,INPUT FILL-IN_ORSAK,
      INPUT flexkvst,INPUT flexkvsl,INPUT Guru.Konstanter:globanv,INPUT regstart,INPUT regslut,
      INPUT regdagnamn, INPUT regvnr ,OUTPUT TABLE felmeddtemp,INPUT-OUTPUT TABLE extratemp).
   END.   
   status-mus2 = SESSION:SET-WAIT-STATE("").
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
      RETURN NO-APPLY.
   END.      
   {musarrow.i}
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON GO OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AVD DIALOG-1
ON VALUE-CHANGED OF CMB_AVD IN FRAME DIALOG-1
DO:
   CMB_AVD = INPUT CMB_AVD.   
   RUN nycolsortprep_UI (INPUT 2).
   RUN openbdynspec_UI IN brwproc[1].
   {CMB_AVDB2.I}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_KNAPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_KNAPP DIALOG-1
ON VALUE-CHANGED OF CMB_KNAPP IN FRAME DIALOG-1 /* Knapp */
DO:
  CMB_KNAPP = INPUT CMB_KNAPP.
  IF CMB_KNAPP = "FLEX UT"  OR CMB_KNAPP = "LUNCH UT" THEN DO:
     ASSIGN
     FILL-IN_AONR:HIDDEN = TRUE
     FILL-IN_DELNR:HIDDEN = TRUE
     BRW_AONR:HIDDEN = TRUE
     RAD_FAST:HIDDEN = TRUE   
     FILL-IN_AONRS:HIDDEN = TRUE 
     FILL-IN_ORTS:HIDDEN = TRUE.
  END.
  ELSE DO:
     ASSIGN
     FILL-IN_AONR:HIDDEN = FALSE
     FILL-IN_DELNR:HIDDEN = FALSE
     BRW_AONR:HIDDEN = FALSE
     RAD_FAST:HIDDEN = FALSE  
     FILL-IN_AONRS:HIDDEN = FALSE 
     FILL-IN_ORTS:HIDDEN = FALSE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR DIALOG-1
ON VALUE-CHANGED OF CMB_OMR IN FRAME DIALOG-1
DO:
   CMB_OMR = INPUT CMB_OMR.            
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
   USE-INDEX OMRNAMN NO-LOCK NO-ERROR.           
   IF CMB_OMR = Guru.Konstanter:gomrk + " : alla" THEN sparomrade = sparomrade.
   ELSE sparomrade = CMB_OMR.
   RUN nycolsortprep_UI (INPUT 1).
   RUN openbdynspec_UI IN brwproc[1].  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONR DIALOG-1
ON ENTRY OF FILL-IN_AONR IN FRAME DIALOG-1 /* Aonr */
DO:
   RUN entryaonr_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONR DIALOG-1
ON LEAVE OF FILL-IN_AONR IN FRAME DIALOG-1 /* Aonr */
DO:
  IF FILL-IN_AONR NE INPUT FILL-IN_AONR THEN DO:
      FILL-IN_AONR = INPUT FILL-IN_AONR.
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN_AONR AND 
      utsokaonr.DELNR = FILL-IN_DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
      IF AVAILABLE utsokaonr THEN DO:                  
         DISPLAY FILL-IN_AONR FILL-IN_DELNR WITH FRAME {&FRAME-NAME}.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_AONRS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONRS DIALOG-1
ON ANY-KEY OF FILL-IN_AONRS IN FRAME DIALOG-1 /* Aonr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_AONRS IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONRS DIALOG-1
ON ENTRY OF FILL-IN_AONRS IN FRAME DIALOG-1 /* Aonr */
DO:
   SESSION:DATA-ENTRY-RETURN = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONRS DIALOG-1
ON LEAVE OF FILL-IN_AONRS IN FRAME DIALOG-1 /* Aonr */
DO:
   FILL-IN_AONRS = INPUT FILL-IN_AONRS.
   SESSION:DATA-ENTRY-RETURN = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONRS DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_AONRS IN FRAME DIALOG-1 /* Aonr */
DO:
   FILL-IN_AONRS = INPUT FILL-IN_AONRS.
   IF FILL-IN_AONRS = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_AONRS IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "AONR", INPUT FILL-IN_AONRS).
   RUN fillinupdate_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_ORTS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORTS DIALOG-1
ON ANY-KEY OF FILL-IN_ORTS IN FRAME DIALOG-1 /* Benämning */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_ORTS IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORTS DIALOG-1
ON ENTRY OF FILL-IN_ORTS IN FRAME DIALOG-1 /* Benämning */
DO:
   SESSION:DATA-ENTRY-RETURN = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORTS DIALOG-1
ON LEAVE OF FILL-IN_ORTS IN FRAME DIALOG-1 /* Benämning */
DO:
   FILL-IN_ORTS = INPUT FILL-IN_ORTS.
   SESSION:DATA-ENTRY-RETURN = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORTS DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_ORTS IN FRAME DIALOG-1 /* Benämning */
DO:
   FILL-IN_ORTS = INPUT FILL-IN_ORTS.
   IF FILL-IN_ORTS = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_ORTS IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "ORT", INPUT FILL-IN_ORTS).
   RUN fillinupdate_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_TID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_TID DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN_TID IN FRAME DIALOG-1 /* Tid */
DO:
   klocka = INPUT FILL-IN_TID.
   {AVBGOMD.I}
   RUN KLOCKAN.W.
   {AVBFRAMD.I}
   
   IF klocka = 00.00 THEN klocka = 24.00.
   FILL-IN_TID = klocka.
   DISPLAY FILL-IN_TID WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_FAST DIALOG-1
ON VALUE-CHANGED OF RAD_FAST IN FRAME DIALOG-1
DO:
   RAD_FAST = INPUT RAD_FAST.   
   IF RAD_FAST = FALSE THEN DO:
      CMB_OMR = sparomrade.
      CMB_OMR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = sparomrade.
      FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
      USE-INDEX OMRNAMN NO-LOCK NO-ERROR.
   END.  
   IF Guru.Konstanter:globforetag = "cELPA" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
      IF RAD_FAST = TRUE THEN DO:
         ASSIGN 
         sparomrade = CMB_OMR. 
         CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
         CMB_OMR = INPUT CMB_OMR.      
      END.
   END.
   RUN nycolsortprep_UI (INPUT 1).
   RUN openbdynspec_UI IN brwproc[1].
   IF AVAILABLE utsokaonr THEN DO:   
      status-ok = BRW_AONR:DESELECT-FOCUSED-ROW() NO-ERROR.
   END.
   DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}. 
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
   {ALLSTARTDYN.I}
   FIND FIRST extratemp NO-ERROR.
   pkod = extratemp.PERSONALKOD.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN ANVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 3,INPUT pkod,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   ELSE DO:
      RUN ANVSKAP.P 
      (INPUT 3,INPUT pkod,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN FLEXTAB.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 4,INPUT "",INPUT-OUTPUT TABLE ansttemp,INPUT-OUTPUT TABLE flexregtemp,
      INPUT-OUTPUT TABLE flexavttemp,INPUT-OUTPUT TABLE flexsaldotemp,INPUT-OUTPUT TABLE utryckningtemp).         END.
   ELSE DO:
      RUN FLEXTAB.P 
      (INPUT 4,INPUT "",INPUT-OUTPUT TABLE ansttemp,INPUT-OUTPUT TABLE flexregtemp,
      INPUT-OUTPUT TABLE flexavttemp,INPUT-OUTPUT TABLE flexsaldotemp,INPUT-OUTPUT TABLE utryckningtemp).      
   END.
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.       
   FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
   NO-LOCK NO-ERROR.   
   FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
   FIND FIRST anvandartemp WHERE anvandartemp.PERSONALKOD = personaltemp.PERSONALKOD 
   NO-LOCK NO-ERROR.
   CMB_AVD:DELIMITER = "$".   
   status-ok = CMB_AVD:ADD-LAST(Guru.Konstanter:gavdk + " : alla").   
   IF AVAILABLE anvandartemp THEN Guru.Konstanter:globallao = anvandartemp.ALLAONR. 
   flexomrade = personaltemp.OMRADE.
   status-ok = CMB_OMR:ADD-LAST(Guru.Konstanter:gomrk + " : alla").   
   {ANVAVDSO.I}
   
   FOR EACH eavdtemp,         
   EACH avdelningtemp WHERE avdelningtemp.AVDELNINGNR = eavdtemp.AVDELNINGNR.
      status-ok = CMB_AVD:ADD-LAST(avdelningtemp.AVDELNINGNAMN).
   END.      
   
   CMB_AVD:SCREEN-VALUE= Guru.Konstanter:gavdk + " : alla".
   FIND FIRST omrtemp WHERE omrtemp.OMRADE = flexomrade 
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE omrtemp THEN DO:
      FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
   END.
   ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN NO-ERROR.
   IF CMB_OMR:SCREEN-VALUE = ? THEN DO:
      CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
   END.
   IF flexomrade = "" OR Guru.Konstanter:globallao = TRUE THEN DO:
      ASSIGN CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
      CMB_OMR = INPUT CMB_OMR.
      DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
   END.    
   ASSIGN
   BRW_AONR:TITLE = "Aktiva " + LC(Guru.Konstanter:gaol)          
   FILL-IN_AONRS:LABEL = Guru.Konstanter:gaok  
   FILL-IN_AONR:LABEL = Guru.Konstanter:gaok
   FILL-IN-TEXT = "Visa " + LC(Guru.Konstanter:gaok) + " för:".
   {TILLFAST.I}
   FIND FIRST utsokaonr WHERE utsokaonr.FASTAAONR = TRUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE utsokaonr THEN DO:
      status-ok = RAD_FAST:DELETE(Guru.Konstanter:gfastl + " " + LC(Guru.Konstanter:gaok)).
   END.
   regdatum = extratemp.DATUM.  
   &Scoped-define FORMATNAMN FILL-IN_AONRS   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN_AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN_DELNR   
   {DELNRFORMAT.I}
   RUN grundtid_UI. 
   RUN enable_UI.       
   {FRMSIZED.I}
   IF VART = "AND" THEN DISABLE CMB_KNAPP WITH FRAME {&FRAME-NAME}.  
   IF Guru.Konstanter:globforetag = "elpa" THEN vart = vart.
   ELSE IF vart = "nya" then do:
      status-ok = CMB_KNAPP:DELETE("ÖVERTID IN") IN FRAME {&FRAME-NAME}.
      status-ok = CMB_KNAPP:DELETE("ÖVERTID UT") IN FRAME {&FRAME-NAME}.
   END.       
   IF Guru.Konstanter:globforetag = "SUFL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
      status-ok = CMB_KNAPP:DELETE("ÖVERTID IN") IN FRAME {&FRAME-NAME}.
      status-ok = CMB_KNAPP:DELETE("ÖVERTID UT") IN FRAME {&FRAME-NAME}.
      status-ok = CMB_KNAPP:DELETE("Lunch in") IN FRAME {&FRAME-NAME}.
      status-ok = CMB_KNAPP:DELETE("Lunch ut") IN FRAME {&FRAME-NAME}.
   END.
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:
      status-ok = CMB_KNAPP:DELETE("Lunch in") IN FRAME {&FRAME-NAME}.
      status-ok = CMB_KNAPP:DELETE("Lunch ut") IN FRAME {&FRAME-NAME}.
   END.
   FILL-IN_AONR:LABEL = Guru.Konstanter:gaok.
   CMB_AVD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   
   RUN entryaonr_UI.
   {musarrow.i}
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
   utsokaonr.OMRADE:READ-ONLY IN BROWSE BRW_AONR = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_AONR:HANDLE IN FRAME {&FRAME-NAME}).       
   RUN sattindex_UI IN brwproc[1] (INPUT "OMRADE").
   IF Guru.Konstanter:appcon THEN DO:      
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph.
   END.
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).
   RUN dynprogextra IN brwproc[1]  (INPUT "omrvisa_UI",INPUT THIS-PROCEDURE).   
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
  DISPLAY FILL-IN_TID CMB_KNAPP FILL-IN_AONR FILL-IN_DELNR FILL-IN_ORSAK 
          FILL-IN-TEXT CMB_AVD CMB_OMR FILL-IN_PERSONALKOD FILL-IN_DATUM 
          FILL-IN-SKP 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN_TID CMB_KNAPP FILL-IN_AONR FILL-IN_DELNR FILL-IN_ORSAK BTN_REG 
         BTN_AVB CMB_AVD CMB_OMR RECT-22 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE entryaonr_UI DIALOG-1 
PROCEDURE entryaonr_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/ 
   ASSIGN
   musz = FALSE
   FILL-IN-SKP = "Sök på:"
   CMB_OMR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
   CMB_AVD:HIDDEN = FALSE
   FILL-IN-TEXT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN_AONR AND utsokaonr.DELNR = FILL-IN_DELNR AND
   utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
   IF AVAILABLE utsokaonr THEN DO:
      IF Guru.Konstanter:globallao = FALSE AND utsokaonr.FASTAAONR = TRUE AND utsokaonr.OMRADE = " " THEN DO:
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = flexomrade 
         USE-INDEX OMR NO-LOCK NO-ERROR.           
         aonrrec = RECID(utsokaonr).
         RAD_FAST = utsokaonr.FASTAAONR.
         ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN NO-ERROR.                        
         IF CMB_OMR:SCREEN-VALUE = ? THEN DO:
            CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
         END.
      END.
      ELSE IF Guru.Konstanter:globallao = FALSE AND utsokaonr.FASTAAONR = FALSE AND utsokaonr.OMRADE = " " THEN DO:
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = flexomrade 
         USE-INDEX OMR NO-LOCK NO-ERROR.           
         aonrrec = RECID(utsokaonr).
         RAD_FAST = utsokaonr.FASTAAONR.
         ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN NO-ERROR.                        
         IF CMB_OMR:SCREEN-VALUE = ? THEN DO:
            CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
         END.
      END.
      ELSE DO:      
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = utsokaonr.OMRADE 
         USE-INDEX OMR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE omrtemp THEN DO:
            IF flexomrade = "" OR Guru.Konstanter:globallao = TRUE THEN DO:
                sparomrade = Guru.Konstanter:gomrk + " : alla".
            END.
            ELSE DO:
               IF sparomrade = "" THEN DO:
                  FIND FIRST omrtemp WHERE omrtemp.OMRADE = flexomrade 
                  USE-INDEX OMR NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE omrtemp THEN DO:
                     FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
                  END.
                  sparomrade = omrtemp.NAMN.
               END.
            END.
            ASSIGN 
            CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
            CMB_OMR = INPUT CMB_OMR.
         END.
         ELSE DO:
            IF sparomrade = "" THEN DO:
               FIND FIRST omrtemp WHERE omrtemp.OMRADE = flexomrade 
               USE-INDEX OMR NO-LOCK NO-ERROR.
               IF NOT AVAILABLE omrtemp THEN DO:
                  FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
               END.
               sparomrade = omrtemp.NAMN.
            END.
            ELSE sparomrade = omrtemp.NAMN.
            FIND FIRST omrtemp WHERE omrtemp.OMRADE = utsokaonr.OMRADE 
            USE-INDEX OMR NO-LOCK NO-ERROR.
            ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN NO-ERROR.
            IF CMB_OMR:SCREEN-VALUE = ? THEN DO:
               CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
            END.
         END.
      END.
      aonrrec = RECID(utsokaonr).
      RAD_FAST = utsokaonr.FASTAAONR.
   END.
   ELSE DO:
      IF flexomrade = "" OR Guru.Konstanter:globallao = TRUE THEN DO:
         sparomrade = Guru.Konstanter:gomrk + " : alla".
         ASSIGN CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
         CMB_OMR = INPUT CMB_OMR.
         DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = flexomrade 
         USE-INDEX OMR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE omrtemp THEN DO:
            FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
         END.
         sparomrade = omrtemp.NAMN.
         CMB_OMR:SCREEN-VALUE = omrtemp.NAMN NO-ERROR.
         IF CMB_OMR:SCREEN-VALUE = ? THEN DO:
            CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
         END.
         CMB_OMR = INPUT CMB_OMR.
         DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
      END.
      aonrrec = 0.
      RAD_FAST = FALSE.
   END.
   DISPLAY RAD_FAST FILL-IN-SKP WITH FRAME {&FRAME-NAME}.
   ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.
   ENABLE FILL-IN_AONRS FILL-IN_ORTS RAD_FAST WITH FRAME {&FRAME-NAME}.
   ASSIGN
   BRW_AONR:HIDDEN = FALSE    
   RAD_FAST:HIDDEN = FALSE   
   FILL-IN_AONRS:HIDDEN = FALSE 
   FILL-IN_ORTS:HIDDEN = FALSE.
   RUN nycolsortprep_UI (INPUT 1).
   RUN openbdynspec_UI IN brwproc[1].
   IF FILL-IN_AONR NE "" THEN DO:
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN_AONR AND utsokaonr.DELNR = FILL-IN_DELNR AND
      utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
      IF AVAILABLE utsokaonr THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(utsokaonr)).
         RUN lastselectdyn_UI IN brwproc[1].
      END.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillinupdate_UI DIALOG-1 
PROCEDURE fillinupdate_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/    
   IF AVAILABLE utsokaonr THEN DO:
      ASSIGN
      FILL-IN_AONR = utsokaonr.AONR
      FILL-IN_DELNR = utsokaonr.DELNR.
      DISPLAY FILL-IN_AONR FILL-IN_DELNR WITH FRAME {&FRAME-NAME}. 
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE grundtid_UI DIALOG-1 
PROCEDURE grundtid_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/
   FILL-IN_DATUM = bdatum. 
   IF extratemp.AUTO = "AUTO" THEN DO: 
      ASSIGN
      FILL-IN_AONR = extratemp.AONR
      FILL-IN_DELNR = extratemp.DELNR.
   END.
   IF vart = "NYA" THEN DO:
      ASSIGN
      bdatum = extratemp.DATUM
      FILL-IN_TID = 00.00
      CMB_KNAPP = "IN".
   END.   
   ELSE DO:
      ASSIGN
      bdatum = extratemp.DATUM
      FILL-IN_TID = extratemp.TID
      FILL-IN_AONR = extratemp.AONR
      FILL-IN_DELNR = extratemp.DELNR
      FILL-IN_ORSAK = extratemp.ORSAK.
      ASSIGN
      CMB_KNAPP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = extratemp.KNAPP. 
   END.  
   FILL-IN_PERSONALKOD = extratemp.PERSONALKOD.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nycolsortprep_UI DIALOG-1 
PROCEDURE nycolsortprep_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   {NYCOL.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE omrvisa_UI DIALOG-1 
PROCEDURE omrvisa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER TABLE FOR coltemp.
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.
   &Scoped-define FORMATNAMN utsokaonr.AONR
   &Scoped-define FORMATNAMNOMR utsokaonr.OMRADE
   &Scoped-define BROWSE-NAME BRW_AONR
   {OMRAOFORMAT.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
