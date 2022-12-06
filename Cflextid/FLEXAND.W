&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1






&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 04/08/97 -  8:27 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{TIDPERS.I}
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE SHARED VARIABLE perssokrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vaxla AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vartgamla AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE tidsedrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE tidsedlog AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO. 
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER NO-UNDO.    
DEFINE VARIABLE flrec AS RECID NO-UNDO.    
DEFINE VARIABLE flrec2 AS RECID NO-UNDO.
DEFINE VARIABLE sok1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sok3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE splus AS INTEGER NO-UNDO.  
DEFINE VARIABLE hjdatum AS DATE NO-UNDO.
DEFINE VARIABLE dagnr AS INTEGER NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-stopp AS LOGICAL NO-UNDO.
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.
DEFINE VARIABLE flexavikapph AS HANDLE NO-UNDO.
DEFINE VARIABLE lasanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE ptraff AS LOGICAL NO-UNDO.
DEFINE VARIABLE persapph AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE RAD_TIDSVAL AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Alla tidsedlar för perioden", 1,
"Godkända", 2,
"Ej godkända", 3
     SIZE 31.5 BY 2
     BGCOLOR 8  NO-UNDO.
DEFINE NEW SHARED VARIABLE TOGGLE-MONTH AS LOGICAL INITIAL ? 
     LABEL "Två tidsedlar vid månadasskifte":L 
     VIEW-AS TOGGLE-BOX
     SIZE 34.5 BY 1 NO-UNDO.
&Scoped-define NEW   
&Scoped-define SHARED

{ANVPERS.I}
{FLEXTAB.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_FLEX

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES flextemp

/* Definitions for BROWSE BRW_FLEX                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_FLEX flextemp.DATUM flextemp.TID ~
flextemp.KNAPP flextemp.AONR flextemp.DELNR flextemp.AUTO flextemp.ORSAK 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FLEX flextemp.DATUM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_FLEX flextemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_FLEX flextemp
&Scoped-define QUERY-STRING-BRW_FLEX FOR EACH flextemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_FLEX OPEN QUERY BRW_FLEX FOR EACH flextemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_FLEX flextemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FLEX flextemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_NDA FILL-IN_DATUM BTN_FDA FILL-IN-FELOK ~
FBTN_KONTROLL FBTN_VPERS FBTN_NAAVB BTN_NY BTN_UPP BTN_BORT BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-REGIS FILL-IN-PKOD ~
FILL-IN_FORNAMN-2 FILL-IN_EFTERNAMN-2 FILL-IN_DATUM FILL-IN_PLUS ~
FILL-IN_KONTROLL FILL-IN-FELMED FILL-IN-FELOK FILL-IN-FLST FILL-IN-FLSL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_FDA 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NDA 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_UPP 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_KONTROLL 
     LABEL "Kontroll" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_NAAVB 
     LABEL "Nästa person":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa tidsedel":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VPERS 
     LABEL "Välj person":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-FELMED AS CHARACTER FORMAT "X(256)":U 
     LABEL "Feltext" 
     VIEW-AS FILL-IN 
     SIZE 30.75 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FELOK AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Ok" 
     VIEW-AS FILL-IN 
     SIZE 7.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FLSL AS DECIMAL FORMAT "99.99" INITIAL 16 
     LABEL "Flexslut" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FLST AS DECIMAL FORMAT "99.99" INITIAL 7 
     LABEL "Flexstart" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-REGIS AS CHARACTER FORMAT "X(17)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_DATUM AS DATE FORMAT "99/99/99" 
     LABEL "Från datum" 
     VIEW-AS FILL-IN 
     SIZE 10.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_EFTERNAMN AS CHARACTER FORMAT "X(13)" 
     VIEW-AS FILL-IN 
     SIZE 13.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_EFTERNAMN-2 AS CHARACTER FORMAT "X(13)" 
     VIEW-AS FILL-IN 
     SIZE 13.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN AS CHARACTER FORMAT "X(14)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN-2 AS CHARACTER FORMAT "X(14)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_KONTROLL AS CHARACTER FORMAT "X(10)" 
     VIEW-AS FILL-IN 
     SIZE 13.75 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_NAMN AS CHARACTER FORMAT "X(16)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_PLUS AS DECIMAL FORMAT "-99.99" INITIAL 0 
     LABEL "Dagens flex" 
     VIEW-AS FILL-IN 
     SIZE 10.25 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_ALLVAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ansvarig tidredovisare", 1,
"Område", 2,
"Alla", 3,
"Enhet/Sign", 4,
"Markerade", 5
     SIZE 63.5 BY 1
     BGCOLOR 8  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_FLEX FOR 
      flextemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_FLEX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FLEX WINDOW-1 _STRUCTURED
  QUERY BRW_FLEX NO-LOCK DISPLAY
      flextemp.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      flextemp.TID COLUMN-LABEL "Tid" FORMAT "99.99":U
      flextemp.KNAPP COLUMN-LABEL "Knapp" FORMAT "X(12)":U
      flextemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      flextemp.DELNR COLUMN-LABEL "Delnr" FORMAT "999":U
      flextemp.AUTO COLUMN-LABEL "Auto" FORMAT "X(4)":U
      flextemp.ORSAK COLUMN-LABEL "Orsak" FORMAT "X(20)":U
  ENABLE
      flextemp.DATUM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 67.88 BY 8.88.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     FILL-IN_FORNAMN AT ROW 2.63 COL 19 COLON-ALIGNED NO-LABEL
     FILL-IN_NAMN AT ROW 2.63 COL 25 COLON-ALIGNED HELP
          "ANGE ORAGNISTIONENSBENÄMNING" NO-LABEL
     FILL-IN_EFTERNAMN AT ROW 2.63 COL 46.25 COLON-ALIGNED NO-LABEL
     FILL-IN-REGIS AT ROW 2.67 COL 5.25 COLON-ALIGNED NO-LABEL
     FILL-IN-PKOD AT ROW 4.04 COL 12 COLON-ALIGNED
     FILL-IN_FORNAMN-2 AT ROW 4.04 COL 24.63 COLON-ALIGNED NO-LABEL
     FILL-IN_EFTERNAMN-2 AT ROW 4.04 COL 46.13 COLON-ALIGNED NO-LABEL
     BTN_NDA AT ROW 5.29 COL 25.25
     FILL-IN_DATUM AT ROW 5.54 COL 12 COLON-ALIGNED
     FILL-IN_PLUS AT ROW 5.54 COL 41.75 COLON-ALIGNED
     FILL-IN_KONTROLL AT ROW 5.54 COL 53.88 COLON-ALIGNED NO-LABEL
     BTN_FDA AT ROW 6.17 COL 25.25
     FILL-IN-FELMED AT ROW 7.04 COL 12 COLON-ALIGNED
     FILL-IN-FELOK AT ROW 7.04 COL 49 COLON-ALIGNED
     FBTN_KONTROLL AT ROW 8 COL 70.5
     FILL-IN-FLST AT ROW 8.54 COL 12 COLON-ALIGNED
     FILL-IN-FLSL AT ROW 8.54 COL 33.88 COLON-ALIGNED
     FBTN_VPERS AT ROW 9.08 COL 70.5
     BRW_FLEX AT ROW 9.88 COL 1.5
     FBTN_NAAVB AT ROW 10.21 COL 70.5
     FBTN_VISA AT ROW 11.29 COL 70.5
     RAD_ALLVAL AT ROW 14.75 COL 2.5 NO-LABEL
     BTN_NY AT ROW 18.96 COL 14.88
     BTN_UPP AT ROW 18.96 COL 29.88
     BTN_BORT AT ROW 18.96 COL 44.88
     BTN_AVB AT ROW 18.96 COL 71
     "Ändra eller registrera flextid för:" VIEW-AS TEXT
          SIZE 44 BY .96 AT ROW 1.25 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84.88 BY 19.25.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: flextemp T "?" NO-UNDO temp-db flextemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Ändra eller registrera flextid"
         HEIGHT             = 19.42
         WIDTH              = 85.13
         MAX-HEIGHT         = 24.79
         MAX-WIDTH          = 101.13
         VIRTUAL-HEIGHT     = 24.79
         VIRTUAL-WIDTH      = 101.13
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_FLEX FBTN_VPERS FRAME-A */
/* SETTINGS FOR BROWSE BRW_FLEX IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BRW_FLEX:HIDDEN  IN FRAME FRAME-A                = TRUE.

/* SETTINGS FOR BUTTON FBTN_VISA IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_VISA:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-FELMED IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-FLSL IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-FLST IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-PKOD IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-REGIS IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_EFTERNAMN IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_EFTERNAMN:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_EFTERNAMN-2 IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_FORNAMN:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN-2 IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_KONTROLL IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_NAMN IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_NAMN:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_PLUS IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RAD_ALLVAL IN FRAME FRAME-A
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_ALLVAL:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FLEX
/* Query rebuild information for BROWSE BRW_FLEX
     _TblList          = "Temp-Tables.flextemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.flextemp.DATUM
"flextemp.DATUM" "Datum" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.flextemp.TID
"flextemp.TID" "Tid" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.flextemp.KNAPP
"flextemp.KNAPP" "Knapp" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.flextemp.AONR
"flextemp.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.flextemp.DELNR
"flextemp.DELNR" "Delnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.flextemp.AUTO
"flextemp.AUTO" "Auto" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.flextemp.ORSAK
"flextemp.ORSAK" "Orsak" "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FLEX */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO: 
   ASSIGN
   vartgamla = ""
   flrec = ?
   flrec2 = ?.    
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN TIDSTOPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 2,INPUT pkod,INPUT Guru.Konstanter:globanv, OUTPUT lasanv, INPUT-OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN TIDSTOPP.P 
      (INPUT 2,INPUT pkod,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
   END.
   APPLY "CLOSE" TO THIS-PROCEDURE.
   RETURN.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON ENDKEY OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta bort */
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   ASSIGN
   flrec = flextemp.FREC.
   flrec2 = flrec.
   
   sok4 = STRING(flextemp.DATUM).
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
   IF sok4 = "" OR flextemp.DATUM > DATE(sok4) THEN DO:   
      EMPTY TEMP-TABLE extratemp NO-ERROR.    
      CREATE extratemp.
      ASSIGN
      extratemp.PERSONALKOD = flextemp.PERSONALKOD 
      extratemp.DATUM       = flextemp.DATUM       
      extratemp.TID         = flextemp.TID         
      extratemp.KNAPP       =  flextemp.KNAPP.
      
      RUN BORTFLEX.W
          (INPUT TABLE extratemp,INPUT-OUTPUT TABLE flexdagtemp,
                      INPUT-OUTPUT TABLE flextemp).
      
      IF musz = FALSE THEN DO:      
         EMPTY TEMP-TABLE extratemp NO-ERROR.       
         EMPTY TEMP-TABLE eflextemp NO-ERROR. 
         FIND flextemp WHERE flextemp.FREC = flrec NO-ERROR. 
   
         CREATE eflextemp.
         BUFFER-COPY flextemp TO eflextemp.
         RUN bortflex_UI IN flexavikapph (INPUT TABLE eflextemp).    
         FIND flextemp WHERE flextemp.FREC = flrec NO-ERROR.      
         FIND flextemp WHERE flextemp.FREC = flrec NO-ERROR.      
         DELETE flextemp.
         RUN selnextprevrow_UI IN brwproc[1].
         FIND FIRST flextemp  NO-ERROR.      
         IF AVAILABLE flextemp THEN DO:               
            IF flextemp.KORD > 01/01/97 THEN DO:
               DISABLE BTN_BORT BTN_UPP FBTN_KONTROLL FILL-IN-FELOK WITH FRAME {&FRAME-NAME}.
            END.
            ELSE IF FILL-IN_DATUM LE flexregtemp.SALDOKORD  THEN DO:     
               DISABLE BTN_NY BTN_UPP BTN_BORT FILL-IN-FELOK FBTN_KONTROLL WITH FRAME {&FRAME-NAME}.
            END.
         END.
         ELSE DO:
            BRW_FLEX:HIDDEN = TRUE.
            DISABLE BTN_BORT BTN_UPP FBTN_KONTROLL FILL-IN-FELOK WITH FRAME {&FRAME-NAME}. 
         END.
         RUN openbdynspec_UI IN brwproc[1].
         RUN lastselectdyn_UI IN brwproc[1].
         
      END.
      ELSE DO:
         FIND flextemp WHERE flextemp.FREC = flrec NO-ERROR. 
         
         IF AVAILABLE flextemp THEN RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(flextemp)).
         RUN openbdynspec_UI IN brwproc[1].
         RUN lastselectdyn_UI IN brwproc[1].
         IF status-stopp = FALSE THEN ENABLE BTN_BORT BTN_UPP FBTN_KONTROLL FILL-IN-FELOK WITH FRAME {&FRAME-NAME}.
      END.
   END.
   ELSE IF regdatum LE DATE(sok4) THEN DO:
      MESSAGE "Tidsedeln är godkänd/färdigrapporterad tom"  DATE(sok4) VIEW-AS ALERT-BOX.       
   END.
   RUN saldoplus_UI.
   DISPLAY FILL-IN_KONTROLL FILL-IN_PLUS FILL-IN-FELMED FILL-IN-FLST FILL-IN-FLSL FILL-IN-FELOK
   WITH FRAME {&FRAME-NAME}.
   {musarrow.i}      
   ASSIGN
   bdatum = FILL-IN_DATUM.
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FDA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FDA WINDOW-1
ON CHOOSE OF BTN_FDA IN FRAME FRAME-A /* - */
DO: 
   ASSIGN
   regdatumspar = FILL-IN_DATUM
   FILL-IN_DATUM = INPUT FILL-IN_DATUM.   
   FILL-IN_DATUM = FILL-IN_DATUM - 1.    
   RUN bytdat_UI (INPUT 1).             
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NDA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NDA WINDOW-1
ON CHOOSE OF BTN_NDA IN FRAME FRAME-A /* + */
DO:   
   ASSIGN
   regdatumspar = FILL-IN_DATUM
   FILL-IN_DATUM = INPUT FILL-IN_DATUM.   
   FILL-IN_DATUM = FILL-IN_DATUM + 1.       
   RUN bytdat_UI (INPUT 2).                   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:   
   RUN nya_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON ENTRY OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:
   musz = musz.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP WINDOW-1
ON CHOOSE OF BTN_UPP IN FRAME FRAME-A /* Ändra */
DO:   
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   {muswait.i}
   ASSIGN
   flrec = RECID(flextemp)
   flrec2 = flrec   
   vart = "AND"
   bdatum = FILL-IN_DATUM.
   IF flextemp.KNAPP = "Övertid in" OR flextemp.KNAPP = "Övertid ut" THEN DO:
      MESSAGE "Det går inte att ändra på knapparna övertid in och övertid ut" VIEW-AS ALERT-BOX.          
   END.
   ELSE DO:   
      EMPTY TEMP-TABLE extratemp NO-ERROR.    
      CREATE extratemp.
      BUFFER-COPY flextemp TO extratemp.                  
      RUN ANDFLEX.W (INPUT-OUTPUT TABLE extratemp).
      
      
      FIND FIRST extratemp NO-ERROR.
      RUN efter_UI.
      {musarrow.i}      
      ASSIGN
      bdatum = FILL-IN_DATUM.
      musz = FALSE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_KONTROLL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_KONTROLL WINDOW-1
ON CHOOSE OF FBTN_KONTROLL IN FRAME FRAME-A /* Kontroll */
DO:
   regdatum = FILL-IN_DATUM. 
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   {muswait.i}
   ASSIGN         
   flrec = RECID(flextemp).
   IF Guru.Konstanter:appcon THEN DO: 
      RUN FDAGKOLLA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT personaltemp.PERSONALKOD,INPUT regdatum).
   END.
   ELSE DO:
      RUN FDAGKOLLA.P 
      (INPUT personaltemp.PERSONALKOD,INPUT regdatum).
   END.
   RUN nydag_UI IN flexavikapph (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT TABLE flexdagtemp).
   RUN nyflextemp_UI IN flexavikapph (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT TABLE flextemp).    
   regdatum = FILL-IN_DATUM.
   RUN saldoplus_UI.
   DISPLAY FILL-IN_KONTROLL FILL-IN_PLUS FILL-IN-FELMED FILL-IN-FLST FILL-IN-FLSL FILL-IN-FELOK
   WITH FRAME {&FRAME-NAME}.
   OPEN QUERY {&BROWSE-NAME} FOR EACH flextemp USE-INDEX FLEX NO-LOCK.
   FIND flextemp WHERE flextemp.FREC = flrec NO-ERROR.      
   RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(flextemp)).
   RUN openbdynspec_UI IN brwproc[1].
  {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_NAAVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_NAAVB WINDOW-1
ON CHOOSE OF FBTN_NAAVB IN FRAME FRAME-A /* Nästa person */
DO:  
   IF NOT AVAILABLE tidpers THEN APPLY "WINDOW-CLOSE":U TO WINDOW-1. 
   ELSE FIND NEXT tidpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.    
   {muswait.i}    
   IF NOT AVAILABLE tidpers THEN DO:    
      status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
      APPLY "WINDOW-CLOSE":U TO WINDOW-1. 
   END.   
   ELSE DO: 
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TIDSTOPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 2,INPUT pkod,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
      END.
      ELSE DO:
         RUN TIDSTOPP.P 
         (INPUT 2,INPUT pkod,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
      END.
      status-stopp = FALSE.
      pkod = tidpers.PERSONALKOD.
      RUN starthelp_UI.
      IF musz = TRUE THEN DO:
         musz = FALSE.
         APPLY "CHOOSE" TO FBTN_NAAVB.
      END.
   END.
   DISPLAY FILL-IN_EFTERNAMN-2 FILL-IN_FORNAMN-2 FILL-IN-PKOD 
   WITH FRAME {&FRAME-NAME}.  
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_NAAVB WINDOW-1
ON GO OF FBTN_NAAVB IN FRAME FRAME-A /* Nästa person */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-1
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-A /* Visa tidsedel */
DO:
   hjdatum = regdatum.
   ASSIGN    
   vaxla = TRUE.
   RAD_TIDSVAL = 1.          
   {muswait.i}
   ASSIGN   
   regdagnamn = 'MÅN'.
   RUN VECODAT.P.         
   ASSIGN
   bdatum = regdatum    
   regdagnamn = 'SÖN'.
   RUN VECODAT.P.          
   ASSIGN
   avdatum = regdatum
   regdatum = bdatum.   
   {AVBGOM.I}
   {AMERICANEUROPEAN.I}
   RUN TIDSEDL.W.
   {EUROPEANAMERICAN.I}
   {AVBFRAM.I}
   vaxla = FALSE.     
   regdatum = hjdatum.
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-1
ON GO OF FBTN_VISA IN FRAME FRAME-A /* Visa tidsedel */
DO:
   musz = musz.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VPERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VPERS WINDOW-1
ON CHOOSE OF FBTN_VPERS IN FRAME FRAME-A /* Välj person */
DO:
   {AVBGOMD.I}
   RUN VPERVAL.W.
   {AVBFRAMD.I}     
   {muswait.i}         
   IF musz = TRUE THEN musz = FALSE.
   ELSE DO:
      FIND tidpers WHERE RECID(tidpers) = perssokrec NO-LOCK NO-ERROR.                             
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TIDSTOPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 2,INPUT pkod,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
      END.
      ELSE DO:
         RUN TIDSTOPP.P 
         (INPUT 2,INPUT pkod,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
      END.
      status-stopp = FALSE.
      pkod = tidpers.PERSONALKOD.
      RUN starthelp_UI.
      IF musz = TRUE THEN DO:
         musz = FALSE.
         APPLY "CHOOSE" TO FBTN_NAAVB.
      END.
   END.
   DISPLAY FILL-IN_EFTERNAMN-2 FILL-IN_FORNAMN-2 FILL-IN-PKOD 
   WITH FRAME {&FRAME-NAME}.  
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FELOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FELOK WINDOW-1
ON LEAVE OF FILL-IN-FELOK IN FRAME FRAME-A /* Ok */
DO:
   IF NOT AVAILABLE flexdagtemp THEN DO:
      FIND FIRST flexdagtemp WHERE flexdagtemp.PERSONALKOD = personaltemp.PERSONALKOD AND
      flexdagtemp.DATUM = regdatum USE-INDEX FLEX NO-LOCK NO-ERROR.   
   END.
   IF flexdagtemp.KONTROLL = "Ejkontroll" THEN DO:
      MESSAGE "Det går inte att sätta Ok på en ej kontrollerad registrering" VIEW-AS ALERT-BOX.
   END.
   ELSE DO:
      FILL-IN-FELOK = INPUT FILL-IN-FELOK.
      ASSIGN flexdagtemp.FELOK = FILL-IN-FELOK.
      EMPTY TEMP-TABLE eflexdagtemp NO-ERROR.       
      CREATE eflexdagtemp.
      BUFFER-COPY flexdagtemp TO eflexdagtemp.
      RUN flexdagok_IU IN flexavikapph (INPUT TABLE eflexdagtemp).
      EMPTY TEMP-TABLE eflexdagtemp NO-ERROR.       
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FELOK WINDOW-1
ON MOUSE-SELECT-CLICK OF FILL-IN-FELOK IN FRAME FRAME-A /* Ok */
DO:
   IF NOT AVAILABLE flexdagtemp THEN DO:
      FIND FIRST flexdagtemp WHERE flexdagtemp.PERSONALKOD = personaltemp.PERSONALKOD AND
      flexdagtemp.DATUM = regdatum USE-INDEX FLEX NO-LOCK NO-ERROR.   
   END.
   IF flexdagtemp.KONTROLL = "Ejkontroll" THEN DO:
      MESSAGE "Det går inte att sätta Ok på en ej kontrollerad registrering" VIEW-AS ALERT-BOX.
   END.
   ELSE DO:
      IF INPUT FILL-IN-FELOK = TRUE THEN FILL-IN-FELOK = FALSE.
      IF INPUT FILL-IN-FELOK = FALSE THEN FILL-IN-FELOK = TRUE.
      DISPLAY FILL-IN-FELOK WITH FRAME {&FRAME-NAME}. 
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_DATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DATUM WINDOW-1
ON LEAVE OF FILL-IN_DATUM IN FRAME FRAME-A /* Från datum */
DO:
   ASSIGN
   regdatumspar = regdatum
   FILL-IN_DATUM = INPUT FILL-IN_DATUM
   regdatum = FILL-IN_DATUM. 
   RUN bytdat_UI (INPUT 3).    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DATUM WINDOW-1
ON MOUSE-MENU-CLICK OF FILL-IN_DATUM IN FRAME FRAME-A /* Från datum */
DO:
   ASSIGN
   FILL-IN_DATUM = INPUT FILL-IN_DATUM
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN_DATUM.
   RUN AlmanBtn.w.
   FILL-IN_DATUM = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN_DATUM WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_FLEX
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN TIDSTOPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 2,INPUT pkod,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN TIDSTOPP.P 
      (INPUT 2,INPUT pkod,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
   END.
   {BORTBRWPROC.I}
   IF VALID-HANDLE(flexavikapph) THEN DELETE PROCEDURE flexavikapph NO-ERROR.
   IF VALID-HANDLE(persapph) THEN DELETE PROCEDURE persapph NO-ERROR.
   flexavikapph = ?.
   persapph = ?.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {WIN_M_START.I}
   {muswait.i}   
   {ALLSTARTDYN.I} 
   ASSIGN
   flrec = 0
   FILL-IN_DATUM = regdatum
   bdatum = FILL-IN_DATUM.
   FIND FIRST tidpers WHERE tidpers.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tidpers THEN DO:  
      IF vartgamla = "XXX" THEN DO:
         FIND tidpers WHERE tidpers.TIDPERSREC = persrec NO-LOCK NO-ERROR. 
      END.
      ELSE IF tidsedlog = FALSE THEN FIND FIRST tidpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.      
      ELSE FIND tidpers WHERE RECID(tidpers) = tidsedrec NO-LOCK NO-ERROR. 
   END.
   persrec = tidpers.TIDPERSREC.   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN FLEXTAB.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 4,INPUT "",INPUT-OUTPUT TABLE ansttemp,INPUT-OUTPUT TABLE flexregtemp,
      INPUT-OUTPUT TABLE flexavttemp,INPUT-OUTPUT TABLE flexsaldotemp,INPUT-OUTPUT TABLE utryckningtemp).         
   END.
   ELSE DO:
      RUN FLEXTAB.P 
      (INPUT 4,INPUT "",INPUT-OUTPUT TABLE ansttemp,INPUT-OUTPUT TABLE flexregtemp,
      INPUT-OUTPUT TABLE flexavttemp,INPUT-OUTPUT TABLE flexsaldotemp,INPUT-OUTPUT TABLE utryckningtemp).      
   END.
   RUN enable_UI.   
   {FRMSIZE.I}  
   RUN starthelp_UI.
   DISPLAY FILL-IN_EFTERNAMN-2 FILL-IN_FORNAMN-2 FILL-IN-PKOD 
   WITH FRAME {&FRAME-NAME}.  
   RUN goma_UI.
   IF tidsedlog = TRUE THEN DO:
      FBTN_NAAVB:HIDDEN = TRUE. 
      tidsedlog = FALSE.
   END.  
   IF vartgamla = "XXX" THEN DO:
      FBTN_NAAVB:HIDDEN = TRUE. 
      FBTN_VPERS:HIDDEN = TRUE. 
   END.         
   IF musz = TRUE THEN DO:
      musz = FALSE.
      APPLY "CHOOSE" TO FBTN_NAAVB.
   END.
   IF vaxla = TRUE THEN FBTN_VISA:HIDDEN = TRUE.        
   {musarrow.i}  
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/ 
   flextemp.DATUM:READ-ONLY IN BROWSE BRW_FLEX = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_FLEX:HANDLE IN FRAME {&FRAME-NAME}). 
   IF Guru.Konstanter:appcon THEN DO:
      RUN FLEXAVIKAPP.P PERSISTENT SET flexavikapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      RUN PHMTAPP.P PERSISTENT SET persapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.       
   END.
   ELSE DO:
      RUN FLEXAVIKAPP.P PERSISTENT SET flexavikapph.
      RUN PHMTAPP.P PERSISTENT SET persapph.
   END.     
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bytdat_UI WINDOW-1 
PROCEDURE bytdat_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER plusminus AS INTEGER NO-UNDO.
   REPEAT:
      ASSIGN
      sok1 = ansttemp.KOD
      sok4 = STRING(FILL-IN_DATUM).
      IF Guru.Konstanter:appcon THEN DO: 
         RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 8,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.
      ELSE DO:
         RUN FLEXTIDH.P 
         (INPUT 8,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.      
      IF sok2 = 0 THEN dagnr = WEEKDAY(FILL-IN_DATUM).
      ELSE dagnr = sok2.
      IF Guru.Konstanter:globforetag = "elpa" THEN LEAVE.
      ELSE IF dagnr = 1 OR dagnr = 7 THEN DO:
         IF plusminus = 1 THEN FILL-IN_DATUM = FILL-IN_DATUM - 1. 
         IF plusminus = 2 THEN FILL-IN_DATUM = FILL-IN_DATUM + 1.
         IF plusminus = 3 THEN DO:
            MESSAGE "Du kan inte registrera flex helgdagar."
            VIEW-AS ALERT-BOX.
            FILL-IN_DATUM = regdatumspar.
            DISPLAY FILL-IN_DATUM WITH FRAME {&FRAME-NAME}.   
            RETURN.
         END.         
      END.
      ELSE LEAVE.               
   END.   
   DISPLAY FILL-IN_DATUM WITH FRAME {&FRAME-NAME}.  
   ASSIGN  
   regdatum = FILL-IN_DATUM.    
   RUN nydag_UI IN flexavikapph (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT TABLE flexdagtemp).   
   RUN saldoplus_UI.
   RUN nyflextemp_UI IN flexavikapph (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT TABLE flextemp).    
   OPEN QUERY {&BROWSE-NAME} FOR EACH flextemp USE-INDEX FLEX NO-LOCK.
   GET FIRST {&BROWSE-NAME}.   
   RUN goma_UI.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
  THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE efter_UI WINDOW-1 
PROCEDURE efter_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN nyflextemp_UI IN flexavikapph (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT TABLE flextemp).    
   RUN nydag_UI IN flexavikapph (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT TABLE flexdagtemp).
   FIND FIRST flexdagtemp WHERE flexdagtemp.PERSONALKOD = extratemp.PERSONALKOD AND 
   flexdagtemp.DATUM = extratemp.DATUM NO-ERROR.       
   RUN saldoplus_UI .      
   OPEN QUERY {&BROWSE-NAME} FOR EACH flextemp USE-INDEX FLEX NO-LOCK.
   FIND FIRST flextemp WHERE flextemp.PERSONALKOD = extratemp.PERSONALKOD AND 
   flextemp.DATUM = extratemp.DATUM AND flextemp.TID = extratemp.TID AND 
   flextemp.KNAPP = extratemp.KNAPP.
   DISPLAY FILL-IN_KONTROLL FILL-IN_PLUS FILL-IN-FELMED FILL-IN-FLST FILL-IN-FLSL FILL-IN-FELOK
   WITH FRAME {&FRAME-NAME}.
   BRW_FLEX:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   IF status-stopp = FALSE THEN 
   ENABLE {&BROWSE-NAME} BTN_BORT BTN_UPP FBTN_KONTROLL FILL-IN-FELOK WITH FRAME {&FRAME-NAME}. 
   RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(flextemp)).
   RUN openbdynspec_UI IN brwproc[1].
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-REGIS FILL-IN-PKOD FILL-IN_FORNAMN-2 FILL-IN_EFTERNAMN-2 
          FILL-IN_DATUM FILL-IN_PLUS FILL-IN_KONTROLL FILL-IN-FELMED 
          FILL-IN-FELOK FILL-IN-FLST FILL-IN-FLSL 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE BTN_NDA FILL-IN_DATUM BTN_FDA FILL-IN-FELOK FBTN_KONTROLL FBTN_VPERS 
         FBTN_NAAVB BTN_NY BTN_UPP BTN_BORT BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goma_UI WINDOW-1 
PROCEDURE goma_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF status-stopp = TRUE THEN RETURN.   
   FIND FIRST flexdagtemp WHERE flexdagtemp.PERSONALKOD = personaltemp.PERSONALKOD AND
   flexdagtemp.DATUM = regdatum USE-INDEX FLEX NO-LOCK NO-ERROR.   
   IF NOT AVAILABLE flextemp THEN DO:
      ASSIGN
      BRW_FLEX:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN_KONTROLL:HIDDEN = TRUE
      FILL-IN_PLUS:HIDDEN = TRUE
      FILL-IN-FLST:HIDDEN = TRUE
      FILL-IN-FLSL:HIDDEN = TRUE.
      IF NOT AVAILABLE flexdagtemp THEN DO:
         FILL-IN-FELOK:HIDDEN = TRUE.
         FILL-IN-FELMED:HIDDEN = TRUE.
      END.   
      IF FILL-IN_DATUM LE flexregtemp.SALDOKORD  THEN DO:     
         DISABLE BTN_NY BTN_UPP BTN_BORT FILL-IN-FELOK FBTN_KONTROLL WITH FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         ENABLE BTN_NY WITH FRAME {&FRAME-NAME}.
         DISABLE BTN_UPP BTN_BORT   WITH FRAME {&FRAME-NAME}.
      END.
   END.
   ELSE DO:
      ASSIGN
      FILL-IN_KONTROLL:HIDDEN = FALSE
      FILL-IN_PLUS:HIDDEN = FALSE
      FILL-IN-FELMED:HIDDEN = FALSE
      FILL-IN-FLST:HIDDEN = FALSE
      FILL-IN-FLSL:HIDDEN = FALSE
      FILL-IN-FELOK:HIDDEN = FALSE.
      IF flextemp.KORD > 01/01/97 THEN DO:     
         DISABLE BTN_NY BTN_UPP BTN_BORT FILL-IN-FELOK FBTN_KONTROLL WITH FRAME {&FRAME-NAME}.
      END.
      ELSE IF FILL-IN_DATUM LE flexregtemp.SALDOKORD  THEN DO:     
         DISABLE BTN_NY BTN_UPP BTN_BORT FILL-IN-FELOK FBTN_KONTROLL WITH FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         ENABLE BRW_FLEX 
         BTN_NY BTN_UPP BTN_BORT FILL-IN-FELOK FBTN_KONTROLL 
         WITH FRAME {&FRAME-NAME}.
         BRW_FLEX:HIDDEN = FALSE. 
      END.
      IF AVAILABLE flextemp THEN DO:
         BRW_FLEX:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(flextemp)).
         RUN openbdynspec_UI IN brwproc[1].
      END.
      ELSE DO:
         APPLY "HOME" TO BRW_FLEX.
         status-ok = BRW_FLEX:SELECT-FOCUSED-ROW() NO-ERROR.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nya_UI WINDOW-1 
PROCEDURE nya_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/                                       
   {muswait.i}      
   IF AVAILABLE flextemp THEN DO:
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.      
      ASSIGN 
      regdatum = flextemp.DATUM
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR.
      APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
      ASSIGN
      flrec = RECID(flextemp)
      flrec2 = flrec.    
      EMPTY TEMP-TABLE extratemp NO-ERROR.       
      CREATE extratemp.
      BUFFER-COPY flextemp TO extratemp.      
   END.
   ELSE DO:
      EMPTY TEMP-TABLE extratemp NO-ERROR.       
      CREATE extratemp.
      ASSIGN
      extratemp.PERSONALKOD = pkod 
      extratemp.DATUM       = bdatum
      extratemp.KNAPP = ?.
      ASSIGN
      regdatum = bdatum
      regdagnamn = "MÅN".
   END.
   ASSIGN
   vart = "NYA"
   bdatum = FILL-IN_DATUM.           
   RUN ANDFLEX.W (INPUT-OUTPUT TABLE extratemp).         
   FIND FIRST extratemp NO-ERROR.
   regdatum = extratemp.DATUM.
   IF musz = FALSE THEN DO:
      RUN efter_UI.     
      {musarrow.i}                     
   END.
   IF musz = TRUE THEN DO:
      musz = FALSE.      
   END.      
   ASSIGN
   musz = FALSE.  
   APPLY "ENTRY" TO BTN_NY IN FRAME {&FRAME-NAME}.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saldoplus_UI WINDOW-1 
PROCEDURE saldoplus_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST flexdagtemp WHERE flexdagtemp.PERSONALKOD = personaltemp.PERSONALKOD AND
   flexdagtemp.DATUM = regdatum USE-INDEX FLEX NO-LOCK NO-ERROR.   
   IF AVAILABLE flexdagtemp THEN DO:
      ASSIGN
      nytid = flexdagtemp.PLUS.
      RUN TIMSEK.P.
      ASSIGN
      splus = sekunder
      nytid = flexdagtemp.OVUTPLUS.
      RUN TIMSEK.P.
      ASSIGN
      splus = splus + sekunder
      nytid = flexdagtemp.FLARB.
      RUN TIMSEK.P.
      sekunder = splus + sekunder.
      RUN FSEKTIM.P.
      ASSIGN
      FILL-IN_KONTROLL = flexdagtemp.KONTROLL
      FILL-IN_PLUS = fnytid
      FILL-IN-FELMED = flexdagtemp.FELMED
      FILL-IN-FLST = flexdagtemp.START
      FILL-IN-FLSL = flexdagtemp.SLUT
      FILL-IN-FELOK = flexdagtemp.FELOK.      
      DISPLAY FILL-IN_KONTROLL FILL-IN_PLUS FILL-IN-FELMED 
      FILL-IN-FLST FILL-IN-FLSL FILL-IN-FELOK WITH FRAME {&FRAME-NAME}.
      IF status-stopp = FALSE THEN 
      ENABLE FILL-IN-FELOK FBTN_KONTROLL WITH FRAME {&FRAME-NAME}.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE starthelp_UI WINDOW-1 
PROCEDURE starthelp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN ANVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 3,INPUT tidpers.PERSONALKOD,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   ELSE DO:
      RUN ANVSKAP.P 
      (INPUT 3,INPUT tidpers.PERSONALKOD,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = tidpers.PERSONALKOD 
   NO-LOCK NO-ERROR.
   RUN nyflextemp_UI IN flexavikapph (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT TABLE flextemp).    
   
   RUN startkoll_UI.
   IF musz = TRUE THEN DO:
      RETURN.      
   END.
   RUN saldoplus_UI.     
   ASSIGN
   persrec = tidpers.TIDPERSREC
   vart = "AND"
   FILL-IN_EFTERNAMN-2 = personaltemp.EFTERNAMN
   FILL-IN_FORNAMN-2 = personaltemp.FORNAMN      
   FILL-IN-PKOD = personaltemp.PERSONALKOD.
   OPEN QUERY {&BROWSE-NAME} FOR EACH flextemp USE-INDEX FLEX NO-LOCK.
   GET FIRST {&BROWSE-NAME}.   
   RUN goma_UI.   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN TIDSTOPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 1,INPUT tidpers.PERSONALKOD,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN TIDSTOPP.P 
      (INPUT 1,INPUT tidpers.PERSONALKOD,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
   END.
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      IF Guru.Konstanter:globanv = lasanv THEN DO:    
         MESSAGE felmeddtemp.FELMEDD  VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO-CANCEL UPDATE val4 AS LOGICAL.
         DELETE felmeddtemp.
         CASE val4:                  
            WHEN TRUE THEN DO:           
               RUN lasupptid_UI IN persapph (INPUT  tidpers.PERSONALKOD, OUTPUT ptraff).
               IF ptraff = TRUE THEN DO:
                  MESSAGE tidpers.PERSONALKOD + " är nu upplåst!."
                  VIEW-AS ALERT-BOX.
               END.                         
            END.                  
         END CASE.
      END.   
      ELSE DO:               
         status-stopp = TRUE.
         DISABLE BTN_NY BTN_UPP BTN_BORT FBTN_KONTROLL WITH FRAME {&FRAME-NAME}.
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.           
      END.
   END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE startkoll_UI WINDOW-1 
PROCEDURE startkoll_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.  
   FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD 
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   FIND FIRST flexregtemp WHERE flexregtemp.KOD = flexavttemp.FLEXKOD 
   USE-INDEX FLEXREG NO-LOCK NO-ERROR.
   IF NOT AVAILABLE flexavttemp THEN DO:
      status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
      MESSAGE "Det går inte att registrera flex på denna person." VIEW-AS ALERT-BOX.
      musz = TRUE.
      RETURN.
   END.  
   ELSE IF flexavttemp.FLEXTID = FALSE THEN DO:
      status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
      MESSAGE "Det går inte att registrera flex på denna person." VIEW-AS ALERT-BOX.
      musz = TRUE.
      RETURN.
   END.  
   RUN nydag_UI IN flexavikapph (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT TABLE flexdagtemp).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

