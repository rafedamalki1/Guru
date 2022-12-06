&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-3


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-3 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/04 -  1:19 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER placerarec AS RECID NO-UNDO.
/* Local Variable Definitions ---                                       */
{AVDTEMP.I}
{ALLDEF.I}
{GLOBVAR2DEL1.I}
{OMRTEMPW.I}
{FRDELTEMP.I}
&Scoped-define SHARED SHARED
{DIRDEF.I}
{PHMT.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE VARIABLE sparomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE nyttaoapph AS HANDLE NO-UNDO.                     /* NYTTAOAPP.P */
DEFINE VARIABLE omravdand AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-3
&Scoped-define BROWSE-NAME BRW_AONR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES utsokaonr personaltemp

/* Definitions for BROWSE BRW_AONR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_AONR utsokaonr.OMRADE utsokaonr.AONR ~
utsokaonr.DELNR utsokaonr.ORT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AONR 
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


/* Definitions for BROWSE BRW_PERS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PERS personaltemp.PERSONALKOD ~
personaltemp.FORNAMN personaltemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERS 
&Scoped-define QUERY-STRING-BRW_PERS FOR EACH personaltemp NO-LOCK ~
    BY personaltemp.PERSONALKOD
&Scoped-define OPEN-QUERY-BRW_PERS OPEN QUERY BRW_PERS FOR EACH personaltemp NO-LOCK ~
    BY personaltemp.PERSONALKOD.
&Scoped-define TABLES-IN-QUERY-BRW_PERS personaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERS personaltemp


/* Definitions for DIALOG-BOX DIALOG-3                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-SOK CMB_AVD CMB_OMR RAD_FAST ~
FILL-IN-AONR FILL-IN-DELNR FILL-IN-PROCENT FILL-IN_AONRS ~
FILL-IN_SPERSONALKOD FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN FILL-IN_ORTS ~
BTN_OK BTN_AVBRYT 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-TEXT CMB_AVD CMB_OMR FILL-IN-PKOD ~
FILL-IN-AONR FILL-IN-DELNR FILL-IN-PROCENT FILL-IN-SOKPA FILL-IN_AONRS ~
FILL-IN_SPERSONALKOD FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN FILL-IN_ORTS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVBRYT 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT ">99" INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "x(5)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN-PROCENT AS INTEGER FORMAT ">99" INITIAL 0 
     LABEL "Procent" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN-SOKPA AS CHARACTER FORMAT "X(7)":U INITIAL "Sök på" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Visa aonr/personal för:" 
     VIEW-AS FILL-IN 
     SIZE 22.5 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONRS AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_ORTS AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_SEFTERNAMN AS CHARACTER FORMAT "x(25)" 
     LABEL "E-namn" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .83.

DEFINE VARIABLE FILL-IN_SFORNAMN AS CHARACTER FORMAT "x(15)" 
     LABEL "F-namn" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .83.

DEFINE VARIABLE FILL-IN_SPERSONALKOD AS CHARACTER FORMAT "x(5)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .83.

DEFINE VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 30.13 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-SOK
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 55.63 BY 2.42
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      utsokaonr SCROLLING.

DEFINE QUERY BRW_PERS FOR 
      personaltemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AONR DIALOG-3 _STRUCTURED
  QUERY BRW_AONR NO-LOCK DISPLAY
      utsokaonr.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      utsokaonr.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      utsokaonr.DELNR COLUMN-LABEL "Del!nr" FORMAT "999":U
      utsokaonr.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 55.63 BY 11.83
         TITLE "Aktiva arbetsordernummer".

DEFINE BROWSE BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERS DIALOG-3 _STRUCTURED
  QUERY BRW_PERS NO-LOCK DISPLAY
      personaltemp.PERSONALKOD COLUMN-LABEL "Enhet/Sign" FORMAT "x(5)":U
      personaltemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(15)":U
      personaltemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(25)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 47.5 BY 12 ROW-HEIGHT-CHARS .63.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     FILL-IN-TEXT AT ROW 1.33 COL 60 COLON-ALIGNED NO-LABEL
     CMB_AVD AT ROW 2.42 COL 60 COLON-ALIGNED NO-LABEL
     CMB_OMR AT ROW 3.5 COL 60 COLON-ALIGNED NO-LABEL
     RAD_FAST AT ROW 3.63 COL 29.63 NO-LABEL
     BRW_AONR AT ROW 4.75 COL 28.88
     BRW_PERS AT ROW 4.75 COL 37
     FILL-IN-PKOD AT ROW 5.08 COL 13 COLON-ALIGNED
     FILL-IN-AONR AT ROW 6.54 COL 13 COLON-ALIGNED
     FILL-IN-DELNR AT ROW 8 COL 13 COLON-ALIGNED
     FILL-IN-PROCENT AT ROW 9.5 COL 13 COLON-ALIGNED
     FILL-IN-SOKPA AT ROW 17.25 COL 27.63 COLON-ALIGNED NO-LABEL
     FILL-IN_AONRS AT ROW 17.38 COL 48.5 COLON-ALIGNED
     FILL-IN_SPERSONALKOD AT ROW 18.13 COL 37.63 COLON-ALIGNED
     FILL-IN_SFORNAMN AT ROW 18.13 COL 46.38
     FILL-IN_SEFTERNAMN AT ROW 18.13 COL 71.5 COLON-ALIGNED
     FILL-IN_ORTS AT ROW 18.5 COL 48.5 COLON-ALIGNED
     BTN_OK AT ROW 20 COL 55.5
     BTN_AVBRYT AT ROW 20 COL 70.5
     RECT-SOK AT ROW 17.08 COL 28.88
     SPACE(0.73) SKIP(1.74)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "FRDEL":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: personaltemp T "?" NO-UNDO temp-db personaltemp
      TABLE: utsokaonr T "?" NO-UNDO temp-db utsokaonr
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_AONR RAD_FAST DIALOG-3 */
/* BROWSE-TAB BRW_PERS BRW_AONR DIALOG-3 */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE
       FRAME DIALOG-3:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_AONR IN FRAME DIALOG-3
   NO-ENABLE                                                            */
ASSIGN 
       BRW_AONR:HIDDEN  IN FRAME DIALOG-3                = TRUE
       BRW_AONR:MAX-DATA-GUESS IN FRAME DIALOG-3         = 1000.

/* SETTINGS FOR BROWSE BRW_PERS IN FRAME DIALOG-3
   NO-ENABLE                                                            */
ASSIGN 
       BRW_PERS:HIDDEN  IN FRAME DIALOG-3                = TRUE
       BRW_PERS:MAX-DATA-GUESS IN FRAME DIALOG-3         = 300.

/* SETTINGS FOR FILL-IN FILL-IN-PKOD IN FRAME DIALOG-3
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-SOKPA IN FRAME DIALOG-3
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-TEXT IN FRAME DIALOG-3
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_SFORNAMN IN FRAME DIALOG-3
   ALIGN-L                                                              */
/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME DIALOG-3
   NO-DISPLAY                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AONR
/* Query rebuild information for BROWSE BRW_AONR
     _TblList          = "Temp-Tables.utsokaonr"
     _Options          = "NO-LOCK "
     _OrdList          = "Temp-Tables.utsokaonr.OMRADE|yes,Temp-Tables.utsokaonr.AONR|yes,Temp-Tables.utsokaonr.DELNR|yes"
     _FldNameList[1]   > Temp-Tables.utsokaonr.OMRADE
"utsokaonr.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.utsokaonr.AONR
"utsokaonr.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.utsokaonr.DELNR
"utsokaonr.DELNR" "Del!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.utsokaonr.ORT
"utsokaonr.ORT" "Ort/Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AONR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERS
/* Query rebuild information for BROWSE BRW_PERS
     _TblList          = "Temp-Tables.personaltemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.personaltemp.PERSONALKOD|yes"
     _FldNameList[1]   > Temp-Tables.personaltemp.PERSONALKOD
"personaltemp.PERSONALKOD" "Enhet/Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.personaltemp.FORNAMN
"personaltemp.FORNAMN" "Förnamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.personaltemp.EFTERNAMN
"personaltemp.EFTERNAMN" "Efternamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PERS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON END-ERROR OF FRAME DIALOG-3 /* FRDEL */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END.
   musz = TRUE.  
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON ENDKEY OF FRAME DIALOG-3 /* FRDEL */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AONR
&Scoped-define SELF-NAME BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AONR DIALOG-3
ON VALUE-CHANGED OF BRW_AONR IN FRAME DIALOG-3 /* Aktiva arbetsordernummer */
DO:
   IF AVAILABLE utsokaonr THEN DO:
      ASSIGN
      FILL-IN-AONR = utsokaonr.AONR
      FILL-IN-DELNR = utsokaonr.DELNR.
      DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.        
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_PERS
&Scoped-define SELF-NAME BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PERS DIALOG-3
ON VALUE-CHANGED OF BRW_PERS IN FRAME DIALOG-3
DO:
   IF AVAILABLE personaltemp THEN DO:
      FILL-IN-PKOD = personaltemp.PERSONALKOD.      
      DISPLAY FILL-IN-PKOD WITH FRAME {&FRAME-NAME}.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVBRYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVBRYT DIALOG-3
ON CHOOSE OF BTN_AVBRYT IN FRAME DIALOG-3 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-3
ON CHOOSE OF BTN_OK IN FRAME DIALOG-3 /* Ok */
DO:
   {muswait.i}
   musz = FALSE.
   FILL-IN-PKOD = INPUT FILL-IN-PKOD.
   FILL-IN-AONR = INPUT FILL-IN-AONR.
   FILL-IN-DELNR = INPUT FILL-IN-DELNR.
   FILL-IN-PROCENT = INPUT FILL-IN-PROCENT.
   IF FILL-IN-PKOD = "" THEN DO:
      MESSAGE "Enhet/Sign kan inte vara blankt." VIEW-AS ALERT-BOX.    
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END. 
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = FILL-IN-PKOD AND 
   personaltemp.AKTIV = TRUE USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.  
   IF NOT AVAILABLE personaltemp THEN DO:
      MESSAGE "Enhet/Sign" FILL-IN-PKOD "finns inte eller är inaktiv." VIEW-AS ALERT-BOX.   
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END.    
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND 
   utsokaonr.DELNR = FILL-IN-DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
   IF NOT AVAILABLE utsokaonr THEN DO:
      MESSAGE Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "finns inte." VIEW-AS ALERT-BOX.      
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END.
   ELSE DO:                        
      IF utsokaonr.PRISTYP = "FRÅNVARO." THEN musz = musz.     
      ELSE DO:
         MESSAGE Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är inte frånvaro." VIEW-AS ALERT-BOX.       
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_OK IN FRAME {&FRAME-NAME}.
      END.
   END.
   DO TRANSACTION:
      ASSIGN 
      frdeltemp.PERSONALKOD = FILL-IN-PKOD
      frdeltemp.AONR = FILL-IN-AONR
      frdeltemp.DELNR = FILL-IN-DELNR
      frdeltemp.PROCENT = FILL-IN-PROCENT.       
   END. 
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END.
   APPLY "GO" TO FRAME {&FRAME-NAME}.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AVD DIALOG-3
ON VALUE-CHANGED OF CMB_AVD IN FRAME DIALOG-3
DO:
   CMB_AVD = INPUT CMB_AVD.   
   RUN nycolsortprep_UI (INPUT 2).
   RUN openbdynspec_UI IN brwproc[1].
   {CMB_AVDB2.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR DIALOG-3
ON VALUE-CHANGED OF CMB_OMR IN FRAME DIALOG-3
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


&Scoped-define SELF-NAME FILL-IN-AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR DIALOG-3
ON ANY-KEY OF FILL-IN-AONR IN FRAME DIALOG-3 /* Aonr */
DO:
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR DIALOG-3
ON ENTRY OF FILL-IN-AONR IN FRAME DIALOG-3 /* Aonr */
DO:
   
   ENABLE  FILL-IN_AONRS FILL-IN_ORTS RAD_FAST WITH FRAME {&FRAME-NAME}.
   RUN goma_UI (INPUT 1).
   FIND FIRST utsokaonr WHERE 
   utsokaonr.AONR = FILL-IN-AONR AND utsokaonr.DELNR = FILL-IN-DELNR AND
   utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
   IF AVAILABLE utsokaonr THEN DO: 
      IF Guru.Konstanter:globallao = FALSE AND utsokaonr.FASTAAONR = TRUE AND utsokaonr.OMRADE = " " THEN DO:
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
         USE-INDEX OMR NO-LOCK NO-ERROR.           
         aonrrec = RECID(utsokaonr).
         RAD_FAST = utsokaonr.FASTAAONR.
         ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN NO-ERROR.                        
         IF CMB_OMR:SCREEN-VALUE = ? THEN DO:
            CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
         END.
      END.
      ELSE IF Guru.Konstanter:globallao = FALSE AND utsokaonr.FASTAAONR = FALSE AND utsokaonr.OMRADE = " " THEN DO:
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
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
            ASSIGN 
            CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
            CMB_OMR = INPUT CMB_OMR.
         END.
         ELSE ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.           
      END.
      DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
      ASSIGN
      aonrrec = RECID(utsokaonr).
      RAD_FAST = utsokaonr.FASTAAONR.      
   END.
   ELSE DO:                   
      ASSIGN 
      CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
      CMB_OMR = INPUT CMB_OMR.
      DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
      ASSIGN
      aonrrec = ?
      RAD_FAST = TRUE.
       DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO RAD_FAST.
   END.
   DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}.
   BRW_AONR:HIDDEN = FALSE.
   RUN openbdynspec_UI IN brwproc[1].
   FILL-IN-AONR = INPUT FILL-IN-AONR.
   IF FILL-IN-AONR NE "" THEN DO:
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND utsokaonr.DELNR = FILL-IN-DELNR AND
      utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
      IF AVAILABLE utsokaonr THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(utsokaonr)).
         RUN lastselectdyn_UI IN brwproc[1].
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR DIALOG-3
ON LEFT-MOUSE-CLICK OF FILL-IN-AONR IN FRAME DIALOG-3 /* Aonr */
DO:
   APPLY "ENTRY" TO FILL-IN-AONR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PKOD DIALOG-3
ON ENTRY OF FILL-IN-PKOD IN FRAME DIALOG-3 /* Enhet/Sign */
DO:
   RUN goma_UI (INPUT 2).         
   RUN openbdyn_UI IN brwproc[2] (INPUT "").
   IF personaltemp.PERSONALKOD NE "" THEN DO:
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = frdeltemp.PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE(personaltemp) THEN DO:         
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(personaltemp)).
         RUN lastselectdyn_UI IN brwproc[2].      
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_AONRS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONRS DIALOG-3
ON ANY-KEY OF FILL-IN_AONRS IN FRAME DIALOG-3 /* Aonr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_AONRS IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONRS DIALOG-3
ON LEAVE OF FILL-IN_AONRS IN FRAME DIALOG-3 /* Aonr */
DO:
   FILL-IN_AONRS = INPUT FILL-IN_AONRS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONRS DIALOG-3
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_AONRS IN FRAME DIALOG-3 /* Aonr */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORTS DIALOG-3
ON ANY-KEY OF FILL-IN_ORTS IN FRAME DIALOG-3 /* Benämning */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_ORTS IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORTS DIALOG-3
ON LEAVE OF FILL-IN_ORTS IN FRAME DIALOG-3 /* Benämning */
DO:
   FILL-IN_ORTS = INPUT FILL-IN_ORTS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORTS DIALOG-3
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_ORTS IN FRAME DIALOG-3 /* Benämning */
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


&Scoped-define SELF-NAME FILL-IN_SEFTERNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SEFTERNAMN DIALOG-3
ON ANY-KEY OF FILL-IN_SEFTERNAMN IN FRAME DIALOG-3 /* E-namn */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SEFTERNAMN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SEFTERNAMN DIALOG-3
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SEFTERNAMN IN FRAME DIALOG-3 /* E-namn */
DO:
   FILL-IN_SEFTERNAMN = INPUT FILL-IN_SEFTERNAMN.
   IF FILL-IN_SEFTERNAMN = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SEFTERNAMN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   RUN sokurvaldyn_UI IN brwproc[2] (INPUT "EFTERNAMN", INPUT FILL-IN_SEFTERNAMN).
   FILL-IN-PKOD = personaltemp.PERSONALKOD.      
   DISPLAY FILL-IN-PKOD WITH FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SFORNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SFORNAMN DIALOG-3
ON ANY-KEY OF FILL-IN_SFORNAMN IN FRAME DIALOG-3 /* F-namn */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SFORNAMN DIALOG-3
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SFORNAMN IN FRAME DIALOG-3 /* F-namn */
DO:
   FILL-IN_SFORNAMN = INPUT FILL-IN_SFORNAMN.
   IF FILL-IN_SFORNAMN = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   RUN sokurvaldyn_UI IN brwproc[2] (INPUT "FORNAMN", INPUT FILL-IN_SFORNAMN).
   FILL-IN-PKOD = personaltemp.PERSONALKOD.      
   DISPLAY FILL-IN-PKOD WITH FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SPERSONALKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERSONALKOD DIALOG-3
ON ANY-KEY OF FILL-IN_SPERSONALKOD IN FRAME DIALOG-3 /* Enhet/Sign */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERSONALKOD DIALOG-3
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SPERSONALKOD IN FRAME DIALOG-3 /* Enhet/Sign */
DO:
   FILL-IN_SPERSONALKOD = INPUT FILL-IN_SPERSONALKOD.
   IF FILL-IN_SPERSONALKOD = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   RUN sokurvaldyn_UI IN brwproc[2] (INPUT "PERSONALKOD", INPUT FILL-IN_SPERSONALKOD).
   FILL-IN-PKOD = personaltemp.PERSONALKOD.      
   DISPLAY FILL-IN-PKOD WITH FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_FAST DIALOG-3
ON VALUE-CHANGED OF RAD_FAST IN FRAME DIALOG-3
DO:
   RAD_FAST = INPUT RAD_FAST.   
   IF RAD_FAST = FALSE THEN DO:
      CMB_OMR = sparomrade.
      CMB_OMR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = sparomrade.
      FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
      USE-INDEX OMRNAMN NO-LOCK NO-ERROR.
   END.  
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
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


&Scoped-define BROWSE-NAME BRW_AONR
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-3 


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
   FIND FIRST utsokaonr WHERE NO-LOCK NO-ERROR.
   CMB_AVD:DELIMITER = "$". 
   status-ok = CMB_AVD:ADD-LAST(Guru.Konstanter:gavdk + " : alla").
   
   status-ok = CMB_OMR:ADD-LAST(Guru.Konstanter:gomrk + " : alla").
   {ANVAVDSO.I}
    
   FOR EACH eavdtemp,         
   EACH avdelningtemp WHERE avdelningtemp.AVDELNINGNR = eavdtemp.AVDELNINGNR.
      status-ok = CMB_AVD:ADD-LAST(avdelningtemp.AVDELNINGNAMN).
   END.      
   CMB_AVD:SCREEN-VALUE= Guru.Konstanter:gavdk + " : alla".
   FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE omrtemp THEN DO:
      FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
   END.
   ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.       
   IF Guru.Konstanter:globomr = "" OR Guru.Konstanter:globallpers = TRUE THEN DO:
      ASSIGN CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
      CMB_OMR = INPUT CMB_OMR.
      DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
   END.
   IF placerarec = ? THEN DO:
      ENABLE FILL-IN-PKOD WITH FRAME {&FRAME-NAME}.    
      CREATE frdeltemp.
      ASSIGN
      frdeltemp.RECIDVIS = RECID(frdeltemp).
      placerarec = frdeltemp.RECIDVIS.
   END.
   ELSE DO:
      FIND FIRST frdeltemp WHERE frdeltemp.RECIDVIS = placerarec NO-LOCK NO-ERROR.
      DISABLE FILL-IN-PKOD WITH FRAME {&FRAME-NAME}. 
   END.
   IF frdeltemp.PERSONALKOD = "" THEN DO:
      ASSIGN FILL-IN-PKOD = ""
      FILL-IN-AONR = ""
      FILL-IN-DELNR = 0
      FILL-IN-PROCENT = 0.
   END.
   ELSE DO:
      ASSIGN FILL-IN-PKOD = frdeltemp.PERSONALKOD
      FILL-IN-AONR = frdeltemp.AONR
      FILL-IN-DELNR = frdeltemp.DELNR
      FILL-IN-PROCENT = frdeltemp.PROC.
   END.      
   ASSIGN
   BRW_AONR:TITLE = "Aktiva " + LC(Guru.Konstanter:gaol)            
   FILL-IN_AONRS:LABEL = Guru.Konstanter:gaok 
   FILL-IN-AONR:LABEL = Guru.Konstanter:gaok
   FILL-IN-TEXT = "Visa " + LC(Guru.Konstanter:gaok) + "/personal för:".
   {TILLFAST.I}
   FIND FIRST utsokaonr WHERE utsokaonr.FASTAAONR = TRUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE utsokaonr THEN DO:
      status-ok = RAD_FAST:DELETE(Guru.Konstanter:gfastl + " " + LC(Guru.Konstanter:gaok)).
   END.
   &Scoped-define FORMATNAMN FILL-IN_AONRS   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-DELNR   
   {DELNRFORMAT.I}
   RUN enable_UI.       
   ENABLE BRW_PERS BRW_AONR WITH FRAME {&FRAME-NAME}.  
   
   {FRMSIZED.I}
   RUN goma_UI (INPUT 2).         
   RUN openbdyn_UI IN brwproc[2] (INPUT "").
   IF personaltemp.PERSONALKOD NE "" THEN DO:
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = frdeltemp.PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE(personaltemp) THEN DO:         
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(personaltemp)).
         RUN lastselectdyn_UI IN brwproc[2].      
      END.
   END.
   {musarrow.i}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-3 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/         
   
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_AONR:HANDLE IN FRAME {&FRAME-NAME}).     
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_PERS:HANDLE IN FRAME {&FRAME-NAME}).
   RUN sattindex_UI IN brwproc[1] (INPUT "OMRADE").
   
   IF Guru.Konstanter:appcon THEN DO:      
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph.
   END.   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-3  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-3.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-3  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-TEXT CMB_AVD CMB_OMR FILL-IN-PKOD FILL-IN-AONR FILL-IN-DELNR 
          FILL-IN-PROCENT FILL-IN-SOKPA FILL-IN_AONRS FILL-IN_SPERSONALKOD 
          FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN FILL-IN_ORTS 
      WITH FRAME DIALOG-3.
  ENABLE RECT-SOK CMB_AVD CMB_OMR RAD_FAST FILL-IN-AONR FILL-IN-DELNR 
         FILL-IN-PROCENT FILL-IN_AONRS FILL-IN_SPERSONALKOD FILL-IN_SFORNAMN 
         FILL-IN_SEFTERNAMN FILL-IN_ORTS BTN_OK BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillinupdate_UI DIALOG-3 
PROCEDURE fillinupdate_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/    
   IF AVAILABLE utsokaonr THEN DO:
      ASSIGN
      FILL-IN-AONR = utsokaonr.AONR
      FILL-IN-DELNR = utsokaonr.DELNR.
      DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}. 
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goma_UI DIALOG-3 
PROCEDURE goma_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER gomavar AS INTEGER NO-UNDO.
   IF gomavar = 1 THEN DO:
      ASSIGN
      CMB_OMR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      CMB_AVD:HIDDEN = FALSE
      FILL-IN-TEXT:HIDDEN = FALSE
      BRW_AONR:HIDDEN  = FALSE
      RECT-SOK:HIDDEN = FALSE
      RAD_FAST:HIDDEN = FALSE   
      FILL-IN_AONRS:HIDDEN = FALSE
      FILL-IN_ORTS:HIDDEN = FALSE
      FILL-IN_SEFTERNAMN:HIDDEN = TRUE
      FILL-IN_SFORNAMN:HIDDEN = TRUE
      FILL-IN_SPERSONALKOD:HIDDEN = TRUE
      BRW_PERS:HIDDEN = TRUE.
   END.
   IF gomavar = 2 THEN DO:
      ASSIGN
      CMB_OMR:HIDDEN = TRUE
      CMB_AVD:HIDDEN = TRUE
      FILL-IN-TEXT:HIDDEN = TRUE
      BRW_AONR:HIDDEN = TRUE
      RECT-SOK:HIDDEN = FALSE 
      RAD_FAST:HIDDEN = TRUE   
      FILL-IN_AONRS:HIDDEN = TRUE
      FILL-IN_ORTS:HIDDEN = TRUE
      FILL-IN_SEFTERNAMN:HIDDEN = FALSE
      FILL-IN_SFORNAMN:HIDDEN = FALSE
      FILL-IN_SPERSONALKOD:HIDDEN = FALSE
      BRW_PERS:HIDDEN = FALSE.                     
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nycolsortprep_UI DIALOG-3 
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

