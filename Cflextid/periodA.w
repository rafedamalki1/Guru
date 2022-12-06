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
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{AVDTEMP.I}
{ALLDEF.I}
&Scoped-define NEW    
&Scoped-define SHARED 
{FLEXTAB.I}
&Scoped-define NEW
{TIDPERS.I}
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE regstartsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE regslutsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO. 
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE regmnr AS INTEGER FORMAT "99" NO-UNDO.
DEFINE SHARED VARIABLE regmannamn AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE regar AS INTEGER FORMAT "99" NO-UNDO.
DEFINE SHARED VARIABLE regstart AS DECIMAL NO-UNDO. 
DEFINE SHARED VARIABLE regslut AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE regtotalt AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE lunchstarten AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE lunchslutet AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER NO-UNDO.    
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE VARIABLE datkoll AS DATE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE sparomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE persnr AS INTEGER EXTENT 10 FORMAT "99" NO-UNDO.
DEFINE VARIABLE tal1 AS INTEGER NO-UNDO.
DEFINE VARIABLE tal2 AS INTEGER NO-UNDO.
DEFINE VARIABLE ksiffran AS INTEGER NO-UNDO.
DEFINE VARIABLE bpnr AS DATE NO-UNDO.
DEFINE VARIABLE balder AS DECIMAL NO-UNDO.
DEFINE VARIABLE nyttaoapph AS HANDLE NO-UNDO.                     /* NYTTAOAPP.P */
DEFINE VARIABLE otbeordapph AS HANDLE NO-UNDO.
DEFINE VARIABLE omravdand AS INTEGER NO-UNDO.   /*används i NYCOL.I*/
DEFINE VARIABLE valdkom AS CHARACTER NO-UNDO.
{OMRTEMPW.I}
&Scoped-define NEW   
&Scoped-define SHARED SHARED
{AVDELNINGTEMPT.I}
{DIRDEF.I}
&Scoped-define NEW   
&Scoped-define SHARED
{ANVPERS.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
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


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-45 CMB_AVD RAD_FAST CMB_OMR BRW_AONR ~
BTN_NVE-2 BTN_NAAVB FILL-IN-STARTDAT BTN_FVE-2 BTN_NVE-3 FILL-IN-SLUTDAT ~
BTN_FVE-3 FILL-IN-AONR FILL-IN-DELNR FILL-IN_RESMAL FILL-IN_AONRS ~
FILL-IN_ORTS BTN_KLAR BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_PERSONALKOD FILL-IN_FORNAMN ~
FILL-IN_EFTERNAMN FILL-IN-TEXT CMB_AVD CMB_OMR FILL-IN-STARTDAT ~
FILL-IN-STDAG FILL-IN-SLUTDAT FILL-IN-SLDAG FILL-IN-AONR FILL-IN-DELNR ~
FILL-IN_RESMAL FILL-IN-SKP 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-GO 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FVE-2 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-3 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_KLAR 
     LABEL "OK":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NAAVB 
     LABEL "Nästa":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NVE-2 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-3 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(6)":U 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SKP AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLDAG AS CHARACTER FORMAT "X(7)":U 
     LABEL "Dag" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLUTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Slutdatum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Startdatum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STDAG AS CHARACTER FORMAT "X(7)":U 
     LABEL "Dag" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Visa aonr för:" 
     VIEW-AS FILL-IN 
     SIZE 22.75 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONRS AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_EFTERNAMN AS CHARACTER FORMAT "x(25)" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.

DEFINE VARIABLE FILL-IN_FORNAMN AS CHARACTER FORMAT "x(15)" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE FILL-IN_ORTS AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_PERSONALKOD AS CHARACTER FORMAT "x(5)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_RESMAL AS CHARACTER FORMAT "X(40)" 
     LABEL "Kommentar" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.

DEFINE VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 32 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-45
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 51 BY 1.54.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      utsokaonr SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AONR Dialog-Frame _STRUCTURED
  QUERY BRW_AONR NO-LOCK DISPLAY
      utsokaonr.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      utsokaonr.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      utsokaonr.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      utsokaonr.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(40)":U
  ENABLE
      utsokaonr.OMRADE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 51 BY 10.96
         TITLE "Aktiva arbetsordernummer".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN_PERSONALKOD AT ROW 1.92 COL 13 COLON-ALIGNED
     FILL-IN_FORNAMN AT ROW 1.92 COL 20.13 COLON-ALIGNED NO-LABEL
     FILL-IN_EFTERNAMN AT ROW 1.92 COL 37.25 COLON-ALIGNED NO-LABEL
     FILL-IN-TEXT AT ROW 2 COL 82.63 COLON-ALIGNED NO-LABEL
     CMB_AVD AT ROW 3.04 COL 82.88 COLON-ALIGNED NO-LABEL
     RAD_FAST AT ROW 4.17 COL 52.13 NO-LABEL
     CMB_OMR AT ROW 4.17 COL 82.88 COLON-ALIGNED NO-LABEL
     BRW_AONR AT ROW 5.54 COL 56.38
     BTN_NVE-2 AT ROW 7 COL 40
     BTN_NAAVB AT ROW 7.25 COL 108.38
     FILL-IN-STARTDAT AT ROW 7.42 COL 12.88 COLON-ALIGNED
     FILL-IN-STDAG AT ROW 7.42 COL 28.88 COLON-ALIGNED
     BTN_FVE-2 AT ROW 7.83 COL 40
     BTN_NVE-3 AT ROW 8.92 COL 40.13
     FILL-IN-SLUTDAT AT ROW 9.25 COL 12.88 COLON-ALIGNED
     FILL-IN-SLDAG AT ROW 9.25 COL 29 COLON-ALIGNED
     BTN_FVE-3 AT ROW 9.75 COL 40.13
     FILL-IN-AONR AT ROW 11.08 COL 12.88 COLON-ALIGNED
     FILL-IN-DELNR AT ROW 12.92 COL 12.88 COLON-ALIGNED
     FILL-IN_RESMAL AT ROW 14.75 COL 12.88 COLON-ALIGNED
     FILL-IN-SKP AT ROW 16.92 COL 55 COLON-ALIGNED NO-LABEL
     FILL-IN_AONRS AT ROW 17 COL 69.38 COLON-ALIGNED
     FILL-IN_ORTS AT ROW 17 COL 86.63 COLON-ALIGNED
     BTN_KLAR AT ROW 18.71 COL 93.38
     BTN_AVB AT ROW 18.71 COL 108.38
     "Ange start och slutdatum för perioden" VIEW-AS TEXT
          SIZE 39.5 BY 1.08 AT ROW 5.63 COL 2.88
     RECT-45 AT ROW 16.71 COL 56.38
     SPACE(15.49) SKIP(1.62)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Registrera frånvaro- tjänstereseperiod".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: utsokaonr T "?" NO-UNDO TEMP-DB utsokaonr
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_AONR CMB_OMR Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BRW_AONR:MAX-DATA-GUESS IN FRAME Dialog-Frame         = 1000.

ASSIGN 
       BTN_KLAR:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SKP IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-SLDAG IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-SLUTDAT:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-STDAG IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-TEXT IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_AONRS IN FRAME Dialog-Frame
   NO-DISPLAY                                                           */
/* SETTINGS FOR FILL-IN FILL-IN_EFTERNAMN IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_ORTS IN FRAME Dialog-Frame
   NO-DISPLAY                                                           */
/* SETTINGS FOR FILL-IN FILL-IN_PERSONALKOD IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME Dialog-Frame
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

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Registrera frånvaro- tjänstereseperiod */
DO:
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Registrera frånvaro- tjänstereseperiod */
DO:
   musz = TRUE.
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define BROWSE-NAME BRW_AONR
&Scoped-define SELF-NAME BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AONR Dialog-Frame
ON VALUE-CHANGED OF BRW_AONR IN FRAME Dialog-Frame /* Aktiva arbetsordernummer */
DO:
   IF musz = FALSE THEN DO:      
      ASSIGN
      FILL-IN-AONR = utsokaonr.AONR
      FILL-IN-DELNR = utsokaonr.DELNR.     
      DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.
      RUN resmallabel_UI.
   END.
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:  
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END.
   IF VALID-HANDLE(otbeordapph) THEN DO:
       RUN borthandle_UI IN otbeordapph.
       DELETE PROCEDURE otbeordapph NO-ERROR.
       otbeordapph = ?.
   END.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-2 Dialog-Frame
ON CHOOSE OF BTN_FVE-2 IN FRAME Dialog-Frame /* - */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT - 1.        
   RUN dag_UI.       
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-3 Dialog-Frame
ON CHOOSE OF BTN_FVE-3 IN FRAME Dialog-Frame /* - */
DO: 
   ASSIGN
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT.   
   FILL-IN-SLUTDAT = FILL-IN-SLUTDAT - 1.        
   RUN sldag_UI.       
   DISPLAY FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KLAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KLAR Dialog-Frame
ON CHOOSE OF BTN_KLAR IN FRAME Dialog-Frame /* OK */
DO:    
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT.
   FILL-IN-AONR = INPUT FILL-IN-AONR.
   FILL-IN-DELNR = INPUT FILL-IN-DELNR.   
   FILL-IN_RESMAL = INPUT FILL-IN_RESMAL.   
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND 
   utsokaonr.DELNR = FILL-IN-DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
   IF NOT AVAILABLE utsokaonr THEN DO:
      MESSAGE Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "finns inte." 
      VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   IF utsokaonr.AONRAVDATUM = 01/01/1991 OR utsokaonr.AONRAVDATUM >= FILL-IN-SLUTDAT 
   THEN FILL-IN-DELNR = FILL-IN-DELNR.
   ELSE DO:
      MESSAGE Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är redan avslutat." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   IF FILL-IN-SLUTDAT < FILL-IN-STARTDAT THEN DO:
      MESSAGE "Slutdatum kan inte vara mindre än startdatum." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF FILL-IN-SLUTDAT > (FILL-IN-STARTDAT + 31 ) THEN DO:
      MESSAGE "Du kan inte registrera frånvaro för mer än 1 månad i taget." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF YEAR(FILL-IN-SLUTDAT) > YEAR(TODAY) THEN DO:   
      IF FILL-IN-SLUTDAT > TODAY + 40  THEN DO:
         MESSAGE "Du kan inte skriva tid längre framåt i tiden" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.    
   IF MONTH(TODAY) LE 10 THEN DO:   
      IF DATE(MONTH(FILL-IN-SLUTDAT),01,YEAR(FILL-IN-SLUTDAT)) > DATE(MONTH(TODAY) + 2,01,YEAR(TODAY)) THEN DO:
         MESSAGE "Du kan inte skriva tid längre framåt i tiden" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END. 
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      IF personaltemp.ANSTALLNING = "Ej tidskrivande personal" THEN musz = musz.
      ELSE DO:      
         musz = FALSE.
         RUN kommentarutbcheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).         
         IF musz = TRUE THEN DO:
            musz = FALSE.
            IF FILL-IN_RESMAL = "" THEN DO:
               MESSAGE "Kommentar om vad utbildningen avser är obligatorisk."  VIEW-AS ALERT-BOX.             
               APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY.            
            END.
         END.
         musz = FALSE.      
         RUN tvbarncheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).         
         IF musz = TRUE THEN DO:
            musz = FALSE.
            IF FILL-IN_RESMAL = "" THEN DO:
               MESSAGE "Fyll i barnets personnummer som kommentar"  VIEW-AS ALERT-BOX.             
               APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY.
            END.
            musz = FALSE.
            RUN pnrkoll_UI (OUTPUT musz).
            IF musz = TRUE THEN DO:
               musz = FALSE.
               MESSAGE "Barnets personnummer felaktigt angivet. Skall vara format 999999-9999"  VIEW-AS ALERT-BOX. 
               APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY.            
            END.
            bpnr = DATE(SUBSTRING(FILL-IN_RESMAL,1,6)).
            balder = (( regdatum - bpnr ) / 365 ).
            IF balder  > 12 THEN DO:
               MESSAGE "Om barnet är äldre än 12 år krävs intyg för " Guru.Konstanter:gaok " 118. Finns intyg?"  
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val2 AS LOGICAL.
               IF val2 = TRUE THEN DO:
                  SUBSTRING(FILL-IN_RESMAL,12,50) = "Intyg krävs".
               END.
               ELSE DO:
                  APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
                  RETURN NO-APPLY.
               END.               
            END.
         END.
         RUN fpbarncheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).         
         IF musz = ? THEN DO:
            /*ok utan personnummer 60 dagar nnnan barns födelse*/         
            musz = FALSE.
            IF FILL-IN_RESMAL = "" THEN DO:
               MESSAGE "Barnets personnummer skall vara ifyllt ( format 999999-9999)." SKIP 
                       "Endast om det är frågan om föräldrapenning innan barnets födelse kan personnummer vara blankt." skip 
                       "Är det föräldrapenning före barnets födelse?"
                       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val5 AS LOGICAL.
               IF val5 = TRUE THEN DO:
                  SUBSTRING(FILL-IN_RESMAL,12,50) = "Innan barns födelse".
               END.
               ELSE DO:
                  APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
                  RETURN NO-APPLY.
               END.                         
            END.
            ELSE musz = TRUE.                  
         END.
         IF musz = TRUE THEN DO:
            musz = FALSE.
            IF FILL-IN_RESMAL = "" THEN DO:
               MESSAGE "Fyll i barnets personnummer som kommentar"  VIEW-AS ALERT-BOX.             
               APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY.
            END.
            musz = FALSE.
            RUN pnrkoll_UI (OUTPUT musz).
            IF musz = TRUE THEN DO:
               musz = FALSE.
               MESSAGE "Barnets personnummer felaktigt angivet. Skall vara format 999999-9999"  VIEW-AS ALERT-BOX. 
               APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY.            
            END.
            bpnr = DATE(SUBSTRING(FILL-IN_RESMAL,1,6)).
            balder = (( regdatum - bpnr ) / 365 ).
            IF bpnr < 01/01/2014 THEN DO:
               IF balder  GE 9  THEN DO:
                  MESSAGE "Barnet är äldre än 8 år " Guru.Konstanter:gaok " 119 eller 117 får inte användas."  VIEW-AS ALERT-BOX. 
                  RETURN.
               END.
               ELSE IF balder  > 8 AND balder  < 9 AND MONTH(bpnr) > 7 THEN.            
               ELSE IF balder  > 8 AND balder  < 9 AND MONTH(regdatum) > 7 THEN DO:
                  MESSAGE "Barnet är äldre än 8 år " Guru.Konstanter:gaok " 119 eller 117 får inte användas."  VIEW-AS ALERT-BOX.                   
                  RETURN.
               END.
            END.
            ELSE DO:
               IF balder  > 12 THEN DO:
                  MESSAGE "Barnet är äldre än 12 år " Guru.Konstanter:gaok " 119 eller 117 får inte användas."  VIEW-AS ALERT-BOX.                   
                  APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
               END.
            END.      
                        
         END.
         musz = FALSE.
         RUN kommoblkom_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz, OUTPUT valdkom ).      
         IF musz = TRUE THEN DO:
            musz = FALSE.
            IF FILL-IN_RESMAL = "" THEN DO:
               MESSAGE valdkom VIEW-AS ALERT-BOX. 
               APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
               RETURN.
            END.
         END.
         musz = FALSE.      
         RUN kommentaroblcheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).         
         IF musz = TRUE THEN DO:
            musz = FALSE.
            IF FILL-IN_RESMAL = "" THEN DO:
               MESSAGE "Obligatorisk kommentar"  VIEW-AS ALERT-BOX.             
               APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY.
            END.
         END.
         musz = FALSE.         
      END.
   END.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN PERIODKAN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT FILL-IN-STARTDAT,INPUT ?,INPUT personaltemp.PERSONALKOD,
      INPUT utsokaonr.AONR,INPUT utsokaonr.DELNR,OUTPUT TABLE felmeddtemp,INPUT FILL-IN-STARTDAT,INPUT FILL-IN-SLUTDAT,INPUT FILL-IN_RESMAL,INPUT Guru.Konstanter:globanv).
   END.
   ELSE DO:
      RUN PERIODKAN.P 
      (INPUT FILL-IN-STARTDAT,INPUT ?,INPUT personaltemp.PERSONALKOD,
       INPUT utsokaonr.AONR,INPUT utsokaonr.DELNR,OUTPUT TABLE felmeddtemp,INPUT FILL-IN-STARTDAT,INPUT FILL-IN-SLUTDAT,INPUT FILL-IN_RESMAL,INPUT Guru.Konstanter:globanv).
   END.               
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   ASSIGN
   FILL-IN-AONR = ""
   FILL-IN-DELNR = 0
   FILL-IN_RESMAL = "".
   DISPLAY FILL-IN-AONR FILL-IN-DELNR FILL-IN_RESMAL WITH FRAME {&FRAME-NAME}.    
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NAAVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NAAVB Dialog-Frame
ON CHOOSE OF BTN_NAAVB IN FRAME Dialog-Frame /* Nästa */
DO:   
   IF NOT AVAILABLE tidpers THEN APPLY "GO" TO FRAME {&FRAME-NAME}.
   ELSE FIND NEXT tidpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.    
   {muswait.i}    
   IF NOT AVAILABLE tidpers THEN DO:    
      status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
      APPLY "GO" TO FRAME {&FRAME-NAME}. 
   END.   
   ELSE DO:       
      EMPTY TEMP-TABLE personaltemp NO-ERROR.       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN ANVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 2,INPUT tidpers.PERSONALKOD,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
      END.
      ELSE DO:
         RUN ANVSKAP.P 
         (INPUT 2,INPUT tidpers.PERSONALKOD,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
      END. 
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = tidpers.PERSONALKOD NO-LOCK NO-ERROR.
      ASSIGN
      persrec = tidpers.TIDPERSREC
      FILL-IN_EFTERNAMN = tidpers.EFTERNAMN
      FILL-IN_FORNAMN = tidpers.FORNAMN.  
      FIND personaltemp WHERE personaltemp.PERSONALKOD = tidpers.PERSONALKOD 
      NO-LOCK NO-ERROR. 
      FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
      FILL-IN_PERSONALKOD = personaltemp.PERSONALKOD.
   END.
   DISPLAY FILL-IN_EFTERNAMN FILL-IN_FORNAMN FILL-IN_PERSONALKOD 
   WITH FRAME {&FRAME-NAME}.  
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NAAVB Dialog-Frame
ON GO OF BTN_NAAVB IN FRAME Dialog-Frame /* Nästa */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-2 Dialog-Frame
ON CHOOSE OF BTN_NVE-2 IN FRAME Dialog-Frame /* + */
DO:   
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT + 1.    
   RUN dag_UI.      
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-3 Dialog-Frame
ON CHOOSE OF BTN_NVE-3 IN FRAME Dialog-Frame /* + */
DO:   
   ASSIGN
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT.   
   FILL-IN-SLUTDAT = FILL-IN-SLUTDAT + 1.  
   RUN sldag_UI.      
   DISPLAY FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AVD Dialog-Frame
ON VALUE-CHANGED OF CMB_AVD IN FRAME Dialog-Frame
DO:
   CMB_AVD = INPUT CMB_AVD.   
   RUN nycolsortprep_UI (INPUT 2).
   RUN openbdynspec_UI IN brwproc[1].
   {CMB_AVDB2.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR Dialog-Frame
ON VALUE-CHANGED OF CMB_OMR IN FRAME Dialog-Frame
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR Dialog-Frame
ON ENTRY OF FILL-IN-AONR IN FRAME Dialog-Frame /* Aonr */
DO:
   RUN entryaonr_UI.
   RUN resmallabel_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR Dialog-Frame
ON LEAVE OF FILL-IN-AONR IN FRAME Dialog-Frame /* Aonr */
DO:  
   IF FILL-IN-AONR NE INPUT FILL-IN-AONR THEN DO:
      FILL-IN-AONR = INPUT FILL-IN-AONR.
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND 
      utsokaonr.DELNR = FILL-IN-DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
      IF NOT AVAILABLE utsokaonr THEN DO:
         MESSAGE Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "finns inte." 
         VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
      DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.
      RUN resmallabel_UI.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DELNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DELNR Dialog-Frame
ON LEAVE OF FILL-IN-DELNR IN FRAME Dialog-Frame /* Delnr */
DO:  
   IF INPUT FILL-IN-AONR = "" THEN DO:
      MESSAGE "Fältet " + LC(Guru.Konstanter:gaok) + " inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.    
   IF FILL-IN-AONR = INPUT FILL-IN-AONR AND FILL-IN-DELNR = INPUT FILL-IN-DELNR THEN DO:
      musz = FALSE.
   END.
   ELSE DO:
      musz = TRUE.
   END.                                  
   ASSIGN
   FILL-IN-AONR = INPUT FILL-IN-AONR
   FILL-IN-DELNR = INPUT FILL-IN-DELNR.
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND 
   utsokaonr.DELNR = FILL-IN-DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
   IF NOT AVAILABLE utsokaonr THEN DO:
      MESSAGE Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "finns inte." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   ELSE DO:     
      IF utsokaonr.AONRAVDATUM = 01/01/1991 OR
      utsokaonr.AONRAVDATUM >= regdatum THEN FILL-IN-DELNR = FILL-IN-DELNR.
      ELSE DO:
         MESSAGE Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är redan avslutat." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      DISPLAY FILL-IN-AONR FILL-IN-DELNR   WITH FRAME {&FRAME-NAME}.   
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SLUTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTDAT Dialog-Frame
ON LEAVE OF FILL-IN-SLUTDAT IN FRAME Dialog-Frame /* Slutdatum */
DO:
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT.
   
   RUN sldag_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTDAT Dialog-Frame
ON MOUSE-MENU-CLICK OF FILL-IN-SLUTDAT IN FRAME Dialog-Frame /* Slutdatum */
DO:
   ASSIGN
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-SLUTDAT.
   RUN AlmanBtn.w.
   FILL-IN-SLUTDAT = Guru.GlobalaVariabler:regdatum.
   RUN sldag_UI.
   DISPLAY FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT Dialog-Frame
ON LEAVE OF FILL-IN-STARTDAT IN FRAME Dialog-Frame /* Startdatum */
DO:
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
   
   RUN dag_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT Dialog-Frame
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME Dialog-Frame /* Startdatum */
DO:
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STARTDAT.
   RUN AlmanBtn.w.
   FILL-IN-STARTDAT = Guru.GlobalaVariabler:regdatum.
   RUN dag_UI.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_AONRS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONRS Dialog-Frame
ON ANY-KEY OF FILL-IN_AONRS IN FRAME Dialog-Frame /* Aonr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_AONRS IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONRS Dialog-Frame
ON LEAVE OF FILL-IN_AONRS IN FRAME Dialog-Frame /* Aonr */
DO:
   FILL-IN_AONRS = INPUT FILL-IN_AONRS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONRS Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_AONRS IN FRAME Dialog-Frame /* Aonr */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORTS Dialog-Frame
ON ANY-KEY OF FILL-IN_ORTS IN FRAME Dialog-Frame /* Benämning */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_ORTS IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORTS Dialog-Frame
ON LEAVE OF FILL-IN_ORTS IN FRAME Dialog-Frame /* Benämning */
DO:
   FILL-IN_ORTS = INPUT FILL-IN_ORTS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORTS Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_ORTS IN FRAME Dialog-Frame /* Benämning */
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


&Scoped-define SELF-NAME FILL-IN_RESMAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_RESMAL Dialog-Frame
ON LEAVE OF FILL-IN_RESMAL IN FRAME Dialog-Frame /* Kommentar */
DO:
   FILL-IN_RESMAL = INPUT FILL-IN_RESMAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_FAST Dialog-Frame
ON VALUE-CHANGED OF RAD_FAST IN FRAME Dialog-Frame
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
   FIND FIRST tidpers WHERE tidpers.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tidpers THEN DO:  
      FIND FIRST tidpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.            
   END.
   IF NOT AVAILABLE tidpers THEN DO:
      MESSAGE "Det finns ingen tid registrerad under denna period." VIEW-AS ALERT-BOX.
      LEAVE MAIN-BLOCK.
   END.
   persrec = tidpers.TIDPERSREC.
   regdatum = TODAY - 1.         
   ASSIGN
   FILL-IN-STARTDAT = regdatum
   FILL-IN-SLUTDAT = regdatum
   datkoll = regdatum.      
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN ANVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 3,INPUT pkod,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   ELSE DO:
      RUN ANVSKAP.P 
      (INPUT 3,INPUT pkod,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = tidpers.PERSONALKOD NO-LOCK NO-ERROR.      
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN FLEXTAB.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 3,INPUT personaltemp.PERSONALKOD,INPUT-OUTPUT TABLE ansttemp,INPUT-OUTPUT TABLE flexregtemp,
      INPUT-OUTPUT TABLE flexavttemp,INPUT-OUTPUT TABLE flexsaldotemp,INPUT-OUTPUT TABLE utryckningtemp).         
   END.
   ELSE DO:
      RUN FLEXTAB.P 
      (INPUT 3,INPUT personaltemp.PERSONALKOD,INPUT-OUTPUT TABLE ansttemp,INPUT-OUTPUT TABLE flexregtemp,
      INPUT-OUTPUT TABLE flexavttemp,INPUT-OUTPUT TABLE flexsaldotemp,INPUT-OUTPUT TABLE utryckningtemp).      
   END.
   FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
   FIND FIRST anvandartemp WHERE anvandartemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
   
   ASSIGN 
   FILL-IN_PERSONALKOD = personaltemp.PERSONALKOD
   FILL-IN_FORNAMN = personaltemp.FORNAMN
   FILL-IN_EFTERNAMN = personaltemp.EFTERNAMN.
   status-ok = CMB_OMR:ADD-LAST(Guru.Konstanter:gomrk + " : alla").
   {OMRHMT.I}
   
   CMB_AVD:DELIMITER = "$".
   status-ok = CMB_AVD:ADD-LAST(Guru.Konstanter:gavdk + " : alla").
   {ANVAVDSOF.I}   
   
   FOR EACH eavdtemp,         
   EACH avdelningtemp WHERE avdelningtemp.AVDELNINGNR = eavdtemp.AVDELNINGNR.
      status-ok = CMB_AVD:ADD-LAST(avdelningtemp.AVDELNINGNAMN).
   END.      

   CMB_AVD:SCREEN-VALUE= Guru.Konstanter:gavdk + " : alla".

   ASSIGN
   BRW_AONR:TITLE = Guru.Konstanter:gaol
   FILL-IN_AONRS:LABEL = Guru.Konstanter:gaok 
   FILL-IN-AONR:LABEL = Guru.Konstanter:gaok 
   FILL-IN-TEXT = "Visa " + LC(Guru.Konstanter:gaok) + " för:".
   {TILLFAST.I}
   FIND FIRST utsokaonr WHERE utsokaonr.FASTAAONR = TRUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE utsokaonr THEN DO:
      status-ok = RAD_FAST:DELETE(Guru.Konstanter:gfastl + " " + LC(Guru.Konstanter:gaok)).
   END.
   &Scoped-define FORMATNAMN FILL-IN-AONR      
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-DELNR   
   {DELNRFORMAT.I}
   FILL-IN-SKP = "Sök på:".
   RUN enable_UI.       
   {FRMSIZED.I} 
   CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".   
   ASSIGN RAD_FAST = TRUE.
   DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}.   
   RUN nycolsortprep_UI (INPUT 1).
   RUN openbdynspec_UI IN brwproc[1].   
   RUN dag_UI.
   RUN sldag_UI.
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
   utsokaonr.OMRADE:READ-ONLY IN BROWSE BRW_AONR = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_AONR:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).
   RUN dynprogextra IN brwproc[1]  (INPUT "omrvisa_UI",INPUT THIS-PROCEDURE).        
   RUN sattindex_UI IN brwproc[1] (INPUT "OMRADE").
   IF Guru.Konstanter:appcon THEN DO:      
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph.
   END.   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN OVERTIDBRDAPP.P PERSISTENT SET otbeordapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN OVERTIDBRDAPP.P PERSISTENT SET otbeordapph.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dag_UI Dialog-Frame 
PROCEDURE dag_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   regdatum = FILL-IN-STARTDAT.
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-STDAG = regdagnamn + "dag".
   DISPLAY FILL-IN-STDAG WITH FRAME {&FRAME-NAME}.
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
  DISPLAY FILL-IN_PERSONALKOD FILL-IN_FORNAMN FILL-IN_EFTERNAMN FILL-IN-TEXT 
          CMB_AVD CMB_OMR FILL-IN-STARTDAT FILL-IN-STDAG FILL-IN-SLUTDAT 
          FILL-IN-SLDAG FILL-IN-AONR FILL-IN-DELNR FILL-IN_RESMAL FILL-IN-SKP 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-45 CMB_AVD RAD_FAST CMB_OMR BRW_AONR BTN_NVE-2 BTN_NAAVB 
         FILL-IN-STARTDAT BTN_FVE-2 BTN_NVE-3 FILL-IN-SLUTDAT BTN_FVE-3 
         FILL-IN-AONR FILL-IN-DELNR FILL-IN_RESMAL FILL-IN_AONRS FILL-IN_ORTS 
         BTN_KLAR BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE entryaonr_UI Dialog-Frame 
PROCEDURE entryaonr_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/ 
   ASSIGN
   musz = FALSE   
   CMB_OMR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
   FILL-IN-TEXT:HIDDEN = FALSE.
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND utsokaonr.DELNR = FILL-IN-DELNR AND
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
      aonrrec = RECID(utsokaonr).                          
      RAD_FAST = utsokaonr.FASTAAONR.
   END.
   ELSE DO:
      ASSIGN 
      CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
      CMB_OMR = INPUT CMB_OMR.
      DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
      aonrrec = 0.
      RAD_FAST = TRUE.
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
   IF FILL-IN-AONR NE "" THEN DO:
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND utsokaonr.DELNR = FILL-IN-DELNR AND
      utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
      IF AVAILABLE utsokaonr THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(utsokaonr)).
         RUN lastselectdyn_UI IN brwproc[1].
      END.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillinupdate_UI Dialog-Frame 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nycolsortprep_UI Dialog-Frame 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE omrvisa_UI Dialog-Frame 
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pnrkoll_UI Dialog-Frame 
PROCEDURE pnrkoll_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/    
   DEFINE OUTPUT PARAMETER musz AS LOGICAL NO-UNDO.
   musz = FALSE.   
   IF FILL-IN_RESMAL NE "0000000000" THEN DO:  
      IF INDEX("0123456789",SUBSTRING(FILL-IN_RESMAL,1,1)) = 0 THEN musz = TRUE.
      IF INDEX("0123456789",SUBSTRING(FILL-IN_RESMAL,2,1)) = 0 THEN musz = TRUE.
      IF INDEX("0123456789",SUBSTRING(FILL-IN_RESMAL,3,1)) = 0 THEN musz = TRUE.
      IF INDEX("0123456789",SUBSTRING(FILL-IN_RESMAL,4,1)) = 0 THEN musz = TRUE.
      IF INDEX("0123456789",SUBSTRING(FILL-IN_RESMAL,5,1)) = 0 THEN musz = TRUE.
      IF INDEX("0123456789",SUBSTRING(FILL-IN_RESMAL,6,1)) = 0 THEN musz = TRUE.
      IF INDEX("0123456789",SUBSTRING(FILL-IN_RESMAL,7,1)) = 0 THEN musz = TRUE.
      IF INDEX("0123456789",SUBSTRING(FILL-IN_RESMAL,8,1)) = 0 THEN musz = TRUE.
      IF INDEX("0123456789",SUBSTRING(FILL-IN_RESMAL,9,1)) = 0 THEN musz = TRUE.
      IF INDEX("0123456789",SUBSTRING(FILL-IN_RESMAL,10,1)) = 0 THEN musz = TRUE.
      IF musz = TRUE THEN musz = musz.
      ELSE DO:      
         persnr[1] = INTEGER(SUBSTRING(STRING(FILL-IN_RESMAL,"999999-9999"),1,1)).
         persnr[2] = INTEGER(SUBSTRING(STRING(FILL-IN_RESMAL,"999999-9999"),2,1)).
         persnr[3] = INTEGER(SUBSTRING(STRING(FILL-IN_RESMAL,"999999-9999"),3,1)).
         persnr[4] = INTEGER(SUBSTRING(STRING(FILL-IN_RESMAL,"999999-9999"),4,1)).
         persnr[5] = INTEGER(SUBSTRING(STRING(FILL-IN_RESMAL,"999999-9999"),5,1)).
         persnr[6] = INTEGER(SUBSTRING(STRING(FILL-IN_RESMAL,"999999-9999"),6,1)).
         persnr[7] = INTEGER(SUBSTRING(STRING(FILL-IN_RESMAL,"999999-9999"),8,1)).
         persnr[8] = INTEGER(SUBSTRING(STRING(FILL-IN_RESMAL,"999999-9999"),9,1)).
         persnr[9] = INTEGER(SUBSTRING(STRING(FILL-IN_RESMAL,"999999-9999"),10,1)).
         persnr[10] = INTEGER(SUBSTRING(STRING(FILL-IN_RESMAL,"999999-9999"),11,1)).         
         persnr[1] = persnr[1] * 2.
         IF persnr[1] > 9 THEN persnr[1] = 
         INTEGER(SUBSTRING(STRING(persnr[1],"99"),1,1)) + 
         INTEGER(SUBSTRING(STRING(persnr[1],"99"),2,1)).
         persnr[3] = persnr[3] * 2.
         IF persnr[3] > 9 THEN persnr[3] =
         INTEGER(SUBSTRING(STRING(persnr[3],"99"),1,1)) +
         INTEGER(SUBSTRING(STRING(persnr[3],"99"),2,1)).
         persnr[5] = persnr[5] * 2.
         IF persnr[5] > 9 THEN persnr[5] =
         INTEGER(SUBSTRING(STRING(persnr[5],"99"),1,1)) +
         INTEGER(SUBSTRING(STRING(persnr[5],"99"),2,1)).
         persnr[7] = persnr[7] * 2.
         IF persnr[7] > 9 THEN persnr[7] =
         INTEGER(SUBSTRING(STRING(persnr[7],"99"),1,1)) +
         INTEGER(SUBSTRING(STRING(persnr[7],"99"),2,1)).
         persnr[9] = persnr[9] * 2.
         IF persnr[9] > 9 THEN persnr[9] =
         INTEGER(SUBSTRING(STRING(persnr[9],"99"),1,1)) +
         INTEGER(SUBSTRING(STRING(persnr[9],"99"),2,1)).
         tal1 = persnr[1] + persnr[2] + persnr[3] + persnr[4] + persnr[5] +
         persnr[6] + persnr[7] + persnr[8] + persnr[9].
   
         IF tal1 > 99 THEN
         tal2 = INTEGER(SUBSTRING(STRING(tal1,"999"),3,1)).
         IF tal1 < 100 THEN
         tal2 = INTEGER(SUBSTRING(STRING(tal1,"99"),2,1)).
         ksiffran = 10 - tal2.
         IF ksiffran = 10 THEN ksiffran = 0.
         IF persnr[10] = ksiffran OR (persnr[7] = 0 AND persnr[8] = 0 AND
         persnr[9] = 0 AND persnr[10] = 0) THEN ksiffran = ksiffran.
         ELSE IF (persnr[7] = 0 AND persnr[8] = 0 AND
         persnr[9] = 0 AND persnr[10] = 1) THEN ksiffran = ksiffran.
         ELSE DO:
            musz = TRUE.         
         END.   
      END.
   END.
   ELSE musz = TRUE.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resmallabel_UI Dialog-Frame 
PROCEDURE resmallabel_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/    
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:
      /*vab ska inte ha krav på personnummer FILL-IN-AONR = "118"  lena 20181211   tillbaka 20220506 Lena*/
      IF Guru.Konstanter:globforetag  = "snat" THEN DO:
          IF  FILL-IN-AONR = "119" OR FILL-IN-AONR = "118"  OR FILL-IN-AONR = "117" OR FILL-IN-AONR = "191" OR FILL-IN-AONR = "192" OR FILL-IN-AONR = "193" OR FILL-IN-AONR = "194"  THEN DO:                
            FILL-IN_RESMAL:FORMAT IN FRAME {&FRAME-NAME} = "XXXXXX-XXXX" .
            FILL-IN_RESMAL:LABEL = "Barnets pnr".         
            FILL-IN_RESMAL = INPUT FILL-IN_RESMAL NO-ERROR.
            IF FILL-IN_RESMAL = "" THEN ASSIGN FILL-IN_RESMAL = "0000000000".
         END.
         ELSE DO:
            FILL-IN_RESMAL:FORMAT = "X(158)".
            FILL-IN_RESMAL:LABEL = "Kommentar".
         END.
      END.
      ELSE  IF FILL-IN-AONR = "119" OR FILL-IN-AONR = "117"   THEN DO:                
         FILL-IN_RESMAL:FORMAT IN FRAME {&FRAME-NAME} = "XXXXXX-XXXX" .
         FILL-IN_RESMAL:LABEL = "Barnets pnr".         
         FILL-IN_RESMAL = INPUT FILL-IN_RESMAL NO-ERROR.
         IF FILL-IN_RESMAL = "" THEN ASSIGN FILL-IN_RESMAL = "0000000000".
      END.
      ELSE DO:
         FILL-IN_RESMAL:FORMAT = "X(158)".
         FILL-IN_RESMAL:LABEL = "Kommentar".
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sldag_UI Dialog-Frame 
PROCEDURE sldag_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   regdatum = FILL-IN-SLUTDAT.
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-SLDAG = regdagnamn + "dag".
   DISPLAY FILL-IN-SLDAG WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

