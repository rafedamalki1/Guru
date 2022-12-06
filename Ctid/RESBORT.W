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
DEFINE INPUT PARAMETER stansdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER varifran AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{UPPGHMT.I}
{TIDPERS.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
{OMRTEMPW.I}
{FLEXTAB.I}
&Scoped-define SHARED SHARED
{PHMT.I}
&Scoped-define NEW NEW 
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE valgubbe AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE enfle AS CHARACTER NO-UNDO. 
DEFINE VARIABLE bdatumspar AS DATE NO-UNDO.
DEFINE VARIABLE avdatumspar AS DATE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_PERS

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidpers

/* Definitions for BROWSE BRW_PERS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PERS tidpers.PERSONALKOD tidpers.FORNAMN ~
tidpers.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERS tidpers.PERSONALKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_PERS tidpers
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_PERS tidpers
&Scoped-define QUERY-STRING-BRW_PERS FOR EACH tidpers NO-LOCK ~
    BY tidpers.PERSONALKOD
&Scoped-define OPEN-QUERY-BRW_PERS OPEN QUERY BRW_PERS FOR EACH tidpers NO-LOCK ~
    BY tidpers.PERSONALKOD.
&Scoped-define TABLES-IN-QUERY-BRW_PERS tidpers
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERS tidpers


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-ENFLE BTN_NAAVB BTN_BORT BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-PKOD FILL-IN-ENFLE 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_FVE-3 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-4 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NAAVB 
     LABEL "Nästa":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NVE-3 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-4 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE VARIABLE FILL-IN-ENFLE AS LOGICAL FORMAT "Endag/Flerdygn":U INITIAL NO 
     LABEL "Endag/Flerdygn" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLUTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Slutdatum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKPA AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Startdatum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN_SEFTERNAMN AS CHARACTER FORMAT "x(25)" 
     LABEL "E-namn" 
     VIEW-AS FILL-IN 
     SIZE 12.88 BY .83.

DEFINE VARIABLE FILL-IN_SFORNAMN AS CHARACTER FORMAT "x(15)" 
     LABEL "F-namn" 
     VIEW-AS FILL-IN 
     SIZE 17.5 BY .83.

DEFINE VARIABLE FILL-IN_SPERSONALKOD AS CHARACTER FORMAT "x(5)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 12.88 BY .83.

DEFINE RECTANGLE RECT-SOK
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 50.13 BY 2.42
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_PERS FOR 
      tidpers SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERS DIALOG-1 _STRUCTURED
  QUERY BRW_PERS NO-LOCK DISPLAY
      tidpers.PERSONALKOD COLUMN-LABEL "Enhet/Sign" FORMAT "X(5)":U
      tidpers.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "X(15)":U
      tidpers.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "X(25)":U
  ENABLE
      tidpers.PERSONALKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 47.5 BY 12.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-PKOD AT ROW 1.46 COL 15.5 COLON-ALIGNED
     BRW_PERS AT ROW 1.5 COL 33.25
     FILL-IN-ENFLE AT ROW 2.63 COL 15.5 COLON-ALIGNED
     BTN_NVE-3 AT ROW 3.54 COL 27.88
     FILL-IN-STARTDAT AT ROW 4 COL 15.5 COLON-ALIGNED
     BTN_FVE-3 AT ROW 4.42 COL 27.88
     BTN_NAAVB AT ROW 4.67 COL 81.75
     BTN_NVE-4 AT ROW 5.54 COL 27.75
     FILL-IN-SLUTDAT AT ROW 6 COL 15.5 COLON-ALIGNED
     BTN_FVE-4 AT ROW 6.42 COL 27.75
     FILL-IN-SOKPA AT ROW 14.54 COL 29 COLON-ALIGNED NO-LABEL
     FILL-IN_SPERSONALKOD AT ROW 14.54 COL 50.38 COLON-ALIGNED
     FILL-IN_SFORNAMN AT ROW 15.71 COL 38 COLON-ALIGNED
     FILL-IN_SEFTERNAMN AT ROW 15.71 COL 64.63 COLON-ALIGNED
     BTN_BORT AT ROW 17.25 COL 30.5
     BTN_AVS AT ROW 17.25 COL 81.75
     RECT-SOK AT ROW 14.33 COL 30.25
     SPACE(16.24) SKIP(1.82)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Borttag av tjänsteresor":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: tidpers T "?" NO-UNDO temp-db tidpers
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_PERS FILL-IN-PKOD DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_PERS IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_PERS:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_PERS:MAX-DATA-GUESS IN FRAME DIALOG-1         = 300.

/* SETTINGS FOR BUTTON BTN_FVE-3 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_FVE-3:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR BUTTON BTN_FVE-4 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_FVE-4:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR BUTTON BTN_NVE-3 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NVE-3:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR BUTTON BTN_NVE-4 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NVE-4:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-PKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-SLUTDAT IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-SLUTDAT:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOKPA IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-SOKPA:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-STARTDAT IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_SEFTERNAMN IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_SEFTERNAMN:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_SFORNAMN IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_SFORNAMN:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_SPERSONALKOD IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_SPERSONALKOD:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-SOK IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       RECT-SOK:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERS
/* Query rebuild information for BROWSE BRW_PERS
     _TblList          = "Temp-Tables.tidpers"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.tidpers.PERSONALKOD|yes"
     _FldNameList[1]   > Temp-Tables.tidpers.PERSONALKOD
"tidpers.PERSONALKOD" "Enhet/Sign" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.tidpers.FORNAMN
"tidpers.FORNAMN" "Förnamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.tidpers.EFTERNAMN
"tidpers.EFTERNAMN" "Efternamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PERS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Borttag av tjänsteresor */
DO:
    {BORTBRWPROC.I}
   FOR EACH tidpers:
      tidpers.REGKOLL = FALSE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Borttag av tjänsteresor */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_PERS
&Scoped-define SELF-NAME BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PERS DIALOG-1
ON LEAVE OF BRW_PERS IN FRAME DIALOG-1
DO:   
   IF INPUT FILL-IN-PKOD = "" THEN DO:
      MESSAGE "Enhet/Sign kan inte vara blank." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.    
   FILL-IN-PKOD = INPUT FILL-IN-PKOD.
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = FILL-IN-PKOD AND 
   personaltemp.AKTIV = TRUE USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.  
   IF NOT AVAILABLE personaltemp THEN DO:
      MESSAGE "Enhet/Sign" FILL-IN-PKOD "finns inte eller är inaktiv." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END. 
   ELSE DO:         
      RUN nastapers_UI.      
      musz = FALSE.
   END.
   DISPLAY FILL-IN-PKOD FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
   ENABLE FILL-IN-STARTDAT FILL-IN-SLUTDAT BTN_FVE-3 BTN_NVE-3 BTN_FVE-4 BTN_NVE-4 
   WITH FRAME {&FRAME-NAME}.
   {musarrow.i}     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PERS DIALOG-1
ON VALUE-CHANGED OF BRW_PERS IN FRAME DIALOG-1
DO:
   IF AVAILABLE tidpers THEN DO:
      FILL-IN-PKOD = tidpers.PERSONALKOD.      
      persrec = tidpers.TIDPERSREC.
   END.
   valgubbe = TRUE.
   RUN nasta_UI.
   RUN nastapers_UI.
   valgubbe = FALSE.
   musz = FALSE.      
   {musarrow.i}  
   DISPLAY FILL-IN-PKOD FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
   ENABLE FILL-IN-STARTDAT FILL-IN-SLUTDAT BTN_FVE-3 BTN_NVE-3 BTN_FVE-4 BTN_NVE-4 
   WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON CHOOSE OF BTN_AVS IN FRAME DIALOG-1 /* Avsluta */
DO:
   FOR EACH tidpers:
      tidpers.REGKOLL = FALSE.
   END.
   APPLY "GO" TO BTN_AVS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON GO OF BTN_AVS IN FRAME DIALOG-1 /* Avsluta */
DO:
    {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT DIALOG-1
ON CHOOSE OF BTN_BORT IN FRAME DIALOG-1 /* Ta bort */
DO:
   {muswait.i}  
   ASSIGN       
   FILL-IN-PKOD = INPUT FILL-IN-PKOD.
   FILL-IN-ENFLE = INPUT FILL-IN-ENFLE.              
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = FILL-IN-PKOD 
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.  
   IF NOT AVAILABLE personaltemp THEN DO:
      MESSAGE "Enhet/Sign" FILL-IN-PKOD "finns inte eller är inaktiv." VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_BORT IN FRAME {&FRAME-NAME}.
   END.     

   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT. 
   IF FILL-IN-STARTDAT > FILL-IN-SLUTDAT THEN DO:
      MESSAGE "Felaktigt angivet datum. Start datum kan ej vara större än slut datum."
      VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-SLUTDAT IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_BORT IN FRAME {&FRAME-NAME}.
   END.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN GODKOLLA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT personaltemp.PERSONALKOD,INPUT FILL-IN-STARTDAT,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN GODKOLLA.P  
      (INPUT personaltemp.PERSONALKOD,INPUT FILL-IN-STARTDAT,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   IF tillochmeddatum NE ? THEN DO:
      IF tillochmeddatum >= FILL-IN-STARTDAT THEN DO:
         MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med"
         tillochmeddatum VIEW-AS ALERT-BOX.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         musz = TRUE.            
         APPLY "ENTRY" TO FILL-IN-STARTDAT IN FRAME {&FRAME-NAME}.
         RETURN.            
      END.            
   END.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN GODKOLLA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT personaltemp.PERSONALKOD,INPUT FILL-IN-SLUTDAT,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN GODKOLLA.P  
      (INPUT personaltemp.PERSONALKOD,INPUT FILL-IN-SLUTDAT,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   IF tillochmeddatum NE ? THEN DO:
      IF tillochmeddatum >= FILL-IN-SLUTDAT THEN DO:
         MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med"
         tillochmeddatum VIEW-AS ALERT-BOX.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         APPLY "ENTRY" TO FILL-IN-SLUTDAT IN FRAME {&FRAME-NAME}.
         RETURN.            
      END.            
   END.
   IF YEAR(FILL-IN-STARTDAT) = YEAR(FILL-IN-SLUTDAT) AND MONTH(FILL-IN-STARTDAT) = MONTH(FILL-IN-SLUTDAT) THEN musz = musz.
   ELSE DO:         
      IF MONTH(FILL-IN-STARTDAT) = 12 THEN DO:
         regdatum = DATE(01,01,YEAR(FILL-IN-STARTDAT) + 1). 
      END.
      ELSE DO:
         regdatum = DATE(MONTH(FILL-IN-STARTDAT) + 1,01,YEAR(FILL-IN-STARTDAT)).
      END.
      REPEAT:   
         IF regdatum > FILL-IN-SLUTDAT THEN LEAVE.
         IF MONTH(regdatum) = MONTH(FILL-IN-SLUTDAT) AND YEAR(regdatum) = YEAR(FILL-IN-SLUTDAT) THEN LEAVE.
         ELSE DO:
            IF Guru.Konstanter:appcon THEN DO:                           
               RUN GODKOLLA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
            END.
            ELSE DO:
               RUN GODKOLLA.P  
               (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
            END.
            IF tillochmeddatum NE ? THEN DO:
               MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med"
               tillochmeddatum VIEW-AS ALERT-BOX.
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN NO-APPLY.
            
            END.
         END.            
         IF MONTH(regdatum) = 12 THEN DO:
            regdatum = DATE(01,01,YEAR(regdatum) + 1). 
         END.
         ELSE DO:
            regdatum = DATE(MONTH(regdatum) + 1,01,YEAR(regdatum)).
         END.   
      END. 
   END.
   IF musz = TRUE THEN DO:
      musz = FALSE.      
      status-mus2 = SESSION:SET-WAIT-STATE("").
      RETURN NO-APPLY.
   END.                            
   ASSIGN
   bdatum = FILL-IN-STARTDAT
   avdatum = FILL-IN-SLUTDAT
   regdatum = FILL-IN-STARTDAT.
   RUN REGVEC.P.
   RUN REGDAG.P.
   ASSIGN
   regdatum = FILL-IN-SLUTDAT.
   RUN REGVEC.P.
   RUN REGDAG.P.
   IF FILL-IN-ENFLE = FALSE THEN DO:
      IF FILL-IN-STARTDAT = FILL-IN-SLUTDAT THEN DO:
         status-mus2 = SESSION:SET-WAIT-STATE("").
         MESSAGE "Startdag och slutdag kan inte vara lika vid flerdygns förättning." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-SLUTDAT IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_BORT IN FRAME {&FRAME-NAME}.
      END.  
   END.
   RUN resbort_UI.
   tidpers.REGKOLL = TRUE.   
   {musarrow.i} 
   IF varifran = 1 THEN
   APPLY "CHOOSE" TO BTN_NAAVB IN FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-3 DIALOG-1
ON CHOOSE OF BTN_FVE-3 IN FRAME DIALOG-1 /* - */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT - 1.      
   IF tillochmeddatum NE ? THEN DO:
      IF tillochmeddatum >= FILL-IN-STARTDAT THEN DO:
         FILL-IN-STARTDAT = tillochmeddatum + 1.
      END.
   END.   
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-4 DIALOG-1
ON CHOOSE OF BTN_FVE-4 IN FRAME DIALOG-1 /* - */
DO: 
   ASSIGN
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT.   
   FILL-IN-SLUTDAT = FILL-IN-SLUTDAT - 1.      
   IF tillochmeddatum NE ? THEN DO:
      IF tillochmeddatum >= FILL-IN-SLUTDAT THEN DO:
         FILL-IN-SLUTDAT = tillochmeddatum + 1.
      END.
   END.   
   DISPLAY FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NAAVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NAAVB DIALOG-1
ON CHOOSE OF BTN_NAAVB IN FRAME DIALOG-1 /* Nästa */
DO:
   RUN nasta_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      APPLY "CHOOSE" TO BTN_AVS.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-3 DIALOG-1
ON CHOOSE OF BTN_NVE-3 IN FRAME DIALOG-1 /* + */
DO:   
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT + 1.        
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-4 DIALOG-1
ON CHOOSE OF BTN_NVE-4 IN FRAME DIALOG-1 /* + */
DO:   
   ASSIGN
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT.   
   FILL-IN-SLUTDAT = FILL-IN-SLUTDAT + 1.        
   DISPLAY FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENFLE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENFLE DIALOG-1
ON LEAVE OF FILL-IN-ENFLE IN FRAME DIALOG-1 /* Endag/Flerdygn */
DO:
   FILL-IN-ENFLE = INPUT FILL-IN-ENFLE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENFLE DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-ENFLE IN FRAME DIALOG-1 /* Endag/Flerdygn */
DO:
   IF INPUT FILL-IN-ENFLE = TRUE THEN FILL-IN-ENFLE = FALSE.
   IF INPUT FILL-IN-ENFLE = FALSE THEN FILL-IN-ENFLE = TRUE.
   DISPLAY FILL-IN-ENFLE WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SLUTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTDAT DIALOG-1
ON LEAVE OF FILL-IN-SLUTDAT IN FRAME DIALOG-1 /* Slutdatum */
DO:
  FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTDAT DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-SLUTDAT IN FRAME DIALOG-1 /* Slutdatum */
DO:
   ASSIGN
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-SLUTDAT.
   RUN AlmanBtn.w.
   FILL-IN-SLUTDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT DIALOG-1
ON LEAVE OF FILL-IN-STARTDAT IN FRAME DIALOG-1 /* Startdatum */
DO:
  FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME DIALOG-1 /* Startdatum */
DO:
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STARTDAT.
   RUN AlmanBtn.w.
   FILL-IN-STARTDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


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
   ASSIGN
   vart = "RES"   
   FILL-IN-ENFLE = FALSE.
   IF varifran = 1 THEN DO:
      regdatum = TODAY.
   END.
   ELSE IF varifran = 2 THEN regdatum = stansdatum.
   RUN REGVEC.P.
   RUN REGDAG.P.
   musz = FALSE.
   FILL-IN-STARTDAT = regdatum.   
   FILL-IN-SLUTDAT = FILL-IN-STARTDAT.      
   DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
   ENABLE FILL-IN-STARTDAT FILL-IN-SLUTDAT BTN_FVE-3 BTN_NVE-3 BTN_FVE-4 BTN_NVE-4 
   WITH FRAME {&FRAME-NAME}.            
   RUN enable_UI.       
   {FRMSIZED.I}
   IF varifran = 1 THEN DO:
      OPEN QUERY BRW_PERS 
      FOR EACH tidpers WHERE USE-INDEX PERSONALKOD NO-LOCK.  
      ENABLE BRW_PERS WITH FRAME {&FRAME-NAME}.
      RELEASE tidpers NO-ERROR.
   END.
   ELSE IF varifran = 2 THEN DO:
      BTN_NAAVB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      BTN_BORT:COLUMN = 16.5.
      BTN_BORT:ROW = 11.
   END.
   RUN nasta_UI. 
   FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR. 
   ASSIGN
   FILL-IN-PKOD = personaltemp.PERSONALKOD.
   DISPLAY FILL-IN-PKOD WITH FRAME {&FRAME-NAME}.
   ASSIGN 
   my1hand = FILL-IN-PKOD:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BRW_PERS:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = BRW_PERS:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = FILL-IN-ENFLE:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = FILL-IN-ENFLE:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = FILL-IN-STARTDAT:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = FILL-IN-STARTDAT:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BTN_NVE-3:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = BTN_NVE-3:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BTN_FVE-3:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = BTN_FVE-3:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = FILL-IN-SLUTDAT:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = FILL-IN-SLUTDAT:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BTN_NVE-4:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = BTN_NVE-4:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BTN_FVE-4:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = BTN_FVE-4:HANDLE IN FRAME {&FRAME-NAME}.
   IF varifran = 2 THEN DO:
      BRW_PERS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
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
   tidpers.PERSONALKOD:READ-ONLY IN BROWSE {&BROWSE-NAME} = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_PERS:HANDLE IN FRAME {&FRAME-NAME}).   
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
  DISPLAY FILL-IN-PKOD FILL-IN-ENFLE 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-ENFLE BTN_NAAVB BTN_BORT BTN_AVS 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nastapers_UI DIALOG-1 
PROCEDURE nastapers_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN GODKOLLA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT personaltemp.PERSONALKOD,INPUT FILL-IN-STARTDAT,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN GODKOLLA.P  
      (INPUT personaltemp.PERSONALKOD,INPUT FILL-IN-STARTDAT,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   IF tillochmeddatum NE ? THEN DO:
      IF tillochmeddatum >= FILL-IN-STARTDAT THEN DO:
         FILL-IN-STARTDAT = tillochmeddatum + 1.
      END.
   END.  
   RUN REGVEC.P.
   RUN REGDAG.P.
   FILL-IN-SLUTDAT = regdatum.
   DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
   ENABLE FILL-IN-STARTDAT FILL-IN-SLUTDAT BTN_FVE-3 BTN_NVE-3 BTN_FVE-4 BTN_NVE-4 
   WITH FRAME {&FRAME-NAME}.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nasta_UI DIALOG-1 
PROCEDURE nasta_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/     
   IF valgubbe = FALSE THEN DO:  
      IF NOT AVAILABLE tidpers THEN DO:
         FIND FIRST tidpers WHERE tidpers.PERSONALKOD = pkod NO-LOCK NO-ERROR.
         persrec = tidpers.TIDPERSREC.
      END.
      ELSE DO:
         FIND NEXT tidpers WHERE tidpers.REGKOLL = FALSE USE-INDEX PERSONALKOD NO-ERROR.   
      END.
      IF NOT AVAILABLE tidpers THEN DO:
         FIND FIRST tidpers WHERE tidpers.REGKOLL = FALSE USE-INDEX PERSONALKOD NO-ERROR.   
         IF NOT AVAILABLE tidpers THEN DO:
            musz = TRUE.
            RETURN.
         END.
      END.   
   END.   
   IF AVAILABLE tidpers THEN DO:
      ASSIGN
      FILL-IN-PKOD = tidpers.PERSONALKOD
      pkod = tidpers.PERSONALKOD
      persrec = tidpers.TIDPERSREC
      persrec2 = RECID(tidpers).   
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidpers)).
   END.
   FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod 
   NO-LOCK NO-ERROR.
   RUN nastapers_UI.
   IF musz = TRUE THEN musz = FALSE.
   ELSE DO:
      DISPLAY BRW_PERS WITH FRAME {&FRAME-NAME}.
      RUN lastselectdyn_UI IN brwproc[1].
   END.
   {musarrow.i}  
   DISPLAY FILL-IN-PKOD FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
   ENABLE FILL-IN-STARTDAT FILL-IN-SLUTDAT BTN_FVE-3 BTN_NVE-3 BTN_FVE-4 BTN_NVE-4 
   WITH FRAME {&FRAME-NAME}.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resbort_UI DIALOG-1 
PROCEDURE resbort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*BORTRES.P */         
   ASSIGN 
   bdatumspar = bdatum
   avdatumspar = avdatum. 
   IF FILL-IN-ENFLE = TRUE THEN enfle = "Endag".
   IF FILL-IN-ENFLE = FALSE THEN enfle = "Flerdag".
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN RESBAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT personaltemp.PERSONALKOD,INPUT bdatum,INPUT avdatum,INPUT enfle).
   END.
   ELSE DO:
      RUN RESBAPP.P
      (INPUT personaltemp.PERSONALKOD,INPUT bdatum,INPUT avdatum,INPUT enfle).
   END.    
   IF FILL-IN-ENFLE = TRUE THEN 
   MESSAGE "Du har nu tagit bort alla endags rese registreringar mellan" SKIP
         bdatumspar "och" avdatumspar VIEW-AS ALERT-BOX.
   IF FILL-IN-ENFLE = FALSE THEN 
   MESSAGE "Du har nu tagit bort alla flerdygns rese registreringar mellan" SKIP
         bdatumspar "OCH" avdatumspar VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

