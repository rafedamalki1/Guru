&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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
DEFINE INPUT PARAMETER valvardnr AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED 
{FASTIGHET.I}  
{MARKVARD.I}
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vardrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE kabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE fastvrec AS RECID NO-UNDO.
DEFINE VARIABLE stlpnr AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE kollkr AS INTEGER NO-UNDO.
DEFINE VARIABLE imon AS INTEGER NO-UNDO.
DEFINE VARIABLE iyr AS INTEGER NO-UNDO.
DEFINE VARIABLE bmon AS INTEGER NO-UNDO.
DEFINE VARIABLE byr AS INTEGER NO-UNDO.
DEFINE VARIABLE ersatt AS INTEGER NO-UNDO.
DEFINE VARIABLE lplus AS INTEGER NO-UNDO.
DEFINE VARIABLE ivvarde AS INTEGER NO-UNDO.
DEFINE VARIABLE bplus AS INTEGER NO-UNDO.
DEFINE VARIABLE eh AS INTEGER NO-UNDO.
DEFINE VARIABLE yh AS INTEGER NO-UNDO.
DEFINE VARIABLE yf AS INTEGER NO-UNDO.
DEFINE VARIABLE yk AS INTEGER NO-UNDO.
DEFINE VARIABLE ykoll AS INTEGER NO-UNDO.
DEFINE VARIABLE ymin AS INTEGER NO-UNDO.
DEFINE VARIABLE btillagg AS INTEGER NO-UNDO.
DEFINE VARIABLE ltillagg AS INTEGER NO-UNDO.
DEFINE VARIABLE eb AS INTEGER NO-UNDO.
DEFINE VARIABLE emin AS INTEGER NO-UNDO.
DEFINE VARIABLE omraknkpiapph AS HANDLE NO-UNDO.
DEFINE VARIABLE fastighbet AS CHARACTER NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOG_ALLA CMB_FAST FILL-IN_VARDETID BTN_REG ~
BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_VARDNR FILL-IN_BENAMNING TOG_ALLA ~
CMB_FAST FILL-IN_VARDETID 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_REG 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_FAST AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Fastighet" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_BENAMNING AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 35.25 BY 1.

DEFINE VARIABLE FILL-IN_VARDETID AS CHARACTER FORMAT "X(4)" 
     LABEL "V‰rdetidpkt ≈≈MM" 
     VIEW-AS FILL-IN 
     SIZE 6.25 BY 1.

DEFINE VARIABLE FILL-IN_VARDNR AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "V‰rdering nr" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE TOG_ALLA AS LOGICAL INITIAL no 
     LABEL "Alla fastigheter" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.38 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_VARDNR AT ROW 1.58 COL 17.38 COLON-ALIGNED
     FILL-IN_BENAMNING AT ROW 1.58 COL 28.75 COLON-ALIGNED NO-LABEL
     TOG_ALLA AT ROW 2.92 COL 19.38
     CMB_FAST AT ROW 2.92 COL 48 COLON-ALIGNED
     FILL-IN_VARDETID AT ROW 5 COL 17.38 COLON-ALIGNED
     BTN_REG AT ROW 6.58 COL 37
     BTN_AVS AT ROW 6.58 COL 52
     SPACE(1.12) SKIP(0.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "ƒndra konsumentprisindex":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_BENAMNING IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_VARDNR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* ƒndra konsumentprisindex */
DO:
   IF VALID-HANDLE(omraknkpiapph) THEN DELETE PROCEDURE omraknkpiapph.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* ƒndra konsumentprisindex */
DO:
   IF VALID-HANDLE(omraknkpiapph) THEN DELETE PROCEDURE omraknkpiapph.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON CHOOSE OF BTN_AVS IN FRAME DIALOG-1 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   {muswait.i}    
   ASSIGN   
   musz = FALSE      
   TOG_ALLA CMB_FAST FILL-IN_VARDETID  
   TOG_ALLA = INPUT TOG_ALLA  
   CMB_FAST = INPUT CMB_FAST     
   FILL-IN_VARDETID = INPUT FILL-IN_VARDETID.       
   IF FILL-IN_VARDETID = "" THEN DO:
      MESSAGE "Ange Âr och mÂnad med formatet ≈≈MM." VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN_VARDETID IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.   
   imon = INTEGER(SUBSTR(FILL-IN_VARDETID,3,2)).      
   iyr = INTEGER(SUBSTRING(FILL-IN_VARDETID,1,2)). 
   IF iyr > 50 THEN ASSIGN  byr = iyr + 1900.
   ELSE ASSIGN  byr = iyr + 2000.  
   FIND FIRST kindextemp WHERE kindextemp.AR = byr AND kindextemp.MANAD = imon USE-INDEX AR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE kindextemp THEN DO:
      FIND LAST kindextemp WHERE kindextemp.AR = byr AND kindextemp.VARDE > 0 USE-INDEX AR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kindextemp THEN DO:
         iyr = iyr - 1.
         byr = byr - 1.
         FIND LAST kindextemp WHERE kindextemp.AR = byr AND kindextemp.VARDE > 0 USE-INDEX AR NO-LOCK NO-ERROR.
         IF AVAILABLE kindextemp THEN DO:
            bmon = kindextemp.MANAD.
            ASSIGN FILL-IN_VARDETID = STRING(iyr) + STRING(bmon).        
            MESSAGE "Senaste uppdaterade v‰rdetidpunkt ‰r" FILL-IN_VARDETID  VIEW-AS ALERT-BOX. 
         END.
      END.
      ELSE DO:          
         bmon = kindextemp.MANAD.
         ASSIGN FILL-IN_VARDETID = STRING(iyr,"99") + STRING(bmon,"99").
         /*DISPLAY FILL-IN_VARDETID WITH FRAME {&FRAME-NAME}.*/
         MESSAGE "Senaste uppdaterade v‰rdetidpunkt ‰r" FILL-IN_VARDETID  VIEW-AS ALERT-BOX. 
      END.   
   END.      
   imon = INTEGER(SUBSTR(FILL-IN_VARDETID,3,2)).      
   iyr = INTEGER(SUBSTRING(FILL-IN_VARDETID,1,2)). 
   IF iyr > 50 THEN ASSIGN  iyr = iyr + 1900.
   ELSE ASSIGN  iyr = iyr + 2000.           
   FIND fastvardtemp WHERE RECID(fastvardtemp) = fastvrec NO-LOCK NO-ERROR.
   OPEN QUERY akq FOR EACH akervardtemp WHERE akervardtemp.VARDNR = varderingtemp.VARDNR USE-INDEX VARDNR NO-LOCK,
   EACH fastighettemp WHERE fastighettemp.BETECKNING = akervardtemp.BETECKNING USE-INDEX FAST NO-LOCK.
   GET FIRST akq NO-LOCK.
   DO WHILE AVAILABLE (akervardtemp):
      ASSIGN 
      valvardnr = akervardtemp.VARDNR
      fastighbet = akervardtemp.BETECKNING
      stlpnr = akervardtemp.STOLPNR.
      IF akervardtemp.FASTPRIS = TRUE THEN musz = TRUE.
      ELSE IF TOG_ALLA = TRUE THEN musz = FALSE.
      ELSE IF akervardtemp.FASTPRIS = TRUE THEN musz = TRUE.
      ELSE IF akervardtemp.BETECKNING = fastvardtemp.BETECKNING THEN musz = FALSE.
      ELSE  musz = TRUE.
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF TOG_ALLA = TRUE THEN ASSIGN akervardtemp.VARDEPKT = DATE(imon,01,iyr).
         ELSE IF akervardtemp.BETECKNING = fastvardtemp.BETECKNING THEN ASSIGN akervardtemp.VARDEPKT = DATE(imon,01,iyr).
      END.
      ELSE DO:
         ASSIGN akervardtemp.VARDEPKT = DATE(imon,01,iyr).
         FIND FIRST kindextemp WHERE kindextemp.AR = YEAR(akervardtemp.VARDEPKT) AND
         kindextemp.MANAD = MONTH(akervardtemp.VARDEPKT) USE-INDEX AR NO-LOCK NO-ERROR.
         ivvarde = kindextemp.VARDE.
         FIND FIRST markdivtemp NO-LOCK NO-ERROR.
         IF akervardtemp.SORT = "≈KER" OR akervardtemp.SORT = "BETESMARK" OR akervardtemp.SORT = "IMPEDIMENT" THEN DO:
            IF akervardtemp.L2 LE 7 THEN DO:
               FIND FIRST prodakertemp WHERE prodakertemp.PRODOMR = fastighettemp.PAKER AND
               prodakertemp.BREDD = akervardtemp.L2 USE-INDEX OMR NO-LOCK NO-ERROR.
            END.
            ELSE DO:
               FIND FIRST prodakertemp WHERE prodakertemp.PRODOMR = fastighettemp.PAKER AND
               prodakertemp.BREDD = 7 USE-INDEX OMR NO-LOCK NO-ERROR.
               ASSIGN bplus = akervardtemp.L2 - 7.
            END.
            ersatt = 0.
            IF akervardtemp.L1 = 1 THEN ASSIGN ersatt = prodakertemp.L1.
            ELSE IF akervardtemp.L1 = 2 THEN ASSIGN ersatt = prodakertemp.L2.
            ELSE IF akervardtemp.L1 = 3 THEN ASSIGN ersatt = prodakertemp.L3.
            ELSE IF akervardtemp.L1 = 4 THEN ASSIGN ersatt = prodakertemp.L4.
            ELSE IF akervardtemp.L1 = 5 THEN ASSIGN ersatt = prodakertemp.L5.
            ELSE IF akervardtemp.L1 = 6 THEN ASSIGN ersatt = prodakertemp.L6.
            ELSE IF akervardtemp.L1 = 7 THEN ASSIGN ersatt = prodakertemp.L7.
            ELSE IF akervardtemp.L1 = 8 THEN ASSIGN ersatt = prodakertemp.L8.
            ELSE IF akervardtemp.L1 = 9 THEN ASSIGN ersatt = prodakertemp.L9.
            ELSE IF akervardtemp.L1 = 10 THEN ASSIGN ersatt = prodakertemp.L10.
            ELSE IF akervardtemp.L1 = 11 THEN ASSIGN ersatt = prodakertemp.L11.
            ELSE IF akervardtemp.L1 = 12 THEN ASSIGN ersatt = prodakertemp.L12.
            ELSE IF akervardtemp.L1 = 13 THEN ASSIGN ersatt = prodakertemp.L13.
            ELSE IF akervardtemp.L1 = 14 THEN ASSIGN ersatt = prodakertemp.L14.
            ELSE IF akervardtemp.L1 = 15 THEN ASSIGN ersatt = prodakertemp.L15.
            ELSE IF akervardtemp.L1 = 16 THEN ASSIGN ersatt = prodakertemp.L16.
            ELSE IF akervardtemp.L1 = 17 THEN ASSIGN ersatt = prodakertemp.L17.
            ELSE IF akervardtemp.L1 = 18 THEN ASSIGN ersatt = prodakertemp.L18.
            ELSE IF akervardtemp.L1 = 19 THEN ASSIGN ersatt = prodakertemp.L19.
            ELSE IF akervardtemp.L1 = 20 THEN ASSIGN ersatt = prodakertemp.L20.
            ELSE DO:
              ASSIGN lplus = akervardtemp.L1 - 20.
            END.  
            IF Guru.Konstanter:globforetag = "umea" THEN DO:                                            
               /* pristidpunkt 200603*/
               IF prodakertemp.PRODOMR = "NN" THEN DO:
                  ltillagg = 705.
                  btillagg = 470.
               END.
               IF prodakertemp.PRODOMR = "N÷" THEN DO:
                  ltillagg = 665.
                  btillagg = 443.
               END. 
               IF prodakertemp.PRODOMR = "GSS" THEN DO:
                  ltillagg = 1094.
                  btillagg = 730.
               END.
               IF prodakertemp.PRODOMR = "GMB" THEN DO:
                  ltillagg = 1008.
                  btillagg = 672.
               END.
               IF prodakertemp.PRODOMR = "GNS" THEN DO:
                  ltillagg = 943.
                  btillagg = 629.
               END.
               IF prodakertemp.PRODOMR = "GSK" THEN DO:
                  ltillagg = 765.
                  btillagg = 510.
               END.              
               IF prodakertemp.PRODOMR = "NO" THEN DO:
                  ltillagg = 665.
                  btillagg = 443.
               END.
               IF prodakertemp.PRODOMR = "SS" THEN DO:
                  ltillagg = 952.
                  btillagg = 634.
               END.
               IF prodakertemp.PRODOMR = "SSk" THEN DO:
                  ltillagg = 780.
                  btillagg = 520.
               END.                           
            END.
            ELSE DO:
               /*VATTENFALL HAR KOMMIT MED NYA SIFFROR I vp2015 SOM ALLA UTOM UME≈ fÂr inlagda */            
               /* pristidpunkt 1501*/
               IF prodakertemp.PRODOMR = "NN" THEN DO:
                  ltillagg = 729.
                  btillagg = 498.
               END.
               IF prodakertemp.PRODOMR = "N÷" THEN DO:
                  ltillagg = 669.
                  btillagg = 485.
               END. 
               IF prodakertemp.PRODOMR = "GSS" THEN DO:
                  ltillagg = 1175.
                  btillagg = 747.
               END.
               IF prodakertemp.PRODOMR = "GMB" THEN DO:
                  ltillagg = 1065.
                  btillagg = 758.
               END.
               IF prodakertemp.PRODOMR = "GNS" THEN DO:
                  ltillagg = 990.
                  btillagg = 643.
               END.
               IF prodakertemp.PRODOMR = "GSK" THEN DO:
                  ltillagg = 797.
                  btillagg = 525.
               END.              
       
               IF prodakertemp.PRODOMR = "SS" THEN DO:
                  ltillagg = 1000.
                  btillagg = 672.
               END.
               IF prodakertemp.PRODOMR = "SSk" THEN DO:
                  ltillagg = 820.
                  btillagg = 546.
               END.                           
            END.          
           
            IF bplus > 0 THEN ASSIGN ersatt = ersatt + bplus * btillagg.
            IF lplus > 0 THEN ASSIGN ersatt = ersatt + lplus * ltillagg.
            ASSIGN eh = ersatt * ivvarde / markdivtemp.IPVARDE.            
            FIND FIRST akerproctemp WHERE akerproctemp.PRODOMR = fastighettemp.PAKER AND
            akerproctemp.L3MIN LE akervardtemp.L3 AND akerproctemp.L3MAX GE akervardtemp.L3
            USE-INDEX PRODOMR NO-LOCK NO-ERROR.
            IF akervardtemp.SORT = "≈KER" THEN DO:
               IF AVAILABLE akerproctemp THEN DO:
                  ASSIGN akervardtemp.KR = eh * akerproctemp.PROCENT / 100.
               END.
            END.
            ELSE IF akervardtemp.SORT = "BETESMARK" THEN DO:
               ASSIGN akervardtemp.KR = eh * 15 / 100.
            END.
            ELSE  IF akervardtemp.SORT = "IMPEDIMENT" THEN DO:
               ASSIGN akervardtemp.KR = eh * 10 / 100.
            END.
         END.         
        
      END.
      RUN btnreg IN omraknkpiapph (INPUT valvardnr,INPUT fastighbet,INPUT stlpnr,INPUT TABLE akervardtemp).
      GET NEXT akq NO-LOCK.
   END.
   CLOSE QUERY akq.
   
   {musarrow.i}   
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON GO OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   IF VALID-HANDLE(omraknkpiapph) THEN DELETE PROCEDURE omraknkpiapph NO-ERROR.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_FAST DIALOG-1
ON VALUE-CHANGED OF CMB_FAST IN FRAME DIALOG-1 /* Fastighet */
DO:
  ASSIGN CMB_FAST = INPUT CMB_FAST.
  FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR = varderingtemp.VARDNR AND 
  fastvardtemp.BETECKNING = CMB_FAST USE-INDEX VARDNR NO-LOCK NO-ERROR.
  fastvrec = RECID(fastvardtemp). 
  FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = varderingtemp.VARDNR AND
  akervardtemp.BETECKNING =  fastvardtemp.BETECKNING USE-INDEX VARDNR NO-LOCK NO-ERROR.    
  IF AVAILABLE akervardtemp THEN DO:
     ASSIGN FILL-IN_VARDETID = SUBSTRING(STRING(akervardtemp.VARDEPKT),1,2) + SUBSTRING(STRING(akervardtemp.VARDEPKT),4,2).
     DISPLAY FILL-IN_VARDETID WITH FRAME {&FRAME-NAME}.
  END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_VARDETID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_VARDETID DIALOG-1
ON LEAVE OF FILL-IN_VARDETID IN FRAME DIALOG-1 /* V‰rdetidpkt ≈≈MM */
DO:
  FILL-IN_VARDETID = INPUT FILL-IN_VARDETID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ALLA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ALLA DIALOG-1
ON VALUE-CHANGED OF TOG_ALLA IN FRAME DIALOG-1 /* Alla fastigheter */
DO:
  TOG_ALLA = INPUT TOG_ALLA.
  IF TOG_ALLA = FALSE THEN DO:
     CMB_FAST:HIDDEN = FALSE.
  END.
  ELSE ASSIGN CMB_FAST:HIDDEN = TRUE.  
  FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = varderingtemp.VARDNR USE-INDEX VARDNR NO-LOCK NO-ERROR.    
  IF AVAILABLE akervardtemp THEN DO:
     ASSIGN FILL-IN_VARDETID = SUBSTRING(STRING(akervardtemp.VARDEPKT),1,2) + SUBSTRING(STRING(akervardtemp.VARDEPKT),4,2).
     DISPLAY FILL-IN_VARDETID WITH FRAME {&FRAME-NAME}.
  END.   
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

   FIND FIRST varderingtemp WHERE varderingtemp.VARDNR = valvardnr NO-LOCK NO-ERROR.
   IF AVAILABLE varderingtemp THEN vardrec = RECID(varderingtemp).
   FIND varderingtemp WHERE RECID(varderingtemp) = vardrec NO-LOCK NO-ERROR.   
   ASSIGN FILL-IN_VARDNR = varderingtemp.VARDNR
   FILL-IN_BENAMNING = varderingtemp.BENAMNING.   
   OPEN QUERY fq FOR EACH fastvardtemp WHERE fastvardtemp.VARDNR = varderingtemp.VARDNR USE-INDEX VARDNR NO-LOCK.
   GET FIRST fq NO-LOCK.
   DO WHILE AVAILABLE(fastvardtemp):
      status-ok = CMB_FAST:ADD-LAST(fastvardtemp.BETECKNING).
      GET NEXT fq NO-LOCK.
   END. 
   FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR = varderingtemp.VARDNR USE-INDEX VARDNR NO-LOCK NO-ERROR.
   ASSIGN  CMB_FAST:SCREEN-VALUE = fastvardtemp.BETECKNING.
   fastvrec = RECID(fastvardtemp). 
   RUN enable_UI.       
   {FRMSIZED.I}   
   ASSIGN TOG_ALLA = TRUE.
   CMB_FAST:HIDDEN = TRUE.
   FIND FIRST akervardtemp WHERE akervardtemp.VARDNR = varderingtemp.VARDNR AND
   akervardtemp.BETECKNING =  fastvardtemp.BETECKNING AND akervardtemp.VARDEPKT NE ? 
    USE-INDEX VARDNR NO-LOCK NO-ERROR.    
   IF AVAILABLE akervardtemp THEN DO:
      ASSIGN FILL-IN_VARDETID = SUBSTRING(STRING(akervardtemp.VARDEPKT),1,2) + SUBSTRING(STRING(akervardtemp.VARDEPKT),4,2).
      DISPLAY FILL-IN_VARDETID WITH FRAME {&FRAME-NAME}.
   END.   
   DISPLAY TOG_ALLA WITH FRAME {&FRAME-NAME}.
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
   IF Guru.Konstanter:appcon THEN DO:
      RUN OMRAKNKPIAPP.P PERSISTENT SET omraknkpiapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN OMRAKNKPIAPP.P PERSISTENT SET omraknkpiapph.
   END.

   tthandle = TEMP-TABLE kindextemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KINDEX", INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KINDEX", INPUT "").
   END.
   tthandle = TEMP-TABLE varderingtemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "VARDERING", INPUT "").
   tthandle = TEMP-TABLE akervardtemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "AKERVARD", INPUT "").
   tthandle = TEMP-TABLE fastvardtemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "FASTVARD", INPUT "").
   tthandle = TEMP-TABLE prodakertemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "PRODAKER", INPUT "").
   tthandle = TEMP-TABLE akerproctemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "AKERPROC", INPUT "").
   tthandle = TEMP-TABLE markdivtemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "MARKDIV", INPUT "").
   tthandle = TEMP-TABLE fastighettemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "FASTIGHET", INPUT "").


   
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
  DISPLAY FILL-IN_VARDNR FILL-IN_BENAMNING TOG_ALLA CMB_FAST FILL-IN_VARDETID 
      WITH FRAME DIALOG-1.
  ENABLE TOG_ALLA CMB_FAST FILL-IN_VARDETID BTN_REG BTN_AVS 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

