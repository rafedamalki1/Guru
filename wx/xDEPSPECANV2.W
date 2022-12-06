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

  Created: 10/15/96 -  1:50 pm

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
DEFINE VAR lagervar AS INTEGER NO-UNDO.


                              
DEFINE NEW SHARED VARIABLE globanv AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE globanvavdnr AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE globanvpkod AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE globallm AS LOGICAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE globniv AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE globallpers AS LOGICAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE globallao AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE globavd AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE globomr AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE globlos AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE globpersnamn AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE globforetag AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE globsidl AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE globsids AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE plusaonr AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE plusdnr AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE plusrec AS RECID  NO-UNDO.
DEFINE NEW SHARED VARIABLE plustidrec AS RECID  NO-UNDO.
DEFINE NEW SHARED VARIABLE plustid AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE plusdval AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE succelval AS LOGICAL NO-UNDO. 

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE ReturnValue AS INTEGER.
DEFINE NEW GLOBAL SHARED VARIABLE hpApi AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE hpWinFunc AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE retvalkoll AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE tth AS HANDLE NO-UNDO.
DEFINE VARIABLE brwproc AS HANDLE EXTENT 25 NO-UNDO.
DEFINE VARIABLE handproc AS HANDLE EXTENT 25 NO-UNDO.
DEFINE VARIABLE leverant AS CHARACTER NO-UNDO.
/*DEFINE NEW SHARED VARIABLE tth AS HANDLE NO-UNDO.*/
DEFINE VARIABLE sok AS LOGICAL NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE numrows AS INTEGER NO-UNDO.
/*DEFINE VARIABLE brwsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE multitid AS INTEGER NO-UNDO.*/
/*DEFINE VARIABLE multibrwsok AS CHARACTER NO-UNDO.*/
DEFINE VARIABLE anyprintquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE forstaenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE delquery1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE delquery2 AS CHARACTER NO-UNDO.
/*DEFINE VARIABLE orgfraga AS CHARACTER NO-UNDO.*/
/*DEFINE VARIABLE rowvar AS ROWID NO-UNDO.*/
DEFINE VARIABLE kollenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE sprow AS ROWID      NO-UNDO.
DEFINE VARIABLE uppdat AS LOGICAL NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE spec_rowid AS ROWID NO-UNDO.

&Scoped-define NEW NEW 

DEFINE {&NEW} SHARED VARIABLE varforetypval AS INTEGER EXTENT 100 NO-UNDO.     
DEFINE {&NEW} SHARED VARIABLE varforetypchar AS CHARACTER EXTENT 100 NO-UNDO.     
DEFINE {&NEW} SHARED TEMP-TABLE felmeddtemp 
  FIELD FELMEDD AS CHARACTER
  FIELD VAL AS INTEGER.
DEFINE {&NEW} SHARED TEMP-TABLE xsektemp
  FIELD AV-LEVEL AS INTEGER
  FIELD MENYVART AS CHARACTER
  FIELD SEK AS LOGICAL EXTENT 20
  INDEX XSEK IS PRIMARY MENYVART AV-LEVEL.
DEFINE {&NEW} SHARED VARIABLE appcon AS LOGICAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE apphand AS HANDLE NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE appfel AS LOGICAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE gavdl AS CHARACTER NO-UNDO. /*AVDELNING LÅNG*/
DEFINE {&NEW} SHARED VARIABLE gavdk AS CHARACTER NO-UNDO.     /*AVDELNING KORT*/
DEFINE {&NEW} SHARED VARIABLE gomrl AS CHARACTER NO-UNDO.  /*OMRÅDE LÅNG*/
DEFINE {&NEW} SHARED VARIABLE gomrk AS CHARACTER NO-UNDO.      /*OMRÅDE KORT*/
DEFINE {&NEW} SHARED VARIABLEgaol AS CHARACTER NO-UNDO.   /*AONR LÅNG*/
DEFINE {&NEW} SHARED VARIABLEgaok AS CHARACTER NO-UNDO.       /*AONR KORT*/   
DEFINE {&NEW} SHARED VARIABLEgpll AS CHARACTER NO-UNDO.  /*PLANNR LÅNG*/
DEFINE {&NEW} SHARED VARIABLEgplk AS CHARACTER NO-UNDO.     /*PLANNR KORT*/
DEFINE {&NEW} SHARED VARIABLEgenl AS CHARACTER NO-UNDO. /*E-NUMMER LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLEgenk AS CHARACTER NO-UNDO.  /*E-NUMMER KORT*/
DEFINE {&NEW} SHARED VARIABLE gjul AS CHARACTER NO-UNDO. /*JURIDISK LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLEgjuk AS CHARACTER NO-UNDO.  /*JURIDISK KORT*/
DEFINE {&NEW} SHARED VARIABLEgfastl AS CHARACTER NO-UNDO. /*FASTA LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE gfastk AS CHARACTER NO-UNDO.     /*FASTA KORT*/
DEFINE {&NEW} SHARED VARIABLE gtilll AS CHARACTER NO-UNDO. /*TILLFÄLIGA LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE gtillk AS CHARACTER NO-UNDO.     /*TILLFÄLIGA KORT*/
DEFINE {&NEW} SHARED VARIABLEgberel AS CHARACTER NO-UNDO. /*BEREDARE LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLEgberek AS CHARACTER NO-UNDO.     /*BEREDARE KORT*/
DEFINE {&NEW} SHARED VARIABLE gprojl AS CHARACTER NO-UNDO. /*PROJEKTÖR LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE gprojk AS CHARACTER NO-UNDO.     /*PROJEKTÖR KORT*/
DEFINE {&NEW} SHARED VARIABLE garbal AS CHARACTER NO-UNDO. /*ARBETSANSVARIG LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE garbak AS CHARACTER NO-UNDO.     /*ARBETSANSVARIG KORT*/
DEFINE {&NEW} SHARED VARIABLE gutfk AS CHARACTER NO-UNDO.     /*Utförande KORT*/
DEFINE {&NEW} SHARED VARIABLE gutfl AS CHARACTER NO-UNDO. /*Utförande LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE gbestk AS CHARACTER NO-UNDO.     /*Beställare/Kund KORT*/
DEFINE {&NEW} SHARED VARIABLE gbestl AS CHARACTER NO-UNDO. /*Beställare/Kund LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE gdebk AS CHARACTER NO-UNDO.     /*Debiterigstyp KORT*/
DEFINE {&NEW} SHARED VARIABLE gdebl AS CHARACTER NO-UNDO.  /*Debiterigstyp LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE gtidlk AS CHARACTER NO-UNDO.     /*tidläge KORT*/
DEFINE {&NEW} SHARED VARIABLE gtidll AS CHARACTER NO-UNDO.  /*tidläge LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE gutfardk AS CHARACTER NO-UNDO.     /*utfärdat av KORT*/
DEFINE {&NEW} SHARED VARIABLE gutfardl AS CHARACTER NO-UNDO.  /*utfärdat av LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE grefbefk AS CHARACTER NO-UNDO.     /*Ref.nr beställare KORT*/
DEFINE {&NEW} SHARED VARIABLE grefbefl AS CHARACTER NO-UNDO.  /*Ref.nr beställare  LÅNG*/ 


&Scoped-define SHARED SHARED
DEFINE {&NEW} {&SHARED} TEMP-TABLE mtrltemp NO-UNDO
   FIELD Enr     AS  CHARACTER  FORMAT "x(11)" LABEL "Enr"                         
   FIELD Benamning     AS CHARACTER FORMAT "x(40)" LABEL "Benämning"                            
   FIELD Bestkvant     AS  DECIMAL FORMAT "->>,>>9.99" INITIAL 0                          
   FIELD Enhet AS  CHARACTER    FORMAT "x(5)"                    
   FIELD BERKVANT    AS  DECIMAL FORMAT "->>,>>9.99" INITIAL 0
   FIELD SUMMA     AS  DECIMAL FORMAT ">>>>99.99" INITIAL 0
   FIELD LEVKOD     AS  CHARACTER FORMAT "x(4)"
   FIELD BPRIS     AS  DECIMAL FORMAT ">>>>99.99" INITIAL 0 LABEL "Bruttopris"
   FIELD NPRIS     AS  DECIMAL FORMAT ">>>>99.99" INITIAL 0 LABEL "Nettopris"
   FIELD KALKNR    AS  INTEGER FORMAT ">,>>>,>>9" INITIAL 0
   FIELD BERLEV     AS  CHARACTER  FORMAT "x(4)"                           
   FIELD OFFERT     AS  LOGICAL FORMAT "Ja/Nej" INITIAL NO
   FIELD KPRIS     AS  DECIMAL   FORMAT ">>>>99.99" INITIAL 0
   FIELD KUND     AS  LOGICAL  FORMAT "Ja/Nej" INITIAL NO
   FIELD MTRLROW AS ROWID
   INDEX BEN LEVKOD KALKNR BENAMNING
   INDEX Enr IS PRIMARY enr
   INDEX Lev LEVKOD KALKNR enr.


DEFINE {&NEW} {&SHARED} TEMP-TABLE list_mtrl NO-UNDO 
   FIELD NUM AS INTEGER     
   FIELD KTYPKOD AS CHARACTER
   FIELD ENR AS CHARACTER FORMAT "x(11)" LABEL "Enr"    
   FIELD BENAMNING AS CHARACTER FORMAT "x(40)" LABEL "Benämning"   
   FIELD ENHET AS CHARACTER FORMAT "x(5)" LABEL "Enhet" 
   FIELD ANTAL AS INTEGER FORMAT ">>>>>9" LABEL "Antal"
   FIELD PRIS AS DECIMAL
   FIELD LEVKOD AS CHARACTER
   FIELD LINKAB AS LOGICAL FORMAT "Ja/Nej" LABEL "Lin/Kab"
   FIELD MODUL AS INTEGER 
   FIELD SKAPNUM AS INTEGER 
   FIELD TYPBER AS LOGICAL
   FIELD SKAPMTRL AS LOGICAL
   FIELD SKAPMODUL AS INTEGER
   FIELD DIAMETER AS INTEGER
   FIELD MTRLTEXT AS CHARACTER
   FIELD SATS AS LOGICAL
   FIELD PAR AS INTEGER
   FIELD PAR2 AS CHARACTER FORMAT "x(1)" LABEL "P"    
   INDEX ENR IS PRIMARY ENR ASCENDING
   INDEX NUM NUM ENR ASCENDING
   INDEX NUM2 NUM SKAPNUM ASCENDING. 

DEFINE TEMP-TABLE emtrltemp NO-UNDO LIKE mtrltemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_MTRL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES list_mtrl mtrltemp

/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL list_mtrl.Enr list_mtrl.Benamning ~
list_mtrl.Enhet list_mtrl.PRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL 
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH list_mtrl NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH list_mtrl NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL list_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL list_mtrl


/* Definitions for BROWSE BRW_VLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.BPRIS mtrltemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VLEV mtrltemp.Enr ~
mtrltemp.Benamning mtrltemp.Enhet mtrltemp.BPRIS mtrltemp.NPRIS 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VLEV mtrltemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VLEV mtrltemp
&Scoped-define QUERY-STRING-BRW_VLEV FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = leverant ~
 AND mtrltemp.KALKNR = 0 NO-LOCK ~
    BY mtrltemp.Enr
&Scoped-define OPEN-QUERY-BRW_VLEV OPEN QUERY BRW_VLEV FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = leverant ~
 AND mtrltemp.KALKNR = 0 NO-LOCK ~
    BY mtrltemp.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_VLEV mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VLEV mtrltemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-57 FBTN_OK FBTN_AVB BRW_VLEV BRW_MTRL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON FBTN_AVB 
     LABEL "Avbryt":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_OK 
     LABEL "Ok":L 
     SIZE 12 BY 1.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 124.5 BY .5
     BGCOLOR 1 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_MTRL FOR 
      list_mtrl SCROLLING.

DEFINE QUERY BRW_VLEV FOR 
      mtrltemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-1 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      list_mtrl.Enr FORMAT "X(11)":U
      list_mtrl.Benamning FORMAT "x(256)":U WIDTH 30
      list_mtrl.Enhet FORMAT "x(5)":U
      list_mtrl.PRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 58.38 BY 9.63
         TITLE "Vald materiel".

DEFINE BROWSE BRW_VLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VLEV WINDOW-1 _STRUCTURED
  QUERY BRW_VLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(256)":U WIDTH 40
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.BPRIS FORMAT ">>>>99.99":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
  ENABLE
      mtrltemp.Enr
      mtrltemp.Benamning
      mtrltemp.Enhet
      mtrltemp.BPRIS
      mtrltemp.NPRIS
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 58.38 BY 9.63
         TITLE "Materiel för vald leverantör".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     FBTN_OK AT ROW 1.25 COL 96.5
     FBTN_AVB AT ROW 1.25 COL 111.5
     BRW_VLEV AT ROW 3.13 COL 1.63
     BRW_MTRL AT ROW 3.13 COL 66
     "  Funktioner:" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 1.25 COL 2
          FGCOLOR 1 FONT 17
     RECT-57 AT ROW 2.33 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.63 BY 20.13.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: list_mtrl T "?" NO-UNDO temp-db list_mtrl
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Uppdatera materielkatalog"
         HEIGHT             = 20.13
         WIDTH              = 124.63
         MAX-HEIGHT         = 30.04
         MAX-WIDTH          = 128
         VIRTUAL-HEIGHT     = 30.04
         VIRTUAL-WIDTH      = 128
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
                                                                        */
/* BROWSE-TAB BRW_VLEV FBTN_AVB FRAME-A */
/* BROWSE-TAB BRW_MTRL BRW_VLEV FRAME-A */
ASSIGN 
       BRW_MTRL:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_MTRL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_MTRL:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BRW_VLEV:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_VLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.list_mtrl"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.list_mtrl.Enr
     _FldNameList[2]   > Temp-Tables.list_mtrl.Benamning
"list_mtrl.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" ""
     _FldNameList[3]   = Temp-Tables.list_mtrl.Enhet
     _FldNameList[4]   = Temp-Tables.list_mtrl.PRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VLEV
/* Query rebuild information for BROWSE BRW_VLEV
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.mtrltemp.Enr|yes"
     _Where[1]         = "Temp-Tables.mtrltemp.LEVKOD = leverant
 AND Temp-Tables.mtrltemp.KALKNR = 0"
     _FldNameList[1]   > Temp-Tables.mtrltemp.Enr
"mtrltemp.Enr" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? "x(256)" "character" ? ? ? ? ? ? yes ? no no "40" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.mtrltemp.Enhet
"mtrltemp.Enhet" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.mtrltemp.BPRIS
"mtrltemp.BPRIS" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.mtrltemp.NPRIS
"mtrltemp.NPRIS" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VLEV */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_MTRL
&Scoped-define SELF-NAME BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL WINDOW-1
ON MOUSE-MENU-CLICK OF BRW_MTRL IN FRAME FRAME-A /* Vald materiel */
DO:
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VLEV
&Scoped-define SELF-NAME BRW_VLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VLEV WINDOW-1
ON ENTRY OF BRW_VLEV IN FRAME FRAME-A /* Materiel för vald leverantör */
DO:
   MESSAGE "1"  VIEW-AS ALERT-BOX.
   /*
   APPLY "VALUE-CHANGED" TO BRW_VLEV.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VLEV WINDOW-1
ON LEAVE OF BRW_VLEV IN FRAME FRAME-A /* Materiel för vald leverantör */
DO:
  /*IF  THEN*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VLEV WINDOW-1
ON ROW-LEAVE OF BRW_VLEV IN FRAME FRAME-A /* Materiel för vald leverantör */
DO:
  MESSAGE "2"  VIEW-AS ALERT-BOX.
  
  IF AVAILABLE mtrltemp THEN DO:
     RUN visa_UI.    
     
     ASSIGN     
     mtrltemp.Enr = INPUT BROWSE BRW_VLEV mtrltemp.Enr
     mtrltemp.Benamning = INPUT BROWSE BRW_VLEV mtrltemp.Benamning
     mtrltemp.Enhet = INPUT BROWSE BRW_VLEV mtrltemp.Enhet
     mtrltemp.Bpris = INPUT BROWSE BRW_VLEV mtrltemp.Bpris
     mtrltemp.NPRIS = INPUT BROWSE BRW_VLEV mtrltemp.NPRIS.
     
     EMPTY TEMP-TABLE emtrltemp NO-ERROR.
     CREATE emtrltemp.     
     IF mtrltemp.NPRIS = 0 THEN DO:
        mtrltemp.NPRIS = mtrltemp.BPRIS.           
     END.
     BUFFER-COPY mtrltemp TO emtrltemp. 
     RUN admsparmtrl_UI IN mtrlbapph ( INPUT-OUTPUT TABLE emtrltemp ). 
     FIND FIRST emtrltemp NO-ERROR.
     IF AVAILABLE emtrltemp THEN DO:
         FIND FIRST mtrltemp WHERE mtrltemp.LEVKOD = emtrltemp.LEVKOD AND   
         mtrltemp.ENR = emtrltemp.ENR NO-LOCK NO-ERROR.
         IF AVAILABLE mtrltemp THEN DO:
            mtrltemp.MTRLROW = emtrltemp.MTRLROW.          
            DELETE emtrltemp.
         END.
     END.
     RUN visa_UI. 
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VLEV WINDOW-1
ON VALUE-CHANGED OF BRW_VLEV IN FRAME FRAME-A /* Materiel för vald leverantör */
DO:
  MESSAGE "3"  VIEW-AS ALERT-BOX.
  
  status-ok = BRW_VLEV:SELECT-FOCUSED-ROW() NO-ERROR.  
  
  /*
  IF rowvar NE ? THEN DO:      
     EMPTY TEMP-TABLE emtrltemp NO-ERROR.
     CREATE emtrltemp.
     FIND FIRST mtrltemp WHERE ROWID(mtrltemp) = rowvar NO-LOCK NO-ERROR.
     IF AVAILABLE mtrltemp THEN DO:     
        IF mtrltemp.NPRIS = 0 THEN DO:
           mtrltemp.NPRIS = mtrltemp.BPRIS.           
        END.
        BUFFER-COPY mtrltemp TO emtrltemp. 
        RUN admsparmtrl_UI IN mtrlbapph ( INPUT TABLE emtrltemp ). 
     END.     
  END.
  rowvar = ?.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.Enr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.Enr BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrltemp.Enr IN BROWSE BRW_VLEV /* Enr */
DO:
   IF AVAILABLE mtrltemp THEN RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.Enr BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON LEAVE OF mtrltemp.Enr IN BROWSE BRW_VLEV /* Enr */
DO:   
   IF AVAILABLE mtrltemp THEN DO:
     
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.Benamning
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.Benamning BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrltemp.Benamning IN BROWSE BRW_VLEV /* Benämning */
DO:
   MESSAGE "4"  VIEW-AS ALERT-BOX.
   
   IF AVAILABLE mtrltemp THEN RUN visa_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.Benamning BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON LEAVE OF mtrltemp.Benamning IN BROWSE BRW_VLEV /* Benämning */
DO:   
   MESSAGE "5"  VIEW-AS ALERT-BOX.
   
   IF AVAILABLE mtrltemp THEN DO:
      IF INPUT BROWSE BRW_VLEV mtrltemp.BENAMNING = "" THEN DO:     
         MESSAGE  "Benämning får inte vara blankt." VIEW-AS ALERT-BOX.
         DISPLAY mtrltemp.BENAMNING WITH BROWSE BRW_VLEV.    
         RETURN NO-APPLY.      
      END.          
      IF mtrltemp.BENAMNING NE INPUT BROWSE BRW_VLEV mtrltemp.BENAMNING THEN DO: 
         /*rowvar = ROWID(mtrltemp).*/
         mtrltemp.BENAMNING = INPUT BROWSE BRW_VLEV mtrltemp.BENAMNING.           
         DISPLAY mtrltemp.BENAMNING  WITH BROWSE BRW_VLEV.    
      END. 
   END.
   /*ELSE rowvar = ?.*/
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.Enhet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.Enhet BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrltemp.Enhet IN BROWSE BRW_VLEV /* Enhet */
DO:
   IF AVAILABLE mtrltemp THEN RUN visa_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.Enhet BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON LEAVE OF mtrltemp.Enhet IN BROWSE BRW_VLEV /* Enhet */
DO:   
   /*IF mtrltemp.ENHET NE INPUT BROWSE BRW_VLEV mtrltemp.ENHET THEN rowvar = ROWID(mtrltemp).
   ELSE rowvar = ?.*/
   IF AVAILABLE mtrltemp THEN DO:
      mtrltemp.ENHET = INPUT BROWSE BRW_VLEV mtrltemp.ENHET.           
      DISPLAY mtrltemp.ENHET WITH BROWSE BRW_VLEV.   
   END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.BPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.BPRIS BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrltemp.BPRIS IN BROWSE BRW_VLEV /* Brutto pris */
DO:
   IF AVAILABLE mtrltemp THEN RUN visa_UI.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.BPRIS BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON LEAVE OF mtrltemp.BPRIS IN BROWSE BRW_VLEV /* Brutto pris */
DO:
   /*IF mtrltemp.BPRIS NE INPUT BROWSE BRW_VLEV mtrltemp.BPRIS THEN rowvar = ROWID(mtrltemp).
   ELSE rowvar = ?.*/   
   IF AVAILABLE mtrltemp THEN DO:
      mtrltemp.BPRIS = INPUT BROWSE BRW_VLEV mtrltemp.BPRIS.           
      DISPLAY mtrltemp.BPRIS WITH BROWSE BRW_VLEV.        
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.NPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.NPRIS BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrltemp.NPRIS IN BROWSE BRW_VLEV /* Netto pris */
DO:
  IF AVAILABLE mtrltemp THEN RUN visa_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.NPRIS BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON LEAVE OF mtrltemp.NPRIS IN BROWSE BRW_VLEV /* Netto pris */
DO:
   /*IF mtrltemp.NPRIS NE INPUT BROWSE BRW_VLEV mtrltemp.NPRIS THEN rowvar = ROWID(mtrltemp).
   ELSE rowvar = ?.*/   
   IF AVAILABLE mtrltemp THEN DO:
      mtrltemp.NPRIS = INPUT BROWSE BRW_VLEV mtrltemp.NPRIS.           
      DISPLAY mtrltemp.NPRIS WITH BROWSE BRW_VLEV.             
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_AVB WINDOW-1
ON CHOOSE OF FBTN_AVB IN FRAME FRAME-A /* Avbryt */
DO:
   musz = TRUE.
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK WINDOW-1
ON CHOOSE OF FBTN_OK IN FRAME FRAME-A /* Ok */
DO: 
           
   
   APPLY "GO" TO FBTN_OK IN FRAME {&FRAME-NAME}.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK WINDOW-1
ON GO OF FBTN_OK IN FRAME FRAME-A /* Ok */
DO:  
  musz = FALSE.  
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE mtrlbapph.
   RUN disable_UI.
END.
   

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
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
   
   RUN allstartbrw_UI.        
   varforetypval[20] = 6.
   ASSIGN WINDOW-1:TITLE = "Uppdatering av specialmateriel".   
   RUN setorgtitle_UI IN brwproc[1] (INPUT "Materiellista specialmateriel").  
   CREATE mtrltemp.
   mtrltemp.benamning = "start".
   CREATE mtrltemp.
   mtrltemp.benamning = "click here".
   CREATE mtrltemp.
   mtrltemp.benamning = "end".
   RUN enable_UI.   
   FIND FIRST mtrltemp  NO-LOCK NO-ERROR.
   IF AVAILABLE mtrltemp THEN RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrltemp)).         
   RUN openbdynspec_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].
   ASSIGN
   mtrltemp.ENR:LABEL IN BROWSE BRW_VLEV = Guru.Konstanter:genk
   list_mtrl.ENR:LABEL IN BROWSE BRW_MTRL = Guru.Konstanter:genk.
   
      /*
    status-ok = BRW_VLEV:SELECT-FOCUSED-ROW() NO-ERROR.   
    */
   {&WINDOW-NAME}:HIDDEN = FALSE.
{&WINDOW-NAME}:MOVE-TO-TOP ().
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/           
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
         (INPUT BRW_VLEV:HANDLE IN FRAME {&FRAME-NAME}).       
   RUN setdefaultcol_UI IN brwproc[1] (INPUT 1).
   RUN DYNBRW.P PERSISTENT SET brwproc[4]
         (INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}). 
         
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
  ENABLE RECT-57 FBTN_OK FBTN_AVB BRW_VLEV BRW_MTRL 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI WINDOW-1 
PROCEDURE visa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF AVAILABLE mtrltemp THEN DO:
      status-ok = BRW_VLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.          
      DISPLAY
      mtrltemp.ENR       
      mtrltemp.BENAMNING
      mtrltemp.ENHET mtrltemp.BPRIS mtrltemp.NPRIS WITH BROWSE BRW_VLEV.         
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

