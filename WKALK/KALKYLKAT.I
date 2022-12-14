/*KALKYLKAT.I*/
/*NAMN P? KATALOGER*/
{KALKARENDEGEM.I}
 
 DEFINE TEMP-TABLE kalkanvtt NO-UNDO 
   BEFORE-TABLE kalkanvttbef  
   FIELD KLOGID AS INTEGER    
   FIELD ANVANDARE AS CHARACTER
   FIELD AV-NAMN AS CHARACTER 
   FIELD AV-LEVEL AS INTEGER 
   FIELD TTRECID AS RECID
   INDEX KLOGID KLOGID ANVANDARE
   INDEX ANVANDARE ANVANDARE. 
DEFINE BUFFER kalkanvttbuf FOR kalkanvtt.   
/*UNDER KATALOGER kopplad till huvudkat UNIK KLOGID KLOGSUBID*/
DEFINE TEMP-TABLE kalkylkatalogsubtt NO-UNDO
   BEFORE-TABLE kalkylkatalogsubttbef
   FIELD KLOGID AS INTEGER
   FIELD KLOGSUBID AS INTEGER
   FIELD BENAMNING AS CHARACTER
   FIELD INDATUM AS DATE
   FIELD TTRECID AS RECID
   FIELD AVSLUTAD AS LOGICAL
   FIELD UPPDATBAR AS LOGICAL
   INDEX KLOGID IS PRIMARY KLOGID DESCENDING KLOGSUBID
   INDEX KLOGSUBID  KLOGSUBID  KLOGID.
 DEFINE BUFFER kalkylkatalogsubttbuf FOR kalkylkatalogsubtt. 
 DEFINE TEMP-TABLE ekalkylkatalogsubtt NO-UNDO LIKE kalkylkatalogsubtt.
/* underkatalog endast en g?ng ej databas SUMMERAD kalkylkatalogsubtt UNIK  KLOGSUBID */
DEFINE TEMP-TABLE kalkyldelkatalogtt NO-UNDO
   FIELD KLOGSUBID AS INTEGER
   FIELD BENAMNING AS CHARACTER
   FIELD INDATUM AS DATE
   FIELD TTRECID AS RECID
   FIELD AVSLUTAD AS LOGICAL
   FIELD UPPDATBAR AS LOGICAL
   INDEX KLOGSUBID  KLOGSUBID DESCENDING.  
/* underkatalog ING?R I KLOGID ej databas UNIK  KLOGSUBID KLOGID f?r att kolla s? inte tv? arbetskoder fr?n tv? olika katalogsubbar ?r lika*/
DEFINE TEMP-TABLE ingkalkylkatalogtt NO-UNDO
   FIELD KLOGID AS INTEGER
   FIELD BENAMNING AS CHARACTER
   FIELD INDATUM AS DATE
   FIELD VISARTAL AS INTEGER
   FIELD HKLOGSUBID AS INTEGER /*BEST?MMER INOM VARJE KATALOG VILKen KALKYLKATALOGSUB SOM SKA GE EGNA PRISER OCH FAKTORER */
   FIELD KLOGSUBID AS INTEGER 
   FIELD TTRECID AS RECID
   FIELD SEKRETESS AS LOGICAL
   FIELD AVSLUTAD AS LOGICAL 
   FIELD UPPDATBAR AS LOGICAL
   FIELD KATALOGTYP AS CHARACTER
   INDEX KLOGID KLOGID
   INDEX VISARTAL IS PRIMARY VISARTAL DESCENDING.
    
   
DEFINE TEMP-TABLE kalkEA NO-UNDO
  FIELD MTIMMAR AS DECIMAL
  FIELD MASKKOST AS DECIMAL.   
  
  

   
/*INNEH?LLER L?PNUMMER OCH DESS TIMMAR F?R RESPEKTIVE KALKYLPRISER*/
DEFINE TEMP-TABLE kalkyllopsubtt NO-UNDO
   BEFORE-TABLE kalkyllopsubttbef  
   FIELD KLOGSUBID AS INTEGER
   FIELD ARBKOD AS CHARACTER
   FIELD LOPNR AS INTEGER
   FIELD KPID AS INTEGER
   FIELD TIMMAR AS DECIMAL  DECIMALS 10  
   FIELD KOSTNAD AS DECIMAL DECIMALS 10
   FIELD TTRECID AS RECID   
   FIELD BENAMNING AS CHARACTER
   FIELD PRIS AS DECIMAL     
   FIELD AVRUND AS DECIMAL  DECIMALS 10
   FIELD POSTKOST AS DECIMAL
   INDEX LOPNR KLOGSUBID ARBKOD LOPNR
   INDEX ARBKOD ARBKOD LOPNR
   INDEX KPID KLOGSUBID KPID  
   INDEX BENAMNING IS PRIMARY BENAMNING.
DEFINE BUFFER kalkyllopsubttbuf FOR  kalkyllopsubtt. 

                          
/*KALKYLEN DET SAKNAST N?GRA F?LT EX. ARBETSUPPGIFT KOMMENTAR OMR?DE MM*/                                 
DEFINE TEMP-TABLE kalkhuvtt NO-UNDO  
   BEFORE-TABLE kalkhuvttbef
   FIELD KALKNR AS INTEGER
   FIELD OMRADE AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD KLOGID AS INTEGER
   FIELD TYPKALK AS INTEGER 
   FIELD EGETMTRL AS LOGICAL
   FIELD EGNAPRISER AS LOGICAL
   FIELD FAKTORER AS LOGICAL
   FIELD TTRECID AS RECID
   FIELD ANMARKNING AS CHARACTER
   FIELD BESTID AS CHARACTER
   FIELD KALKANV AS CHARACTER
   FIELD ANVANDARE AS CHARACTER
   FIELD AKTIV AS LOGICAL INITIAL TRUE
   FIELD UTYP AS INTEGER INITIAL 1
   INDEX KALKNR KALKNR
   INDEX TYPKALK TYPKALK.
DEFINE TEMP-TABLE ekalkhuvtt NO-UNDO LIKE kalkhuvtt.
  /*vilka kalkyler som ?r ?ppnade*/
DEFINE TEMP-TABLE anvkalkyltt NO-UNDO  
   FIELD KALKNR AS INTEGER
   FIELD OMRADE AS CHARACTER 
   INDEX KALKNR KALKNR.
   
DEFINE  TEMP-TABLE kalknumtt NO-UNDO
   BEFORE-TABLE kalknumttbef
   FIELD KALKNR AS INTEGER
   FIELD OMRADE AS CHARACTER
   FIELD KLOGSUBID AS INTEGER
   FIELD ARBKOD AS CHARACTER
   FIELD LOPNR AS INTEGER    
   FIELD NUM AS INTEGER
   FIELD MATRIS AS INTEGER 
   FIELD BENAMNING AS CHARACTER
   FIELD ANTAL AS DECIMAL  LABEL "Antal"  DECIMALS 6  /*OBS M?NGA DECIMALER*/
   FIELD ENHET AS CHARACTER
   FIELD KOMMENTAR AS CHARACTER
   FIELD ANMARKNING AS CHARACTER
   FIELD TTRECID AS RECID
   FIELD TYPKALK AS INTEGER 
   FIELD TOTKOST AS DECIMAL   DECIMALS 4 /*OBS M?NGA DECIMALER*/  
   FIELD MARKNING AS CHARACTER
   FIELD MARKSUB AS CHARACTER
   FIELD RISK AS DECIMAL  DECIMALS 4
   FIELD VINST AS DECIMAL  DECIMALS 4
   FIELD FRITOTKOST AS DECIMAL DECIMALS 4
  
   FIELD SID AS INTEGER   
   FIELD BERNUM AS INTEGER
   FIELD BERID AS INTEGER
   FIELD NGRUPP AS INTEGER
   FIELD ID2 AS CHARACTER
   FIELD EXTRA1 AS CHARACTER
   FIELD FRANTILL AS CHARACTER
   INDEX SID SID ARBKOD LOPNR
   INDEX BERID BERID ARBKOD LOPNR
   INDEX BERNUM BERNUM ARBKOD LOPNR
   INDEX NGRUPP NGRUPP ARBKOD LOPNR
   INDEX KALKNR KALKNR 
   INDEX MATRIS KALKNR MATRIS
   INDEX KLOGSUBID KLOGSUBID
   INDEX ARBKOD IS PRIMARY ARBKOD LOPNR NUM
   INDEX NUM NUM.
   
   /*FIELD STTOTKOST  AS DECIMAL  /*styckkostnad lena*/  
   FIELD STFRITOTKOST  AS DECIMAL
   FIELD STPROJKOST  AS DECIMAL
   FIELD STFRIPROJKOST  AS DECIMAL
   FIELD STUTSMET  AS DECIMAL
   FIELD STFRIUTSMET  AS DECIMAL*/
   
   
   
DEFINE BUFFER kalknumttbuf FOR kalknumtt.   
DEFINE TEMP-TABLE ekalknumtt NO-UNDO LIKE kalknumtt.
   
DEFINE TEMP-TABLE kalknumsubtt NO-UNDO
   BEFORE-TABLE kalknumsubttbef
   FIELD KALKNR AS INTEGER
   FIELD OMRADE AS CHARACTER
   FIELD NUM AS INTEGER 
   FIELD NUMSUBID AS INTEGER  
   FIELD KPID AS INTEGER
   FIELD TIMMAR AS DECIMAL     DECIMALS 10
   FIELD KOSTNAD AS DECIMAL         DECIMALS 4                                
   FIELD BENAMNING AS CHARACTER
   FIELD PRIS AS DECIMAL      DECIMALS 10
   FIELD MARKNING AS CHARACTER
   FIELD MARKSUB AS CHARACTER
   FIELD FRITIMMAR AS DECIMAL   DECIMALS 10
   FIELD FRIKOSTNAD AS DECIMAL   DECIMALS 10
   FIELD FRIBENAMNING AS CHARACTER
   FIELD AVRUND AS DECIMAL     DECIMALS 10
   FIELD FRIAVRUND AS DECIMAL     DECIMALS 10
   FIELD FRIPRIS AS DECIMAL       DECIMALS 10
   FIELD TTRECID AS RECID
   FIELD EGENPRISUPP AS LOGICAL
   FIELD EGENKODUPP AS LOGICAL
   INDEX NUM NUM NUMSUBID
   INDEX BENAMNING IS PRIMARY BENAMNING.
DEFINE BUFFER kalknumsubttbuf FOR kalknumsubtt.   
DEFINE TEMP-TABLE ekalknumsubtt NO-UNDO LIKE kalknumsubtt.
/*
DEFINE  TEMP-TABLE kalknumanvegentt NO-UNDO
   BEFORE-TABLE kalknumanvegenttbef
   FIELD ANVANDARE AS CHARACTER
   FIELD KLOGSUBID AS INTEGER
   FIELD ARBKOD AS CHARACTER
   FIELD LOPNR AS INTEGER    
   FIELD NUM AS INTEGER
   FIELD MATRIS AS INTEGER 
   FIELD BENAMNING AS CHARACTER
   FIELD ANTAL AS DECIMAL  LABEL "Antal"  DECIMALS 6  /*OBS M?NGA DECIMALER*/
   FIELD ENHET AS CHARACTER
   FIELD KOMMENTAR AS CHARACTER
   FIELD ANMARKNING AS CHARACTER
   FIELD TTRECID AS RECID
   FIELD TYPKALK AS INTEGER 
   FIELD TOTKOST AS DECIMAL   DECIMALS 4 /*OBS M?NGA DECIMALER*/  
   FIELD MARKNING AS CHARACTER
   FIELD MARKSUB AS CHARACTER
   FIELD RISK AS DECIMAL  DECIMALS 4
   FIELD VINST AS DECIMAL  DECIMALS 4
   FIELD FRITOTKOST AS DECIMAL DECIMALS 4
   INDEX ANV ANVANDARE 
   INDEX MATRIS ANVANDARE MATRIS
   INDEX KLOGSUBID KLOGSUBID
   INDEX ARBKOD IS PRIMARY ARBKOD LOPNR NUM
   INDEX NUM NUM.

DEFINE TEMP-TABLE kalknumanvegensubtt NO-UNDO
   BEFORE-TABLE kalknumanvegenttsubbef
   FIELD ANVANDARE AS CHARACTER
   FIELD NUM AS INTEGER 
   FIELD NUMSUBID AS INTEGER  
   FIELD KPID AS INTEGER
   FIELD TIMMAR AS DECIMAL     DECIMALS 10
   FIELD KOSTNAD AS DECIMAL         DECIMALS 4                                
   FIELD BENAMNING AS CHARACTER
   FIELD PRIS AS DECIMAL      DECIMALS 10
   FIELD MARKNING AS CHARACTER
   FIELD MARKSUB AS CHARACTER
   FIELD FRITIMMAR AS DECIMAL   DECIMALS 10
   FIELD FRIKOSTNAD AS DECIMAL   DECIMALS 10
   FIELD FRIBENAMNING AS CHARACTER
   FIELD AVRUND AS DECIMAL     DECIMALS 10
   FIELD FRIAVRUND AS DECIMAL     DECIMALS 10
   FIELD FRIPRIS AS DECIMAL       DECIMALS 10
   FIELD TTRECID AS RECID
   FIELD EGENPRISUPP AS LOGICAL
   FIELD EGENKODUPP AS LOGICAL
   INDEX NUM NUM NUMSUBID
   INDEX BENAMNING IS PRIMARY BENAMNING.
*/
  
DEFINE TEMP-TABLE frikalknumsubtt NO-UNDO LIKE kalknumsubtt.   

DEFINE TEMP-TABLE kalkfaktorertt NO-UNDO
   BEFORE-TABLE kalkfaktorerttbef
   FIELD KLOGSUBID AS INTEGER
   FIELD KALKNR AS INTEGER
   FIELD OMRADE AS CHARACTER
   FIELD KPID AS INTEGER
   FIELD FAKTOR AS DECIMAL DECIMALS 2
   FIELD BENAMNING AS CHARACTER
   FIELD TTRECID AS RECID
   INDEX KALKNR KALKNR KPID
   INDEX BENAMNING IS PRIMARY BENAMNING.
/*de man har valt*/
DEFINE TEMP-TABLE kalkegnaprisertt NO-UNDO
   BEFORE-TABLE kalkegnapriserttbef   
   FIELD KLOGSUBID AS INTEGER
   FIELD KALKNR AS INTEGER
   FIELD OMRADE AS CHARACTER
   FIELD KPID AS INTEGER
   FIELD PRIS AS DECIMAL DECIMALS 4
   FIELD BENAMNING AS CHARACTER
   FIELD PROCENT AS DECIMAL  INITIAL 0  /*enbart hj?lp vid r?kning*/
   FIELD TTRECID AS RECID
   INDEX KALKNR KALKNR KPID
   INDEX BENAMNING IS PRIMARY BENAMNING.

DEFINE TEMP-TABLE kalktmtrlTT NO-UNDO
   BEFORE-TABLE kalktmtrlTTbef
   FIELD KALKNR         AS INTEGER
   FIELD OMRADE     AS CHARACTER
   FIELD Enr            AS CHARACTER                         
   FIELD Benamning      AS CHARACTER                            
   FIELD Bestkvant      AS DECIMAL                           
   FIELD Enhet          AS CHARACTER                    
   FIELD BERKVANT       AS DECIMAL DECIMALS 2
   FIELD SUMMA          AS DECIMAL
   FIELD LEVKOD         AS CHARACTER
   FIELD BPRIS          AS DECIMAL DECIMALS 2
   FIELD NPRIS          AS DECIMAL  DECIMALS 2  
   FIELD BERLEV         AS CHARACTER                           
   FIELD OFFERT         AS LOGICAL
   FIELD KPRIS          AS DECIMAL  DECIMALS 2
   FIELD KUND           AS LOGICAL
   FIELD MTRLROW        AS ROWID
   FIELD MATRIS         AS INTEGER
   FIELD TTRECID        AS RECID
   FIELD MID            AS INTEGER
   FIELD BREAKDUMMY     AS INTEGER
   INDEX BEN LEVKOD KALKNR BENAMNING
   INDEX Enr IS PRIMARY enr
   INDEX Lev LEVKOD KALKNR enr
   INDEX MID MID
   INDEX KALKNR KALKNR OMRADE.
DEFINE BUFFER kalktmtrlTTbuf FOR kalktmtrlTT.   
DEFINE TEMP-TABLE ekalktmtrlTT NO-UNDO LIKE kalktmtrlTT .


DEFINE TEMP-TABLE kalkaonrTT NO-UNDO
   BEFORE-TABLE kalkaonrTTbef
   FIELD KALKNR AS INTEGER
   FIELD OMRADE AS CHARACTER
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER 
   FIELD AKTIV AS LOGICAL
   FIELD TYP AS INTEGER
   FIELD STATUSNIV AS CHARACTER
   FIELD LASTA AS LOGICAL
   FIELD PLANNR AS CHARACTER
   FIELD ARTAL AS INTEGER
   FIELD TTRECID AS RECID
   INDEX KALKNR KALKNR OMRADE
   INDEX AONR AONR DELNR
   INDEX PLANNR PLANNR ARTAL.
 DEFINE TEMP-TABLE kalkaonrTTalla NO-UNDO LIKE kalkaonrTT.  
 DEFINE TEMP-TABLE kalkttidlageTT NO-UNDO
    BEFORE-TABLE kalkttidlageTTbef
    FIELD KALKNR AS INTEGER
    FIELD OMRADE AS CHARACTER 
    FIELD BENAMNING AS CHARACTER
    FIELD DATUM1 AS DATE     
    FIELD DATUMTID AS DATETIME FORMAT "99/99/9999 HH:MM:SS"
    FIELD IDTIDLAG AS CHARACTER
    FIELD ANVANDARE1 AS CHARACTER
    FIELD ORDNING AS CHARACTER
    FIELD TIDLAGE AS CHARACTER
    FIELD TTRECID AS RECID
    FIELD PERSONALKOD AS CHARACTER
    FIELD NAMNANVANDARE1 AS CHARACTER
    FIELD NAMN AS CHARACTER
    INDEX KALKNR KALKNR OMRADE ORDNING
    INDEX ORDNING IS PRIMARY ORDNING.
DEFINE TEMP-TABLE ekalkttidlageTT NO-UNDO  LIKE kalkttidlageTT.


    
DEFINE TEMP-TABLE AvtalKodertt NO-UNDO LIKE kalknumtt.
DEFINE TEMP-TABLE BerAvtalKodertt NO-UNDO LIKE kalknumtt.
DEFINE TEMP-TABLE BerAvtalKoderSumtt NO-UNDO LIKE kalknumtt.
DEFINE TEMP-TABLE SchAvtalKodertt NO-UNDO LIKE kalknumtt.
DEFINE BUFFER avtbuff FOR  AvtalKodertt.


DEFINE TEMP-TABLE Avtalskalktt NO-UNDO
    FIELD ID AS INTEGER
    FIELD KATAR AS INTEGER
    FIELD AVTALAR AS INTEGER
    FIELD AVTALTXT AS CHARACTER
    FIELD AVTALTXTHELA AS CHARACTER 
    INDEX KATAR IS PRIMARY KATAR  AVTALAR AVTALTXT 
    INDEX ID ID. 
{KalkylimportTT.I}
{KaladmimportTT.I}    

{VOLYMTT.I}

   
   
   
 {FREKVENSTEMPC.I}  
 
DEFINE TEMP-TABLE tidutorg
   FIELD UT AS CHARACTER       
   FIELD TTRECID AS RECID.
