/*TTEXPIMPMV.I*/
DEFINE TEMP-TABLE tempvard NO-UNDO       /*varderingtemp*/
   FIELD AKTIV          AS LOGICAL     
   FIELD ANVANDARE      AS CHARACTER   
   FIELD AONR           AS CHARACTER   
   FIELD BENAMNING      AS CHARACTER   
   FIELD DELNR          AS INTEGER     
   FIELD NATAGARE       AS CHARACTER   
   FIELD OMRADE         AS CHARACTER   
   FIELD VARDANV        AS CHARACTER   
   FIELD VARDNR         AS INTEGER.

DEFINE TEMP-TABLE tempfastighet NO-UNDO  /*fastighettemp*/
   FIELD BETECKNING  AS CHARACTER  
   FIELD KOMMUN      AS CHARACTER  
   FIELD SOCKEN      AS CHARACTER  
   FIELD PAKER       AS CHARACTER  
   FIELD VAKER       AS CHARACTER  
   FIELD VSKOG       AS CHARACTER  
   FIELD ARRENDATOR  AS LOGICAL    
   FIELD ANAMN       AS CHARACTER  
   FIELD AADRESS     AS CHARACTER  
   FIELD APONR       AS CHARACTER  
   FIELD APADRESS    AS CHARACTER  
   FIELD APERNR      AS CHARACTER          
   FIELD GOMRTALL   AS CHARACTER  
   FIELD GOMRGRAN    AS CHARACTER

   FIELD LOPNR    AS CHARACTER   /*extradata "FASTLOPNR"*/
   FIELD ABOXEN    AS CHARACTER
   FIELD ATELEFON    AS CHARACTER
   FIELD ATELEFON2    AS CHARACTER
   FIELD ATELEFONM    AS CHARACTER
   FIELD AMAIL    AS CHARACTER
   FIELD KONTAKT    AS INTEGER.  /*markägarnummer*/

DEFINE BUFFER tempfastighet2 FOR tempfastighet.

DEFINE TEMP-TABLE dubfastighet LIKE tempfastighet.   
   

DEFINE TEMP-TABLE tempfastvard NO-UNDO  /*fastvardtemp*/
   FIELD BETECKNING  AS CHARACTER 
   FIELD VARDNR      AS INTEGER     
   FIELD FORDYRAD    AS INTEGER. 

DEFINE TEMP-TABLE tempfastextra NO-UNDO     /*fastextra extradata "vardfast"*/
   FIELD VARDNR       AS  INTEGER     
   FIELD BETECKNING   AS  CHARACTER    
   /*FIELD MARKNR      AS  INTEGER */        
   FIELD CH1         AS  CHARACTER  /*STAKTILL      */
   FIELD CH2         AS  CHARACTER  /*ROJFALL       */ 
   FIELD CH3        AS  CHARACTER   /*AFSKICKAT        */
   FIELD CH4          AS  CHARACTER /*AFMOTTAGET       */
   FIELD CH5     AS  CHARACTER      /*AFERSATT         */
   FIELD CH6     AS  CHARACTER      /*AFSKICKAT        */
   FIELD CH7       AS  CHARACTER    /*SERVITUTSOK      */
   FIELD CH8     AS  CHARACTER      /*SERVITUTNR       */
   FIELD CH9       AS  CHARACTER    /*ANM              */
   FIELD CH10      AS  CHARACTER.   /* KOMMENTAR       */


DEFINE TEMP-TABLE tempfastextra2 NO-UNDO     /*fastextra extradata "vardfast2"*/
   FIELD VARDNR       AS  INTEGER     
   FIELD BETECKNING   AS  CHARACTER               
   FIELD CH1         AS  CHARACTER  /*Löpnr markägare      */
   FIELD LOG1         AS  LOGICAL  /*F      */ 
   FIELD LOG2        AS  LOGICAL   /*T      */
   FIELD LOG3        AS  LOGICAL   /*T      */
   FIELD INT1          AS  INTEGER /*F-Rotnetto       */
   FIELD INT2     AS  INTEGER      /*T-Expl på rotnetto        */
   FIELD INT3     AS  INTEGER.      /*Vp Skog        */
   
               

DEFINE TEMP-TABLE tempmarkagare NO-UNDO   /* markagaretemp*/
   FIELD MARKNR       AS INTEGER   
   FIELD MARKAGARE    AS CHARACTER 
   FIELD GATUADRESS   AS CHARACTER 
   FIELD POSTNUMMER   AS CHARACTER 
   FIELD POSTADRESS   AS CHARACTER 
   FIELD BOXEN        AS CHARACTER 
   FIELD TELEFON      AS CHARACTER 
   FIELD TELEFON2     AS CHARACTER 
   FIELD PERSONNUMMER AS CHARACTER 
   FIELD PNR2         AS CHARACTER

   FIELD MOBIL     AS CHARACTER     /*extradata  "markag"*/
   FIELD MAIL         AS CHARACTER
   FIELD MAKA         AS CHARACTER
   FIELD MBANKKONTO         AS CHARACTER
   FIELD MBANKNAMN         AS CHARACTER

   FIELD NMARKNR       AS INTEGER. /*NYTT MARKNR*/
DEFINE BUFFER tempmarkagare2 FOR tempmarkagare.
/*DEFINE TEMP-TABLE etempmarkagare LIKE tempmarkagare.*/

DEFINE TEMP-TABLE tempmarkfast NO-UNDO   /*markfasttemp*/
   FIELD BETECKNING  AS CHARACTER 
   FIELD MARKNR      AS INTEGER   
   FIELD PROCENT     AS INTEGER

   FIELD ANDEL       AS CHARACTER  /*extradata  "MARKFASTANDEL*/
   FIELD ANDEL1      AS INTEGER   
   FIELD ANDEL2      AS INTEGER   
   FIELD PRODEL      AS DECIMAL

   FIELD NMARKNR      AS  INTEGER.  /*NYTT MARKNR*/    

DEFINE BUFFER tempmarkfast2  FOR  tempmarkfast .

DEFINE  TEMP-TABLE tempakerkab NO-UNDO   /*akerkabtemp*/
  FIELD VARDNR     AS INTEGER    
  FIELD BETECKNING AS CHARACTER  
  FIELD KRONOR     AS INTEGER    
  FIELD FASTPRIS   AS LOGICAL    
  FIELD L1         AS INTEGER    
  FIELD FLKAB      AS LOGICAL    
  FIELD L2         AS DECIMAL    
  FIELD AKERKABREC AS RECID.
  

DEFINE TEMP-TABLE tempakervard NO-UNDO /*akervardtemp*/
   FIELD VARDNR      AS INTEGER    
   FIELD BETECKNING  AS CHARACTER  
   FIELD KRONOR      AS INTEGER    
   FIELD FASTPRIS    AS LOGICAL    
   FIELD SORT        AS CHARACTER  
   FIELD BENAMNING   AS CHARACTER  
   FIELD L1          AS INTEGER    
   FIELD L2          AS INTEGER    
   FIELD L3          AS INTEGER    
   FIELD L4          AS INTEGER    
   FIELD L5          AS INTEGER    
   FIELD STOLPNR     AS INTEGER    
   FIELD VARDEPKT    AS DATE.       
   

DEFINE TEMP-TABLE tempskogvard NO-UNDO  /*skogvardtemp*/
   FIELD VARDNR       AS  INTEGER      
   FIELD BETECKNING   AS  CHARACTER    
   FIELD BESTAND      AS  INTEGER      
   FIELD BENAMNING    AS  CHARACTER    
   FIELD LANGD        AS  INTEGER      
   FIELD BREDD        AS  INTEGER      
   FIELD HA           AS  DECIMAL      
   FIELD MARKTYP      AS  CHARACTER    
   FIELD BESTTYP      AS  CHARACTER    
   FIELD SLENH        AS  DECIMAL      
   FIELD PROCENT      AS  INTEGER      
   FIELD KR-HA        AS  INTEGER      
   FIELD KRONOR       AS  INTEGER      
   FIELD FASTPRIS     AS  LOGICAL      
   FIELD BARRLOV      AS  CHARACTER

   FIELD SOKCHAR1      AS  CHARACTER /*EXTRADATA  "FSVOL*/
   FIELD SOKDEC1        AS  DECIMAL      
   FIELD SOKINT2      AS  INTEGER      
   FIELD SOKINT3        AS  INTEGER      
   FIELD SOKDEC2        AS  DECIMAL      
   FIELD SOKDEC3        AS  DECIMAL.      

  
DEFINE  TEMP-TABLE tempvolskog NO-UNDO  /*volskogtemp*/
   FIELD VARDNR      AS INTEGER    
   FIELD BETECKNING  AS CHARACTER  
   FIELD DIAM        AS INTEGER    
   FIELD TANT        AS INTEGER    
   FIELD THKL        AS CHARACTER  
   FIELD TM3         AS DECIMAL    
   FIELD GANT        AS INTEGER    
   FIELD GHKL        AS CHARACTER  
   FIELD GM3         AS DECIMAL    
   FIELD BANT        AS INTEGER    
   FIELD BHKL        AS CHARACTER  
   FIELD BM3         AS DECIMAL    
   FIELD OANT        AS INTEGER    
   FIELD OKORR       AS INTEGER    
   FIELD TKR         AS INTEGER    
   FIELD GKR         AS INTEGER    
   FIELD BKR         AS INTEGER    
   FIELD OKR         AS INTEGER    
   FIELD OHKL        AS CHARACTER  
   FIELD OM3         AS DECIMAL    
   FIELD VOLSKOGREC  AS RECID.
   
 DEFINE  TEMP-TABLE temprevavtal NO-UNDO
   FIELD VARDNR      AS INTEGER    
   FIELD BETECKNING  AS CHARACTER           
   FIELD SOKINT1      AS  INTEGER  /*ZON*/  /*EXTRADATA  "REVAVTAL*/
   FIELD SOKDEC1        AS  DECIMAL /*KRPM*/         
   FIELD SOKDEC2        AS  DECIMAL  /*METER*/      
   FIELD SOKDEC3        AS  DECIMAL.  /*KRONOR*/
 
  


DEFINE  TEMP-TABLE tempnatproj NO-UNDO  /*EXTRADATA  "NATPROJ*/      
   FIELD PONR  AS CHARACTER
   FIELD SOKCHAR1      AS  CHARACTER  /*FILL-IN-LEDLITT*/
   FIELD SOKCHAR2      AS  CHARACTER /*FILL-IN-PNAMN*/
   FIELD SOKCHAR3      AS  CHARACTER /*FILL-IN-PLED*/
   FIELD SOKCHAR4      AS  CHARACTER /*FILL-IN-KONC*/
   FIELD SOKCHAR5      AS  CHARACTER /*FILL-IN-DOMSAGA*/
   FIELD SOKCHAR6      AS  CHARACTER /*FILL-IN-UNAMN*/
   FIELD SOKCHAR7      AS  CHARACTER /*FILL-IN-ATTEST*/
   FIELD SOKCHAR8      AS  CHARACTER /*FILL-IN-PN*/
   FIELD SOKCHAR9      AS  CHARACTER /*FILL-IN-NAT*/
   FIELD SOKCHAR10      AS  CHARACTER  /**/            
   FIELD SOKINT1      AS  INTEGER  /*FILL-IN-MUEON1A*/
   FIELD SOKINT2      AS  INTEGER  /*FILL-IN-MUEON1B*/
   FIELD SOKINT3      AS  INTEGER  /*FILL-IN-MUEON2C*/
   FIELD SOKDATE1        AS  DATE.  /*FILL-IN-UTFARD*/               
   

DEFINE  TEMP-TABLE tempnatproj2 NO-UNDO  /*EXTRADATA  "NATPROJ2*/      
   FIELD PONR  AS CHARACTER
   FIELD SOKCHAR1      AS  CHARACTER  /*FILL-IN-PLATS*/
   FIELD SOKCHAR2      AS  CHARACTER /*FILL-IN-TIDPUNKT*/
   FIELD SOKCHAR3      AS  CHARACTER.  /*FILL-IN-ARTAL*/
   
   
   DEFINE  TEMP-TABLE tempnathsand NO-UNDO  /*EXTRADATA  "PROJHSAND*/      
   FIELD PONR  AS CHARACTER
   FIELD SOKCHAR1      AS  CHARACTER  /*FILL-IN-GAMLED*/
   FIELD SOKCHAR2      AS  CHARACTER /*FILL-IN-DODAS*/
   FIELD SOKCHAR3      AS  CHARACTER  /*FILL-IN-TKMETER*/
   FIELD SOKINT1      AS  INTEGER  /*FILL-IN-KRHA*/
   FIELD SOKINT2      AS  INTEGER  /*FILL-IN-SKADEERS*/
   FIELD SOKINT3      AS  INTEGER  /**/
   FIELD SOKINT4      AS  INTEGER  /*FILL-IN-NSTN*/
   FIELD SOKDATE1        AS  DATE.  /*FILL-IN-TILLDAG*/               
   
   
        
       
   
     
         
         
   
   
   
   
   
   
    
   
   
   
   
   
    
   
   
   
    
   
   
   
   
   
   
   

   
   
    
   
   
   
   
    
   
   
   
   
   
   
   
   
   
   
