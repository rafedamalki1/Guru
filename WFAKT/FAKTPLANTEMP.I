/*FAKTPLANTEMP.I*/
DEFINE {&NEW} SHARED TEMP-TABLE faktplantemp  NO-UNDO
   FIELD ALLTTAK                   AS LOGICAL     
   FIELD ANVANDARE                 AS CHARACTER   
   FIELD BESTID                    AS CHARACTER   
   FIELD EGNAOVR                   AS LOGICAL     
   FIELD FAKTNR                    AS INTEGER     
   FIELD FAKTTYP                   AS CHARACTER   
   FIELD FAKTTYPUNDER              AS INTEGER     
   FIELD FDELNR                    AS INTEGER     
   FIELD HANVANDARE                AS CHARACTER   
   FIELD KALKPRIS                  AS DECIMAL     
   FIELD NAMN                      AS CHARACTER   
   FIELD OFFERTPRIS                AS DECIMAL     
   FIELD OMRADE                    AS CHARACTER   
   FIELD PANVANDARE                AS CHARACTER   
   FIELD PRISNUM                   AS INTEGER     
   FIELD PRISNUMO                  AS INTEGER     
   FIELD PROJEKTKOD                AS CHARACTER   
   FIELD RESTORE                   AS DECIMAL     
   FIELD SENASTFAK                 AS DATE        
   FIELD SENASTTID                 AS DATE        
   FIELD SLUTFAKT                  AS LOGICAL     
   FIELD TAKPRIS                   AS DECIMAL       
   FIELD TOTPRIS                   AS DECIMAL       
   FIELD VFAKTNR                   AS INTEGER       
   FIELD VISAONR                   AS CHARACTER 
   FIELD VIBESTID                   AS CHARACTER 
   INDEX FAKTNR IS PRIMARY FAKTNR
   INDEX VIBESTID VIBESTID
   INDEX NAMN NAMN.
DEFINE {&NEW} SHARED TEMP-TABLE vfaktplantemp NO-UNDO LIKE faktplantemp .
DEFINE TEMP-TABLE evfaktplantemp NO-UNDO LIKE faktplantemp .

DEFINE TEMP-TABLE extravfaktplantemp NO-UNDO LIKE faktplantemp 
    FIELD AONR AS CHARACTER
    FIELD DELNR AS INTEGER.
DEFINE TEMP-TABLE faktureradtemp NO-UNDO 
   FIELD FAKTNR AS INTEGER   
   FIELD FDELNR AS INTEGER  
   FIELD NAMN AS CHARACTER 
   FIELD DATUM AS DATE 
   FIELD TOTPRIS AS DECIMAL 
   FIELD TIMMAR AS DECIMAL 
   FIELD BELOPP AS DECIMAL 
   FIELD OBELOPP AS DECIMAL 
   FIELD TBELOPP AS DECIMAL 
   FIELD OTIMMAR AS DECIMAL 
   FIELD LONKOST AS DECIMAL 
   FIELD MTRLKOST AS DECIMAL 
   FIELD OVRKOST AS DECIMAL 
   FIELD KOSTBELOPP AS DECIMAL 
   FIELD FAKTXT AS CHARACTER 
   FIELD ANTALRADER AS integer 
   FIELD RESKOST AS INTEGER 
   FIELD SENASTTID AS DATE 
   FIELD MOMSBELOPP AS DECIMAL 
   FIELD VFAKTNR AS INTEGER 
   FIELD AVGAR AS DECIMAL LABEL "Avg?r tidigare fakturerat"
   FIELD ORESUTJ AS DECIMAL 
   FIELD TIDIGARMOMS  AS DECIMAL  
   FIELD RESKOSTDEC  AS DECIMAL  
   FIELD FAKTTYPID AS INTEGER  
   FIELD PRELGOD AS LOGICAL  
   FIELD BILAGOR AS CHARACTER 
   FIELD BOKDATUM  AS DATE FORMAT "99/99/99" INITIAL ?
   INDEX FAKTNR IS PRIMARY FAKTNR FDELNR.
DEFINE TEMP-TABLE allafaktureradtemp NO-UNDO LIKE faktureradtemp.
DEFINE TEMP-TABLE kredallafaktureradtemp NO-UNDO LIKE faktureradtemp
   FIELD KREDIT AS DECIMAL.
   
DEFINE TEMP-TABLE faktkredtemp NO-UNDO LIKE faktureradtemp 
   FIELD SPARAD AS LOGICAL
   FIELD VKREDIT AS INTEGER
   FIELD DEBKRED AS LOGICAL
   INDEX VFAKTNR IS PRIMARY FAKTNR VKREDIT VFAKTNR FDELNR DATUM
   INDEX FDELNR FDELNR.
DEFINE BUFFER faktkredtempbuff FOR faktkredtemp.
DEFINE TEMP-TABLE faktdebkred NO-UNDO LIKE faktkredtemp 
   FIELD KDATUM AS DATE 
   FIELD KTOTPRIS AS DECIMAL.
DEFINE TEMP-TABLE efaktkredtemp NO-UNDO LIKE faktkredtemp .
DEFINE TEMP-TABLE faktnamntemp NO-UNDO
   FIELD VKREDIT  AS INTEGER
   FIELD VFAKTNR AS INTEGER                     
   FIELD VARREF  AS CHARACTER                   
   FIELD TEL     AS CHARACTER                   
   FIELD LOPFAST AS LOGICAL                     
   FIELD KONTAKT AS CHARACTER                   
   FIELD FDELNR      AS INTEGER                 
   FIELD FAKTURNR  AS INTEGER                   
   FIELD FAKTURADATUM AS DATE                   
   FIELD FAKPNR  AS CHARACTER                   
   FIELD FAKORT  AS CHARACTER                   
   FIELD FAKADRESS AS CHARACTER                 
   FIELD BESTNAMN  AS CHARACTER                 
   FIELD BESTID    AS CHARACTER                 
   FIELD BESTALLARE AS CHARACTER                
   FIELD ARBOMF     AS CHARACTER                
   INDEX FAKTURNR IS PRIMARY FAKTURNR FDELNR VFAKTNR.
DEFINE TEMP-TABLE efaktnamntemp NO-UNDO LIKE faktnamntemp .
{FAKTFRIT.I}
DEFINE TEMP-TABLE faktintakkontotemp NO-UNDO 
   FIELD AONR           AS CHARACTER
   FIELD BELOPP         AS DECIMAL
   FIELD DELNR         AS INTEGER
   FIELD FAKTNR        AS INTEGER
   FIELD FDELNR        AS INTEGER
   FIELD INTAKTID      AS INTEGER
   FIELD MOMSID        AS INTEGER
   FIELD MOTPARTID     AS INTEGER
   FIELD OMRADE        AS CHARACTER
   FIELD VFAKTNR       AS INTEGER
   FIELD VKREDIT       AS INTEGER
   INDEX FAKTNR IS PRIMARY FAKTNR FDELNR VFAKTNR
   INDEX VFAKTNR FAKTNR VFAKTNR.
DEFINE TEMP-TABLE skrivutfakttemp NO-UNDO
   FIELD VFAKTNR       AS INTEGER
   INDEX VFAKTNR VFAKTNR.
{FAKTMAILTT.I}   
