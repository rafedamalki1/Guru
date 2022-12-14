/*FASTEXTRA.I*/
DEFINE TEMP-TABLE fastextra NO-UNDO
   FIELD VARDNR       AS  INTEGER     
   FIELD BETECKNING   AS  CHARACTER    
   FIELD MARKNR      AS  INTEGER   
   FIELD MARKAGARE   AS  CHARACTER   
   FIELD ANDEL       AS CHARACTER
   FIELD GATUADRESS AS CHARACTER             
   FIELD POSTNUMMER AS CHARACTER             
   FIELD POSTADRESS AS CHARACTER
   FIELD CH1         AS  CHARACTER    
   FIELD CH2         AS  CHARACTER      
   FIELD CH3        AS  CHARACTER      
   FIELD CH4          AS  CHARACTER      
   FIELD CH5     AS  CHARACTER    
   FIELD CH6     AS  CHARACTER    
   FIELD CH7       AS  CHARACTER      
   FIELD CH8     AS  CHARACTER      
   FIELD CH9       AS  CHARACTER      
   FIELD CH10      AS  CHARACTER
   FIELD CH11         AS  CHARACTER    
   FIELD CH12         AS  CHARACTER      
   FIELD CH13        AS  CHARACTER      
   FIELD CH14          AS  CHARACTER      
   FIELD CH15     AS  CHARACTER    
   FIELD CH16     AS  CHARACTER    
   FIELD CH17       AS  CHARACTER      
   FIELD CH18     AS  CHARACTER      
   FIELD CH19       AS  CHARACTER      
   FIELD CH20      AS  CHARACTER  
   FIELD INT1     AS INTEGER
   FIELD INT2     AS INTEGER
   FIELD INT3     AS INTEGER 
   FIELD LG1         AS  LOGICAL    
   FIELD LG2         AS  LOGICAL
   FIELD LG3         AS  LOGICAL
   FIELD LG4         AS  LOGICAL
   FIELD DAT1         AS DATE       
   INDEX VARDNR IS PRIMARY VARDNR BETECKNING MARKNR.
   
DEFINE TEMP-TABLE felfastextra NO-UNDO LIKE fastextra.   

