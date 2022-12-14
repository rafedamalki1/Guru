/*STANSLONEDEF.I*/

DEFINE  TEMP-TABLE stanslonefil
   FIELD PPERSONNUMMER AS CHARACTER FORMAT "999999-9999"
   FIELD ANSTNR AS CHARACTER 
   FIELD START AS DECIMAL FORMAT "99.99"                
   FIELD SLUT AS DECIMAL FORMAT "99.99"
   FIELD TOTALT AS DECIMAL 
   FIELD HEROSORT AS CHARACTER                 
   FIELD PLONTILLAGG AS CHARACTER FORMAT "X(4)"         
   FIELD PLONTILLANTAL AS DECIMAL               
   FIELD OVERTIDUTTAG AS CHARACTER        
   FIELD POVERTIDTILL AS CHARACTER FORMAT "X(4)"        
   FIELD POVERANTAL AS DECIMAL                          
   FIELD PTRAKTKOD AS CHARACTER FORMAT "X(4)"           
   FIELD PTRAKTANTAL AS DECIMAL                         
   FIELD PBEREDSKAP AS CHARACTER FORMAT "X(4)"
   FIELD BERSTART AS DECIMAL
   FIELD BERSLUT AS DECIMAL           
   FIELD PBERANTAL AS DECIMAL                           
   FIELD PDATUM AS DATE   
   FIELD STARTDATUM AS DATE                              
   FIELD SLUTDATUM AS DATE                     
   FIELD PVECKONUMMER AS INTEGER                        
   FIELD MANAD AS INTEGER                               
   FIELD PSORT AS CHARACTER FORMAT "XX"                 
   FIELD PPAR8 AS DECIMAL FORMAT "-99.99"               
   FIELD AONR AS CHARACTER FORMAT "X(6)"                
   FIELD DELNR AS INTEGER                               
   FIELD PAVTAL AS CHARACTER                            
   FIELD TID AS LOGICAL                                 
   FIELD BERKOLL AS LOGICAL INITIAL FALSE               
   FIELD PROCENT AS DECIMAL FORMAT "999.9"              
   FIELD KLARTEXT AS CHARACTER                              
   FIELD RESMAL AS CHARACTER                            
   FIELD ERSATTNING AS DECIMAL     
   FIELD VIJUDID AS CHARACTER
   INDEX PPERSONNUMMER IS PRIMARY VIJUDID PPERSONNUMMER ASCENDING PLONTILLAGG PTRAKTKOD PDATUM
   INDEX PPERSONNUMMER2 VIJUDID PPERSONNUMMER ASCENDING PDATUM PLONTILLAGG PTRAKTKOD.
   
   DEFINE TEMP-TABLE franvarotemp
   FIELD PERSONALKOD AS CHARACTER 
   FIELD ANSTNR AS CHARACTER
   FIELD AONR AS CHARACTER 
   FIELD DELNR AS INTEGER
   FIELD START AS DECIMAL FORMAT "99.99"
   FIELD SLUT AS DECIMAL FORMAT "99.99"
   FIELD FRAN AS DATE
   FIELD FRTID AS DECIMAL FORMAT "99.99"
   FIELD TILL AS DATE
   FIELD TITID AS DECIMAL FORMAT "99.99"
   FIELD PROCENT AS INTEGER FORMAT "999"
   FIELD TIMMAR AS DECIMAL FORMAT "99.99"
   FIELD LART AS CHARACTER FORMAT "X(4)"   
   FIELD PKOD AS CHARACTER 
   FIELD VIJUDID AS CHARACTER
   FIELD PNR AS CHARACTER
   FIELD TIMANSTALLD AS LOGICAL
   FIELD SJUK AS INTEGER EXTENT 31
   FIELD SJTIM AS DECIMAL EXTENT 31
   FIELD SJUKTIMMAR AS DECIMAL 
   FIELD ORSAK AS CHARACTER   
   FIELD KOMMENTAR AS CHARACTER   
   FIELD ARBDAGAR AS INTEGER   
   FIELD TTID AS DECIMAL     /*regtotalt -dagens arbetstid*/
   FIELD TOTTID AS DECIMAL   /*periodens totala timmar*/
   FIELD ORT AS CHARACTER 
   INDEX FRANVARO IS PRIMARY PNR FRAN LART ASCENDING
   INDEX VIJUDID VIJUDID PNR FRAN LART ASCENDING.

   DEFINE TEMP-TABLE perskoll
   FIELD PERSONALKOD AS CHARACTER
   FIELD FORNAMN AS CHARACTER
   FIELD EFTERNAMN AS CHARACTER
   FIELD ANSTNR AS CHARACTER
   FIELD PNR AS CHARACTER
   FIELD OMRADETAB AS CHARACTER
   FIELD VIJUDID AS CHARACTER
   INDEX PNR IS PRIMARY PNR ASCENDING.