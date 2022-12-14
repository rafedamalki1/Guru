/*SMTRL.I*/

/*?ndring 040909 inl?gg av f?lt NUM*/ 
DEFINE {&NEW} {&SHARED} TEMP-TABLE spec_mtrl NO-UNDO 
    FIELD NUM AS INTEGER 
    FIELD KALKNR AS INTEGER                                  
    FIELD BENAMNING AS CHARACTER FORMAT "x(40)" LABEL "Ben?mning"   
    FIELD ENHET AS  CHARACTER FORMAT "x(5)"   LABEL "Enhet"         
    FIELD PRIS AS DECIMAL                                 
    FIELD MARK AS LOGICAL                                  
    FIELD LINKAB AS LOGICAL LABEL "Lin/Kab"                
    FIELD MODUL AS INTEGER LABEL "Moduler"                 
    FIELD TYPBER AS LOGICAL                                
    FIELD DIAMETER AS INTEGER                              
    FIELD ROR AS LOGICAL                                   
    FIELD SATS AS LOGICAL                                                                         
    FIELD ARBKOD AS CHARACTER                             
    FIELD LOPNR AS INTEGER LABEL "L?pnr"                  
    FIELD ANTAL AS DECIMAL LABEL "Antal"
    FIELD DEPNR AS INTEGER FORMAT ">>9" LABEL "Dep?nr"              
    FIELD ENR AS  CHARACTER  FORMAT "x(11)" LABEL "Enr"                
    FIELD NPRIS AS  DECIMAL FORMAT ">>>>99.99" LABEL "Nettopris"   
    FIELD BPRIS AS  DECIMAL FORMAT ">>>>99.99"                      
    FIELD SUMMA AS  DECIMAL FORMAT ">>>>99.99"                      
    FIELD BERKVANT AS  DECIMAL FORMAT "->>>>9.99" LABEL "Antal"    
    FIELD LEVKOD AS  CHARACTER FORMAT "x(4)"                           
    FIELD BESTKVANT AS DECIMAL FORMAT ">>>>9" LABEL "Best-kvant"    
    FIELD BESTPUNKT AS INTEGER FORMAT ">>>>9" LABEL "Best-punkt"    
    FIELD INVANT AS INTEGER FORMAT ">>>>9" LABEL "Inv-antal"        
    FIELD BESTDATUM AS DATE FORMAT "99/99/99"                       
    FIELD FACKID AS  CHARACTER  FORMAT "x(8)" LABEL "Fackid"                      
    FIELD FACKID1 AS  CHARACTER  FORMAT "x(8)" LABEL "Fackid (aa)"                      
    FIELD FACKID2 AS  CHARACTER  FORMAT "x(8)" LABEL "+ (999999)"                      
    FIELD OMSATT AS DECIMAL LABEL "Oms?ttning/?r"                   
    FIELD SALDO AS INTEGER FORMAT ">>>>>9" LABEL "Saldo"           
    FIELD IB AS INTEGER                                             
    FIELD IBDATUM AS DATE                                           
    FIELD INVDATUM AS DATE                                          
    FIELD LAGER AS LOGICAL FORMAT "Ja/Nej"  LABEL "Lager"                                          
    FIELD KLAR AS LOGICAL
    FIELD KUND AS LOGICAL     
    FIELD EJMOT AS INTEGER    
    FIELD LEVNAMN AS CHARACTER  
    FIELD INDATETIME AS DATETIME                                
    INDEX ENR IS PRIMARY ENR LEVKOD                                       
    INDEX FACKID FACKID                                             
    INDEX BENAMNING BENAMNING.                                      
    
DEFINE {&NEW} {&SHARED} TEMP-TABLE espec_mtrl NO-UNDO LIKE spec_mtrl.
DEFINE TEMP-TABLE trspec_mtrl NO-UNDO LIKE spec_mtrl.

