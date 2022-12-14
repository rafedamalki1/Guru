/*BESTMTRL.I*/


DEFINE {&NEW} {&SHARED} TEMP-TABLE best_mtrl    
    FIELD ENR AS CHARACTER
    FIELD BENAMNING AS CHARACTER
    FIELD ENHET AS CHARACTER
    FIELD PRIS AS DECIMAL
    FIELD OPRIS AS DECIMAL      
    FIELD ANTAL AS INTEGER    
    FIELD BESTANT AS INTEGER
    FIELD LEVKOD AS CHARACTER
    FIELD BERLEV AS CHARACTER   
    FIELD DBEST AS CHARACTER
   /* fler best per dag          */
    FIELD DATUM AS DATE
    FIELD DELNR AS INTEGER
    FIELD KLAR AS LOGICAL    
    FIELD LEVNAMN AS CHARACTER
    INDEX DATUM DATUM DELNR LEVKOD
    INDEX ENR IS PRIMARY ENR ASCENDING. 

DEFINE {&NEW} {&SHARED} TEMP-TABLE ebest_mtrl NO-UNDO LIKE best_mtrl.    
