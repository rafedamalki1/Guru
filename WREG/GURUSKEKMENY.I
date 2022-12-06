/*GURUSKEKMENY.I*/
DEFINE {&NEW} {&SHARED} TEMP-TABLE xgurutemp NO-UNDO  
   FIELD AV-LEVEL AS INTEGER                          
   FIELD MENY AS CHARACTER                            
   FIELD MENYOK AS LOGICAL                            
   FIELD ORDNING AS INTEGER                           
   FIELD MEDOK AS LOGICAL                             
   FIELD MENYVART AS CHARACTER                        
   FIELD KOPPLING AS INTEGER                          
   FIELD STORKOPPLING AS INTEGER                      
   INDEX MENYVART IS PRIMARY MENYVART AV-LEVEL ORDNING 
   INDEX AV-LEVEL AV-LEVEL                            
   INDEX KOPPLING KOPPLING MENYVART                   
   INDEX STORKOPPLING STORKOPPLING MENYVART
   INDEX ORDNING ORDNING.
   
/*
          DEFINE {&NEW} {&SHARED} TEMP-TABLE xgurutemp NO-UNDO
             FIELD STORKOPPLING AS INTEGER                    
             FIELD ORDNING      AS INTEGER                    
             FIELD MENYVART    AS CHARACTER                   
             FIELD MENYOK      AS LOGICAL                     
             FIELD MENY        AS CHARACTER                   
             FIELD MEDOK       AS LOGICAL                     
             FIELD KOPPLING    AS INTEGER                     
             FIELD AV-LEVEL    AS INTEGER                     
             INDEX MENYVART IS PRIMARY MENYVART AV-LEVEL      
             INDEX ORDNING ORDNING.                           
             */
