/*KOMPTEMP.I*/
   FIELD NUM AS INTEGER                   
   FIELD F1 AS CHARACTER                  
   FIELD GRUPP AS INTEGER                 
   FIELD FORNR AS CHARACTER               
   FIELD LINNR AS CHARACTER               
   FIELD NATNR AS CHARACTER               
   FIELD FRI1 AS INTEGER                  
   FIELD FRI2 AS INTEGER                  
   FIELD UPPLAG AS INTEGER                
   FIELD KOMB AS INTEGER                  
   /*                                     
   FIELD XKORD AS INTEGER                 
   */                                     
   FIELD YKORD AS INTEGER                 
   FIELD ID AS LOGICAL                    
   FIELD ORD AS INTEGER                   
   FIELD ID2 AS CHARACTER FORMAT "X(10)"  
   INDEX NUM IS PRIMARY NUM ASCENDING     
   INDEX UPPLAG UPPLAG                    
   INDEX KOMB KOMB                        
   /*                                     
   INDEX X XKORD ASCENDING                
   */                                     
   INDEX ORD ORD ASCENDING.               
