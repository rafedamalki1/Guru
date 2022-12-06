/* KALKTEMPTAB.I*/
 FIELD ARBKOD AS CHARACTER                             
 FIELD LOPNR AS INTEGER                                
 FIELD BENAMNING AS CHARACTER                          
 FIELD ENHET AS CHARACTER                              
 FIELD F1 AS DECIMAL                                   
 FIELD F2 AS DECIMAL                                   
 FIELD F3 AS DECIMAL                                   
 FIELD F4 AS DECIMAL                                   
 FIELD F5 AS DECIMAL                                   
 FIELD F6 AS DECIMAL                                   
 FIELD F7 AS DECIMAL                                   
 FIELD F8 AS DECIMAL                                   
 FIELD F9 AS DECIMAL                                   
 FIELD F10 AS DECIMAL                                  
 FIELD EA AS DECIMAL 
 FIELD EAST AS DECIMAL
 FIELD ARBETE AS DECIMAL                               
 FIELD MATERIEL AS DECIMAL                             
 FIELD MASKINKOST AS DECIMAL                           
 FIELD OVRIGT AS DECIMAL                               
 FIELD ENTRP AS DECIMAL                                
 FIELD ANTAL AS DECIMAL                                
 FIELD UTRUST AS DECIMAL                               
 FIELD UTRUSTKOST AS DECIMAL                                   
 FIELD MASKINTIMMAR AS DECIMAL                   
 FIELD PROCENT AS INTEGER                        
 FIELD NUM AS INTEGER                     
 FIELD UPPLAG AS INTEGER                                                                  
 FIELD GRUPP AS INTEGER                                                                   
 FIELD XKORD AS INTEGER                   
 INDEX KOD ARBKOD LOPNR ASCENDING      

 INDEX MATRIS F10 ARBKOD LOPNR ASCENDING
         
 INDEX NUM NUM ARBKOD LOPNR ASCENDING.    
