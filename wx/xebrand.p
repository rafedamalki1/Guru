
DEFINE NEW SHARED TEMP-TABLE temp_kund 
   FIELD ARBKOD LIKE P1.ARBKOD
   FIELD LOPNR LIKE LOP1.LOPNR
   FIELD BENAMNING LIKE LOP1.BENAMNING
   FIELD ENHET LIKE LOP1.ENHET
   FIELD F1 LIKE LOP1.F1
   FIELD F2 LIKE LOP1.F2
   FIELD F3 LIKE LOP1.F3   
   FIELD MATERIEL LIKE LOP1.MATERIEL
   FIELD OVRIGT LIKE LOP1.OVRIGT
   FIELD EA LIKE LOP1.EA
   FIELD ARBETE LIKE LOP1.ARBETE
   FIELD MASKINKOST LIKE LOP1.MASKINKOST.
   
FOR EACH LOP1:
   CREATE temp_kund.
   ASSIGN
   temp_kund.ARBKOD = LOP1.ARBKOD
   temp_kund.LOPNR = LOP1.LOPNR
   temp_kund.BENAMNING = LOP1.BENAMNING
   temp_kund.ENHET = LOP1.ENHET
   temp_kund.MATERIEL = LOP1.MATERIEL
   temp_kund.OVRIGT = LOP1.OVRIGT
   temp_kund.F1 = LOP1.F2
   temp_kund.F2 = LOP1.F3
   temp_kund.F3 = LOP1.F4.
END.      
   
FOR EACH temp_kund: 
   temp_kund.ARBETE = (temp_kund.F1 * 464). 
   temp_kund.EA = temp_kund.F1 + ((temp_kund.F2 * 405) / 239) + ((temp_kund.F3 * 365) / 239).
   temp_kund.MASKINKOST = ((temp_kund.F2 * 405) + (temp_kund.F3 * 365)).       
END.   

FOR EACH LOP1:
   DELETE LOP1.
END.   

FOR EACH temp_kund:      
   CREATE LOP1.
   ASSIGN
   LOP1.ARBKOD = temp_kund.ARBKOD
   LOP1.LOPNR = temp_kund.LOPNR
   LOP1.BENAMNING = temp_kund.BENAMNING   
   LOP1.ENHET = temp_kund.ENHET      
   LOP1.F1 = temp_kund.F1
   LOP1.F2 = temp_kund.F2
   LOP1.F3 = temp_kund.F3
   LOP1.EA = temp_kund.EA
   LOP1.ARBETE = temp_kund.ARBETE
   LOP1.MASKINKOST = temp_kund.MASKINKOST   
   LOP1.MATERIEL = temp_kund.MATERIEL
   LOP1.OVRIGT = temp_kund.OVRIGT. 
END.  
