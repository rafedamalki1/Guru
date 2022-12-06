/*FAKBIL.I*/
IF fakbilag.TIDMED = TRUE AND fakbilag.TIDEJMED = TRUE THEN DO:                                          
   FOR EACH sumtidtemp BREAK 
   BY sumtidtemp.MED BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR BY sumtidtemp.TRAKTKOD:         
      {FAKBILTR.I}      
   END.
   
   FOR EACH sumtidtemp BREAK 
   BY sumtidtemp.MED BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR BY sumtidtemp.LONTILLAGG:         
      {FAKBILLO.I}               
   END.
   
END.

ELSE IF fakbilag.TIDMED = TRUE THEN DO:                                        
   FOR EACH sumtidtemp WHERE sumtidtemp.MED = TRUE BREAK 
   BY sumtidtemp.MED BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR BY sumtidtemp.TRAKTKOD:         
      {FAKBILTR.I}         
   END.
   FOR EACH sumtidtemp WHERE sumtidtemp.MED = TRUE BREAK 
   BY sumtidtemp.MED BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR BY sumtidtemp.LONTILLAGG:         
      {FAKBILLO.I}         
   END.
END.            
ELSE IF fakbilag.TIDEJMED = TRUE THEN DO:                                        
   FOR EACH sumtidtemp WHERE sumtidtemp.MED = FALSE BREAK 
   BY sumtidtemp.MED BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR BY sumtidtemp.TRAKTKOD:         
      {FAKBILTR.I}
   END.
   FOR EACH sumtidtemp WHERE sumtidtemp.MED = FALSE BREAK 
   BY sumtidtemp.MED BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR BY sumtidtemp.LONTILLAGG:         
      {FAKBILLO.I}         
   END.        
END.

