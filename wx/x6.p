def var aa as char.
aa = '" "' .
OUTPUT TO kalkyl.d convert target "iso8859-1" source "iso8859-1".
FOR EACH kalkyl :
 IF KALKYL.PERSMAS = ? THEN KALKYL.PERSMASK = FALSE.
 put   
   RECKALKYL "$"
   BEFATTNING "$"  
   string(PRIS,"->>>>>>>>>>>9.99") "$" 
   TIMMAR  "$"  
   OT50 "$" 
   OT75 "$" 
   OT100 "$"  
   PERSMASK "$"  
   TYP "$"
   RADNR "$" 
   OMRADE "$" skip.

END.  
output close.
OUTPUT TO kalkspec.d convert target "iso8859-1" source "iso8859-1".
for each kalkspec:
put unformatted
 KALKNR "$"
  KALKTEXT "$"
  OMRADE "$" 
  BESTID "$" 
  KALKANV "$" 
  FASTPRIS "$" 
  ANVANDARE "$" 
  AONR "$" 
  DELNR "$" 
  AKTIV "$" 
  VINSTPA "$" 
  OMKOSTP "$"
  KUNDPRISF "$"
  OMKMTR "$"  
  STARTDAG "$"
  SLUTDAG "$"
  STARTVNR "$"
  SLUTVNR "$"
  FRIKUND "$" 
  KALKPRIS "$" 
  PLANNR "$"   
  ARTAL "$" 
  recid(kalkspec) "$" skip.
end.
