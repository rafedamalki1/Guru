/*TIDUPPUTW.I*/       

{TIDAPPDEF.I}
DEFINE INPUT PARAMETER TABLE FOR tidapptemp.
DEFINE INPUT PARAMETER TABLE FOR extratidallt.
DEFINE OUTPUT PARAMETER placerarec AS RECID.
DEFINE OUTPUT PARAMETER TABLE FOR tidallt.
FIND FIRST tidapptemp NO-LOCK NO-ERROR.
ASSIGN
 
persrec = tidapptemp.RECPERS 
tidtabrec = tidapptemp.RECTID
regdatum = tidapptemp.DATUM.      
     
      
