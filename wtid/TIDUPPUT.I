/*TIDUPPUT.I*/       
{TIDAPPDEF.I}

DEFINE INPUT PARAMETER TABLE FOR tidapptemp.

FIND FIRST tidapptemp NO-LOCK NO-ERROR.
ASSIGN

persrec = tidapptemp.RECPERS 
tidtabrec = tidapptemp.RECTID
regdatum = tidapptemp.DATUM.      
     
      
