
/*------------------------------------------------------------------------
    File        : BERUPPLPACK.I
    Purpose     : 

    Syntax      :körs inte

    Description : 

    Author(s)   : 
    Created     : Mon Apr 04 17:44:25 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

   DEFINE VARIABLE anmhelp AS CHARACTER NO-UNDO.
   DEFINE VARIABLE anmhelpstart AS INTEGER NO-UNDO.
   DEFINE VARIABLE anmhelpslut AS INTEGER NO-UNDO. 
   DEFINE VARIABLE anmhelpeslut AS INTEGER NO-UNDO. 
   UPP:
   FOR EACH berupptemp WHERE NO-LOCK:
      anmhelp = berupptemp.ANMARK.
      IF INDEX(berupptemp.ANMARK,"$") = 0 THEN NEXT UPP.
      IF INDEX(berupptemp.ANMARK,"$") = 1  THEN DO:
         berupptemp.ANMARK = "".
      END.
      ELSE DO:
         berupptemp.ANMARK = SUBSTRIN(anmhelp,1,INDEX(anmhelp,"$") - 2).
      END.   
                                  
      anmhelpstart = INDEX(anmhelp,"$") + 1.
      anmhelpslut =  INDEX(anmhelp,"$",anmhelpstart).
      anmhelpeslut = anmhelpslut.
      anmhelpslut = anmhelpslut - anmhelpstart.
      
      
      IF anmhelpstart = 0 OR anmhelpslut = 0 THEN NEXT upp.
      
      berupptemp.POSTNR =  SUBSTRIN(anmhelp,anmhelpstart,anmhelpslut).  
      
      anmhelpstart = INDEX(anmhelp,"$",anmhelpeslut) + 1.
      anmhelpslut =  INDEX(anmhelp,"$",anmhelpstart).
      anmhelpeslut = anmhelpslut.
      anmhelpslut = anmhelpslut - anmhelpstart.
      IF anmhelpstart = 0 OR anmhelpslut = 0 THEN NEXT upp.
      berupptemp.ORT =  SUBSTRING(anmhelp,anmhelpstart,anmhelpslut).  
      anmhelpstart = INDEX(anmhelp,"$",anmhelpeslut) + 1.
      anmhelpslut =  INDEX(anmhelp,"$",anmhelpstart).
      anmhelpeslut = anmhelpslut.
      anmhelpslut = anmhelpslut - anmhelpstart.
      IF anmhelpstart = 0 OR anmhelpslut = 0 THEN NEXT upp.
      ccc
      berupptemp.XKORD = DECIMAL(SUBSTRING(anmhelp,anmhelpstart,anmhelpslut)).
      anmhelpstart = INDEX(anmhelp,"$",anmhelpeslut) + 1.
      anmhelpslut =  INDEX(anmhelp,"$",anmhelpstart).
      anmhelpeslut = anmhelpslut.
      anmhelpslut = anmhelpslut - anmhelpstart.
      IF anmhelpstart = 0 OR anmhelpslut = 0 THEN NEXT upp.
      berupptemp.YKORD = DECIMAL(SUBSTRING(anmhelp,anmhelpstart,anmhelpslut)).
   END.