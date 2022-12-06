
/*------------------------------------------------------------------------
    File        : BERUPPLPUNKT.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
       
    Created     : Mon Apr 04 17:44:25 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/
   DEFINE VARIABLE berpBuffer AS HANDLE NO-UNDO.
   DEFINE VARIABLE qString AS CHARACTER NO-UNDO.
   CREATE WIDGET-POOL "DynTableBU" NO-ERROR.
   CREATE BUFFER berpBuffer FOR TABLE "BERPUNKT" IN WIDGET-POOL "DynTableBU". 
   FOR EACH berupptemp WHERE NO-LOCK:
      qString = "WHERE AONR = '" + berupptemp.AONR + "' AND OMRADE = '" + berupptemp.OMRADE + "' AND NUM = " + STRING(berupptemp.UPPLAG).
      berpBuffer:FIND-FIRST(qString,NO-LOCK) NO-ERROR.
      IF berpBuffer:AVAILABLE THEN DO:
         BUFFER berupptemp:HANDLE:BUFFER-COPY(berpBuffer,"DELNR,ANMARK,OMRADE").
         ASSIGN
         berupptemp.ORT     =   berpBuffer:BUFFER-FIELD("PUNKTNAMN"):BUFFER-VALUE
         berupptemp.POSTNR  =   berpBuffer:BUFFER-FIELD("VINKEL"):BUFFER-VALUE
         berupptemp.XKORD   =   berpBuffer:BUFFER-FIELD("XKORD"):BUFFER-VALUE / 10000000   
         berupptemp.YKORD   =   berpBuffer:BUFFER-FIELD("YKORD"):BUFFER-VALUE / 10000000.
         IF berupptemp.XKORD > 0 AND berupptemp.XKORDCH = "" THEN  berupptemp.XKORDCH = STRING(berupptemp.XKORD).
         IF berupptemp.YKORD > 0 AND berupptemp.YKORDCH = "" THEN  berupptemp.YKORDCH = STRING(berupptemp.YKORD).
         
      END.
      
   END.
   DELETE WIDGET-POOL "DynTableBU" NO-ERROR.