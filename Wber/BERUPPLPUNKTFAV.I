
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
   FOR EACH favberupptemp WHERE NO-LOCK:
      qString = "WHERE AONR = '" + "FAVO" + fanv + "' AND ANMARK = '" + favberupptemp.ADRESS + "'" .
      
      berpBuffer:FIND-FIRST(qString,NO-LOCK) NO-ERROR.
      IF berpBuffer:AVAILABLE THEN DO:
         BUFFER favberupptemp:HANDLE:BUFFER-COPY(berpBuffer,"DELNR,ANMARK,OMRADE").
         ASSIGN
         favberupptemp.ORT     =   berpBuffer:BUFFER-FIELD("PUNKTNAMN"):BUFFER-VALUE
         favberupptemp.POSTNR  =   berpBuffer:BUFFER-FIELD("VINKEL"):BUFFER-VALUE
         favberupptemp.XKORD   =   berpBuffer:BUFFER-FIELD("XKORD"):BUFFER-VALUE / 10000000   
         favberupptemp.YKORD   =   berpBuffer:BUFFER-FIELD("YKORD"):BUFFER-VALUE / 10000000.
         IF favberupptemp.XKORD > 0 AND favberupptemp.XKORDCH = "" THEN  favberupptemp.XKORDCH = STRING(favberupptemp.XKORD).
         IF favberupptemp.YKORD > 0 AND favberupptemp.YKORDCH = "" THEN  favberupptemp.YKORDCH = STRING(favberupptemp.YKORD).
        
      END.
      
   END.
   DELETE WIDGET-POOL "DynTableBU" NO-ERROR.