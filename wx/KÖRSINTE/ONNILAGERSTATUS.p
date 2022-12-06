
/*------------------------------------------------------------------------
    File        : ONNILAGERSTATUS.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Oct 25 14:00:06 CEST 2017
    Notes       :
  ----------------------------------------------------------------------*/
USING Progress.Lang.Object.
USING Progress.Lang.Object.
USING OpenEdge.Core.WidgetHandle.
USING OpenEdge.Core.String.

USING OpenEdge.Net.HTTP.IHttpRequest.
USING OpenEdge.Net.HTTP.IHttpResponse.
USING OpenEdge.Net.HTTP.ClientBuilder.
USING OpenEdge.Net.HTTP.RequestBuilder.
USING OpenEdge.Net.HTTP.IHttpClient.

DEFINE TEMP-TABLE Onninen NO-UNDO
   FIELD ArticleNo AS CHARACTER 
   FIELD InStockQuantity AS CHARACTER .
{ESLAGERSTATUSTT.I}
DEFINE INPUT  PARAMETER pnrkod AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR eslagersatatustt.
{ONNILAGERWEB.I}
DEFINE VARIABLE onniart AS CHARACTER NO-UNDO.
DEFINE VARIABLE ivar AS INTEGER NO-UNDO.

   FOR EACH eslagersatatustt WHERE NO-LOCK:
      onniart = eslagersatatustt.ENR.
      IF SUBSTRING(onniart,1,1) = "E" THEN.
      ELSE onniart = "E" + onniart.
      oRequest =  RequestBuilder:Get('http://eai.onninen.com/prd/seatp?ArticleNo=' + onniart):REQUEST NO-ERROR.
      IF oRequest = ? THEN DO:
         MESSAGE "Ingen kontakt med Onninen eller så saknar ni behörighet!" 
         VIEW-AS ALERT-BOX.
         RETURN.
      END.  
     /*
      
         If ERROR-STATUS:NUM-MESSAGES > 0  THEN DO:
            MESSAGE "Ingen kontakt med Onninen eller så saknar ni behörighet!" SKIP 
            SKIP     "Vill du se felmeddelandena ?" 
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Fel på anslutningen"
            UPDATE view-errs AS LOGICAL .       
            IF view-errs THEN DO ivar = 1 TO ERROR-STATUS:NUM-MESSAGES:
               MESSAGE ERROR-STATUS:GET-NUMBER(ivar)
               ERROR-STATUS:GET-MESSAGE(ivar)
               VIEW-AS ALERT-BOX.
            END.     
            RETURN.     
         END.
      */    
      oResponse = oClient:Execute(oRequest).
      oEntity = oResponse:Entity.
      IF oResponse:StatusCode EQ 200 THEN DO:
         
         respDoc = CAST(oEntity, WidgetHandle):Value.
         respDoc:SAVE("LONGCHAR", onniArtikelLong).
         lRetOK = TEMP-TABLE Onninen:READ-XML("LONGCHAR", onniArtikelLong, "EMPTY", ?, FALSE).
         FIND FIRST Onninen WHERE NO-LOCK NO-ERROR.
         IF AVAILABLE Onninen THEN DO:       
            eslagersatatustt.ANTALILAGER = DECIMAL(Onninen.InStockQuantity).
            IF eslagersatatustt.ANTALILAGER >= eslagersatatustt.ANTAL THEN eslagersatatustt.SVAR = "OK".
            ELSE eslagersatatustt.SVAR = "För få i lager".
         END.
         ELSE DO:
            eslagersatatustt.SVAR = "Lagerförs inte!".
         END.   
      END.
      ELSE DO:
          {musarrow.i}
          MESSAGE "Det gick inte att ansluta till Onninens lager!" VIEW-AS ALERT-BOX.    
      END.
   END.