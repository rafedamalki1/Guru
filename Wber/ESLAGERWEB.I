
/*------------------------------------------------------------------------
    File        : ESLAGERWEB.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Oct 26 13:57:51 CEST 2017
    Notes       :
  ----------------------------------------------------------------------*/
/*
   UserId1 ='esguru'. 
   Pwd = '63es5419'.
  */
   userID1 = CHR(101) + CHR(115) + CHR(103) + CHR(117) + CHR(114) + CHR(117) . 
   pwd = CHR(54) + CHR(51) + CHR(101) + CHR(115) + CHR(53) + CHR(52) + CHR(49) + CHR(57) .
   CREATE SERVER hWebService.
   
   
   lReturn = hWebService:CONNECT("-WSDL 'https://www.elektroskandia.se/service/ExternKundService.asmx?wsdl' -servername 'www.elektroskandia.se'") .
   

   
   /*  lReturn = hWebService:CONNECT("-WSDL 'https://www.elektroskandia.se/service/ExternKundService.asmx?wsdl' ").
   
        lReturn = hWebService:CONNECT("-WSDL 'https://www.elektroskandia.se/service/ExternKundService.asmx?wsdl' -servername 'www.elektroskandia.se'") .   
   -WSDLUserid = esguru -WSDLPassword 63es5419
   lReturn = hWebService:CONNECT("-WSDL 'https://www.elektroskandia.se/service/ExternKundService.asmx?wsdl -ServiceNamespace ElektroskandiaService'"). 
                                         https://www.elektroskandia.se/service/ExternKundService.asmx?wsdl -Namespace ElektroskandiaService*/
   IF lReturn = NO THEN DO:         
      resultvar = "OBS!!! Webtjänsten är inte i drift! Inget har skickats. " + "Kontakta Elpool 090/184540".
      hWebService:DISCONNECT() NO-ERROR.
      DELETE OBJECT hWebService NO-ERROR.
      hWebService = ?.
      /*
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      */
     MESSAGE resultvar
     VIEW-AS ALERT-BOX.
   END.
   IF ERROR-STATUS:NUM-MESSAGES > 0  THEN DO:
       MESSAGE "Ingen kontakt med Elektroskandia eller så saknar ni behörighet!" 
      VIEW-AS ALERT-BOX.
      RETURN.
      /*
      MESSAGE "Ingen kontakt med Elektroskandia eller så saknar ni behörighet!" SKIP 
      SKIP     "Vill du se felmeddelandena ?" 
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Fel på anslutningen"
      UPDATE view-errs AS LOGICAL .       
      IF view-errs THEN DO ivar = 1 TO ERROR-STATUS:NUM-MESSAGES:
         MESSAGE ERROR-STATUS:GET-NUMBER(ivar)
         ERROR-STATUS:GET-MESSAGE(ivar)
         VIEW-AS ALERT-BOX.
      END.     
      RETURN.
      */     
   END.   
   
   RUN ExternKundServiceSoap SET hExternKundServiceSoap ON hWebService.


   /* Function invocation of GetAvailableStock operation. */
   GetAvailableStockResult = GetAvailableStock(UserId1, Pwd, ItemNo).
   
   /*
   00918205
   
   My  provider of the web service claims that this should work. Because if you run this in a Powershell, it works
   
   $klient = New-WebServiceProxy -Uri https://www.elektroskandia.se/service/ExternKundService.asmx?wsdl -Namespace ElektroskandiaService
 
# parametrar för GetAvailableStock
[string]$anvandarnamn = "esguru";   # Ändra användarnamnet
[string]$losenord = "63es5419";           # Ändra lösenordet
[string]$artnr = "1830296";
 
# Svaret är (string) saldot för den aktuella artikeln, eller en info text om att artikeln inte lagerförs 
$svaret = $klient.GetAvailableStock($anvandarnamn, $losenord, $artnr)
 
Write-Host "Saldo för artikel $($artnr) är $($svaret)" -ForegroundColor Green
 */
   