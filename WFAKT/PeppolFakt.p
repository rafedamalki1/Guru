
/*------------------------------------------------------------------------
    File        : PeppolFakt.p
    Purpose     : 
DEFINE INPUT PARAMETER kredfakt AS LOGICAL NO-UNDO.  /*FRÅGA*/ hur visas kredit faktura
Invoicepeppol.InvoiceTypeCode = "380"               /*FRÅGA*/
Invoicepeppol.Note = "Kundnummer:" + STRING(fakturaexcelTT.FAKTNR)            /*FRÅGA*/
 Party.EndpointID = "elpool".  /*FRÅGA*/   fårt GLN
 PartyIdentification.ID = "elpool".

köpare
Party.EndpointID =  Epeppolin.PEPPOL. /*FRÅGA*/.
PartyIdentification.ID = Epeppolin.PEPPOL. /*FRÅGA*/

bankgiro
PayeeFinancialAccount.ID = "HANDSESS / SE73 6000 0000 0007 8820 3088". /*FRÅGA*/
PayeeFinancialAccount.ID = "750-1976".

"KOMMENTAR"  RUN Invoicepeppol_UI (INPUT "ItemTT",INPUT RECID(InvoiceLine)) . /*FRÅGA*/
 
    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Nov 26 16:45:11 CET 2019
    Notes       :
  ----------------------------------------------------------------------*/
{ExcelDS.i}
{FAKTMAILTT.I}
{PEPPOLTT.I}
{PEPPOLDS.I}
DEFINE INPUT PARAMETER kredfakt AS LOGICAL NO-UNDO.  /*FRÅGA*/
DEFINE INPUT PARAMETER faknnamn AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR Epeppolin.
DEFINE INPUT PARAMETER TABLE FOR fakturaexcelTT.
DEFINE INPUT PARAMETER TABLE FOR faktposterexcelTT.



DEFINE VARIABLE InvoicepeppolDSh AS HANDLE NO-UNDO.

DEFINE VARIABLE filelong AS LONGCHAR NO-UNDO.
DEFINE VARIABLE tembuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE Efaktpool AS CHARACTER NO-UNDO.
Efaktpool = "Efaktpool" + STRING(TIME).
CREATE WIDGET-POOL Efaktpool NO-ERROR.
FIND FIRST Epeppolin WHERE NO-LOCK NO-ERROR.
FIND FIRST fakturaexcelTT WHERE NO-LOCK NO-ERROR.
RUN InvoicepeppolStart_UI.
InvoicepeppolDSh = DATASET InvoicepeppolDS:HANDLE.

RUN writexmlomini_UI (INPUT InvoicepeppolDSh ,INPUT "LONGCHAR",INPUT faknnamn ,INPUT "UTF-8",INPUT "",INPUT YES,INPUT NO,INPUT NO, INPUT NO, INPUT NO).
/*level 0*/
FUNCTION DecBelopp RETURNS DECIMAL
  ( INPUT belopp AS CHARACTER):
  RETURN  DECIMAL(REPLACE(belopp,",",".")).

END FUNCTION.
PROCEDURE InvoicepeppolStart_UI :
   RUN Invoicepeppol_UI (INPUT "Invoicepeppol",INPUT  ?) .
   FIND FIRST Invoicepeppol WHERE NO-LOCK NO-ERROR.
   IF AVAILABLE Invoicepeppol THEN DO:
      
      ASSIGN                         /*    urn:cen.eu:en16931:2017#conformant#urn:fdc:peppol.eu:2017:poacc:billing:international:aunz:3.0*/
      Invoicepeppol.CustomizationID = "urn:cen.eu:en16931:2017#compliant#urn:fdc:peppol.eu:2017:poacc:billing:3.0"
      Invoicepeppol.ProfileID = "urn:fdc:peppol.eu:2017:poacc:billing:01:1.0"
      Invoicepeppol.ID = STRING(fakturaexcelTT.FAKTNUMMER)
      Invoicepeppol.IssueDate = fakturaexcelTT.EFAKTDATUM
      Invoicepeppol.DueDate = fakturaexcelTT.EFORFALLDATUM
      Invoicepeppol.InvoiceTypeCode = "380"               /*FRÅGA*/
      Invoicepeppol.Note = "Kundnummer:" + STRING(fakturaexcelTT.FAKTNR)            /*FRÅGA*/  
      Invoicepeppol.TaxPointDate = fakturaexcelTT.EFAKTDATUM /*fakturaexcelTT.BOKDATUM*/
      Invoicepeppol.DocumentCurrencyCode = "SEK" 
      Invoicepeppol.AccountingCost = fakturaexcelTT.ERREF
      Invoicepeppol.BuyerReference = Epeppolin.BUYREF.
      RUN Invoicepeppol_UI (INPUT "AccountingSupplierParty",INPUT RECID(Invoicepeppol)) .
      RUN AccountingSupplierParty_UI.
      RUN Invoicepeppol_UI (INPUT "AccountingCustomerParty",INPUT RECID(Invoicepeppol)) .
      RUN AccountingCustomerParty_UI.
       
      RUN Invoicepeppol_UI (INPUT "Delivery",INPUT RECID(Invoicepeppol)) .
      RUN Delivery_UI.
      RUN Invoicepeppol_UI (INPUT "PaymentMeans",INPUT RECID(Invoicepeppol)) .
      RUN PaymentMeans_UI.
      RUN Invoicepeppol_UI (INPUT "PaymentTerms",INPUT RECID(Invoicepeppol)) .
      RUN PaymentTerms_UI.
      RUN Invoicepeppol_UI (INPUT "TaxTotal",INPUT RECID(Invoicepeppol)) .
      RUN TaxTotal_UI.
      RUN Invoicepeppol_UI (INPUT "LegalMonetaryTotal",INPUT RECID(Invoicepeppol)) .
      RUN LegalMonetaryTotal_UI.
      RUN InvoiceLine_UI.
   END.
   
END PROCEDURE.
/*level 1*/
PROCEDURE AccountingSupplierParty_UI :
   FIND FIRST AccountingSupplierParty WHERE NO-LOCK NO-ERROR.
   IF AVAILABLE AccountingSupplierParty THEN DO:
      RUN Invoicepeppol_UI (INPUT "Party",INPUT RECID(AccountingSupplierParty)) .
      FIND FIRST Party  WHERE Party.RID = RECID(AccountingSupplierParty) NO-LOCK NO-ERROR. 
      IF AVAILABLE Party THEN DO:
         Party.EndpointID = "elpool".  /*FRÅGA*/
         RUN Party_UI.
      END.   
      FIND FIRST PartyIdentification  WHERE PartyIdentification.RID = RECID(Party) NO-LOCK NO-ERROR.
      IF AVAILABLE PartyIdentification THEN DO:
         PartyIdentification.ID = "elpool". /*FRÅGA*/
      END.   
      FIND FIRST PartyName  WHERE PartyName.RID = RECID(Party) NO-LOCK NO-ERROR.
      IF AVAILABLE PartyName THEN DO:
         PartyName.NAME = "Elpool i Umeå AB".
      END.    
      FIND FIRST PostalAddress  WHERE PostalAddress.RID = RECID(Party) NO-LOCK NO-ERROR.
      IF AVAILABLE PostalAddress THEN DO:
         ASSIGN
         PostalAddress.StreetName = "Västra Norrlandsgatan 11d"
         PostalAddress.CityName = "Umeå"
         PostalAddress.PostalZone = "903 27".
         FIND FIRST Country WHERE Country.RID = RECID(PostalAddress) NO-LOCK NO-ERROR.
         IF AVAILABLE Country THEN DO:
            ASSIGN
            Country.IdentificationCode = "SE".
         END.
      END. 
      FIND FIRST PartyTaxScheme  WHERE PartyTaxScheme.RID = RECID(Party) NO-LOCK NO-ERROR.  
      IF AVAILABLE PartyTaxScheme THEN DO:
         ASSIGN 
         PartyTaxScheme.CompanyID = "SE556423000001".   
      END.
      RUN Invoicepeppol_UI (INPUT "PartyTaxScheme",INPUT RECID(Party)) .
      FIND FIRST PartyTaxScheme  WHERE PartyTaxScheme.RID = RECID(Party) AND PartyTaxScheme.CompanyID = "" NO-LOCK NO-ERROR.
      IF AVAILABLE PartyTaxScheme THEN DO: 
         PartyTaxScheme.CompanyID = "Godkänd för F-skatt".
         RUN Invoicepeppol_UI (INPUT "TaxScheme",INPUT RECID(PartyTaxScheme)) .
         FIND FIRST TaxScheme WHERE TaxScheme.RID = RECID(PartyTaxScheme) NO-LOCK NO-ERROR.
         IF AVAILABLE TaxScheme THEN DO:
            TaxScheme.ID = "TAX". 
         END.    
      END.
      FIND FIRST PartyLegalEntity  WHERE PartyLegalEntity.RID = RECID(Party) NO-LOCK NO-ERROR.  
      IF AVAILABLE PartyLegalEntity THEN DO:
         ASSIGN 
         PartyLegalEntity.RegistrationName = "Elpool i Umeå AB"
         PartyLegalEntity.CompanyID = "556423-0000"
         PartyLegalEntity.CompanyLegalForm = "Datakonsulter".   
      END.
      FIND FIRST Contact  WHERE Contact.RID = RECID(Party) NO-LOCK NO-ERROR.
        
      IF AVAILABLE Contact THEN DO:
         ASSIGN 
         Contact.NameC = "Elpool i Umeå AB"
         Contact.Telephone = "090-184540"
         Contact.ElectronicMail = "elpool.ume@elpool.se".
      END.
   END.
END PROCEDURE.
PROCEDURE AccountingCustomerParty_UI :
   FIND FIRST AccountingCustomerParty WHERE NO-LOCK NO-ERROR.
   IF AVAILABLE AccountingCustomerParty THEN DO:
      RUN Invoicepeppol_UI (INPUT "Party",INPUT RECID(AccountingCustomerParty)) .
      FIND FIRST Party  WHERE Party.RID = RECID(AccountingCustomerParty) NO-LOCK NO-ERROR.
      IF AVAILABLE Party THEN DO:
         Party.EndpointID =  Epeppolin.PEPPOL. /*FRÅGA*/.
         RUN Party_UI. 
      END.   
      FIND FIRST PartyIdentification  WHERE PartyIdentification.RID = RECID(Party) NO-LOCK NO-ERROR.
      IF AVAILABLE PartyIdentification THEN DO:
         PartyIdentification.ID = Epeppolin.PEPPOL. /*FRÅGA*/
      END.  
      
         
      FIND FIRST PartyName  WHERE PartyName.RID = RECID(Party) NO-LOCK NO-ERROR.
      IF AVAILABLE PartyName THEN DO:
         PartyName.NAME = fakturaexcelTT.BESTNAMN.
      END.    
      FIND FIRST PostalAddress  WHERE PostalAddress.RID = RECID(Party) NO-LOCK NO-ERROR.
      IF AVAILABLE PostalAddress THEN DO:
         ASSIGN
         PostalAddress.StreetName = TRIM(SUBSTRING(fakturaexcelTT.FAKTADRESS,1,25))
         PostalAddress.CityName = TRIM(SUBSTRING(fakturaexcelTT.FAKTORT,1,25))
         PostalAddress.PostalZone = STRING(fakturaexcelTT.FAKTPOSTNR,"999 99") .
         FIND FIRST Country WHERE Country.RID = RECID(PostalAddress) NO-LOCK NO-ERROR.
         IF AVAILABLE Country THEN DO:
            ASSIGN
            Country.IdentificationCode = "SE".
         END.
      END. 
      FIND FIRST PartyTaxScheme  WHERE PartyTaxScheme.RID = RECID(Party) NO-LOCK NO-ERROR.  
      IF AVAILABLE PartyTaxScheme THEN DO:
         ASSIGN 
         PartyTaxScheme.CompanyID = Epeppolin.VAT.   
      END.
      
      FIND FIRST PartyLegalEntity  WHERE PartyLegalEntity.RID = RECID(Party) NO-LOCK NO-ERROR.  
      IF AVAILABLE PartyLegalEntity THEN DO:
         ASSIGN 
         PartyLegalEntity.RegistrationName = fakturaexcelTT.BESTNAMN
         PartyLegalEntity.CompanyID = Epeppolin.ORGNR.
         /*
         PartyLegalEntity.CompanyLegalForm = "Sundsvall".   /*FRÅGA*/
         */ 
      END.
      
      FIND FIRST Contact  WHERE Contact.RID = RECID(Party) NO-LOCK NO-ERROR.
      DELETE  Contact. 
      IF AVAILABLE Contact THEN DO:
         ASSIGN 
         Contact.NameC = fakturaexcelTT.BESTNAMN.
        /*
         Contact.Telephone = "010-205 20 00"   /*FRÅGA*/
         Contact.ElectronicMail = "ekonomi@folkhalsomyndigheten.se".
         */
      END.
      
   END. 
END PROCEDURE.
PROCEDURE Delivery_UI :
   FIND FIRST Delivery WHERE  NO-LOCK NO-ERROR.
   IF AVAILABLE Delivery THEN DO:
      Delivery.ActualDeliveryDate = DATE(MONTH(TODAY),01,YEAR(TODAY)).  
      RUN Invoicepeppol_UI (INPUT "DeliveryLocation",INPUT RECID(Delivery)) .
      FIND FIRST DeliveryLocation  WHERE DeliveryLocation.RID = RECID(Delivery) NO-LOCK NO-ERROR.
      IF AVAILABLE DeliveryLocation THEN DO:
         DeliveryLocation.ID = Epeppolin.ORGNR.
         RUN Invoicepeppol_UI (INPUT "Address",INPUT RECID(DeliveryLocation)). 
         RUN Address_UI.
       
      END.     
   END.
END PROCEDURE.
PROCEDURE PaymentMeans_UI :
   FIND FIRST PaymentMeans WHERE NO-LOCK NO-ERROR.
   IF AVAILABLE PaymentMeans THEN DO:
      ASSIGN
      PaymentMeans.PaymentMeansCode = 30
      PaymentMeans.PaymentID = fakturaexcelTT.FAKTNUMMER.
      RUN Invoicepeppol_UI (INPUT "PayeeFinancialAccount",INPUT RECID(PaymentMeans)) .
      RUN PayeeFinancialAccount_UI.  
   END.
END PROCEDURE.
PROCEDURE PaymentTerms_UI :
   FIND FIRST PaymentTerms WHERE NO-LOCK NO-ERROR.
   IF AVAILABLE PaymentTerms THEN DO:
      PaymentTerms.Note = "30 dagar netto".   
   END.
END PROCEDURE.
PROCEDURE TaxTotal_UI :
   FIND FIRST TaxTotal WHERE NO-LOCK NO-ERROR.
   IF AVAILABLE TaxTotal THEN DO:
      TaxTotal.TaxAmount = DecBelopp(fakturaexcelTT.MOMS). 
      RUN Invoicepeppol_UI (INPUT "TaxSubtotal",INPUT RECID(TaxTotal)) .
      RUN TaxSubtotal_UI.  
   END.
END PROCEDURE.
PROCEDURE LegalMonetaryTotal_UI :
   FIND FIRST LegalMonetaryTotal WHERE NO-LOCK NO-ERROR.
   IF AVAILABLE LegalMonetaryTotal THEN DO:
      ASSIGN
      LegalMonetaryTotal.LineExtensionAmount = DecBelopp(fakturaexcelTT.SUMMA)
      LegalMonetaryTotal.TaxExclusiveAmount = DecBelopp(fakturaexcelTT.SUMMA)
      LegalMonetaryTotal.TaxInclusiveAmount = DecBelopp(fakturaexcelTT.SLUTSUMMA)
      LegalMonetaryTotal.AllowanceTotalAmount = 0.00
      LegalMonetaryTotal.ChargeTotalAmount = 0.00
      LegalMonetaryTotal.PayableAmount = DecBelopp(fakturaexcelTT.SLUTSUMMA).
   END.
END PROCEDURE.
PROCEDURE InvoiceLine_UI :
   DEFINE VARIABLE InvoiceLineNr AS INTEGER NO-UNDO.
   MESSAGE "InvoiceLine"
   VIEW-AS ALERT-BOX.
   FOR EACH faktposterexcelTT WHERE faktposterexcelTT.ANTAL NE "KOMMENTAR" NO-LOCK:
     /*
      RUN Invoicepeppol_UI (INPUT "InvoiceLine",INPUT RECID(Invoicepeppol)) .
      */
      CREATE InvoiceLine.
      InvoiceLineNr = InvoiceLineNr + 1.
      ASSIGN
      InvoiceLine.RID = RECID(Invoicepeppol)
      InvoiceLine.ID = InvoiceLineNr
      InvoiceLine.Note = faktposterexcelTT.BESKIVNING
      InvoiceLine.InvoicedQuantity = DecBelopp(faktposterexcelTT.ANTAL)
      InvoiceLine.LineExtensionAmount = DecBelopp(faktposterexcelTT.SUMMA).
     /*
      RUN Invoicepeppol_UI (INPUT "ItemTT",INPUT RECID(InvoiceLine)) .
      RUN ItemTT_UI.*/
      RUN Invoicepeppol_UI (INPUT "Price",INPUT RECID(InvoiceLine)) .
      RUN Price_UI.
           
   END.
    FOR EACH faktposterexcelTT WHERE faktposterexcelTT.ANTAL = "KOMMENTAR" NO-LOCK:
      IF faktposterexcelTT.BESKIVNING NE "" THEN DO:
         CREATE InvoiceLine.
         InvoiceLineNr = InvoiceLineNr + 1.
         ASSIGN
         InvoiceLine.RID = RECID(Invoicepeppol)
         InvoiceLine.ID = InvoiceLineNr
         InvoiceLine.Note = faktposterexcelTT.ANTAL.
         RUN Invoicepeppol_UI (INPUT "ItemTT",INPUT RECID(InvoiceLine)) . /*FRÅGA*/
         FIND FIRST ItemTT  WHERE ItemTT.RID = RECID(InvoiceLine) NO-LOCK NO-ERROR.
         IF AVAILABLE ItemTT THEN DO:
            ItemTT.NameID = faktposterexcelTT.BESKIVNING.
         END. 
         /*
         RUN Invoicepeppol_UI (INPUT "Price",INPUT RECID(InvoiceLine)) .
         RUN Price_UI.
         */
      END.        
   END.
END PROCEDURE.
/*level 2*/
PROCEDURE Party_UI :
   IF AVAILABLE Party THEN DO:   
      RUN Invoicepeppol_UI (INPUT "PartyIdentification",INPUT RECID(Party)) .
      RUN Invoicepeppol_UI (INPUT "PartyName",INPUT RECID(Party)) .
      RUN Invoicepeppol_UI (INPUT "PostalAddress",INPUT RECID(Party)) .
      RUN Invoicepeppol_UI (INPUT "PartyTaxScheme",INPUT RECID(Party)) .
      RUN Invoicepeppol_UI (INPUT "PartyLegalEntity",INPUT RECID(Party)) .
      RUN Invoicepeppol_UI (INPUT "Contact",INPUT RECID(Party)) .
      FIND FIRST PostalAddress  WHERE PostalAddress.RID = RECID(Party) NO-LOCK NO-ERROR.  
      IF AVAILABLE PostalAddress THEN DO:
         RUN Invoicepeppol_UI (INPUT "Country",INPUT RECID(PostalAddress)) .
      END.
      FIND FIRST PartyTaxScheme  WHERE PartyTaxScheme.RID = RECID(Party) NO-LOCK NO-ERROR.  
      IF AVAILABLE PartyTaxScheme THEN DO:
         RUN Invoicepeppol_UI (INPUT "TaxScheme",INPUT RECID(PartyTaxScheme)) .
         FIND FIRST TaxScheme WHERE TaxScheme.RID = RECID(PartyTaxScheme) NO-LOCK NO-ERROR.
         IF AVAILABLE TaxScheme THEN DO:
            TaxScheme.ID = "VAT". 
         END.    
      END.
      
   END.
END PROCEDURE.     
PROCEDURE Address_UI :
   FIND FIRST Address WHERE NO-LOCK NO-ERROR.
   IF AVAILABLE Address THEN DO:
      ASSIGN 
      Address.StreetName = TRIM(SUBSTRING(fakturaexcelTT.FAKTADRESS,1,25))
      Address.CityName = TRIM(SUBSTRING(fakturaexcelTT.FAKTORT,1,25))
      Address.PostalZone = fakturaexcelTT.FAKTPOSTNR .
      RUN Invoicepeppol_UI (INPUT "Country",INPUT RECID(Address)) .
      FIND FIRST Country WHERE Country.RID = RECID(Address) NO-LOCK NO-ERROR.
      IF AVAILABLE Country THEN DO:
         ASSIGN
         Country.IdentificationCode = "SE".
      END.
   END.   
END PROCEDURE.

PROCEDURE PayeeFinancialAccount_UI :
   FIND FIRST PayeeFinancialAccount WHERE PayeeFinancialAccount.RID = RECID(PaymentMeans) NO-LOCK NO-ERROR.
   IF AVAILABLE PayeeFinancialAccount THEN DO:
      ASSIGN 
      /*
      PayeeFinancialAccount.ID = "HANDSESS / SE73 6000 0000 0007 8820 3088". /*FRÅGA*/
      */
      PayeeFinancialAccount.ID = "750-1976".
      RUN Invoicepeppol_UI (INPUT "FinancialInstitutionBranch",INPUT RECID(PayeeFinancialAccount)) .
      FIND FIRST FinancialInstitutionBranch WHERE FinancialInstitutionBranch.RID = RECID(PayeeFinancialAccount) NO-LOCK NO-ERROR.
      IF AVAILABLE FinancialInstitutionBranch THEN DO:
         FinancialInstitutionBranch.ID = "SE:BANKGIRO".
      END.   
   END.   
END PROCEDURE.

PROCEDURE TaxSubtotal_UI :
   FIND FIRST TaxSubtotal WHERE NO-LOCK NO-ERROR.
   IF AVAILABLE TaxSubtotal THEN DO:
      ASSIGN 
      TaxSubtotal.TaxableAmount = DecBelopp(fakturaexcelTT.SUMMA)
      TaxSubtotal.TaxAmount = DecBelopp(fakturaexcelTT.MOMS).
      
      RUN Invoicepeppol_UI (INPUT "TaxCategory",INPUT RECID(TaxSubtotal)) .
      RUN TaxCategory_UI.
   END. 
END PROCEDURE.
PROCEDURE ItemTT_UI :
   FIND FIRST ItemTT  WHERE ItemTT.RID = RECID(InvoiceLine) NO-LOCK NO-ERROR.
   IF AVAILABLE ItemTT THEN DO:
      ItemTT.NameID = faktposterexcelTT.BESKIVNING.
      IF AVAILABLE ItemTT THEN DO:
         RUN Invoicepeppol_UI (INPUT "SellersItemIdentification",INPUT RECID(ItemTT)) .
         RUN SellersItemIdentification_UI.
         RUN Invoicepeppol_UI (INPUT "ClassifiedTaxCategory",INPUT RECID(ItemTT)) .
         RUN ClassifiedTaxCategory_UI.
      END.   
   END. 
END PROCEDURE.
PROCEDURE Price_UI :
   FIND FIRST Price  WHERE Price.RID = RECID(InvoiceLine) NO-LOCK NO-ERROR.
   IF AVAILABLE Price THEN DO:
      Price.PriceAmount = DecBelopp(faktposterexcelTT.PRIS).
         
   END. 
END PROCEDURE.
/*level 3*/
PROCEDURE SellersItemIdentification_UI :
   FIND FIRST SellersItemIdentification  WHERE SellersItemIdentification.RID = RECID(ItemTT) NO-LOCK NO-ERROR.
   IF AVAILABLE SellersItemIdentification THEN DO:
      SellersItemIdentification.ID = "".
   END.   
END PROCEDURE.
PROCEDURE ClassifiedTaxCategory_UI :
   FIND FIRST ClassifiedTaxCategory WHERE ClassifiedTaxCategory.RID = RECID(ItemTT) NO-LOCK NO-ERROR.
   IF AVAILABLE ClassifiedTaxCategory THEN DO:
      ASSIGN 
      ClassifiedTaxCategory.ID = "S"
      ClassifiedTaxCategory.Percent = 25. 
      RUN Invoicepeppol_UI (INPUT "TaxScheme",INPUT RECID(ClassifiedTaxCategory)) .
      FIND FIRST TaxScheme WHERE TaxScheme.RID = RECID(ClassifiedTaxCategory) NO-LOCK NO-ERROR.
      IF AVAILABLE TaxScheme THEN DO:
         TaxScheme.ID = "VAT". 
      END.
   END.   
END PROCEDURE.
PROCEDURE TaxCategory_UI :
   FIND FIRST TaxCategory WHERE NO-LOCK NO-ERROR.
   IF AVAILABLE TaxCategory THEN DO:
      ASSIGN
      TaxCategory.ID = "S"
      TaxCategory.Percent = 25.
      RUN Invoicepeppol_UI (INPUT "TaxScheme",INPUT RECID(TaxCategory)) .
      FIND FIRST TaxScheme WHERE TaxScheme.RID = RECID(TaxCategory) NO-LOCK NO-ERROR.
      IF AVAILABLE TaxScheme THEN DO:
         TaxScheme.ID = "VAT". 
      END.        
   END.
END PROCEDURE.

PROCEDURE Invoicepeppol_UI  :
   DEFINE INPUT  PARAMETER efaktablename AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER erid AS RECID NO-UNDO.
   CREATE BUFFER tembuffh FOR TABLE efaktablename IN WIDGET-POOL Efaktpool.
   tembuffh:BUFFER-CREATE().
   IF erid = ? THEN  tembuffh:BUFFER-FIELD("RID"):BUFFER-VALUE = tembuffh:RECID.
   ELSE tembuffh:BUFFER-FIELD("RID"):BUFFER-VALUE = erid.
   /*
   DEFINE VARIABLE falth AS HANDLE NO-UNDO.
   falth = tembuffh:BUFFER-FIELD("RID").
   */
END PROCEDURE.
  
PROCEDURE writexmlomini_UI :            
   DEFINE INPUT  PARAMETER hPDS AS HANDLE NO-UNDO.
   DEFINE INPUT  PARAMETER cTargetType AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER cFile AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER cEncoding AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER cSchemaLocation AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER lFormatted AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER lWriteSchema AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER lMinSchema AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER lWriteBeforeImage AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER omit-initial AS LOGICAL NO-UNDO.
    
   DEFINE VARIABLE lReturn AS LOGICAL NO-UNDO.
   DEFINE VARIABLE r1 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE r2 AS CHARACTER NO-UNDO.
   cFile = "D:\Elpool\FaktE\" + cFile + ".xml".
   cTargetType = "FILE".
   IF cTargetType = "FILE" THEN DO:
      lReturn = hPDS:WRITE-XML(cTargetType, "D:\Elpool\FaktE\PR.xml", lFormatted,cEncoding,cSchemaLocation,lWriteSchema,lMinSchema,lWriteBeforeImage, omit-initial).      
   END.
   cTargetType = "LONGCHAR".
   IF cTargetType = "LONGCHAR" THEN DO:
      lReturn = hPDS:WRITE-XML(cTargetType, filelong, lFormatted,cEncoding,cSchemaLocation,lWriteSchema,lMinSchema,lWriteBeforeImage, omit-initial).
      r1 = '<Invoice xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'.
 r2 = '<Invoice xmlns="urn:oasis:names:specification:ubl:schema:xsd:Invoice-2" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:cbc="urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2" xmlns:ccts="urn:un:unece:uncefact:documentation:2" xmlns:cac="urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2" xmlns:ext="urn:oasis:names:specification:ubl:schema:xsd:CommonExtensionComponents-2" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'.
      RUN Replace_UI (INPUT r1, INPUT r2). 
      RUN Replace_UI (INPUT ' xmlns:cac="">', INPUT '>').
      RUN Replace_UI (INPUT ' xmlns:cac=""/>', INPUT '>').
      RUN Replace_UI (INPUT ' xmlns:cbc="">', INPUT '>').
      RUN Replace_UI (INPUT ' xmlns:cbc=""/>', INPUT '>').
      /*
      RUN Replace_UI (INPUT '<cbc:InvoiceTypeCode>','<cbc:InvoiceTypeCode listID="UNCL1001">').
      RUN Replace_UI (INPUT '<cbc:DocumentCurrencyCode>','<cbc:DocumentCurrencyCode listID="ISO4217">').
      */
      RUN Replace_UI (INPUT '<cbc:EndpointID>','<cbc:EndpointID schemeID="0007">').
      RUN Replace_UI (INPUT '<cac:PartyIdentification><cbc:ID>','<cac:PartyIdentification><cbc:ID schemeID="0007">').
      RUN Replace_UI (INPUT '<cac:Country><cbc:IdentificationCode>','<cac:Country><cbc:IdentificationCode listID="ISO3166-1:Alpha2">').
      RUN Replace_UI (INPUT '<cbc:TaxAmount>','<cbc:TaxAmount currencyID="SEK">').
      RUN Replace_UI (INPUT '<cbc:TaxableAmount>','<cbc:TaxableAmount currencyID="SEK">').
      RUN Replace_UI (INPUT '<cac:TaxCategory>cbc:ID>','<cac:TaxCategory>cbc:ID schemeID="UNCL5305">').
      RUN Replace_UI (INPUT '<cbc:LineExtensionAmount>','<cbc:LineExtensionAmount currencyID="SEK">').
      RUN Replace_UI (INPUT '<cbc:TaxExclusiveAmount>','<cbc:TaxExclusiveAmount currencyID="SEK">').
      RUN Replace_UI (INPUT '<cbc:TaxInclusiveAmount>','<cbc:TaxInclusiveAmount currencyID="SEK">').
      RUN Replace_UI (INPUT '<cbc:AllowanceTotalAmount>','<cbc:AllowanceTotalAmount currencyID="SEK">').
      RUN Replace_UI (INPUT '<cbc:ChargeTotalAmount>','<cbc:ChargeTotalAmount currencyID="SEK">').
      RUN Replace_UI (INPUT '<cbc:PayableAmount>','<cbc:PayableAmount currencyID="SEK">').
      /*
      RUN Replace_UI (INPUT '<cbc:InvoicedQuantity>','<cbc:InvoicedQuantity unitCode="EA" unitCodeListID="UNECERec20">').
      */
      RUN Replace_UI (INPUT '<cbc:InvoicedQuantity>','<cbc:InvoicedQuantity unitCode="EA">').
      RUN Replace_UI (INPUT '<cbc:LineExtensionAmount>','<cbc:LineExtensionAmount currencyID="SEK">').
      RUN Replace_UI (INPUT '<cac:ClassifiedTaxCategory><cbc:ID>','<cac:ClassifiedTaxCategory><cbc:ID schemeID="UNCL5305">').
      RUN Replace_UI (INPUT '<cbc:PriceAmount>','<cbc:PriceAmount currencyID="SEK">').
           
           
      COPY-LOB  filelong TO FILE cFile CONVERT TARGET CODEPAGE "UTF-8".
   END.     
 
 END PROCEDURE.   

 PROCEDURE Replace_UI :
    DEFINE INPUT  PARAMETER r1 AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER r2 AS CHARACTER NO-UNDO.
                                 
    filelong = REPLACE(filelong,r1,r2).
    
 END PROCEDURE.