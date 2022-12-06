                                                                                                                                                                                                                             
/*------------------------------------------------------------------------                                  
    File        : PEPPOLTT.I                                                                                
    Purpose     :                                                                                           
                                                                                                            
    Syntax      :                                                                                           
                                                                                                            
    Description :                                                                                           
                                                                                                            
    Author(s)   :                                                                                           
    Created     : Tue Aug 11 17:00:37 CEST 2015                                                             
    Notes       :                                                                                           
  ----------------------------------------------------------------------*/                                  
                                                                                                            
                                                                                                            
DEFINE TEMP-TABLE Invoicepeppol  NO-UNDO                                                                    
 /* NAMESPACE-URI "urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2"*/                  
 /* NAMESPACE-PREFIX "cbc"*/                                                                                
  XML-NODE-NAME "Invoice"                                                                                   
                                                                                                            
  FIELD RID AS RECID                                                              XML-NODE-TYPE  "HIDDEN"                                                                  
  FIELD CustomizationID AS CHARACTER                                              XML-NODE-NAME "cbc:CustomizationID"                                                                            
  FIELD ProfileID AS CHARACTER                                                    XML-NODE-NAME "cbc:ProfileID"                                                                                  
  FIELD ID AS CHARACTER                                                           XML-NODE-NAME "cbc:ID"                                                                                         
  FIELD IssueDate AS DATE   FORMAT "9999-99-99"                                                      XML-NODE-NAME "cbc:IssueDate"                                                                                  
  FIELD DueDate AS DATE     FORMAT "9999-99-99"                                                       XML-NODE-NAME "cbc:DueDate"                                                                                    
  FIELD InvoiceTypeCode AS CHARACTER                                              XML-NODE-NAME "cbc:InvoiceTypeCode"                                                                            
  FIELD listID AS CHARACTER INITIAL "UNCL1001"                                    XML-NODE-TYPE  "HIDDEN"            
  FIELD Note AS CHARACTER                                                         XML-NODE-NAME "cbc:Note"                                                                                       
  FIELD TaxPointDate AS DATE  FORMAT "9999-99-99"                                                    XML-NODE-NAME "cbc:TaxPointDate"                                                                               
  FIELD DocumentCurrencyCode  AS CHARACTER                                        XML-NODE-NAME "cbc:DocumentCurrencyCode"                                                                      
  FIELD listID2 AS CHARACTER INITIAL "ISO4217"                                    XML-NODE-TYPE  "HIDDEN"           
  FIELD AccountingCost AS CHARACTER                                               XML-NODE-NAME "cbc:AccountingCost"                                                                             
  FIELD BuyerReference AS CHARACTER                                               XML-NODE-NAME "cbc:BuyerReference".                                                                            
                                                                                                                                                                                                                             
                                                                                                                                                                                                                             
  DEFINE TEMP-TABLE AccountingSupplierParty NO-UNDO                               XML-NODE-NAME "cac:AccountingSupplierParty"
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN".                                    
  DEFINE TEMP-TABLE Party NO-UNDO                                                 XML-NODE-NAME "cac:Party"
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                             
     FIELD EndpointID AS CHARACTER                                                XML-NODE-NAME "cbc:EndpointID"                                                                              
     FIELD schemeID AS CHARACTER INITIAL "0007"                                   XML-NODE-TYPE  "HIDDEN".               
                                                                                                                                                                                                   
  DEFINE TEMP-TABLE PartyIdentification NO-UNDO                                   XML-NODE-NAME "cac:PartyIdentification"
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                        
     FIELD ID  AS CHARACTER                                                       XML-NODE-NAME "cbc:ID"                                                                                     
     FIELD schemeID AS CHARACTER INITIAL "0007"                                   XML-NODE-TYPE  "HIDDEN".               
  DEFINE TEMP-TABLE PartyName NO-UNDO                                             XML-NODE-NAME "cac:PartyName" 
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                             
     FIELD NameID  AS CHARACTER                                                   XML-NODE-NAME "cbc:Name".
                                                                                                                                                                                                   
                                                                                                                                                                                                   
  DEFINE TEMP-TABLE PostalAddress NO-UNDO                                         XML-NODE-NAME "cac:PostalAddress"
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                
     FIELD StreetName AS CHARACTER                                                XML-NODE-NAME "cbc:StreetName"                                                                              
     FIELD CityName AS CHARACTER                                                  XML-NODE-NAME "cbc:CityName"                                                                                
     FIELD PostalZone AS CHARACTER                                                XML-NODE-NAME "cbc:PostalZone".   /*SKAPA EN CONTRY*/ /*skapa två PartyTaxScheme*/                          
                                                                                                                                                                                                                             
  DEFINE TEMP-TABLE Country NO-UNDO                                               XML-NODE-NAME "cac:Country"
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                                                       
     FIELD IdentificationCode  AS CHARACTER                                       XML-NODE-NAME "cbc:IdentificationCode"                                                                     
     FIELD listID AS CHARACTER INITIAL "ISO3166-1:Alpha2"                         XML-NODE-TYPE  "HIDDEN".                               
  DEFINE TEMP-TABLE PartyTaxScheme NO-UNDO                                        XML-NODE-NAME "cac:PartyTaxScheme"
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                                                       
     FIELD CompanyID AS CHARACTER                                                 XML-NODE-NAME "cbc:CompanyID".    /* skapa TaxScheme*/ /* även Godkänd för F-skatt*/                        
                                                                                                                                                                                                                             
  DEFINE TEMP-TABLE TaxScheme NO-UNDO                                             XML-NODE-NAME "cac:TaxScheme"
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                             
     FIELD ID AS CHARACTER INITIAL "VAT"  /*även  TAX*/                           XML-NODE-NAME "cbc:ID" .                                                     
                                                                                                                                                                                                   
  DEFINE TEMP-TABLE PartyLegalEntity NO-UNDO                                      XML-NODE-NAME "cac:PartyLegalEntity"
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                             
     FIELD RegistrationName AS CHARACTER                                          XML-NODE-NAME "cbc:RegistrationName"                                                                        
     FIELD CompanyID AS CHARACTER                                                 XML-NODE-NAME "cbc:CompanyID"                                                                               
     FIELD CompanyLegalForm AS CHARACTER           XML-NODE-TYPE  "HIDDEN"                                  XML-NODE-NAME "cbc:CompanyLegalForm".                                                                       
                                                                                                                                                                                                   
   DEFINE TEMP-TABLE Contact NO-UNDO                                              XML-NODE-NAME "cac:Contact"                                                                            
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                             
     FIELD NameC AS CHARACTER                                                     XML-NODE-NAME "cbc:Name"                                                                                    
     FIELD Telephone AS CHARACTER                                                 XML-NODE-NAME "cbc:Telephone"                                                                               
     FIELD ElectronicMail AS CHARACTER                                            XML-NODE-NAME "cbc:ElectronicMail".                                                                         
                                                                                                                                                                                                                             
  DEFINE TEMP-TABLE AccountingCustomerParty NO-UNDO                               XML-NODE-NAME "cac:AccountingCustomerParty"
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN". /*skapa en party mm*/                                           
                                                                                                                                                                                                                             
  DEFINE TEMP-TABLE Delivery NO-UNDO                                              XML-NODE-NAME "cac:Delivery"
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                                
     FIELD ActualDeliveryDate AS DATE FORMAT "9999-99-99"                                            XML-NODE-NAME "cbc:ActualDeliveryDate".                                                                     
                                                                                                                                                                                                      
  DEFINE TEMP-TABLE DeliveryLocation NO-UNDO                                      XML-NODE-NAME "cac:DeliveryLocation"
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                                
     FIELD ID  AS CHARACTER                                                       XML-NODE-NAME "cbc:ID".                                                                                    
                                                                                                                                                                                                      
                                                                                                                                                                                                      
  DEFINE TEMP-TABLE Address NO-UNDO                                               XML-NODE-NAME "cac:Address"
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                                
     FIELD StreetName AS CHARACTER                                                XML-NODE-NAME "cbc:StreetName"                                                                              
     FIELD CityName AS CHARACTER                                                  XML-NODE-NAME "cbc:CityName"                                                                                
     FIELD PostalZone AS CHARACTER                                                XML-NODE-NAME "cbc:PostalZone".   /*SKAPA EN CONTRY*/                                                       
                                                                                                                                                                                                
  DEFINE TEMP-TABLE PaymentMeans NO-UNDO                                          XML-NODE-NAME "cac:PaymentMeans"
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                          
     FIELD PaymentMeansCode AS INTEGER                                            XML-NODE-NAME "cbc:PaymentMeansCode"                                                              
     FIELD PaymentID AS CHARACTER                                                 XML-NODE-NAME "cbc:PaymentID".                                                                              
                                                                                                                                                                                                
  DEFINE TEMP-TABLE PayeeFinancialAccount NO-UNDO                                 XML-NODE-NAME "cac:PayeeFinancialAccount"                                                              
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                          
     FIELD ID  AS CHARACTER                                                       XML-NODE-NAME "cbc:ID".
                                                                                 
                                                                                                                                                                                                
   DEFINE TEMP-TABLE FinancialInstitutionBranch NO-UNDO                           XML-NODE-NAME "cac:FinancialInstitutionBranch"                                                         
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                          
     FIELD ID  AS CHARACTER                                                       XML-NODE-NAME "cbc:ID".                                                                                    
                                                                                                                                                                                                
  DEFINE TEMP-TABLE PaymentTerms NO-UNDO                                          XML-NODE-NAME "cac:PaymentTerms"                                                                        
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                          
     FIELD Note  AS CHARACTER  INITIAL "30 dagar netto"                           XML-NODE-NAME "cbc:Note".                         
                                                                                                                                                                                                
  DEFINE TEMP-TABLE TaxTotal NO-UNDO                                              XML-NODE-NAME "cac:TaxTotal"
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                          
     FIELD TaxAmount  AS DECIMAL                                                  XML-NODE-NAME "cbc:TaxAmount"                                                                               
     FIELD currencyID AS CHARACTER INITIAL "SEK"                                  XML-NODE-TYPE  "HIDDEN"  .         
  DEFINE TEMP-TABLE TaxSubtotal NO-UNDO                                           XML-NODE-NAME "cac:TaxSubtotal"
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                          
     FIELD TaxableAmount  AS DECIMAL                                              XML-NODE-NAME "cbc:TaxableAmount"                                                                           
     FIELD TaxAmount  AS DECIMAL                                                  XML-NODE-NAME "cbc:TaxAmount"                                                                               
     FIELD currencyID AS CHARACTER INITIAL "SEK"                                  XML-NODE-TYPE  "HIDDEN".                                                  
  DEFINE TEMP-TABLE TaxCategory NO-UNDO                                           XML-NODE-NAME "cac:TaxCategory"
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                          
     FIELD ID  AS CHARACTER                                                       XML-NODE-NAME "cbc:ID"                                                                                     
     FIELD schemeID AS CHARACTER INITIAL "UNCL5305"                               XML-NODE-TYPE  "HIDDEN"         
     FIELD Percent AS INTEGER                                                     XML-NODE-NAME "cbc:Percent".
                                                                                                                                                                                                                             
                                                                                                                                                                                                  
  DEFINE TEMP-TABLE LegalMonetaryTotal NO-UNDO                                    XML-NODE-NAME "cac:LegalMonetaryTotal"                                                                 
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                            
     FIELD LineExtensionAmount  AS DECIMAL                                        XML-NODE-NAME "cbc:LineExtensionAmount"                                                                     
     FIELD TaxExclusiveAmount   AS DECIMAL                                        XML-NODE-NAME "cbc:TaxExclusiveAmount"                                                                     
     FIELD TaxInclusiveAmount   AS DECIMAL                                        XML-NODE-NAME "cbc:TaxInclusiveAmount"                                                                     
     FIELD AllowanceTotalAmount AS DECIMAL                                        XML-NODE-NAME "cbc:AllowanceTotalAmount"                                                                    
     FIELD ChargeTotalAmount    AS DECIMAL                                        XML-NODE-NAME "cbc:ChargeTotalAmount"                                                                     
     FIELD PayableAmount         AS DECIMAL                                       XML-NODE-NAME "cbc:PayableAmount"                                                                    
     FIELD currencyID AS CHARACTER INITIAL "SEK"                                  XML-NODE-TYPE  "HIDDEN".                                                   
  DEFINE TEMP-TABLE InvoiceLine NO-UNDO                                           XML-NODE-NAME "cac:InvoiceLine"                                                                         
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                         
     FIELD ID AS INTEGER                                                          XML-NODE-NAME "cbc:ID"
     FIELD Note AS CHARACTER                                                      XML-NODE-NAME "cbc:Note"                                                                                    
     FIELD InvoicedQuantity AS INTEGER                                            XML-NODE-NAME "cbc:InvoicedQuantity"
     FIELD EA AS CHARACTER INITIAL "UNECERec20"                                   XML-NODE-TYPE  "HIDDEN"            
     FIELD LineExtensionAmount    AS DECIMAL                                      XML-NODE-NAME "cbc:LineExtensionAmount"                                                                   
     FIELD currencyID AS CHARACTER INITIAL "SEK"                                  XML-NODE-TYPE  "HIDDEN".          
  DEFINE TEMP-TABLE SellersItemIdentification NO-UNDO                             XML-NODE-NAME "cac:SellersItemIdentification"                                                           
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                            
     FIELD ID  AS CHARACTER                                                       XML-NODE-NAME "cbc:ID"                                                                                     
     FIELD schemeID AS CHARACTER INITIAL "UNCL5305"                               XML-NODE-TYPE  "HIDDEN".         
                                                                                                                                                                                                 
  DEFINE TEMP-TABLE ItemTT NO-UNDO                                                XML-NODE-NAME "cac:Item"
     
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                            
     FIELD NameID  AS CHARACTER                                                   XML-NODE-NAME "cbc:Name".                                                           
                                                                                                                                                                                                                   
  DEFINE TEMP-TABLE ClassifiedTaxCategory NO-UNDO                                 XML-NODE-NAME "cac:ClassifiedTaxCategory"
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                             
     FIELD ID AS CHARACTER                                                        XML-NODE-NAME "cbc:ID"                                                                                      
     FIELD schemeID AS CHARACTER INITIAL "0007"                                   XML-NODE-TYPE  "HIDDEN"                
     FIELD Percent AS INTEGER /*Skap TaxScheme*/                                  XML-NODE-NAME "cbc:Percent".                                                   
  DEFINE TEMP-TABLE Price NO-UNDO                                                 XML-NODE-NAME "cac:Price"
     FIELD RID AS RECID                                                           XML-NODE-TYPE  "HIDDEN"                                                                                             
     FIELD PriceAmount         AS DECIMAL                                          XML-NODE-NAME "cbc:PriceAmount"                                                                       
     FIELD currencyID AS CHARACTER INITIAL "SEK"                                  XML-NODE-TYPE  "HIDDEN".              
                                                                                                                                                                                                   
 


