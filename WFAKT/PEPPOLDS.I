
/*------------------------------------------------------------------------
    File        : PEPPOLDS.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Aug 11 17:00:37 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

  

 

DEFINE DATASET InvoicepeppolDS  

XML-NODE-TYPE  "HIDDEN"   
FOR 
   Invoicepeppol,
   AccountingSupplierParty,
   Party,
   PartyIdentification,
   PartyName,
   PostalAddress,
   Country,
   PartyTaxScheme,
   TaxScheme,
   PartyLegalEntity,
   Contact,
   AccountingCustomerParty,
   Delivery,
   DeliveryLocation,
   Address,
   PaymentMeans,
   PayeeFinancialAccount,
   FinancialInstitutionBranch,
   PaymentTerms,
   TaxTotal,
   TaxSubtotal,
   TaxCategory,
   LegalMonetaryTotal,
   InvoiceLine,
   ItemTT,
   SellersItemIdentification,
   ClassifiedTaxCategory,
   Price
   PARENT-ID-RELATION Relation1 FOR Invoicepeppol, AccountingSupplierParty PARENT-ID-FIELD RID  
   PARENT-ID-RELATION Relation1 FOR Invoicepeppol, AccountingCustomerParty PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR Invoicepeppol, Delivery PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR Invoicepeppol, PaymentMeans PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR Invoicepeppol, PaymentTerms PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR Invoicepeppol, TaxTotal PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR Invoicepeppol, LegalMonetaryTotal PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR Invoicepeppol, InvoiceLine PARENT-ID-FIELD RID 
   
   
   PARENT-ID-RELATION Relation1 FOR AccountingSupplierParty, Party  PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR Party, PartyIdentification PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR Party, PartyName PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR Party, PostalAddress PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR Party, PartyTaxScheme  PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR Party, PartyLegalEntity  PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR Party, Contact  PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR PostalAddress, Country  PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR PartyTaxScheme, TaxScheme  PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR AccountingCustomerParty, Party  PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR Delivery, DeliveryLocation  PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR DeliveryLocation, Address   PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR Address, Country  PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR PaymentMeans, PayeeFinancialAccount  PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR PayeeFinancialAccount, FinancialInstitutionBranch  PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR TaxTotal, TaxSubtotal  PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR TaxSubtotal, TaxCategory  PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR TaxCategory, TaxScheme  PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR InvoiceLine, ItemTT  PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR InvoiceLine, Price  PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR ItemTT, SellersItemIdentification PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR ItemTT, ClassifiedTaxCategory PARENT-ID-FIELD RID 
   PARENT-ID-RELATION Relation1 FOR ClassifiedTaxCategory, TaxScheme PARENT-ID-FIELD RID .
   