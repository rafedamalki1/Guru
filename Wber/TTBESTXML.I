/*TTBESTXML.I*/

DEFINE TEMP-TABLE ProjectOrder 
   FIELD ut AS CHARACTER.

DEFINE TEMP-TABLE tempHeader 
   FIELD ut AS CHARACTER.
   
DEFINE TEMP-TABLE tempMessage 
   FIELD SenderID AS CHARACTER
   FIELD MessageType AS CHARACTER 
   FIELD ReceiverID AS CHARACTER
   FIELD MessageDate AS CHARACTER.

DEFINE TEMP-TABLE Order NO-UNDO
   FIELD BuyerOrderNumber   AS CHARACTER
   FIELD RegistrationDate   AS CHARACTER
   FIELD ObjectID           AS CHARACTER
   FIELD CurrencyCode       AS CHARACTER
   FIELD DeliveryNoteText   AS CHARACTER
   FIELD GoodsMarking       AS CHARACTER
   FIELD OrderText          AS CHARACTER   
   FIELD DeliveryTermCode   AS CHARACTER   
   FIELD DeliveryTermCity   AS CHARACTER
   FIELD SplitDelivery      AS CHARACTER
   FIELD BuyerInvoiceNumber AS CHARACTER
   FIELD OrderChange        AS CHARACTER
   FIELD AKG                AS CHARACTER.
  
DEFINE TEMP-TABLE bDeliveryAddress 
   FIELD Name AS CHARACTER
   FIELD Street AS CHARACTER
   FIELD City AS CHARACTER
   FIELD PostalCode AS CHARACTER
   FIELD CountryCode AS CHARACTER.

DEFINE TEMP-TABLE bForeignDeliveryAddress 
   FIELD Address1 AS CHARACTER
   FIELD Address2 AS CHARACTER
   FIELD Address3 AS CHARACTER
   FIELD Address4 AS CHARACTER
   FIELD Address5 AS CHARACTER
   FIELD GateCountryCode AS CHARACTER.

DEFINE TEMP-TABLE Buyer NO-UNDO
   FIELD BuyerID   AS CHARACTER
   FIELD Name    AS CHARACTER
   FIELD Handler AS CHARACTER
   FIELD Phone AS CHARACTER
   FIELD Fax AS CHARACTER
   FIELD MobilePhone AS CHARACTER
   FIELD Reference AS CHARACTER
   FIELD eMailAddress AS CHARACTER   
   FIELD ProjectName AS CHARACTER.

DEFINE TEMP-TABLE Seller NO-UNDO
   FIELD SellerID   AS CHARACTER
   FIELD Name    AS CHARACTER
   FIELD Handler AS CHARACTER
   FIELD Phone AS CHARACTER
   FIELD Fax AS CHARACTER.
   
DEFINE TEMP-TABLE DeliveryAddress 
   FIELD PassageID    AS CHARACTER
   FIELD Name AS CHARACTER
   FIELD Street AS CHARACTER
   FIELD City AS CHARACTER
   FIELD PostalCode AS CHARACTER
   FIELD CountryCode AS CHARACTER.

DEFINE TEMP-TABLE ForeignDeliveryAddress 
   FIELD PassageID    AS CHARACTER
   FIELD Address1 AS CHARACTER
   FIELD Address2 AS CHARACTER
   FIELD Address3 AS CHARACTER
   FIELD Address4 AS CHARACTER
   FIELD Address5 AS CHARACTER
   FIELD GateCountryCode AS CHARACTER.
      
DEFINE TEMP-TABLE Passage NO-UNDO
   FIELD PassageID    AS CHARACTER
   FIELD PassageName  AS CHARACTER
   FIELD Infotext   AS CHARACTER.
    
DEFINE TEMP-TABLE BuildingPart NO-UNDO
   FIELD PassageID    AS CHARACTER  
   FIELD BuildingPartID   AS CHARACTER
   FIELD PartName         AS CHARACTER
   FIELD Infotext         AS CHARACTER.


DEFINE TEMP-TABLE OrderRows NO-UNDO
   FIELD PassageID    AS CHARACTER  
   FIELD BuildingPartID       AS CHARACTER  
   FIELD BuyerRowNumber AS CHARACTER
   FIELD BuyerArticleNumber AS CHARACTER
   FIELD BuyerArticleDescription AS CHARACTER
   FIELD SellerRowNumber AS CHARACTER
   FIELD SellerArticleNumber AS CHARACTER
   FIELD SellerArticleDescription AS CHARACTER
   FIELD OrderedQuantity AS CHARACTER
   FIELD UnitCode AS CHARACTER
   FIELD Price AS CHARACTER
   FIELD PriceType  AS CHARACTER
   FIELD RequestedDeliveryDate  AS CHARACTER
   FIELD Discount  AS CHARACTER   
   FIELD Comment  AS CHARACTER
   FIELD NewRequestedDeliveryDate  AS CHARACTER
   FIELD DynamicArticle  AS CHARACTER
   INDEX BuyerArticleNumber IS PRIMARY PassageID BuildingPartID SellerArticleNumber.
DEFINE BUFFER OrderRowsbuff FOR OrderRows.


   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
    
   
   
   
   
   
    
   
   
   
    
   
   
   
   
   
   
   

   
   
    
   
   
   
   
    
   
   
   
   
   
   
   
   
   
   
