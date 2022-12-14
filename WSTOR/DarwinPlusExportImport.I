
/*------------------------------------------------------------------------
    File        :DarwinPlusExportImport.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Feb 24 13:29:30 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/
/*
Darwins olika DATASET

StorExpDS.i  [Darwinplus] -  Hanterar Export fr?n Darwinplus REST
StorImpDS.i [Darwinplus]  -  Hanterar Import fr?n REST till fil

DarwinPlusExportImport.I [Guru] - Hanterar Exporten fr?n guru till Darwinplus
StorImpDSFilinfofix.I [Guru]  - Hanterar Import fr?n fil till Guru

*/
   
DEFINE TEMP-TABLE ForetagData  NO-UNDO 
   XML-NODE-NAME "ForetagData"
   FIELD IdentifikationsnrForetag AS INTEGER
   FIELD Foretag AS CHARACTER
   FIELD IMPORTDATUM AS DATETIME
   FIELD TTRECID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   INDEX idf IdentifikationsnrForetag.

DEFINE TEMP-TABLE DistriktData NO-UNDO
   XML-NODE-NAME "DistriktData"
   FIELD IdentifikationsnrForetag AS INTEGER XML-NODE-TYPE  "HIDDEN"
   FIELD DistriktsId AS INTEGER 
   FIELD Artal AS INTEGER
   FIELD DistriktsNamn AS CHARACTER
   FIELD IMPORTDATUM AS DATETIME
   FIELD TTRECID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   INDEX id DistriktsId Artal.

DEFINE TEMP-TABLE KundData NO-UNDO XML-NODE-NAME "KundData"
   FIELD IdentifikationsnrForetag AS INTEGER XML-NODE-TYPE  "HIDDEN"
   FIELD DistriktsId AS INTEGER XML-NODE-TYPE  "HIDDEN"
   FIELD Artal AS INTEGER XML-NODE-TYPE  "HIDDEN"
   FIELD SpanNivId AS INTEGER
   FIELD SpanNamn  AS CHARACTER
   FIELD AntalKunderTotalt AS INTEGER
   FIELD IMPORTDATUM AS DATETIME
   FIELD TTRECID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   INDEX id DistriktsId Artal SpanNivId.
      

DEFINE TEMP-TABLE LedningData NO-UNDO
   XML-NODE-NAME "LedningData"
   FIELD IdentifikationsnrForetag AS INTEGER XML-NODE-TYPE  "HIDDEN"
   FIELD DistriktsId AS INTEGER XML-NODE-TYPE  "HIDDEN"
   FIELD Artal AS INTEGER XML-NODE-TYPE  "HIDDEN"
   FIELD SpanNivId AS INTEGER
   FIELD SpanNamn  AS CHARACTER
   FIELD Text1 AS CHARACTER  XML-NODE-NAME "Faltbeskrivning1" 
   FIELD Text2 AS CHARACTER  XML-NODE-NAME "Faltbeskrivning2" 
   FIELD AntalTotalt AS INTEGER
   FIELD IMPORTDATUM AS DATETIME
   FIELD TTRECID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   INDEX id DistriktsId Artal SpanNivId.



DEFINE TEMP-TABLE StorningData NO-UNDO
   XML-NODE-NAME "StorningData"
   FIELD IdentifikationsnrForetag AS INTEGER XML-NODE-TYPE  "HIDDEN"
   FIELD DistriktsId  AS INTEGER XML-NODE-TYPE  "HIDDEN"
   FIELD Artal AS INTEGER XML-NODE-TYPE  "HIDDEN"
   FIELD StorId AS INTEGER
   FIELD StortypId AS INTEGER
   FIELD StorningsDatum AS DATE       
   FIELD StorningKlockan AS INTEGER  
   FIELD SlutDatum AS DATE        
   FIELD SlutKlockan AS INTEGER    
   FIELD KundAvbrottsTid AS DECIMAL  
   FIELD AntalHSP AS INTEGER   
   FIELD AntalLSP AS INTEGER
   FIELD BortKW AS INTEGER
   FIELD FranspanId AS INTEGER
   FIELD FelspanId AS INTEGER
   FIELD BrytoId AS INTEGER
   FIELD BrytorgLit AS CHARACTER
   FIELD SektionerId AS INTEGER
   FIELD SektionLit AS CHARACTER
   FIELD UtlosId AS INTEGER
   FIELD RelinId AS INTEGER
   FIELD FelYId AS INTEGER  
   FIELD StDriftId AS INTEGER
   
   FIELD StDriftNamn AS CHARACTER
   FIELD StDriftLit AS CHARACTER 
   FIELD AdelId AS INTEGER
   FIELD FelOId AS INTEGER 
   FIELD StortypNamn  AS CHARACTER
   FIELD FranspanNamn AS CHARACTER
   FIELD FelspanNamn AS CHARACTER
   FIELD BrytonNamn AS CHARACTER
   FIELD SektionNamn AS CHARACTER
   FIELD UtlosNamn  AS CHARACTER
   FIELD RelinNamn AS CHARACTER
   FIELD FelYNamn  AS CHARACTER
   FIELD AdelNamn  AS CHARACTER
   FIELD FelONamn  AS CHARACTER
   FIELD Mer3Min AS INTEGER
   FIELD EjKundPaverk AS INTEGER
   FIELD IMPORTDATUM AS DATETIME
   FIELD TTRECID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN
   /*
   FIELD AntalReservKraft AS INTEGER XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   FIELD TidReservKraft AS CHARACTER XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   FIELD EjBortKund AS INTEGER XML-NODE-NAME "AntalEjBerordaKunder" XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   FIELD EjBortKW AS INTEGER   XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN
   */  
   INDEX Sid DistriktsId Artal StorId.

DEFINE TEMP-TABLE StorFelMtrlData NO-UNDO 
   XML-NODE-NAME "StorFelMtrlData"
   FIELD IdentifikationsnrForetag AS INTEGER XML-NODE-TYPE  "HIDDEN"
   FIELD DistriktsId AS INTEGER XML-NODE-TYPE  "HIDDEN"
   FIELD StorId AS INTEGER XML-NODE-TYPE  "HIDDEN" 
   FIELD SfmId AS INTEGER
   FIELD Apparat AS CHARACTER
   FIELD Leverantor AS CHARACTER
   FIELD Fabrikat AS CHARACTER
   FIELD Artikelnr AS CHARACTER
   FIELD Artal AS INTEGER
   FIELD Antal AS DECIMAL
   FIELD IMPORTDATUM AS DATETIME
   FIELD TTRECID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   INDEX Smid DistriktsId StorId SfmId.

      
 
 
