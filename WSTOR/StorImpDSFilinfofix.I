
/*------------------------------------------------------------------------
    File        : StorImpDSFilinfofix.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Feb 24 13:29:30 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/
/*
Darwins olika DATASET

StorExpDS.i  [Darwinplus] -  Hanterar Export från Darwinplus REST
StorImpDS.i [Darwinplus]  -  Hanterar Import från REST till fil

DarwinPlusExportImport.I [Guru] - Hanterar Exporten från guru till Darwinplus
StorImpDSFilinfofix.I [Guru]  - Hanterar Import från fil till Guru

*/
DEFINE TEMP-TABLE FilInfo  NO-UNDO 
   /*
   NAMESPACE-URI "Energiforetagen/Sverige/2.0"  XML-NODE-TYPE "HIDDEN"
   */
   XML-NODE-NAME "FilInfo"
   FIELD Skapad AS DATETIME
   FIELD TTRECID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN.
   
DEFINE TEMP-TABLE ForetagData  NO-UNDO 
   /*
   NAMESPACE-URI "Energiforetagen/Sverige/2.0"  XML-NODE-TYPE "HIDDEN"
   */
   XML-NODE-NAME "ForetagData"
   FIELD IdentifikationsnrForetag AS INTEGER
   FIELD Foretag AS CHARACTER
   FIELD TTRECID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   INDEX idf IdentifikationsnrForetag.
 DEFINE BUFFER foredatabuff FOR ForetagData.  

DEFINE TEMP-TABLE DistriktData NO-UNDO
   /*
   NAMESPACE-URI "Energiforetagen/Sverige/2.0" XML-NODE-TYPE "HIDDEN"
   */
   XML-NODE-NAME "DistriktData"
   FIELD IdentifikationsnrForetag AS INTEGER XML-NODE-TYPE  "HIDDEN"
   FIELD DistriktsId AS CHARACTER 
   FIELD Artal AS INTEGER
   FIELD DistriktsNamn AS CHARACTER
   FIELD EXPORTDATUM AS DATETIME XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN    /*bara tillför att kolla mot databasen om den är överläst till darawinplus*/
   FIELD TTRECID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   INDEX id DistriktsId Artal.
DEFINE BUFFER didatabuff FOR DistriktData.      

DEFINE TEMP-TABLE KundData NO-UNDO
/*
   NAMESPACE-URI "Energiforetagen/Sverige/2.0" XML-NODE-TYPE "HIDDEN"
   */
   XML-NODE-NAME "KundData"
   FIELD IdentifikationsnrForetag AS INTEGER XML-NODE-TYPE  "HIDDEN"
   FIELD DistriktsId AS CHARACTER XML-NODE-TYPE  "HIDDEN"
   FIELD Artal AS INTEGER XML-NODE-TYPE  "HIDDEN"
   FIELD SpanNivId AS INTEGER
   /*Anders Olsson Elpool i Umeå AB  10 aug 2017 10:48:19 
   Bort enligt Matz Tapper
   FIELD AntalKunderLuftnat AS INTEGER
   FIELD AntalKunderBlandNat AS INTEGER
   FIELD AntalKunderKabelNat AS INTEGER
   */
   FIELD AntalKunderTotalt AS INTEGER
   FIELD EXPORTDATUM AS DATETIME XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN    /*bara tillför att kolla mot databasen om den är överläst till darawinplus*/
   FIELD TTRECID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   INDEX id DistriktsId Artal SpanNivId.
      
DEFINE BUFFER kunddatabuff FOR KundData.
DEFINE TEMP-TABLE LedningData NO-UNDO
   /*
   NAMESPACE-URI "Energiforetagen/Sverige/2.0" XML-NODE-TYPE "HIDDEN"
   */
   XML-NODE-NAME "LedningData"
   FIELD IdentifikationsnrForetag AS INTEGER XML-NODE-TYPE  "HIDDEN"
   FIELD DistriktsId AS CHARACTER XML-NODE-TYPE  "HIDDEN"
   FIELD Artal AS INTEGER XML-NODE-TYPE  "HIDDEN"
   FIELD SpanNivId AS INTEGER
   FIELD EXPORTDATUM AS DATETIME XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN    /*bara tillför att kolla mot databasen om den är överläst till darawinplus*/
   FIELD Text1 AS CHARACTER  XML-NODE-NAME "Faltbeskrivning1" 
   FIELD Text2 AS CHARACTER  XML-NODE-NAME "Faltbeskrivning2" 
   /*Anders Olsson Elpool i Umeå AB  10 aug 2017 10:48:19 
   Bort enligt Matz Tapper
   FIELD AntalLuftnat AS INTEGER
   FIELD AntalBlandNat AS INTEGER
   FIELD AntalKabelNat AS INTEGER
   */
   FIELD AntalTotalt AS INTEGER
   FIELD TTRECID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   INDEX id DistriktsId Artal SpanNivId.

DEFINE BUFFER leddatabuff FOR LedningData.

DEFINE TEMP-TABLE StorningData NO-UNDO
   /*
   NAMESPACE-URI "Energiforetagen/Sverige/2.0" XML-NODE-TYPE "HIDDEN"
   */
   XML-NODE-NAME "StorningData"
   FIELD IdentifikationsnrForetag AS INTEGER XML-NODE-TYPE  "HIDDEN"
   FIELD DistriktsId AS CHARACTER XML-NODE-TYPE  "HIDDEN"
   FIELD StorId AS INTEGER
   FIELD StortypId AS INTEGER
   FIELD StorningsDatum AS DATE       
   FIELD StorningKlockan AS INTEGER  
   /*Anders Olsson Elpool i Umeå AB  10 aug 2017 10:48:19 
   Bort enligt Matz Tapper 
   FIELD SlutDatum70 AS CHARACTER    
   FIELD SlutKlockan70 AS INTEGER
   */    
   FIELD SlutDatum AS DATE        
   FIELD SlutKlockan AS INTEGER    
   FIELD KundAvbrottsTid AS DECIMAL  
   FIELD AntalHSP AS INTEGER   
   FIELD AntalLSP AS INTEGER
    /*Anders Olsson Elpool i Umeå AB  10 aug 2017 10:48:19 
   Bort enligt Matz Tapper
   FIELD AntalRegStn AS INTEGER
   FIELD AntalNatStn AS INTEGER
   */
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
   FIELD StDriftLit AS CHARACTER 
   /*Anders Olsson Elpool i Umeå AB  10 aug 2017 10:48:19 
   Bort enligt Matz Tapper 
   
   FIELD NatStrukHSPID AS INTEGER
   FIELD NatStrukLSPID AS INTEGER  
   FIELD NattypId AS INTEGER
   */
   FIELD AdelId AS INTEGER
   FIELD FelOId AS INTEGER 
   
   FIELD TTRECID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN
   /*
   FIELD AntalReservKraft AS INTEGER XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   FIELD TidReservKraft AS CHARACTER XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   FIELD EjBortKund AS INTEGER XML-NODE-NAME "AntalEjBerordaKunder" XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   FIELD EjBortKW AS INTEGER   XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN
   */  
   INDEX IdentifikationsnrForetag DistriktsId StorId.
DEFINE BUFFER stordatabuff FOR StorningData.
DEFINE TEMP-TABLE StorFelMtrlData NO-UNDO 
   /*
   NAMESPACE-URI "Energiforetagen/Sverige/2.0" XML-NODE-TYPE "HIDDEN"
   */
   XML-NODE-NAME "StorFelMtrlData"
   FIELD IdentifikationsnrForetag AS INTEGER XML-NODE-TYPE  "HIDDEN"
   FIELD DistriktsId AS CHARACTER XML-NODE-TYPE  "HIDDEN"
   FIELD StorId AS INTEGER XML-NODE-TYPE  "HIDDEN" 
   FIELD SfmId AS INTEGER
   FIELD Apparat AS CHARACTER
   FIELD Leverantor AS CHARACTER
   FIELD Fabrikat AS CHARACTER
   FIELD Artikelnr AS CHARACTER
   FIELD Artal AS INTEGER
   FIELD Antal AS DECIMAL
   FIELD TTRECID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN" SERIALIZE-HIDDEN 
   INDEX IdentifikationsnrForetag DistriktsId StorId.

DEFINE BUFFER stormtrldatabuff FOR StorFelMtrlData.

DEFINE  DATASET StorningImportDs NAMESPACE-URI "Energiforetagen/Sverige/2.0" 
   FOR ForetagData, DistriktData, KundData, LedningData, StorningData, StorFelMtrlData, FilInfo
   
   DATA-RELATION DRELATION1 FOR ForetagData, DistriktData RELATION-FIELDS (IdentifikationsnrForetag, IdentifikationsnrForetag) NESTED 
   DATA-RELATION DRELATION2 FOR DistriktData, KundData RELATION-FIELDS (IdentifikationsnrForetag, IdentifikationsnrForetag, DistriktsId, DistriktsId, Artal, Artal) NESTED 
   DATA-RELATION DRELATION3 FOR DistriktData, LedningData RELATION-FIELDS (IdentifikationsnrForetag, IdentifikationsnrForetag, DistriktsId, DistriktsId, Artal, Artal) NESTED 
   DATA-RELATION DRELATION4 FOR DistriktData, StorningData RELATION-FIELDS (IdentifikationsnrForetag, IdentifikationsnrForetag, DistriktsId, DistriktsId) NESTED 
   DATA-RELATION DRELATION5 FOR StorningData, StorFelMtrlData RELATION-FIELDS (IdentifikationsnrForetag, IdentifikationsnrForetag, DistriktsId, DistriktsId, StorId, StorId) NESTED.

   /*PARENT-ID-RELATION RELATION1 FOR ForetagData, DistriktData
      PARENT-ID-FIELD RID
   PARENT-ID-RELATION RELATION10 FOR DistriktData, KundData
      PARENT-ID-FIELD RID  
   PARENT-ID-RELATION RELATION11 FOR DistriktData, LedningData
      PARENT-ID-FIELD RID  
   PARENT-ID-RELATION RELATION13 FOR DistriktData, StorningData
      PARENT-ID-FIELD RID
   PARENT-ID-RELATION RELATION2 FOR StorningData, StorFelMtrlData
      PARENT-ID-FIELD RID. */
      
   .
      
 
 
