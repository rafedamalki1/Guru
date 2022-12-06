
/*------------------------------------------------------------------------
    File        : EKGEBREGURU.I
      ----------------------------------------------------------------------*/
   
   
   DEFINE TEMP-TABLE Ebr-Data_Katalog NO-UNDO
      FIELD KostnadsKatalog  AS CHARACTER
      FIELD Type AS INTEGER
      FIELD Year  AS CHARACTER
      FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN".
   
   DEFINE TEMP-TABLE Ebr-Data_PLevel NO-UNDO
      FIELD Year  AS CHARACTER  
      FIELD Level AS CHARACTER
      FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN".
      
   DEFINE TEMP-TABLE Ebr-Data_Arbetskod NO-UNDO
      FIELD Level AS CHARACTER 
      FIELD ArbetsKod AS CHARACTER
      FIELD Arbete AS CHARACTER
      FIELD Kommentar AS CHARACTER
      FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN".
   
   /*EXPORT EBRDATA  Ea_Tid läggs hidden för alla utom globanv = elpao i EkgExportControl.cls*/   
   DEFINE TEMP-TABLE Ebr-Data_lop NO-UNDO
      FIELD Level AS CHARACTER
      FIELD ArbetsKod AS CHARACTER 
      FIELD Lopnummer AS CHARACTER 
      FIELD Arbete AS CHARACTER
      FIELD Enhet              AS CHARACTER 
      FIELD Bered_Tid       AS DECIMAL 
      FIELD Mont_Tid        AS DECIMAL
      FIELD Maskin_Tid      AS DECIMAL
      FIELD Utrust_Tid      AS DECIMAL
      FIELD Ea_Tid          AS DECIMAL /* XML-NODE-TYPE  "HIDDEN"*/
      FIELD Arbete_Kost       AS DECIMAL
      FIELD Material_Kost     AS DECIMAL
      FIELD Maskin_Kost       AS DECIMAL
      FIELD Utrust_Kost       AS DECIMAL
      FIELD Ovrigt_Kost       AS DECIMAL
      FIELD Summa_Kost        AS DECIMAL
      FIELD Antal_Man         AS INTEGER
      FIELD Kommentar          AS CHARACTER
      FIELD TidUnder          AS CHARACTER
      FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN"
      INDEX koder ArbetsKod Lopnummer Arbete.
      
      /*
   DEFINE TEMP-TABLE Ebr-Data_lopanm NO-UNDO LIKE Ebr-Data_lop.
   */ 
   DEFINE TEMP-TABLE Ebr-Data_lopanm NO-UNDO 
      FIELD Level AS CHARACTER
      FIELD ArbetsKod AS CHARACTER 
      FIELD Lopnummer AS CHARACTER    
      FIELD Kommentar          AS CHARACTER
      INDEX koder ArbetsKod Lopnummer.
      
   DEFINE TEMP-TABLE Ebr-Data_mtrl NO-UNDO
      FIELD Level AS CHARACTER 
      FIELD ArbetsKod AS CHARACTER 
      FIELD Lopnummer AS CHARACTER
      FIELD Arbete AS CHARACTER  XML-NODE-NAME "Arbete"
      FIELD MtrlBenamning AS CHARACTER  XML-NODE-NAME "Benamning"
      FIELD Antal AS DECIMAL  XML-NODE-NAME "Antal"
      FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN"
      INDEX koder Level ArbetsKod Arbete.
   
    
   DEFINE DATASET Ebr-DataDS FOR Ebr-Data_Katalog,Ebr-Data_PLevel, Ebr-Data_Arbetskod,Ebr-Data_lop,Ebr-Data_mtrl
   PARENT-ID-RELATION Relation2 FOR Ebr-Data_Katalog, Ebr-Data_PLevel PARENT-ID-FIELD RID
   PARENT-ID-RELATION Relation2 FOR Ebr-Data_PLevel, Ebr-Data_Arbetskod PARENT-ID-FIELD RID
   PARENT-ID-RELATION Relation2 FOR Ebr-Data_PLevel, Ebr-Data_lop PARENT-ID-FIELD RID
   PARENT-ID-RELATION Relation2 FOR Ebr-Data_PLevel, Ebr-Data_mtrl PARENT-ID-FIELD RID.
   
   
   DEFINE TEMP-TABLE Ebr-Data_FrekP1P2Katalog NO-UNDO XML-NODE-NAME "Ebr-Data_Katalog"
      FIELD KostnadsKatalog  AS CHARACTER
      FIELD Type AS INTEGER
      FIELD Year  AS CHARACTER
      FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN".
   
   DEFINE TEMP-TABLE Ebr-Data_P1P2Frekvens NO-UNDO
      FIELD Year  AS CHARACTER
      FIELD P1ARBKOD AS CHARACTER             XML-NODE-NAME "P1Arbkod"
      FIELD P1LOPNR AS INTEGER                XML-NODE-NAME "P1Lopnr"
      FIELD P1BENAMNING AS CHARACTER          XML-NODE-NAME "P1Arbete" /*BENAMNING FRÅN EKGP1*/
      FIELD P2ARBKOD AS CHARACTER             XML-NODE-NAME "P2Arbkod"
      FIELD P2LOPNR AS INTEGER                XML-NODE-NAME "P2Lopnr"
      FIELD P2BENAMNING AS CHARACTER          XML-NODE-NAME "P2Arbete" /*BENAMNING FRÅN EKGP2*/  
      FIELD ANTAL AS DECIMAL                  XML-NODE-NAME "Antal" DECIMALS 10  
      FIELD RID AS RECID                      XML-NODE-TYPE  "HIDDEN"
      INDEX P1ARBKOD P1ARBKOD P1LOPNR P2ARBKOD P2LOPNR.
  
   DEFINE DATASET Ebr-DataP1P2DS FOR Ebr-Data_FrekP1P2Katalog,Ebr-Data_P1P2Frekvens
   PARENT-ID-RELATION Relation1 FOR Ebr-Data_FrekP1P2Katalog, Ebr-Data_P1P2Frekvens PARENT-ID-FIELD RID.
       
   
 DEFINE TEMP-TABLE Ebr-Data_FrekP2P3Katalog NO-UNDO XML-NODE-NAME "Ebr-Data_Katalog"
      FIELD KostnadsKatalog  AS CHARACTER
      FIELD Type AS INTEGER
      FIELD Year  AS CHARACTER
      FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN".
 
   
 DEFINE TEMP-TABLE Ebr-Data_P2P3Frekvens NO-UNDO
    FIELD Year  AS CHARACTER
    FIELD P2ARBKOD AS CHARACTER             XML-NODE-NAME "P2Arbkod"
    FIELD P2LOPNR AS INTEGER                XML-NODE-NAME "P2Lopnr"
    FIELD P2BENAMNING AS CHARACTER          XML-NODE-NAME "P2Arbete" /*BENAMNING FRÅN EKGP1*/
    FIELD P3ARBKOD AS CHARACTER             XML-NODE-NAME "P3Arbkod"
    FIELD P3LOPNR AS INTEGER                XML-NODE-NAME "P3Lopnr"
    FIELD P3BENAMNING AS CHARACTER          XML-NODE-NAME "P3Arbete" /*BENAMNING FRÅN EKGP2*/  
    FIELD ANTAL AS DECIMAL                  XML-NODE-NAME "Antal" DECIMALS 10    
    FIELD RID AS RECID                      XML-NODE-TYPE  "HIDDEN"
      INDEX P2ARBKOD P2ARBKOD P2LOPNR P3ARBKOD P3LOPNR.
   
   
   DEFINE DATASET Ebr-DataP2P3DS FOR Ebr-Data_FrekP2P3Katalog,Ebr-Data_P2P3Frekvens
   PARENT-ID-RELATION Relation1 FOR Ebr-Data_FrekP2P3Katalog, Ebr-Data_P2P3Frekvens PARENT-ID-FIELD RID.
   
  /*p2 kom*/ 
DEFINE TEMP-TABLE Ebr-Data_P2KomKatalog NO-UNDO XML-NODE-NAME "Ebr-Data_Katalog"
   FIELD KostnadsKatalog  AS CHARACTER
   FIELD Type AS INTEGER
   FIELD Year  AS CHARACTER
   FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN".
 
   
 DEFINE TEMP-TABLE Ebr-Data_P2Kom NO-UNDO
    FIELD Year  AS CHARACTER    
    FIELD P2ARBKOD AS CHARACTER XML-NODE-NAME "P2Arbkod"
    FIELD P2LOPNR AS INTEGER XML-NODE-TYPE   "HIDDEN"
    FIELD SVEPLOPNR AS CHARACTER   XML-NODE-NAME "P2Lopnr" 
    FIELD ANMARKNING AS CHARACTER XML-NODE-NAME "Kommentar"
    FIELD ORDNING AS INTEGER
    FIELD INGAREJ AS CHARACTER LABEL "I kostnaden ingår ej"
    FIELD SEP2ARBKOD AS CHARACTER LABEL "Arbkod"   XML-NODE-TYPE  "HIDDEN"
    FIELD SESVEPLOPNR AS CHARACTER  LABEL "Löpnr"  XML-NODE-TYPE  "HIDDEN" 
    FIELD RID AS RECID                      XML-NODE-TYPE  "HIDDEN"
    INDEX P2ARBKOD IS PRIMARY P2ARBKOD P2LOPNR  ORDNING 
    INDEX SEP2ARBKOD SEP2ARBKOD SESVEPLOPNR ORDNING.   
 DEFINE BUFFER  Ebr-Data_P2Kombuff FOR Ebr-Data_P2Kom. 
  DEFINE BUFFER  Ebr-Data_P2Kombuff2 FOR Ebr-Data_P2Kom. 
   DEFINE DATASET Ebr-DataP2KomDS FOR Ebr-Data_P2KomKatalog,Ebr-Data_P2Kom
   PARENT-ID-RELATION Relation1 FOR Ebr-Data_P2KomKatalog, Ebr-Data_P2Kom PARENT-ID-FIELD RID.   