
/*------------------------------------------------------------------------
    File        : EKGEBRE.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Dec 02 17:22:26 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

   DEFINE TEMP-TABLE ebr-e_costcatalogue NO-UNDO
      FIELD kostnadskatalog_header  AS CHARACTER.
   
   DEFINE TEMP-TABLE ebr-e_type NO-UNDO   
      FIELD kostnadskatalog_header  AS CHARACTER XML-NODE-TYPE  "HIDDEN"
      FIELD type AS INTEGER
      FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN".
           
   
   DEFINE TEMP-TABLE ebr-e_year NO-UNDO  
      FIELD type AS INTEGER XML-NODE-TYPE  "HIDDEN"
      FIELD year  AS CHARACTER
      FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN".
      
   DEFINE TEMP-TABLE ebr-e_catalogue NO-UNDO
      FIELD year  AS CHARACTER  XML-NODE-TYPE  "HIDDEN"
      FIELD catalogue AS CHARACTER
      FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN".
      
   DEFINE TEMP-TABLE ebr-e_workcode NO-UNDO
      FIELD catalogue AS CHARACTER   XML-NODE-TYPE  "HIDDEN"
      FIELD code AS CHARACTER
      FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN".
      
      
   DEFINE TEMP-TABLE ebr-e_lop NO-UNDO
   /*
      FIELD Level AS CHARACTER   XML-NODE-TYPE  "HIDDEN"
      FIELD code AS CHARACTER XML-NODE-TYPE  "HIDDEN"
      FIELD code AS CHARACTER SERIALIZE-HIDDEN 
      
      */
      /*
      FIELD Level AS CHARACTER   
      FIELD code AS CHARACTER
      */
      FIELD Level AS CHARACTER   XML-NODE-TYPE  "HIDDEN"
      FIELD code AS CHARACTER XML-NODE-TYPE  "HIDDEN"
      FIELD number AS CHARACTER 
      FIELD work_construction AS CHARACTER
      FIELD unit              AS CHARACTER 
      FIELD bered_hours       AS DECIMAL 
      FIELD mont_hours        AS DECIMAL
      FIELD maskin_hours      AS DECIMAL
      FIELD utrust_hours      AS DECIMAL
      FIELD ea_hours          AS DECIMAL  XML-NODE-TYPE  "HIDDEN"
      FIELD arbete_cost       AS DECIMAL 
      FIELD material_cost     AS DECIMAL
      FIELD maskin_cost       AS DECIMAL
      FIELD utrust_cost       AS DECIMAL
      FIELD ovrigt_cost       AS DECIMAL
      FIELD summa_cost        AS DECIMAL  
      FIELD count_men         AS INTEGER
      FIELD comments          AS CHARACTER  
      FIELD timebase          AS CHARACTER
      FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN"
      INDEX koder CODE number work_construction.
      
    
   
    
   
    
   DEFINE DATASET EBR-eDS XML-NODE-NAME "BODY"  FOR ebr-e_costcatalogue,ebr-e_type, ebr-e_year, ebr-e_catalogue, ebr-e_workcode, ebr-e_lop
   PARENT-ID-RELATION Relation1 FOR ebr-e_costcatalogue, ebr-e_type PARENT-ID-FIELD RID
   PARENT-ID-RELATION Relation1 FOR ebr-e_type, ebr-e_year PARENT-ID-FIELD RID
   PARENT-ID-RELATION Relation1 FOR ebr-e_year, ebr-e_catalogue PARENT-ID-FIELD RID
   PARENT-ID-RELATION Relation1 FOR ebr-e_catalogue, ebr-e_workcode PARENT-ID-FIELD RID
   PARENT-ID-RELATION Relation1 FOR ebr-e_workcode, ebr-e_lop PARENT-ID-FIELD RID.
   
   
   
   DEFINE TEMP-TABLE ebr-tryck_costcatalogue NO-UNDO
      FIELD kostnadskatalog_header  AS CHARACTER.
   
   DEFINE TEMP-TABLE ebr-tryck_type NO-UNDO   
      FIELD kostnadskatalog_header  AS CHARACTER XML-NODE-TYPE  "HIDDEN"
      FIELD type AS INTEGER
      FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN".
           
   
   DEFINE TEMP-TABLE ebr-tryck_year NO-UNDO  
      FIELD type AS INTEGER XML-NODE-TYPE  "HIDDEN"
      FIELD year  AS CHARACTER
      FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN".
      
   DEFINE TEMP-TABLE ebr-tryck_catalogue NO-UNDO
      FIELD year  AS CHARACTER  XML-NODE-TYPE  "HIDDEN"
      FIELD catalogue AS CHARACTER
      FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN".
      
   DEFINE TEMP-TABLE ebr-tryck_workcode NO-UNDO
      FIELD catalogue AS CHARACTER   XML-NODE-TYPE  "HIDDEN"
      FIELD code AS CHARACTER
      FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN".
      
   
  
   DEFINE TEMP-TABLE ebr-tryck_lop NO-UNDO
   /*
      FIELD code AS CHARACTER XML-NODE-TYPE  "HIDDEN"
      */
      FIELD Level AS CHARACTER   XML-NODE-TYPE  "HIDDEN"
      /*
      FIELD code AS CHARACTER
      */
      FIELD code AS CHARACTER XML-NODE-TYPE  "HIDDEN" 
      FIELD number AS CHARACTER 
      FIELD work_construction AS CHARACTER
      FIELD unit              AS CHARACTER 
      FIELD bered_hours       AS CHARACTER 
      FIELD mont_hours        AS CHARACTER
      FIELD maskin_hours      AS CHARACTER
      FIELD utrust_hours      AS CHARACTER
      FIELD ea_hours          AS CHARACTER  XML-NODE-TYPE  "HIDDEN"
      FIELD arbete_cost       AS CHARACTER
      FIELD material_cost     AS CHARACTER
      FIELD maskin_cost       AS CHARACTER
      FIELD utrust_cost       AS CHARACTER
      FIELD ovrigt_cost       AS CHARACTER
      FIELD summa_cost        AS CHARACTER
      FIELD count_men         AS CHARACTER
      FIELD comments          AS CHARACTER
      FIELD timebase          AS CHARACTER
      FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN"
      INDEX koder CODE number work_construction.
    
   
   DEFINE DATASET EBR-TryckDS XML-NODE-NAME "BODY"  FOR ebr-tryck_costcatalogue,ebr-tryck_type, ebr-tryck_year, ebr-tryck_catalogue, ebr-tryck_workcode, ebr-tryck_lop
   PARENT-ID-RELATION Relation1 FOR ebr-tryck_costcatalogue, ebr-tryck_type PARENT-ID-FIELD RID
   PARENT-ID-RELATION Relation1 FOR ebr-tryck_type, ebr-tryck_year PARENT-ID-FIELD RID
   PARENT-ID-RELATION Relation1 FOR ebr-tryck_year, ebr-tryck_catalogue PARENT-ID-FIELD RID
   PARENT-ID-RELATION Relation1 FOR ebr-tryck_catalogue, ebr-tryck_workcode PARENT-ID-FIELD RID
   PARENT-ID-RELATION Relation1 FOR ebr-tryck_workcode, ebr-tryck_lop PARENT-ID-FIELD RID.
   
   
   DEFINE TEMP-TABLE Ebr-Tryck_P2KomKatalog NO-UNDO XML-NODE-NAME "Ebr-Tryck_Katalog"
      FIELD KostnadsKatalog  AS CHARACTER
      FIELD Type AS INTEGER
      FIELD Year  AS CHARACTER
      FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN".
   
   
   DEFINE TEMP-TABLE Ebr-Tryck_P2Kom NO-UNDO
    FIELD Year  AS CHARACTER
    FIELD P2ARBKOD AS CHARACTER XML-NODE-NAME "P2Arbkod"
    FIELD P2LOPNR AS INTEGER XML-NODE-TYPE  "HIDDEN"
    FIELD SVEPLOPNR AS CHARACTER  XML-NODE-NAME "P2Lopnr"  
    FIELD ANMARKNING AS CHARACTER XML-NODE-NAME "Kommentar"
    FIELD ORDNING AS INTEGER
    FIELD INGAREJ AS CHARACTER LABEL "I kostnaden ingår ej"
    FIELD RID AS RECID                      XML-NODE-TYPE  "HIDDEN"
    INDEX P2ARBKOD P2ARBKOD P2LOPNR ORDNING.
   
   
   DEFINE DATASET Ebr-TryckP2KomDS FOR Ebr-Tryck_P2KomKatalog,Ebr-Tryck_P2Kom
   PARENT-ID-RELATION Relation1 FOR Ebr-Tryck_P2KomKatalog, Ebr-Tryck_P2Kom PARENT-ID-FIELD RID.   
   
   
                        
      
   
   /*
  DEFINE VARIABLE EBR-eDS0  AS HANDLE NO-UNDO.
  DEFINE VARIABLE EBR-eDS1  AS HANDLE NO-UNDO.
  DEFINE VARIABLE EBR-eDS2  AS HANDLE NO-UNDO.
  DEFINE VARIABLE EBR-eDS3  AS HANDLE NO-UNDO.
  DEFINE VARIABLE EBR-eDS4  AS HANDLE NO-UNDO.
  
   DEFINE VARIABLE pcFields AS CHARACTER NO-UNDO.
   DEFINE DATASET EBR-eDS XML-NODE-NAME "BODY" FOR ebr-e_costcatalogue,ebr-e_type, ebr-e_year, ebr-e_catalogue, ebr-e_workcode, ebr-e_lop
      /* Dynamic DataSet  minOccurs Omit-Initial-Values SERIALIZE-HIDDEN  XML-NODE-TYPE   XML-DATA-TYPE   "HIDDEN"*/                                                     
   DATA-RELATION EBR-eDS0 FOR ebr-e_costcatalogue, ebr-e_type REPOSITION RELATION-FIELDS(ebr-e_costcatalogue.kostnadskatalog_header, ebr-e_type.kostnadskatalog_header) NESTED
   DATA-RELATION EBR-eDS1 FOR ebr-e_type, ebr-e_year REPOSITION RELATION-FIELDS(ebr-e_type.type, ebr-e_year.type) NESTED
   DATA-RELATION EBR-eDS2 FOR ebr-e_year, ebr-e_catalogue  REPOSITION RELATION-FIELDS(ebr-e_year.year, ebr-e_catalogue.year) NESTED
   DATA-RELATION EBR-eDS3 FOR ebr-e_catalogue, ebr-e_workcode REPOSITION RELATION-FIELDS( ebr-e_catalogue.catalogue,ebr-e_workcode.catalogue) NESTED
   DATA-RELATION EBR-eDS4 FOR ebr-e_workcode, ebr-e_lop REPOSITION RELATION-FIELDS(ebr-e_workcode.code,ebr-e_lop.code) NESTED.
   */
 /*
 DEFINE VARIABLE EBR-eDS AS HANDLE NO-UNDO. 
 CREATE DATASET  EBR-eDS.
  
  
  
  EBR-eDS:SET-BUFFERS(BUFFER ebr-e_costcatalogue:HANDLE, BUFFER ebr-e_type:HANDLE, BUFFER ebr-e_year:HANDLE, BUFFER ebr-e_catalogue:HANDLE, BUFFER ebr-e_workcode:HANDLE, BUFFER ebr-e_lop:HANDLE).
  MESSAGE EBR-eDS:GET-BUFFER-HANDLE(1):NAME EBR-eDS:GET-BUFFER-HANDLE(2):NAME  EBR-eDS:GET-BUFFER-HANDLE(3):NAME
  EBR-eDS:GET-BUFFER-HANDLE(4):NAME  EBR-eDS:GET-BUFFER-HANDLE(5):NAME EBR-eDS:GET-BUFFER-HANDLE(6):NAME
  VIEW-AS ALERT-BOX.
  pcFields = "kostnadskatalog_header,kostnadskatalog_header". 
  EBR-eDS = EBR-eDS:ADD-RELATION(EBR-eDS:GET-BUFFER-HANDLE(1), EBR-eDS:GET-BUFFER-HANDLE(2), pcFields).
  
  pcFields = "catalogue,catalogue".
  EBR-eDS3 = EBR-eDS:ADD-RELATION(EBR-eDS:GET-BUFFER-HANDLE(4), EBR-eDS:GET-BUFFER-HANDLE(5),pcFields).
  pcFields = "code,code".
  EBR-eDS4 = EBR-eDS:ADD-RELATION(EBR-eDS:GET-BUFFER-HANDLE(5), EBR-eDS:GET-BUFFER-HANDLE(6),pcFields).
  
  pcFields = "year,year".
  EBR-eDS2 = EBR-eDS:ADD-RELATION(EBR-eDS:GET-BUFFER-HANDLE(3), EBR-eDS:GET-BUFFER-HANDLE(4),pcFields).
  
 pcFields = "type,type".
  EBR-eDS1 = EBR-eDS:ADD-RELATION(EBR-eDS:GET-BUFFER-HANDLE(2), EBR-eDS:GET-BUFFER-HANDLE(3),pcFields).
 RUN DS_UI 
  (INPUT "ebr-e_costcatalogue,ebr-e_type",
   INPUT "",
   INPUT "ebr-e_costcatalogue.kostnadskatalog_header,ebr-e_type.kostnadskatalog_header",
   INPUT "= 1",
   OUTPUT DATASET-HANDLE EBR-eDS).      
   pcFields = "ebr-e_costcatalogue.kostnadskatalog_header,ebr-e_type.kostnadskatalog_header". 
  EBR-eDS0 = EBR-eDS:ADD-RELATION(BUFFER ebr-e_costcatalogue:HANDLE, BUFFER ebr-e_type:HANDLE, pcFields).
  pcFields = "ebr-e_type.type,ebr-e_year.type".
  EBR-eDS1 = EBR-eDS:ADD-RELATION(BUFFER ebr-e_type:HANDLE, BUFFER ebr-e_year:HANDLE,pcFields).
  pcFields = "ebr-e_year.year,ebr-e_catalogue.year".
  EBR-eDS2 = EBR-eDS:ADD-RELATION(BUFFER ebr-e_year:HANDLE, BUFFER ebr-e_catalogue:HANDLE,pcFields).
  pcFields = "ebr-e_catalogue.catalogue,ebr-e_workcode.catalogue".
  EBR-eDS3 = EBR-eDS:ADD-RELATION(BUFFER ebr-e_catalogue:HANDLE, BUFFER ebr-e_workcode:HANDLE,pcFields).
  pcFields = "ebr-e_workcode.code,ebr-e_lop.code".
  EBR-eDS4 = EBR-eDS:ADD-RELATION(BUFFER ebr-e_workcode:HANDLE, BUFFER ebr-e_lop:HANDLE,pcFields).
     
 
 PROCEDURE DS_UI :
    DEFINE INPUT  PARAMETER pcSources    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcSourceKeys AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcFields     AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcKeyValue   AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER DATASET-HANDLE phDataSet.

DEFINE VARIABLE iEntry      AS INTEGER    NO-UNDO.
DEFINE VARIABLE hDataSource AS HANDLE     NO-UNDO.
DEFINE VARIABLE hBuffer     AS HANDLE     NO-UNDO.
DEFINE VARIABLE hQuery      AS HANDLE     NO-UNDO.
DEFINE VARIABLE hTable      AS HANDLE     NO-UNDO.

CREATE DATASET phDataSet.

DO iEntry = 1 TO NUM-ENTRIES(pcSources):
  CREATE TEMP-TABLE hTable.
  hTable:CREATE-LIKE(ENTRY(iEntry, pcSources)).
  hTable:TEMP-TABLE-PREPARE("tt" + ENTRY(iEntry, pcSources)).
  phDataSet:ADD-BUFFER(hTable:DEFAULT-BUFFER-HANDLE).
END.

phDataSet:ADD-RELATION(phDataSet:GET-BUFFER-HANDLE(1), 
                       phDataSet:GET-BUFFER-HANDLE(2),
                       pcFields).
    
 END PROCEDURE.
 */
