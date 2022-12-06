/*KALKYLADMEXPIMPDATA.i*/



DEFINE TEMP-TABLE eikalkylkatalogsubtt NO-UNDO LIKE kalkylkatalogsubtt
FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN".

DEFINE TEMP-TABLE eikalkylprisertt NO-UNDO LIKE kalkylprisertt
FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN".

DEFINE TEMP-TABLE eikalkvisningtt NO-UNDO LIKE kalkvisningtt
FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN".

DEFINE TEMP-TABLE eikalkylarbkodertt NO-UNDO LIKE kalkylarbkodertt
FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN".

DEFINE TEMP-TABLE eikalkylloppostertt NO-UNDO LIKE kalkylloppostertt
FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN".

DEFINE TEMP-TABLE eikalkyllopsubtt NO-UNDO LIKE kalkyllopsubtt
FIELD RID AS RECID XML-NODE-TYPE  "HIDDEN".


DEFINE DATASET ExpImpsubDS XML-NODE-NAME "BODY"  FOR eikalkylkatalogsubtt,eikalkylprisertt, eikalkylarbkodertt, eikalkylloppostertt, eikalkyllopsubtt
   PARENT-ID-RELATION Relation1 FOR eikalkylkatalogsubtt, eikalkylprisertt PARENT-ID-FIELD RID
   PARENT-ID-RELATION Relation1 FOR eikalkylkatalogsubtt, eikalkylarbkodertt PARENT-ID-FIELD RID
   PARENT-ID-RELATION Relation1 FOR eikalkylkatalogsubtt, eikalkylloppostertt PARENT-ID-FIELD RID
   PARENT-ID-RELATION Relation1 FOR eikalkylkatalogsubtt, eikalkyllopsubtt PARENT-ID-FIELD RID.
   

/*CCC

DEFINE DATASET EBR-eDS XML-NODE-NAME "BODY"  FOR ebr-e_costcatalogue,ebr-e_type, ebr-e_year, ebr-e_catalogue, ebr-e_workcode, ebr-e_lop
   PARENT-ID-RELATION Relation1 FOR ebr-e_costcatalogue, ebr-e_type PARENT-ID-FIELD RID
   PARENT-ID-RELATION Relation1 FOR ebr-e_type, ebr-e_year PARENT-ID-FIELD RID
   PARENT-ID-RELATION Relation1 FOR ebr-e_year, ebr-e_catalogue PARENT-ID-FIELD RID
   PARENT-ID-RELATION Relation1 FOR ebr-e_catalogue, ebr-e_workcode PARENT-ID-FIELD RID
   PARENT-ID-RELATION Relation1 FOR ebr-e_workcode, ebr-e_lop PARENT-ID-FIELD RID.*/