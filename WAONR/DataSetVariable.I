
/*------------------------------------------------------------------------
    File        : DataSetVariable.I
    Purpose     : 
*/



DEFINE TEMP-TABLE DatasetDeftt NO-UNDO
   FIELD dataDsName AS CHARACTER
   FIELD antaltab     AS INTEGER 
   FIELD pcBuffers    AS CHARACTER  {DataSetExtent.i}
   FIELD pcRelFields  AS CHARACTER  {DataSetExtent.i}
   FIELD pcRelTables  AS CHARACTER  {DataSetExtent.i}
   FIELD pcSources    AS CHARACTER  {DataSetExtent.i}
   FIELD pcSourceKeys AS CHARACTER  {DataSetExtent.i}
   FIELD pcKeyValue   AS CHARACTER  {DataSetExtent.i}
   FIELD repMode  AS LOGICAL
   FIELD nestMode AS LOGICAL
   FIELD NrDS AS INTEGER.
   
 