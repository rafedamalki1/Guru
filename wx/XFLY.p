   DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE  VARIABLE vkdatum AS DATE NO-UNDO.
vkdatum = 09/01/99.
prognamndat = "/eko1/guru/export/GURUX" + 
SUBSTRING(STRING(YEAR(vkdatum),"9999"),3,2) + STRING(MONTH(vkdatum),"99") + ".408".
kommandoprog = "/eko1/guru/infaftp " + "GURUX" + 
SUBSTRING(STRING(YEAR(vkdatum),"9999"),3,2) + STRING(MONTH(vkdatum),"99") + ".408".

   
   OS-COMMAND SILENT VALUE(kommandoprog).   
   
 
