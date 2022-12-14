/*DEFVALUESNETTT.I
 */

DEFINE {&STATIC} TEMP-TABLE GuruDefaultsTT NO-UNDO
   BEFORE-TABLE GuruDefaultsTTBef
   FIELD PROGRAM AS CHARACTER       /* Vilket program v?rdet tillh?r */
   FIELD ANVANDARE AS CHARACTER
   FIELD HUVUDINT AS INTEGER        /* Identifierande int-variabel */
   FIELD HUVUDCHAR AS CHARACTER     /* Identifierande char-variabel */
   FIELD EXTRADATA AS CHARACTER     /* Extraparametrar f?r att s?ka/lagra */
   FIELD TOOLGROUP AS CHARACTER     /* Vilken grupp den tillh?r */
   FIELD TOOLKEY AS CHARACTER       /* Key/Ben?mning */
   FIELD TOOLTYPE AS CHARACTER      /* Vilken typ v?rdet tillh?r, f?r att s?tta/h?mta l?ttare via klasser */
   FIELD TOOLVALUE AS CHARACTER     /* Vilket v?rde */
   FIELD VALIDTOOLVALUE AS CHARACTER     /* Vilket v?rde NU */
   FIELD TTRECID AS RECID
   INDEX PROGRAM PROGRAM ANVANDARE HUVUDINT HUVUDCHAR                       /* Standard-index, s?ker program, int, char*/
   INDEX PROKEY PROGRAM ANVANDARE TOOLKEY 
   INDEX TOOLKEY TOOLTYPE TOOLGROUP TOOLKEY.                                  /* Standard-index f?r specifierad key */
   
   
DEFINE {&STATIC} TEMP-TABLE GuruDefaultValuesTT NO-UNDO LIKE GuruDefaultsTT.