/*t_lodname.p*/

DEFINE VARIABLE nam  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pass AS INTEGER   NO-UNDO.

DEFINE SHARED VARIABLE drec_db AS RECID  NO-UNDO.


FOR EACH _File
  WHERE   _Dump-name    =    ?
    AND  _db-recid = drec_db
  AND NOT _File-name  BEGINS "_"
  AND     _Fil-misc2[8] =    ?:
    
    IF INTEGER(DBVERSION("DICTDB")) > 8 
      AND(_File._Owner<> "PUB" AND _File._Owner <>"_FOREIGN") THEN NEXT.
      
    assign nam = _File-name.
    
    assign _Dump-name = nam.

END.

FOR EACH _File
  WHERE   _Dump-name    =    ?
    AND _db-recid = drec_db
  AND NOT _File-name  BEGINS "_"
  BY      _Fil-misc2[8]:

    IF INTEGER(DBVERSION("DICTDB")) > 8 
      AND (_File._Owner<> "PUB" AND _File._Owner <>"_FOREIGN") THEN NEXT.
      
    assign nam = _File-name.
    
    assign _Dump-name = nam.

END.

RETURN.


