/*dumpname.i*/

/*---------------------------  MAIN-CODE  --------------------------*/

  IF INTEGER(DBVERSION("DICTDB")) > 8 THEN DO:
     nam = SUBSTRING(nam,1,32,"character").
 
    IF CAN-FIND(_File WHERE _File._Db-recid = drec_db
                        AND _Dump-name = nam  
              AND  (_File._Owner = "PUB" OR _File._Owner = "_FOREIGN")) THEN
      ASSIGN pass = 1 /*ABSOLUTE(_File-num)*/
             nam  = SUBSTRING(nam + "-------"
                      ,1
                      ,32 - LENGTH(STRING(pass),"character")
                      ,"character"
                      )
                       + STRING(pass).

    DO pass = 1 TO 9999 WHILE 
        CAN-FIND(_File WHERE _File._Db-recid = drec_db
                         AND _Dump-name = nam
                         AND (_File._Owner = "PUB" OR _File._Owner = "_FOREIGN")):      
      ASSIGN nam = SUBSTRING(nam + "-------"
                   ,1
                   ,32 - LENGTH(STRING(pass),"character")
                   ,"character"
                   )
                   + STRING(pass).
    END.
  END.
  ELSE DO:
    nam = SUBSTRING(nam,1,8,"character").

    IF CAN-FIND(_File WHERE _File._Db-recid = drec_db
                        AND _Dump-name = nam) THEN
      ASSIGN pass = 1 /*ABSOLUTE(_File-num)*/
             nam  = SUBSTRING(nam + "-------"
                      ,1
                      ,8 - LENGTH(STRING(pass),"character")
                      ,"character"
                      )
                       + STRING(pass).

    DO pass = 1 TO 9999 WHILE CAN-FIND(_File WHERE _File._Db-recid = drec_db
                                               AND _Dump-name = nam):      
      ASSIGN nam = SUBSTRING(nam + "-------"
                   ,1
                   ,8 - LENGTH(STRING(pass),"character")
                   ,"character"
                   )
                   + STRING(pass).
    END.
  END.

/*------------------------------------------------------------------*/
