 /*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------
  as4dict/dbaocmd.p      
  Created 4/14/95 Donna McMann  
  Modified 06/20/96 D. McMann Added SYNCING for native 4gl client
           06/24/97 D. McMann Added CMD CHKOBJ, CMD QRYSRVCAP and CMD VALWRDIDX
                              all for word index support.
           08/07/97 D. McMann Corrected VALWRDIDX command
  
  Shared variables defined in as4dict/dictvar.i     
              
    dba_cmd character = START, END, CLOSEALL, COMMIT, ROLLBACK, SYNSCHIMG, 
                        CHKF, RESERVE, UNRESERVE, DLTOBJ, CHKOBJ, QRYSRVCAP,
                        VALWRDIDX
                                                       
    PARAMETERS NEEDED DEPENDING ON COMMAND
          type, name, library             CHKF, RESERVE       
          name, library                   UNRESERVE, VALWRDIDX
          name, library, file#, Index#    DLTOBJ
          name, library, type             CHKOBJ
          type                            QRYSRVCAP type in this case is the
                                             type of capabilities to be checked
   
          dba_return to hold return code for those procedures which must perform
          logic based upon the return code from command.
              
  ---------------------------------------------------------------------*/                 
  
  DEFINE INPUT PARAMETER obj_type        AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER obj_name      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER obj_lib             AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER obj_File_nbr  AS INTEGER        NO-UNDO.
  DEFINE INPUT PARAMETER obj_Idx_nbr   AS INTEGER         NO-UNDO.      
  
  { as4dict/dictvar.i shared }
   
CASE dba_cmd:             
  WHEN  "START" THEN DO:
    FIND FIRST as4dict.p__db NO-LOCK.
    /* This is the original command before dictionary version was introduced */
    IF as4dict.p__Db._Db-misc1[3] = 0 THEN DO:
        CREATE as4dict.qcmd.
        ASSIGN as4dict.qcmd.cmd = "** DBA START".
        ASSIGN dba_return = RECID(as4dict.qcmd).     
    END. 
    /* New version of utilities */
    ELSE DO:
        CREATE as4dict.qcmd.
        ASSIGN as4dict.qcmd.cmd = "** DBA START 100".
        ASSIGN dba_return = RECID(as4dict.qcmd).     
    END.        
             
    IF  dba_return = 1 or dba_return = 7  or dba_return = 9 THEN 
        ASSIGN  dba_passed = TRUE.
    ELSE DO:
       RUN as4dict/_dbamsgs.p.
      ASSIGN dba_passed = FALSE.
    END.
  END.     
  WHEN "CLOSEALL" THEN DO:
    CREATE as4dict.qcmd.         
    ASSIGN as4dict.qcmd.cmd = "** DBA CLOSEALL " + obj_lib.
    ASSIGN dba_return = RECID(as4dict.qcmd).
    IF dba_return = 1 THEN ASSIGN dba_passed = TRUE.
    ELSE DO:
        RUN as4dict/_dbamsgs.p.
        ASSIGN dba_passed = FALSE.
     END.
  END.                  
  WHEN "COMMIT" THEN DO:
    CREATE as4dict.qcmd.
    ASSIGN as4dict.qcmd.cmd = "** DBA COMMIT". 
    ASSIGN dba_return =  RECID(as4dict.qcmd).
    IF dba_return = 1 THEN ASSIGN dba_passed = TRUE.    
    ELSE DO:
        RUN as4dict/_dbamsgs.p.
        ASSIGN dba_passed = FALSE.
     END.
  END.
  WHEN  "END" THEN DO:
    CREATE as4dict.qcmd.       
    ASSIGN as4dict.qcmd.cmd  = "** DBA END".
    ASSIGN dba_return = RECID(as4dict.qcmd).
          
    IF dba_return = 1 THEN ASSIGN dba_passed = TRUE.     
    ELSE DO:
        RUN as4dict/_dbamsgs.p.
        ASSIGN dba_passed = FALSE.
     END.    
  END.      
  WHEN  "ROLLBACK" THEN DO:
    CREATE as4dict.qcmd.       
    ASSIGN as4dict.qcmd.cmd  = "** DBA ROLLBACK".
    ASSIGN dba_return = RECID(as4dict.qcmd).
          
    IF dba_return = 1 THEN ASSIGN dba_passed = TRUE.    
    ELSE DO:
        RUN as4dict/_dbamsgs.p.
        ASSIGN dba_passed = FALSE.
     END.
  END.  
  WHEN  "SYNSCHIMG" THEN DO:
    CREATE as4dict.qcmd.       
    ASSIGN as4dict.qcmd.cmd  = "** DBA SYNSCHIMG".
    ASSIGN dba_return = RECID(as4dict.qcmd).
          
    IF dba_return = 1 THEN ASSIGN dba_passed = TRUE.    
    ELSE DO:
        RUN as4dict/_dbamsgs.p.
        ASSIGN dba_passed = FALSE.
    END.     
  END.                            
  WHEN "GETNUM" THEN DO:
      CREATE as4dict.qcmd.
      ASSIGN as4dict.qcmd.cmd = "** DBA GETNUM " + obj_lib.
      ASSIGN dba_return = RECID(qcmd).
  END.    
  WHEN "CHKOBJ" THEN DO:
      CREATE as4dict.qcmd.
      ASSIGN as4dict.qcmd.cmd = "** CMD CHKOBJ "  
                                + obj_name + "," 
                                + obj_Lib + "," 
                                + obj_type.   
      ASSIGN dba_return = RECID(qcmd).         
      
   END.   
  WHEN  "CHKF" THEN DO:
        CREATE as4dict.qcmd.
        ASSIGN as4dict.qcmd.cmd = "** DBA CHKF " 
                                 + obj_type + ","  
                                 + obj_name + "," 
                                 + obj_lib.                            
           ASSIGN dba_return = RECID(qcmd).        
              
   END.
  WHEN  "RESERVE" THEN DO:
        CREATE as4dict.qcmd.
        ASSIGN as4dict.qcmd.cmd = "** DBA RESERVE " 
                                  + obj_type + "," 
                                  + obj_name + "," 
                                  +  CAPS(obj_lib).
        ASSIGN dba_return = RECID(as4dict.qcmd).       
             
   END.                    
  WHEN "UNRESERVE" THEN DO:
        CREATE as4dict.qcmd.
        ASSIGN as4dict.qcmd.cmd = "** DBA UNRESERVE " 
                                  + obj_name + ","   
                                  + obj_lib. 
        ASSIGN dba_return = RECID(as4dict.qcmd).    
                              
  END.
  WHEN "DLTOBJ" THEN DO:
        CREATE as4dict.qcmd.
        ASSIGN as4dict.qcmd.cmd =  "** DBA DLTOBJ "   +
                                  obj_name + ","
                                + obj_lib + ","
                                + STRING(obj_file_nbr) + ","
                                + STRING(obj_Idx_nbr).   
        ASSIGN dba_return = RECID(as4dict.qcmd).   
                      
  END.  
  WHEN "QRYSRVCAP" THEN DO:
        CREATE as4dict.qcmd.
        ASSIGN as4dict.qcmd.cmd = "** CMD QRYSRVCAP " + obj_type.
        ASSIGN dba_return = RECID(as4dict.qcmd).
  END.
  WHEN "VALWRDIDX" THEN DO:
      CREATE as4dict.qcmd.
      ASSIGN as4dict.qcmd.cmd = "** CMD VALWRDIDX " + obj_name + "," + obj_lib.
      ASSIGN dba_return = RECID(qcmd).
  END.    
        
END.
          
                  
             
  
  
                                       
