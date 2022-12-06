/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/
/* File:_chgdbmd.p

Description:
    This procedure changes the mode the user is in depending on whether
    the read only indicator is on or off.  The s_DictDirty indicator is set if any
    updates have been made.  This indicator is checked to know if the commit
    action must be taken.
    
History:
     Created  03/23/95  D. McMann  
     Modified 03/25/96  D. McMann Changed QUESTION to WARNING in messages
              06/25/97  D. McMann Added allow_word_index for word index support
              08/14/97  D. McMann Added check for having more than word indexes
                        97-08-14-22   
*/                                                                       

{ as4dict/dictvar.i  shared }
{ as4dict/brwvar.i  shared }
 { as4dict/menu.i shared }



SESSION:IMMEDIATE-DISPLAY = TRUE.

DEFINE VARIABLE answer AS LOGICAL INITIAL FALSE NO-UNDO.     
DEFINE VARIABLE dbhold  AS CHARACTER NO-UNDO.


/*===========================Internal Procedures=========================*/

/*----------------------------------------------------------------
   See if the user made any changes in a property window 
   that he hasn't saved.  This (_changed.p) will ask if he wants 
   to save and do the save if he says yes. 

   Returns: "error" if an error occurs when the user tries to 
	    save, otherwise, "".
---------------------------------------------------------------*/
Procedure Check_For_Changes:

   Define var err as logical NO-UNDO.

   err = no.
   if s_win_Tbl <> ? then
      run as4dict/_changed.p (INPUT {&OBJ_TBL}, yes, OUTPUT err).
   if NOT err AND s_win_Seq <> ? then
      run as4dict/_changed.p (INPUT {&OBJ_SEQ}, yes, OUTPUT err).
   /*-----
   if NOT err AND s_win_Dom <> ? then
      run as4dict/_changed.p (INPUT {&OBJ_DOM}, yes, OUTPUT err).
   ----*/
   if NOT err AND s_win_Fld <> ? then
      run as4dict/_changed.p (INPUT {&OBJ_FLD}, yes, OUTPUT err).
   if NOT err AND s_win_Idx <> ? then
      run as4dict/_changed.p (INPUT {&OBJ_IDX}, yes, OUTPUT err).

   if err then 
      return "error".
   else
      return "".
End.
/*===========================  Main Line Code  ===========================*/   
/* This variable is set when entering the dictionary and if the user was in the middle
     of a transaction, we don't want them to do any maintenance */

IF s_InTran_ReadOnly THEN  DO:
  MESSAGE  "Since your are either in the middle of a transaction or the version"  
            "of PROGRESS only allows the Progress/400 dictionary to be in"  
            "read-only mode, you can not performance any maintenance."    
     VIEW-AS ALERT-BOX INFORMATION buttons OK.         
  ASSIGN s_mod_chg = "Read Only" 
         s_mod_chg:screen-value in frame browse = "Read Only"
         allow_word_idx = FALSE.    
  apply "back-tab" to s_mod_chg in frame browse.
END.             
ELSE IF s_ReadOnly   THEN DO:      
  ASSIGN dbhold = s_CurrDb + "?".
  MESSAGE  "Selecting modify schema will place an exclusive lock on"  
           "the Progress/400 server schema.  This locks the schema for" 
           "all users until you commit the changes.  It is recommended " 
           "you save your current server schema and DB2/400 database"
           "before doing maintenance."  SKIP (1)
           "Are you sure you want to modify the Progress/400" 
           "server schema in library"  dbhold SKIP
        VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE answer. 
     
  IF answer  THEN DO:   
    run adecomm/_setcurs.p ("WAIT").
    ASSIGN dba_cmd = "START".       
    RUN as4dict/_dbaocmd.p (INPUT "", INPUT "", INPUT "", INPUT 0, INPUT 0).       
        
    IF dba_passed THEN  DO :
      IF dba_return = 1 THEN DO:
        ASSIGN s_ReadOnly = FALSE
               s_mod_chg = "Modify Schema" .   
                             
        ASSIGN dba_cmd = "QRYSRVCAP".
        RUN as4dict/_dbaocmd.p (INPUT "WRDIDX", INPUT "", INPUT "", INPUT 0, INPUT 0).      
        IF dba_return = 1 THEN
          ASSIGN allow_word_idx = TRUE.
        ELSE
          ASSIGN allow_word_idx = FALSE.                                                          
        RUN as4dict/_brwgray.p  (INPUT false).
      END.    
      ELSE IF dba_return = 9 THEN DO:             
        ASSIGN answer = TRUE.
        MESSAGE "You have opened DB2/400 Database files."
                 "Before being allowed to modify the server"
                 "schema, they must be closed."  SKIP (1)
                 "Do you want to close them now?"  
                VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE answer.  
                  
        IF answer THEN DO:              
          run adecomm/_setcurs.p ("WAIT").
          ASSIGN dba_cmd = "CLOSEALL".       
          RUN as4dict/_dbaocmd.p (INPUT "", INPUT "", INPUT s_CurrDb, INPUT 0, INPUT 0).   
          ASSIGN dba_cmd = "START".       
          RUN as4dict/_dbaocmd.p (INPUT "", INPUT "", INPUT "", INPUT 0, INPUT 0).       
          IF dba_return = 1 THEN DO:
            ASSIGN s_ReadOnly = FALSE
                   s_mod_chg = "Modify Schema" .
  
            ASSIGN dba_cmd = "QRYSRVCAP".
            RUN as4dict/_dbaocmd.p (INPUT "WRDIDX", INPUT "", INPUT "", INPUT 0, INPUT 0).      
            IF dba_return = 1 THEN
              ASSIGN allow_word_idx = TRUE.
            ELSE
              ASSIGN allow_word_idx = FALSE.
                                                                            
            RUN as4dict/_brwgray.p  (INPUT false).
          END. 
          ELSE DO:        
            ASSIGN s_mod_chg = "Read Only" 
                   s_mod_chg:screen-value in frame browse = "Read Only".    
            apply "back-tab" to s_mod_chg in frame browse.
          END.    
          run adecomm/_setcurs.p ("").
        END.
        ELSE DO:        
          ASSIGN s_mod_chg = "Read Only" 
                 s_mod_chg:screen-value in frame browse = "Read Only".    
          apply "back-tab" to s_mod_chg in frame browse.
        END.   
      END.                                                                                             
      ELSE DO:           
        MESSAGE "An error must have occurred during your last DBA session."
                 "You are currently in DBA mode which means that any changes"
                 "entered since your last commit have been lost and must be"
                 "re-entered. " SKIP (1)
                 VIEW-AS ALERT-BOX INFORMATION BUTTON OK.    
		  
        run adecomm/_setcurs.p ("WAIT").           
   
        ASSIGN dba_cmd = "ROLLBACK".      
        RUN as4dict/_dbaocmd.p (INPUT "", INPUT "", INPUT "", INPUT 0, INPUT 0).    
             
        ASSIGN dba_cmd = "END".      
        RUN as4dict/_dbaocmd.p (INPUT "", INPUT "", INPUT "", INPUT 0, INPUT 0).    
           
        ASSIGN dba_cmd = "START".
        RUN as4dict/_dbaocmd.p (INPUT "", INPUT "", INPUT "", INPUT 0, INPUT 0).
 
        ASSIGN s_ReadOnly =  FALSE
               s_mod_chg = "Modify Schema" .                 
       
        ASSIGN dba_cmd = "QRYSRVCAP".
        RUN as4dict/_dbaocmd.p (INPUT "WRDIDX", INPUT "", INPUT "", INPUT 0, INPUT 0).      
        IF dba_return = 1 THEN
          ASSIGN allow_word_idx = TRUE.
        ELSE
          ASSIGN allow_word_idx = FALSE.
                                   
        RUN as4dict/_brwgray.p  (INPUT false).         
      END.                                   
    END.              
    ELSE DO:        
      ASSIGN s_mod_chg = "Read Only" 
             s_mod_chg:screen-value in frame browse = "Read Only".    
      apply "back-tab" to s_mod_chg in frame browse.
    END.    
    run adecomm/_setcurs.p ("").                                                                            
  END.            
  else do:
    ASSIGN s_mod_chg = "Read Only" 
           s_mod_chg:screen-value in frame browse = "Read Only".  
    apply "back-tab" to s_mod_chg in frame browse.                           
  end.
END.
ELSE DO:           
   /* Check to see if user has made any changes in open property windows
      that he hasn't saved.  If there are, and he saves now, and an error
      occurs, don't continue.  
   */
  run Check_For_Changes.
  if RETURN-VALUE = "error" then  return.  

  IF s_dictDirty THEN DO:                           
    current-window = s_win_Browse.
    message  "You have uncommited transactions that must be commited"
                "before returning to read only mode.   Commiting may potentially"
                "take a long time.  Are you sure you want to change to read only?" SKIP
 	        view-as ALERT-BOX WARNING  buttons YES-NO  update answer
 	        in window s_win_Browse.

    IF answer THEN DO:            
        RUN as4dict/_chkfld.p.
        if user_env[34] = "N" THEN DO:          
          ASSIGN s_ReadOnly = FALSE
                 s_Mod_chg = "Modify Schema".        
                 s_mod_chg:screen-value in frame browse = "Modify Schema".  
          apply "back-tab" to s_mod_chg in frame browse.                                                                       
          RETURN.    
        end.                                                                               
                                         
      /* If focus is in Db list, we want it to remain there instead
	 of defaulting to fill-in.  So remember if list has the focus.
	 focus should never be ? here (I don't think) but the
	 GUI focus model is a bit wierd so sometimes it is.  
      */
      if focus <> ? then
	   s_Dblst_Focus = (if s_lst_Dbs:handle in frame browse = 
			  focus:handle then yes else no).

      s_Trans = {&TRANS_COMMIT}.   
            
      /* apply u2 will cause the commit to be issued in procedure _dcttran.p 
         then we will return here and the dba end will be done */
            
      apply "U2" to frame browse.    
      ASSIGN s_ReadOnly = TRUE
             s_mod_chg = "Read Only"
             allow_word_idx = FALSE .
      RUN as4dict/_brwgray.p (INPUT false).
    END.     
    ELSE                    
      ASSIGN s_mod_chg = "Modify Schema" 
             s_mod_chg:screen-value in frame browse = "Modify Schema".      
      apply "back-tab" to s_mod_chg in frame browse.                       
    END.      
    ELSE DO:                   
      run adecomm/_setcurs.p ("WAIT").
      ASSIGN dba_cmd = "END".      
      RUN as4dict/_dbaocmd.p (INPUT "", INPUT "", INPUT "", INPUT 0, INPUT 0).
      IF dba_passed THEN DO:          
        run as4dict/_delwins.p (INPUT yes).
        ASSIGN s_ReadOnly = TRUE      
               s_mod_chg = "Read Only"
               allow_word_idx = FALSE .
        RUN as4dict/_brwgray.p (INPUT false).
      END.    
      run adecomm/_setcurs.p ("").
    END.      
END.
 
