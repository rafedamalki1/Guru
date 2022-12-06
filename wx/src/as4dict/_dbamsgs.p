/* _dbamsgs.p

     Procedure to run error messages from dba commands   
     Created Donna McMann
     History:
             06/20/96 Added message for Native 4GL client.
             08/07/97 Added message for word indexing.
*/

DEFINE VARIABLE error_txt AS CHARACTER EXTENT 35 NO-UNDO.
 
 ASSIGN
    error_txt[1]   = "OK"
    error_txt[2]   = "Object not found"     
    error_txt[3]   =  "Library not found"     
    error_txt[4]   = "Invalid object type"
    error_txt[5]   =  "Invalid User Authority for Maintenance"
    error_txt[6]   =  "Not in DBA Mode"
    error_txt[7]   =  "Already in DBA Mode"
    error_txt[8]   =  "In LBI Mode"
    error_txt[9]   =  "User File open"                  
    error_txt[10] = "Commitment control not started"
    error_txt[11] =  "Wrong File Format, cannot modify"    
    error_txt[12] =  "File created and locked"
    error_txt[13] =  "File not created"
    error_txt[15] =  "User Index Error"
    error_txt[16] =  "Dictionary access denied"
    error_txt[17] =  "Unlock problem"
    error_txt[18] =  "File Not reserved"
    error_txt[19] =  "Object not locked"
    error_txt[20] =  "Journal entry error"
    error_txt[21] =  "Not a physical file"
    error_txt[22] =  "Entry not journaled"
    error_txt[23] =  "File not journaled"
    error_txt[24] =  "File not found"
    error_txt[25] =  "Member not found"
    error_txt[26] =  "Journal receiver not started"
    error_txt[27] =  "Commit Failed"
    error_txt[28] =  "Rollback Failed"                
    error_txt[29] = "File not deleted"
    error_txt[30] = "Invalid operation"
    error_txt[31] = "Sequences not journaled"
    error_txt[32] = "Commit Failed and save file could not be restored"
    error_txt[33] = "AS/400 Native 4GL Client Sync failed"
    error_txt[34] = "*USRSPC exists and can not be used for word index"
    error_txt[35] = "*USRIDX exists and can not be used for word index".             
    
 { as4dict/dictvar.i shared }
      
IF dba_return < 35 THEN  
  MESSAGE "The following DBA command error message was received:"  SKIP 
                         "         "  error_txt[dba_return]   SKIP 
                           VIEW-AS ALERT-BOX ERROR BUTTON OK.
     
 ELSE
   MESSAGE "A DBA command error has occurred contact Progress Software"
                        "with the following error code " dba_return dba_cmd
          VIEW-AS ALERT-BOX ERROR BUTTON OK.     
          
