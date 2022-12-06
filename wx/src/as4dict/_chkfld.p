/* Procedure _chkfld.p
   Donna L. McMann September 25, 1996

   A check must be performed to see if a table has been created that does
   not have any fields associated with it.  If a COMMIT is issued it will
   fail since the AS/400 does not support files with no fields.  This
   must be done in a seperate .p so that no references are made to a
   database in _dictg.p.

   A check must now be performed to ensure that if a word index has been
   defined for a file, that the file has at least one non-word index
   D. McMann August 14, 1997.

*/

{as4dict/dictvar.i  shared}   

FOR EACH as4dict.p__File NO-LOCK:
   IF as4dict.p__file._numfld = 0 THEN DO:
         message "There are tables which have been defined"
                 "with no fields.  You must define at least"
                 "one field per table" SKIP
                 VIEW-AS ALERT-BOX ERROR BUTTON OK.
         assign user_env[34] = "N".        
         return.                    
   end.    
   ELSE IF as4dict.p__File._Prime-Index = -1 AND allow_word_idx THEN DO:
        IF CAN-FIND(FIRST as4dict.p__Index where as4dict.p__Index._File-number =
                             as4dict.p__File._File-number AND
                             as4dict.p__Index._Wordidx = 1) THEN DO:
           message "There are tables which have word indexes that do not"
                   "have a primary index defined.  You must define a primary"
                   "index before you can commit." SKIP
                   VIEW-AS ALERT-BOX ERROR BUTTON OK.                 
           assign user_env[34] = "N".        
           return.  
        END.
   END.                                                                                                                                                                     
end.    
assign user_env[34] = "Y".   



