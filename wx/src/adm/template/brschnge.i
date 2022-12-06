/* brschnge.i - trigger code for VALUE-CHANGED trigger of SmartBrowse */
     RUN get-attribute('ADM-NEW-RECORD':U). /* If this is triggered by initial*/
     IF RETURN-VALUE NE "YES":U THEN        /*  values in an Add, ignore it. */
       RUN notify ('row-available':U).
