/* sndkyend.i - end of CASE code for KEY support for send-key */

      WHEN "":U THEN .  /* default case - no filter */
      OTHERWISE MESSAGE "Entry ":U pc_key-name
                        " was not found in the send-key cases in ":U
                        THIS-PROCEDURE:FILE-NAME 
                      VIEW-AS ALERT-BOX ERROR.
  END CASE.
