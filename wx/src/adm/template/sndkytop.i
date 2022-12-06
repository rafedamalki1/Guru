/* sndkytop.i - for KEY link code */

  DEFINE INPUT  PARAMETER pc_key-name   AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER pc_key-value  AS CHARACTER NO-UNDO INITIAL ?.
  
  CASE pc_key-name:
  
