/* s-alert.p - alert box routine */

/*
Input text comes in 'pi_text' parameter, and may be any length.
This program splits it up into 45-character chunks, breaking at
spaces.  Embedded '^' marks get translated into line-feeds (like
in column-labels).

pi_type the the type of alert box to use.
  pi_type = "e*" -> error
  pi_type = "i*" -> information
  pi_type = "m*" -> message
  pi_type = "q*" -> question
  pi_type = "w*" -> warning

pi_butn is the type of button(s) to use.
pio_ans is the initial value and also the result.
  pi_butn = "yes-no"        -> pio_ans = TRUE/FALSE
  pi_butn = "yes-no-cancel" -> pio_ans = TRUE/FALSE/?
  pi_butn = "ok"            -> pio_ans = TRUE
  pi_butn = "ok-cancel"     -> pio_ans = TRUE/?
  pi_butn = "retry-cancel"  -> pio_ans = TRUE/?

End-error handling:
  If there is a "cancel" button, then the result will be as if the user
  pressed the "cancel" button.  If there is no "cancel" button but
  there is a "no" button, then the result will be as if the user
  pressed the "no" button.  If there is only the "ok" button, then the
  result will be as if the user selected the "ok" button.
*/

DEFINE INPUT-OUTPUT PARAMETER pio_ans AS LOGICAL   NO-UNDO. /* value */
DEFINE INPUT        PARAMETER pi_type AS CHARACTER NO-UNDO. /* alert type */
DEFINE INPUT        PARAMETER pi_butn AS CHARACTER NO-UNDO. /* buttons */
DEFINE INPUT        PARAMETER pi_text AS CHARACTER NO-UNDO. /* text */

DEFINE VARIABLE v_size AS INTEGER   NO-UNDO. /* limit on dialog size */
DEFINE VARIABLE v_text AS CHARACTER NO-UNDO. /* word-wrapped text */
DEFINE VARIABLE v_w1   AS INTEGER   NO-UNDO. /* scrap */
DEFINE VARIABLE v_w2   AS INTEGER   NO-UNDO. /* scrap */

ASSIGN
  pi_text = TRIM(pi_text) + " ":u
  v_size  = 1.
DO WHILE TRUE:
  ASSIGN
    v_w1    = INDEX(pi_text,"^":u)
    v_w1    = (IF v_w1 = 0 OR v_w1 > 45 THEN 45 ELSE v_w1)
    v_w2    = MAXIMUM(R-INDEX(pi_text,"^":u,45),R-INDEX(pi_text," ":u,45))
    v_w2    = (IF v_w2 = 0 THEN 45 ELSE v_w2)
    v_w2    = MINIMUM(v_w2,v_w1)
    v_text  = v_text + (IF v_text = "" THEN "" ELSE CHR(10))
            + TRIM(SUBSTRING(pi_text,1,v_w2 - 1,"CHARACTER":u))
    pi_text = TRIM(SUBSTRING(pi_text,v_w2 + 1,-1,"CHARACTER":u))
    v_size  = v_size + 1.
  IF (v_size = 16) OR (LENGTH(pi_text,"CHARACTER":u) <= 45 
          AND INDEX(pi_text,"^":u) = 0) THEN DO:
    IF LENGTH(pi_text,"CHARACTER":u) > 45 OR INDEX(pi_text,"^":u) > 0 THEN 
      pi_text = "...":u.
    v_text = v_text + (IF v_text = "" THEN "" ELSE CHR(10)) + TRIM(pi_text).
    LEAVE.
  END.
END.

CASE SUBSTRING(pi_type,1,1,"CHARACTER":u) + "/":u + pi_butn:
  WHEN "e/yes-no":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX error BUTTONS yes-no
      UPDATE pio_ans.
  WHEN "e/yes-no-cancel":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX error BUTTONS yes-no-cancel
      UPDATE pio_ans.
  WHEN "e/ok":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX error BUTTONS ok
      UPDATE pio_ans.
  WHEN "e/ok-cancel":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX error BUTTONS ok-cancel
      UPDATE pio_ans.
  WHEN "e/retry-cancel":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX error BUTTONS retry-cancel
      UPDATE pio_ans.
  WHEN "i/yes-no":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX information BUTTONS yes-no
      UPDATE pio_ans.
  WHEN "i/yes-no-cancel":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX information BUTTONS yes-no-cancel
      UPDATE pio_ans.
  WHEN "i/ok":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX information BUTTONS ok
      UPDATE pio_ans.
  WHEN "i/ok-cancel":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX information BUTTONS ok-cancel
      UPDATE pio_ans.
  WHEN "i/retry-cancel":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX information BUTTONS retry-cancel
      UPDATE pio_ans.
  WHEN "m/yes-no":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX message BUTTONS yes-no
      UPDATE pio_ans.
  WHEN "m/yes-no-cancel":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX message BUTTONS yes-no-cancel
      UPDATE pio_ans.
  WHEN "m/ok":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX message BUTTONS ok
      UPDATE pio_ans.
  WHEN "m/ok-cancel":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX message BUTTONS ok-cancel
      UPDATE pio_ans.
  WHEN "m/retry-cancel":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX message BUTTONS retry-cancel
      UPDATE pio_ans.
  WHEN "q/yes-no":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX question BUTTONS yes-no
      UPDATE pio_ans.
  WHEN "q/yes-no-cancel":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX question BUTTONS yes-no-cancel
      UPDATE pio_ans.
  WHEN "q/ok":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX question BUTTONS ok
      UPDATE pio_ans.
  WHEN "q/ok-cancel":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX question BUTTONS ok-cancel
      UPDATE pio_ans.
  WHEN "q/retry-cancel":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX question BUTTONS retry-cancel
      UPDATE pio_ans.
  WHEN "w/yes-no":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX warning BUTTONS yes-no
      UPDATE pio_ans.
  WHEN "w/yes-no-cancel":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX warning BUTTONS yes-no-cancel
      UPDATE pio_ans.
  WHEN "w/ok":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX warning BUTTONS ok
      UPDATE pio_ans.
  WHEN "w/ok-cancel":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX warning BUTTONS ok-cancel
      UPDATE pio_ans.
  WHEN "w/retry-cancel":u THEN
    MESSAGE v_text
      VIEW-AS ALERT-BOX warning BUTTONS retry-cancel
      UPDATE pio_ans.
END CASE.

/* proper end-error handling of return value. */
IF NOT pio_ans AND NUM-ENTRIES(pi_butn,"-":u) = 2 THEN
  pio_ans = (IF pi_butn MATCHES "*-cancel":u THEN ? ELSE pi_butn <> "yes-no":u).

RETURN.

/* s-alert.p - end of file */
