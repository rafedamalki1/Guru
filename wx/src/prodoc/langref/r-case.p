/* r-case.p */

DEFINE VARIABLE pay-stat    AS INTEGER INITIAL 1.

UPDATE pay-stat VIEW-AS RADIO-SET RADIO-BUTTONS "Unpaid", 1,
        "Partially paid", 2, "Paid in full", 3.

CASE pay-stat:
  WHEN 1 THEN
    MESSAGE "This account is unpaid.".
  WHEN 2 THEN  
    MESSAGE "This account is partially paid.".
  WHEN 3 THEN  
    MESSAGE "This account is paid in full.".
END CASE.
