DEFINE VARIABLE num-recs AS INTEGER.
DEFINE VARIABLE msg-txt AS CHARACTER INITIAL
       "There are <x> records in the table.".

/* Count the records. */
FOR EACH customer:
  num-recs = num-recs + 1.
END.

/* If there is only one record, make the message singular. */
IF num-recs = 1
THEN ASSIGN ENTRY(2, msg-txt, " ") = "is"
            ENTRY(4, msg-txt, " ") = "record".

/* Insert the record count into the string. */
ENTRY(3, msg-txt, " ") = STRING(num-recs).

MESSAGE msg-txt.
