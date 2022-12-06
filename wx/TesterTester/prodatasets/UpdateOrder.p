/* updateOrder.p -- accepts a ProDataSet and saves changes to the OrderLine 
   to the database. */
{dsOrderTT.i}
{dsOrder.i}

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrder.

DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
DEFINE VARIABLE hAfterBuf  AS HANDLE NO-UNDO.
DEFINE VARIABLE hBeforeBuf AS HANDLE NO-UNDO.

DEFINE DATA-SOURCE srcOline FOR OrderLine.

BUFFER ttOline:ATTACH-DATA-SOURCE(DATA-SOURCE srcOline:HANDLE).
hDSChanges = DATASET dsOrder:HANDLE.
hAfterBuf = hDSChanges:GET-BUFFER-HANDLE("ttOline").
hAfterBuf:FIND-FIRST().
/* hBeforeBuf = hAfterBuf:TABLE-HANDLE:BEFORE-TABLE:DEFAULT-BUFFER-HANDLE. */
hBeforeBuf = hAfterBuf:BEFORE-BUFFER.
hBeforeBuf:FIND-FIRST().

/* MESSAGE "After: " hAfterBuf:BUFFER-FIELD("Price"):BUFFER-VALUE
     "Before:" hBeforeBuf:BUFFER-FIELD("Price"):BUFFER-VALUE. */
/* Alternatively you can refer to the before-table and its buffer by name 
   because they are statically defined. */
FOR EACH ttOlineBefore:
  /* This code illustrates setting the ERROR status and the REJECTED status 
     for a row. */
  BUFFER ttOline:FIND-BY-ROWID(BUFFER ttOlineBefore:AFTER-ROWID).
  IF ttOline.Price > (ttOlineBefore.Price * 1.1) THEN
    ASSIGN 
      DATASET dsorder:ERROR = TRUE
      BUFFER ttOlineBefore:ERROR = TRUE
      BUFFER ttOlineBefore:REJECTED = TRUE
      BUFFER ttOlineBefore:ERROR-STRING =
        "Line " + BUFFER ttOlineBefore:BUFFER-FIELD("LineNum"):STRING-VALUE +
        " price change from " +
        TRIM(BUFFER ttOlineBefore:BUFFER-FIELD("Price"):STRING-VALUE) +
        " to " + TRIM(BUFFER ttOline:BUFFER-FIELD("Price"):STRING-VALUE) +
        " is too high.".
  ELSE /* else SAVE-ROW-CHANGES below */
    /* This CASE block is entirely replaced by SAVE-ROW-CHANGES below: */
    /* 
    CASE BUFFER ttOlineBefore:ROW-STATE:
      WHEN ROW-MODIFIED THEN
        DO TRANSACTION ON ERROR UNDO, LEAVE:
          /* This is what SAVE-CHANGES will do for us. */
          /* 
          FIND OrderLine WHERE 
            OrderLine.OrderNum = ttOlineBefore.OrderNum AND
            OrderLine.LineNum = ttOlineBefore.LineNum EXCLUSIVE-LOCK. */
          /* Alternative to the FIND: */
          BUFFER OrderLine:FIND-FIRST
            (DATA-SOURCE srcOline:SAVE-WHERE-STRING(1), EXCLUSIVE-LOCK).
          IF NOT BUFFER OrderLine:BUFFER-COMPARE(BUFFER ttOlineBefore:HANDLE) THEN
            BUFFER ttOlineBefore:ERROR-STRING = "Someone else changed it.".
          ELSE DO:
            FIND ttOline WHERE ROWID(ttOline) = BUFFER ttOlineBefore:AFTER-ROWID.
            BUFFER OrderLine:BUFFER-COPY(BUFFER ttOline:HANDLE).
            /* Force execution of any triggers. */
            VALIDATE OrderLine.
            BUFFER ttOline:BUFFER-COPY(BUFFER OrderLine:HANDLE).
            RELEASE OrderLine.
          END. /* ELSE DO IF not changed by someone else. */
        END. /* DO WHEN ROW-MODIFIED */
    END CASE.
    */
    BUFFER ttOlineBefore:SAVE-ROW-CHANGES().
END. /* FOR EACH ttOlineBefore */
