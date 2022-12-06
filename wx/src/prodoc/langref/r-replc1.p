/* r-replc1.p */

DEFINE VARIABLE chktext  AS CHARACTER.
DEFINE VARIABLE i        AS INTEGER.
DEFINE VARIABLE chkndx   AS INTEGER.
DEFINE VARIABLE ndx      AS INTEGER.
DEFINE VARIABLE old-text AS CHARACTER.
DEFINE VARIABLE new-text AS CHARACTER.
DEFINE VARIABLE max-len  AS INTEGER.
DEFINE VARIABLE comment  AS CHARACTER FORMAT "x(49)" EXTENT 5
   INITIAL ["You are probably interested in PROGRESS because",
            "you have a lot of information to organize.  You",
            "want to get at the information, add to it, and",
            "change it, without a lot of work and aggravation.",
            "You made the right choice with PROGRESS." ].

DISPLAY comment WITH CENTERED FRAME comm NO-LABELS
     TITLE "Why You Chose PROGRESS" ROW 4.

REPEAT:
   SET old-text LABEL "Enter text to search for"
       new-text LABEL "Enter text to replace with"
   WITH FRAME replace SIDE-LABELS CENTERED.
   max-len = MAXIMUM(LENGTH(old-text), LENGTH(new-text)).
   DO i = 1 TO 5:
      ndx = 1.
      DO ndx = 1 TO LENGTH(comment[i]):
         chktext = SUBSTRING(comment[i], ndx).
         chkndx = INDEX(chktext, old-text).
         IF chkndx <> 0 THEN DO:
            ndx = ndx + chkndx - 1.
            OVERLAY(comment[i], ndx, max-len, "CHARACTER") = new-text.
            ndx = max-len.
         END.
      END.
      DISPLAY comment[i] WITH FRAME comm.
   END.
END.
