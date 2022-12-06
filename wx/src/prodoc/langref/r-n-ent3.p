/* r-n-ent3.p */

DEFINE VARIABLE sentence AS CHARACTER.

sentence = "This sentence would be seven words long " +
           "if it were six words shorter.".
DISPLAY NUM-ENTRIES(sentence, " ").
