/*KALKYLANVEGENDATA.i KÖRS EJ*/
DEFINE DATASET KalkylAnvEgenDS FOR kalknumanvegentt,kalknumsubanvegentt
DATA-RELATION anvnumSubsDR FOR kalknumanvegentt, kalknumsubanvegentt RELATION-FIELDS (kalknumanvegentt.ANVANDARE,kalknumsubanvegentt.ANVANDARE,kalknumanvegentt.NUM,kalknumsubanvegentt.NUM).
