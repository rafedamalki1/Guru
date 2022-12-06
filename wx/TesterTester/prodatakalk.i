/*prodatakalk.i*/
DEFINE DATASET KalkylDS FOR kalkhuvtt,kalknumtt,kalknumsubtt
DATA-RELATION KalkhuvNumsDR FOR kalkhuvtt, kalknumtt RELATION-FIELDS (kalkhuvtt.KALKNR,kalknumtt.KALKNR,kalkhuvtt.OMRADE,kalknumtt.OMRADE)
DATA-RELATION KalknumSubsDR FOR kalknumtt, kalknumsubtt RELATION-FIELDS (kalknumtt.KALKNR,kalknumsubtt.KALKNR,kalknumtt.OMRADE,kalknumsubtt.OMRADE,kalknumtt.NUM,kalknumsubtt.NUM).  