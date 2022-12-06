/*ARENDEPRODATA.i*/
DEFINE DATASET ArendeDS FOR arendehuvtt,arendenumtt,arendenumsubtt,arendeaonrTT,arendemtrlTT,arendetidlageTT
DATA-RELATION huvNumsDR FOR arendehuvtt, arendenumtt RELATION-FIELDS (arendehuvtt.ARENDENR,arendenumtt.ARENDENR,arendehuvtt.OMRADE,arendenumtt.OMRADE)
DATA-RELATION numSubsDR FOR arendenumtt, arendenumsubtt RELATION-FIELDS (arendenumtt.ARENDENR,arendenumsubtt.ARENDENR,arendenumtt.OMRADE,arendenumsubtt.OMRADE,arendenumtt.NUM,arendenumsubtt.NUM)
DATA-RELATION huvAonrDR FOR arendehuvtt, arendeaonrTT RELATION-FIELDS (arendehuvtt.ARENDENR,arendeaonrTT.ARENDENR,arendehuvtt.OMRADE,arendeaonrTT.OMRADE)
DATA-RELATION huvMtrlDR FOR arendehuvtt, arendemtrlTT RELATION-FIELDS (arendehuvtt.ARENDENR,arendemtrlTT.ARENDENR,arendehuvtt.OMRADE,arendemtrlTT.OMRADE)
DATA-RELATION huvTidlDR FOR arendehuvtt, arendetidlageTT RELATION-FIELDS (arendehuvtt.ARENDENR,arendetidlageTT.ARENDENR,arendehuvtt.OMRADE,arendetidlageTT.OMRADE).