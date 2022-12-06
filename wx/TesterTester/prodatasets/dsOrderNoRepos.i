/* dsOrderNoRepos.i -- include file definition of DATASET dsOrder with no 
   REPOSITION qualifier. */
DEFINE DATASET dsOrder FOR ttOrder, ttOline, ttItem
  DATA-RELATION OrderLine FOR ttOrder, ttOline
    RELATION-FIELDS (OrderNum, OrderNum)
  DATA-RELATION LineItem FOR ttOline, ttItem
    RELATION-FIELDS (ItemNum, ItemNum).
