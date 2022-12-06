/*2NEW*/

open query x for each customer no-lock.

def browse x query x
    display cust-num name address state format "x(10)" address2
    enable cust-num name address state 
        with 10 down width 78.

form x with frame y.

on leave of customer.cust-num in browse x do:

  /* Filter out cursor movements down with a mouse. */
  if last-event:event-type = "Progress" then return.

  /* Support TAB */
  if last-event:widget-enter = customer.name:handle in browse x then
  do:
    apply "entry" to customer.state in browse x.
    return no-apply.
  end.

  /* Support BACK-TAB */
  if last-event:widget-enter <> customer.cust-num:handle in browse x then
  do:
    apply "cursor-up" to browse x.
    apply "entry" to customer.address in browse x.
    return no-apply.
  end.
end.

on leave of customer.state in browse x do:

  /* Filter out cursor movements down with a mouse. */
  if last-event:event-type = "Progress" then return.

  /* Support TAB */
  if last-event:widget-enter = customer.cust-num:handle in browse x then
  do:
    apply "entry" to customer.name in browse x.
    return no-apply.
  end.

  /* Support BACKTAB */
  if last-event:widget-enter = customer.address:handle in browse x then
  do:
    apply "entry" to customer.cust-num in browse x.
    return no-apply.
  end.
end.

on leave of customer.name in browse x do:

  /* Filter out cursor movements down with a mouse. */
  if last-event:event-type = "Progress" then return.

  /* Support BACKTAB */
  if last-event:widget-enter = customer.cust-num:handle in browse x then
  do:
    apply "entry" to customer.state in browse x.
    return no-apply.
  end.
end.

on leave of customer.address in browse x do:

  /* Filter out cursor movements down with a mouse. */
  if last-event:event-type = "Progress" then return.

  /* Support TAB */
  if last-event:widget-enter <> customer.address:handle in browse x and
     last-event:widget-enter <> customer.name:handle in browse x then
  do:
    apply "cursor-down" to browse x.
    apply "entry" to customer.cust-num in browse x.
    return no-apply.
  end.
end.



update x with frame y.

