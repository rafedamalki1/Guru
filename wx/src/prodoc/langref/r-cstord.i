/* r-cstord.i */

FORM   "Cust #" AT 1  customer.cust-num AT 10 SKIP(1)
	customer.name AT 10
	customer.address AT 10
	customer.address2 AT 10
	customer.city AT 10  customer.city customer.state
	customer.postal-code SKIP(1)
	"Phone " AT 1 customer.phone FORMAT "999/999-9999" AT 10
	"Max Crd" AT 1 customer.credit-limit AT 10
	WITH FRAME cust-ord OVERLAY {&frame-options}.
