This test is here to ensure that the very minimal API we provide to ensure
users can eventually custromize the order of global transformations (i.e.
structure -> structure and signature -> signature) does work as intended.

For this test we have two different ppx-es: exp_to_a and exp_to_b which both
rewrite the same [%exp] into either a or b.

We have two different drivers, both of them have both ppx-es linked but one
reorders transformations to ensure exp_to_a is first in the list and the other
that exp_to_b is first.

If we consider the following ml file:

  $ cat > test.ml << EOF
  > let x = [%exp]
  > EOF

If we run the driver with exp_to_a first:

  $ ./driver_a_first.exe test.ml
  let x = a

And the one with exp_to_b first:

  $ ./driver_b_first.exe test.ml
  let x = b
