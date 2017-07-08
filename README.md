# Phoebe

A dimensional library to aid with physical dimensions.

I hate working with units in my programs - there's no native support for units in any programming language I use.
This is especially annoying in Python since I often use it for writing scripts related to physics.
Hopefully this library will be helpful in adding a little sanity to my programs.

## Reasons for writing Phoebe.

Units throw me off. I'm terrible at keeping track of and converting from SI units to US customary units and I'm even worse at keeping track of units that are particular to a specific field. Lightyears make sense in astronomy - not in construction. Using metres to work with both of them in a script doesn't really pan out. 

A dimesnion library would help by allowing my to input units in any format I want while still using it as a normal variable.
Should I work with different units (which I always will when working with someone else), I'd like to use other people's code without writing an adapter each and everytime. 

I'd also like native support for displaying units. I really don't like messing around with print statements and the like - writing " variable.print " should suffice for printing stuff.

Most importantly, I should be able to say with certainty that if my program compiles, my program is dimensionally correct. For runtime languages, I want the program to crash if the units don't match up - if it doesn't make sense in real life, it has no purpose being in my program. 


# FAQ

## Is this code production ready?

HAHAHAHAHAHAHAHA.

No.

## Why Phoebe?

Because I want this library to be small and unintrusive yet strong and dependable - just like a beetle. 
(I like to think beetles are dependable. I'm not too sure.)
Also, I really like Phoebe beetles - they're cute.

