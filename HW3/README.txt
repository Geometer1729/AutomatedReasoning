Hi Chris! For this one, the syntax is hopefully intuitive. I didn't include implication in the parser however.
Use a '.' after quantifying a variable; for example:
    forall x. exists y. greaterThan(y,x)
The main function lets you enter more than one goal clause, and prints out the boolean for each one of them.
Example run:
>>>"Enter axioms line by line. Enter a blank line. Enter the goal clauses. Enter a blank line."
forall x. a(x) or b(x)
forall y. !b(y)

a(z)
b(w)

>>>[True,False]
