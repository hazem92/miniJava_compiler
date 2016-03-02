
Compiling/environment.ml :
This is the execution environment. It contains
- the class description (Attribute classes)
- the heap (Attribute tas)
- the methods area (Attribute global_methodes)
- the stacks (Attribute scopes)
 
Compiling/execute.ml :
 
    This module contains the function that executes the "main" method of the "Main" class.
 
    So that a class named Main should exist and should contain a method named main. Otherwise,
 
    nothing is executed.
 
    The function "gather_toplevel" adds defined classes to the execution environment.
 
    The interface declaration are not taken into account.
 
    This module contains also all functions to execute expressions and statements.
 
    Some expressions are not covered : NewArray, ArrayInit, Array, CondOp, Type, ClassOf, Instanceof, Pre, Post and VoidClass.
 
    Some statements are not covered :Throw, Try and Nop.
 
    The visibility management is also not covered.
 
    The function responsible for printing data is System_out.print()
 
    In a method implementation, the name of the attributes should be precessed by "this".
 
    The constructors are distinguished by the number of their parameters. So that, it is not allowed to define two
 
    constructors of the same class with the same number of parameters.

