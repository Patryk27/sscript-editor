/*
 This is demo of SScript libraries.
 At first, you need to compile "library.ss" (library.ssp), then this.
*/

@("stdlib\\stdio.ss")
@("stdlib\\library.ssm")

use std;

function<int> main()
{
 println(test()); // function "test" is our library-imported-function. Notice, that we didn't write its header anywhere - it is automatically determined by the compiler.
 return 0;
}