@("stdlib\\stdio.ss")

function<string> test() in "library.ssm";

use std;

function<int> main()
{
 println(test());
 return 0;
}