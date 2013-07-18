@("stdlib\\stdio.ss")

function<string> test() [library="library.ssm"];

use std;

function<int> main()
{
 println(test());
 return 0;
}
