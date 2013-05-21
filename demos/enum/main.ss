@("stdlib\\stdio.ss")

use std;

type<enum> state = (stFirst, stSecond);

function<int> main()
{
 var<state> st = stFirst;

 if (st == stFirst)
  println("First"); else
  println("Second");

 return 0;
}
