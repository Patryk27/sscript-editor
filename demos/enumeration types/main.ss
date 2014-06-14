/*
 This is a simple enumeration-types demo.
*/

@("stdlib/stdio.ss")

use std;

type<enum> state = [stFirst, stSecond];

function print_enum(const state value)
{
 if (value == stFirst)
  println("First"); else
  println("Second");
}

function<int> main()
{
 var<state> st;

 st = stFirst;
 print_enum(st);

 st = stSecond;
 print_enum(st);

 return 0;
}
