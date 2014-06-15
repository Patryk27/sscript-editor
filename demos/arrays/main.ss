/*
 This is demo of arrays in SScript.
 It basically "explodes" (splits into tokens) by space (' ') string entered by user and displays it.
*/

@("stdlib/stdio.ss")
@("stdlib/string.ss")

use std;

function<int> main()
{
 var<string> input = read_string_t("Input some words separated by a space: ");
 var<string[]> words = strexplode(input, ' ');

 println("You entered:");
 foreach (var<string> word in words)
  println(word);

 return 0;
}
