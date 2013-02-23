@("stdlib\\stdio.ss")
@("stdlib\\string.ss")

use std;

function<int> main()
{
 var<string> input = read_string_t("Input some words separated by a space: ");
 var<string[]> words = strexplode(input, ' ');

 println("You entered:");
 for (var<int> i=0; i<array_length(words); i++)
  println(words[i]);

 delete words;

 return 0;
}
