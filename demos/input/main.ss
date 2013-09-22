/*
 Demo of SScript's input.
*/

@("stdlib\\stdio.ss")
@("stdlib\\numbers.ss")

use std;

function<int> main()
{
 var<string> name = read_string_t("Enter your name: ");
 var<int> age = read_int_t("How old are you? ");

 println("Hello, "+intstr(age)+"-old "+name+"! :)");

 return 0;
}
