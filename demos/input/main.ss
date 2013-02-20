@("stdlib\\stdio.ss")

use std;

function<int> main()
{
 var<string> name = read_string_t("Enter your name: ");
 println("Hello, "+name+"!");

 return 0;
}
