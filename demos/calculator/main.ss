/*
 This is a really simple floating-point calculator.
 It can add, subtract, multiply and divide two numbers entered by user.
*/

@("stdlib\\stdio.ss")
@("stdlib\\numbers.ss")

use std;

function<int> main()
{
 while (true)
 {
  var<char> ch = read_char_t("Enter operation (+ - * / e): ");
  newline();

  if (ch == 'e')
   break;

  var<float> n1 = read_float_t("Enter first number: "),
             n2 = read_float_t("Enter second number: "),
             result = 0;

  if (ch == '+')
  {
   result = n1+n2;
  } else

  if (ch == '-')
  {
   result = n1-n2;
  } else

  if (ch == '*')
  {
   result = n1*n2;
  } else

  if (ch == '/')
  {
   if (n2 == 0)
    println("You cannot divide by 0!"); else
    result = n1/n2;
  } else

  {
   println("Unknown operation: "+ch);
   newline();
   continue;
  }

  println("Result: "+fltstr(result));
  newline();
 }

 println("Exited.");

 return 0;
}
