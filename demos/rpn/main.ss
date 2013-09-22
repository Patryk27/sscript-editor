/*
 This is demo of Reverse Polish Notation (RPN).
 RPN is used to calculate values of mathematical expressions (like "2+2*2").
 Current implementation doesn't support parentheses, unary "-" operator and
 works on ints only.
*/

@("stdlib/stdio.ss")
@("stdlib/string.ss")
@("stdlib/numbers.ss")

use std;

/*
 @name: is_operator
 @desc: returns "true" if passed char is an operator (+ - * /).
*/
function<bool> is_operator(const char op)
{
 return (op == '+') || (op == '-') || (op == '*') || (op == '/');
}

/*
 @name: is_left_assoc
 @desc: returns "true" if passed operator is left-associative.
*/
function<bool> is_left_assoc(const char op)
{
 return (op == '+') || (op == '-') || (op == '*') || (op == '/');
}

/*
 @name: is_right_assoc
 @desc: returns "true" if passed operator is right-associative. As there aren't any right-associative operators implemented, it always returns "false".
*/
function<bool> is_right_assoc(const char op)
{
 return false;
}

/*
 @name: get_order
 @desc: returns order of specified operator.
*/
function<int> get_order(const char op)
{
 if ((op == '+') || (op == '-'))
  return 1;

 if ((op == '*') || (op == '/'))
  return 2;

 return 0;
}

/*
 @name: calc_rpn
 @desc: returns RPN form of an expression.
*/
function<string> calc_rpn(const string expr)
{
 var<string> result = "";
 var<char[]> stack = new char[512]; // allocate operator stack
 var<int> stack_pos = 0;

 var<int> pos = 1, length = strlen(expr);

 while (pos <= length)
 {
  /* number */
  if (isdigit(expr[pos]))
  {
   while (isdigit(expr[pos])) // read whole number
   {
    result += expr[pos];
    pos++;

    if (pos > length)
     break;
   }

   result += " ";
  } else

  /* operator */
  if (is_operator(expr[pos]))
  {
   var<char> op = expr[pos];

   if (stack_pos > -1)
   {
    while ((is_left_assoc(op) && (get_order(op) <= get_order(stack[stack_pos]))) ||
           (is_right_assoc(op) && (get_order(op) < get_order(stack[stack_pos]))))
    {
     result += stack[stack_pos--]+" ";

     if (stack_pos <= 0)
      break;
    }
   }

   stack[++stack_pos] = op;

   pos++;
  } else

   throw "Unexpected symbol: "+expr[pos];
 }

 while (stack_pos > -1) // read until there're no operators on the stack
 {
  result += stack[stack_pos]+" ";
  stack_pos--;
 }

 return result;
}

/*
 @name: eval_rpn
 @desc: returns value of RPN expression.
*/
function<int> eval_rpn(const string expr)
{
 var<int[]> stack = new int[512]; // allocate stack
 var<int> stack_pos = 0, length = strlen(expr), pos = 1;

 while (pos <= length)
 {
  /* number */
  if (isdigit(expr[pos]))
  {
   var<string> number = "";

   while (isdigit(expr[pos])) // read whole number
   {
    number += expr[pos];
    pos++;
   }

   stack[++stack_pos] = strint(number);
  } else

  /* operator */
  if (is_operator(expr[pos]))
  {
   var<char> op = expr[pos];

   if (stack_pos < 2)
    throw "Unexpected operator: "+op+" (expecting operands before)";

   var<int> val_b = stack[stack_pos--],
            val_a = stack[stack_pos--],
            result = 0;

   if (op == '+')
    result = val_a + val_b; else

   if (op == '-')
    result = val_a - val_b; else

   if (op == '*')
    result = val_a * val_b; else

   if (op == '/')
    result = val_a / val_b; else

    throw "Invalid operator: "+op;

   stack[++stack_pos] = result;

   pos++;
  } else

  /* space */
  if (expr[pos] == ' ')
  {
   pos++;
  } else

   throw "Unexpected char: "+expr[pos];
 }

 if (stack_pos != 1)
  throw "Something went wrong! Only one element at the stack was expected.";

 return stack[stack_pos];
}

function<int> main()
{
 var<string> expr = read_string_t("Enter expression to calculate (like \"2+2*2\", no parentheses!): ");

 var<string> rpn = calc_rpn(expr);
 var<int> result = eval_rpn(rpn);

 println("Expression = "+expr);
 println("RPN form   = "+rpn);
 println("Result     = "+intstr(result));

 return 0;
}
