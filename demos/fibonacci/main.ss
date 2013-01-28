@("stdlib\\stdio.ss")

// I used 'float' in code, because it is more accurate than normal 'int'
function<float> fib(int n)
{
 var<float> a=1, b=1, c;

 if (n <= 0)
  return 0;

 if (n <= 2)
  return n;

 for (var<float> i=3; i<=n; i++)
 {
  c = a+b;
  a = b;
  b = c;
 }

 return c;
}

function<int> main()
{
 var<int> n = read_int_t("Which number calculate? ");

 print("fibonacci(");
 print(n);
 print(") = ");
 print(fib(n));
}
