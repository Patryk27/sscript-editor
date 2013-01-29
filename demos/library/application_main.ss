@("stdlib\\stdio.ss")

function<string> test() in "library.ssm";

function<int> main()
{
 println(test());
 return 0;
}
