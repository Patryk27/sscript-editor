function<string> func1()
{
 return "Hello";
}

function<string> func2()
{
 return "World";
}

public function<string> test()
{
 return func1()+" "+func2()+" from a Library! :)";
}
