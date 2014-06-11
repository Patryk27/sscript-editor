/*
 This is demo of SScript libraries.
*/

function<string> func1() // by default, every function is "private" (not visible outside this module and/or library)
{
 return "Hello";
}

function<string> func2()
{
 return "World";
}

public function<string> test() // only this function will be able to be called by a programmer when we compile this library, because this is the only one marked as "public"
{
 return func1()+" "+func2()+" from a Library! :)";
}
