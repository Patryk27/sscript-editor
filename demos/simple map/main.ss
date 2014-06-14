/*
 This is a simple demo showing array.resize() and array.length() features.
*/

@("stdlib/stdio.ss")

type<char[]> Map;

/*
 @name: map_create
 @desc: creates and returns an empty map
*/
function<Map> map_create()
{
 return new char[0];
}

/*
 @name: map_add
 @desc: adds a new element into the map
*/
function map_add(Map map, char value)
{
 map.resize(map.length()+1);

 map[map.length()-1] = value;
}

/*
 @name: map_get
 @desc: returns ID of element on the map
*/
function<int> map_get(Map map, char value)
{
 for (var<int> i=0; i<map.length(); i++)
 {
  if (map[i] == value)
   return i;
 }

 return -1;
}

/*
 @name: main
*/
function<int> main()
{
 use std;

 var<Map> alphabet = map_create();

 map_add(alphabet, 'a');
 map_add(alphabet, 'b');
 map_add(alphabet, 'c');

 println(map_get(alphabet, 'c')); // 2
 println(map_get(alphabet, 'a')); // 0
 println(map_get(alphabet, 'b')); // 1

 return 0;
}
