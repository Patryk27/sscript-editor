/*
 This is demo of quicksort.
*/

@("stdlib/stdio.ss")

use std;

/*
 @name: display
 @desc: displays an int array.
*/
function<void> display(int[] tab)
{
 for (var<int> i=0; i<tab.length(); i++)
  println(tab[i]);
}

/*
 @name: swap
 @desc: swaps two numbers.
*/
function<void> swap(var int a, var int b)
{
 var<int> tmp = a;
 a = b;
 b = tmp;
}

/*
 @name: sort
 @desc: sorts an array of ints.
*/
function<void> sort(int[] tab, int l, int r)
{
 if (l < r)
 {
  var<int> m = l;
  for (var<int> i=l+1; i<=r; i++)
   if (tab[i] < tab[l])
   {
    ++m;

    swap(tab[m], tab[i]);
   }

  swap(tab[l], tab[m]);

  sort(tab, l, m-1);
  sort(tab, m+1, r);
 }
}

function<int> main()
{
 var<int[]> tab(10, 3, 14, 20, 9, 0);

 display(tab);
 println("------------");
 sort(tab, 0, tab.length()-1);
 display(tab);
}