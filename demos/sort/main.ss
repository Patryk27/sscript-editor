/*
 This is a demo of the quicksort algorithm.
*/

@("stdlib/stdio.ss")
@("stdlib/random.ss")

const size = 10;

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
 @desc: (quick)sorts an array of ints.
*/
function<void> sort(int[] tab, int l, int r)
{
 if (l < r)
 {
  var<int> m = l;
 
  for (var<int> i=l+1; i<=r; i++)
  {
   if (tab[i] < tab[l])
   {
    ++m;

    swap(tab[m], tab[i]);
   }
  }

  swap(tab[l], tab[m]);

  sort(tab, l, m-1);
  sort(tab, m+1, r);
 }
}

function<int> main()
{
 randomize();

 var<int[]> tab = new int[size];

 for (var<int> i=0; i<size; i++)
  tab[i] = random_8();

 display(tab);
 println("------------");

 sort(tab, 0, tab.length()-1);

 display(tab);
}