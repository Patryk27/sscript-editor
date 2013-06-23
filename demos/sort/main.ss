@("stdlib/stdio.ss")

use std;

function<void> display(int[] tab)
{
 for (var<int> i=0; i<tab.length(); i++)
  println(tab[i]);
}

function<void> swap(var int a, var int b)
{
 var<int> tmp = a;
 a = b;
 b = tmp;
}

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
 var<int[]> tab = new int[6];

 tab[0] = 10;
 tab[1] = 3;
 tab[2] = 14;
 tab[3] = 20;
 tab[4] = 9;
 tab[5] = 0;

 display(tab);
 println("------------");
 sort(tab, 0, tab.length()-1);
 display(tab);
}
