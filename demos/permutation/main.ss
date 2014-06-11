/*
 This demo displays permuatations of set: 'a', 'b', 'c'.
*/

@("stdlib/stdio.ss")

use std;

function<void> permut(string[] tab, int n, int k)
{
 var<string> tmp = tab[k];

 if (k == n)
 {
  for (var<int> i=0; i<tab.length(); i++)
   print(tab[i]+" ");
  newline();
 } else
 {
  for (var<int> i=k; i<=n; i++)
  {
   tab[k] = tab[i];
   tab[i] = tmp;
   permut(tab, n, k+1);
   tab[i] = tab[k];
   tab[k] = tmp;
  }
 }
}

function<int> main()
{
 var<string[]> tab("a", "b", "c");

 permut(tab, tab.length()-1, 0);
}
