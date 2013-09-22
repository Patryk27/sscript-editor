/*
 Mandelbrot fractal demo.
 It renders The Mandelbrot set.
*/

@("stdlib\\stdio.ss")

use std;

function<int> main()
{
 set_size(141, 54);

 const w1=140, h1=52, recen=-2.2, imcen=0.15, r=2.28, w2=24, h2=31, s=2*r/w1;
 var<int> x, y;
 var<float> rec, imc, re, im, re2, im2;

 set_buffered(true);
 for (y=0; y<=h1; y++)
 {
  imc = s*(y-h2)+imcen;
  for (x=0; x<=w1; x++)
  {
   var<int> color = 150;
   rec = s*(x-w2)+recen;
   re = rec;
   im = imc;
   re2 = re*re;
   im2 = im*im;
   while (((re2+im2)<1000000) && (color>0))
   {
    im  = re*im*2+imc;
    re  = re2-im2+rec;
    re2 = re*re;
    im2 = im*im;
    color--;
   }
   print(" .:,;!/>)|&IH%*#"[color%16+1]);
  }

  flush();
 }
}
