def foo(a b) a*a + 2*a*b + b*b;
def bar(a) foo(a, foo(4.0, 1));
def f() for y = 0, y+1 < 100, 1 in (y);
bar(31337);
# extern cos(x);
# for y = ymin, y < ymax, ystep in (1);
  