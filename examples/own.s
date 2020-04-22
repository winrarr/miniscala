{
    val fac2 = (f, n) => if (n == 0) 1 else n * f(f, n - 1);
    val fac = (n) => fac2(fac2, n);
    fac(3)
 }