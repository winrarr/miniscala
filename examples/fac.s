{
  def fac(n: Int): Int =
    if (n == 0) 1
    else n * fac(n - 1);
  fac(5)
}
