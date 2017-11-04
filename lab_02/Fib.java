class Fib {
  public static void main(String[] args) {
    System.out.println(fibR(10));
  }

  static int fibR(int n) {
    if (n <= 2) return 1;
    return fibR(n-1) + fibR(n-2);
  }
}
