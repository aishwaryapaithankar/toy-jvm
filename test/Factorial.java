public class Factorial {
  public static int fact(int n) {
    if (n==1) return 1;
    return n * fact (n-1);
  }
  public static void main(String[] args) {
    System.out.println("Factorial of 4 is " + fact(4));
    System.out.println("Factorial of 8 is " + fact(8));
  }
}
