public class IterativeFactorial {
  public static int fact(int n) {
    int ans = 1;
    while (n > 0) {
      ans *= n--;
    }
    return ans;
  }
  public static void main(String[] args) {
     System.out.println("Factorial of 4 is " + fact(4));
  }
}
