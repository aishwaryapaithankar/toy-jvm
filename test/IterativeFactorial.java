public class IterativeFactorial {
  public static int fact(int n) {
    int ans = 1;
    while (n > 0) {
      ans *= n--;
    }
    return ans;
  }
  public static void main(String[] args) {
    fact(4);
  }
}
