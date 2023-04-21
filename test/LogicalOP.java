public class LogicalOP {

  public static boolean foo() {
    return false;
  }

  public static void main(String[] args) {
    if(foo()) { 
      int a = 5;
    } else
    {
      int a = 6;
    }
  }
}