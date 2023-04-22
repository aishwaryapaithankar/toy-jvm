public class BasicCalculator {
  public static int add(int num1,int num2) {
	  return num1+num2;
  }
  public static int sub(int num1,int num2) {
	  return num1-num2;
  }
  public static int mul(int num1,int num2) {
	  return num1*num2;
  }
  public static int div(int num1,int num2) {
	  return num1/num2;
  }
  public static void main(String[] args) {
    int ans = div(mul(2,add(1,2)),sub(5,3));
    System.out.println("Ans is "+ ans);
  }
}
