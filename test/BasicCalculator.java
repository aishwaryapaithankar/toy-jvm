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
  public static float div(int num1,int num2) {
	  return ((float)num1/num2);
  }
  public static void main(String[] args) {
    float ans = div(mul(3,add(4,7)),sub(9,1));
    System.out.println("Ans is "+ ans);
  }
}
