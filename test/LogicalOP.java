public class LogicalOP {
   public static void main(String[] args) {
        int x = 10, y = 5, z = 7;
        
        // using AND (&&) operator
        if (x > y && x > z) {
            System.out.println("x is the largest number.");
        }
        
        // using OR (||) operator
        if (y > z || x < y) {
            System.out.println("y is greater than z or x is less than y.");
        }
        
        // using NOT (!) operator
        boolean flag = true;
        if (!flag) {
            System.out.println("This will not be printed.");
        } else {
            System.out.println("The flag is true.");
        }
  }
}