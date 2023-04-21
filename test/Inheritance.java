class Person {
  private String name;
  private int age;
  
  public Person(String name, int age) {
    this.name = name;
    this.age = age;
  }
  
  public String getName() {
    return name;
  }
  
  public void setName(String name) {
    this.name = name;
  }
  
  public int getAge() {
    return age;
  }
  
  public void setAge(int age) {
    this.age = age;
  }
}

// class Student extends Person {
//   private int grade;
  
//   public Student(String name, int age, int grade) {
//     super(name, age);
//     this.grade = grade;
//   }
  
//   public int getGrade() {
//     return grade;
//   }
  
//   public void setGrade(int grade) {
//     this.grade = grade;
//   }
  
//   public void increaseGrade() {
//     grade++;
//   }
  
//   public void decreaseGrade() {
//     grade--;
//   }
// }

public class Inheritance {
  public static void main(String[] args) {
    Person myStudent = new Person("John", 16);
    myStudent.getAge();
    // Student myStudent = new Student("John", 16, 9);
    // int currentGrade = myStudent.getGrade();
    // System.out.println(myStudent.getName() + " is in grade " + currentGrade);
    
    // myStudent.increaseGrade();
    // int newGrade = myStudent.getGrade();
    // System.out.println("After promotion, " + myStudent.getName() + " is now in grade " + newGrade);
    
    // myStudent.decreaseGrade();
    // newGrade = myStudent.getGrade();
    // System.out.println("After demotion, " + myStudent.getName() + " is now in grade " + newGrade);
  }
}
