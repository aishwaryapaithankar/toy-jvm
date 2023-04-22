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

class Student extends Person {
  private int grade;
  
  public Student(String name, int age, int grade) {
    super(name, age);
    this.grade = grade;
  }
  
  public int getGrade() {
    return grade;
  }
  
  public void setGrade(int grade) {
    this.grade = grade;
  }
  
  public void increaseGrade() {
    grade++;
  }
  
  public void decreaseGrade() {
    grade--;
  }
}

public class Inheritance {
  public static void main(String[] args) {
    Student student1 = new Student("John", 16, 9);
    Student student2 = new Student("Amy", 13, 6);
    int currentGrade = student1.getGrade();

    System.out.println(student1.getName() + " is in grade " + currentGrade);
    
    student1.increaseGrade();
    int newGrade = student1.getGrade();
    System.out.println("After promotion, " + student1.getName() + " is now in grade " + newGrade);
    
    student1.decreaseGrade();
    newGrade = student1.getGrade();
    System.out.println("After demotion, " + student1.getName() + " is now in grade " + newGrade);

    System.out.println(student2.getName() + " is in grade " + student2.getGrade());
  }
}
