// Define a base class
class Vehicle {
    private int speed;
    private int numOfWheels;

    public Vehicle(int speed, int numOfWheels) {
        this.speed = speed;
        this.numOfWheels = numOfWheels;
    }

    public void setSpeed(int speed) {
        this.speed = speed;
    }

    public int getSpeed() {
        return this.speed;
    }

    public void setNumOfWheels(int numOfWheels) {
        this.numOfWheels = numOfWheels;
    }

    public int getNumOfWheels() {
        return this.numOfWheels;
    }

    public void move() {
        System.out.println("Vehicle is moving");
    }
}

// Define a derived class
class Car extends Vehicle {
    private String make;
    private String model;

    public Car(int speed, int numOfWheels, String make, String model) {
        super(speed, numOfWheels);
        this.make = make;
        this.model = model;
    }

    public void setMake(String make) {
        this.make = make;
    }

    public String getMake() {
        return this.make;
    }

    public void setModel(String model) {
        this.model = model;
    }

    public String getModel() {
        return this.model;
    }

    public void move() {
        System.out.println("Car is moving");
    }
}

// Main class
public class Inheritance2 {
    public static void main(String[] args) {
        // Create a Vehicle object
        Vehicle vehicle = new Vehicle(60, 4);
        System.out.println("Speed: " + vehicle.getSpeed());
        System.out.println("Number of wheels: " + vehicle.getNumOfWheels());
        vehicle.move();

        // Create a Car object
        Car car = new Car(100, 4, "Toyota", "Camry");
        System.out.println("Speed: " + car.getSpeed());
        System.out.println("Number of wheels: " + car.getNumOfWheels());
        System.out.println("Make: " + car.getMake());
        System.out.println("Model: " + car.getModel());
        car.move(); //calls overloaded method
    }
}