// Define a base class
class Vehicle {
    private float speed;
    private int numOfWheels;

    public Vehicle(float speed, int numOfWheels) {
        this.speed = speed;
        this.numOfWheels = numOfWheels;
    }

    public void setSpeed(float speed) {
        this.speed = speed;
    }

    public float getSpeed() {
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

    public void move(float s) {
        System.out.println("Vehicle is moving at speed " + s);
    }
}

// Define a derived class
class Car extends Vehicle {
    private String make;
    private String model;

    public Car(float speed, int numOfWheels, String make, String model) {
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

    @Override
    public void move() {
        System.out.println("Car is moving");
    }
}

// Main class
public class Inheritance2 {
    public static void main(String[] args) {
        // Create a Vehicle object
        Vehicle vehicle = new Vehicle(60.0f, 4);
        System.out.println("Speed: " + vehicle.getSpeed());
        System.out.println("Number of wheels: " + vehicle.getNumOfWheels());
        vehicle.move();

        // Create a Car object
        Car car = new Car(100.3f, 4, "Toyota", "Camry");
        System.out.println("Speed: " + car.getSpeed());
        System.out.println("Number of wheels: " + car.getNumOfWheels());
        System.out.println("Make: " + car.getMake());
        System.out.println("Model: " + car.getModel());
        car.move();
        car.move(car.getSpeed()); //calls overloaded method in base class
    }
}
