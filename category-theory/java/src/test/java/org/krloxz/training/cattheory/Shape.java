package org.krloxz.training.cattheory;

/**
 * Example of an Algebraic Data Type written in Java.
 * <p>
 * Notice how factory methods emulate the role of data constructors and virtual methods replace
 * pattern matching.
 * <p>
 * An abstract class is required to properly encapsulate implementations and ensure that only the
 * interface methods are exposed.
 *
 * @author Carlos Gomez
 *
 */
public abstract class Shape {

  public abstract double area();

  public abstract double circumference();

  public static Shape circle(final double radio) {
    return new Circle(radio);
  }

  public static Shape rectangle(final double length, final double width) {
    return new Rectangle(length, width);
  }

  public static Shape square(final double side) {
    return new Square(side);
  }

  private static class Circle extends Shape {

    private final double radio;

    private Circle(final double radio) {
      this.radio = radio;
    }

    @Override
    public double area() {
      return Math.PI * this.radio * this.radio;
    }

    @Override
    public double circumference() {
      return 2 * Math.PI * this.radio;
    }

  }

  private static class Rectangle extends Shape {

    private final double length;
    private final double width;

    private Rectangle(final double length, final double width) {
      this.length = length;
      this.width = width;
    }

    @Override
    public double area() {
      return this.length * this.width;
    }

    @Override
    public double circumference() {
      return 2 * (this.length + this.width);
    }

  }

  private static class Square extends Shape {

    private final double side;

    private Square(final double side) {
      this.side = side;
    }

    @Override
    public double area() {
      return this.side * this.side;
    }

    @Override
    public double circumference() {
      return 4 * this.side;
    }

  }

}
