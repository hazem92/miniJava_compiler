package myPackage;

public class A {

  private int a = 2;
  private String b = "asdf";

  public A() {
  }

  public int someMethod(int a, String b) {
    B b;
    b.doSomething();
    //Math.bla();
    getFloat();
    this.someMethod(1, "adsf");
  }

  public float getFloat() { return 1; }

}

public class B extends A{

  public void doSomething() {
    int a = 1;
  }

}

public class Main {

  public static void main () {}
}
