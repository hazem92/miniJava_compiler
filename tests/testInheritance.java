package myPackage;

public class A {
  
  public int a = 2;
  private String b = "asdf";
  
  public A() {
    
  }
  
  public int someMethod(int a, String b) {
    int a = 1;
    A b = new B();
    (A) new A();
    new B();
    return a;
  }
  
}

public class B extends A {

  private int c;

  public B() {
    super();
    this();
    this.asd();
    super.someMethod();
    this.someMethod();
    this.c;
    this.a;
  }
  
  private void asd() {}

}
