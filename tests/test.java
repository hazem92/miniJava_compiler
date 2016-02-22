package tuto;

 class B {
  int a = 0;
  int b = 0 ;
  String r (int c, String b)  {
    this.a = c ;
    return (this.a+100) ;
  }
}

 class Main {
   String main () {

     int ab = 2+2 ;


     B b = new B () ;
     b.a = (2+1);
     b.a = b.b ;
     b.b = b.r(555," hh") ;

     if ( 1 == 1) ab = 88999999; else ab = 1; 


   }
 }
