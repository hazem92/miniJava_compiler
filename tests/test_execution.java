package tuto;

 class B {
  // attributes
  int account = 0;
  int id = 0 ;
  String name = "";

  // constructors
  public B () {}
  public B (String name) { this.name = name ;}
  public B (int id,int account) {this.id = id ; this.account = account ;}

  // methods
  int set_id (int id)  {this.id = id ;}
  int get_id () { return this.id ;}

  String get_name () { return this.name;}

  String put_money (int a)  {this.account = (this.account+a) ; return "added some " ;}
  String get_money (int a) {
    if ( (this.account - a) > 0) {this.account = (this.account-a) ;
       return "done retrieve" ;} else {return "not enough money" ;}
      }
}


 class Main {
   String main () {

     B x = new B ("X") ;
     B y = new B ("Y") ;

     for (int i =0; i < 10 ; i=(i+1) ) { x.put_money(1) ; }
     System_out.print ("+++++++","the client ",x.name , "has" , x.account,"+++++++") ;

     //int some ;
     //some = ( (1 + 1 ) -1 );

     while ( y.account < 10 ) {y.put_money(1);}
     System_out.print ( "+++++++" ,y.get_money (1000000) ,"+++++++") ;

     B y_child = new B ("Y_child") ;
     y_child.account = y.account ;
     System_out.print ("+++++++","the client ",y_child.name , "has" , y_child.account,"+++++++") ;


   }
 }
