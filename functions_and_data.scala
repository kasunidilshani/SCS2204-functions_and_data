object functions_and_data extends App{

  class rational(x: Int, y: Int) {
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    def numer = x / gcd(x, y)
    def denom = y / gcd(x, y)

    def +(r:rational) = new rational(numer * r.denom + r.numer * denom, denom * r.denom)

    override def toString= numer+ "/" + denom

    def neg = new rational(-this.numer,this.denom)

    def - (r:rational)= this + r.neg


  }

  //Question1

  val x= new rational(3,4)
  val y= new rational(5,8)
  val z= new rational(2,7)

  println("Question1")
  println(x.neg)
  
  

  //Question2

  println("\nQuestion2")
  println(x-y-z)


  //Question3

  class Account (id:String,n: Int, b: Double) {
    val nic:String=id
    val acnumber: Int = n
    var balance: Double = b

    override def toString = "["+nic+":"+acnumber +":"+ balance+"]"

    def withdraw(a:Double) = this.balance=this.balance-a

    def deposit(a:Double) = this.balance=this.balance+a

    def transfer(a:Account,c:Double): Unit ={
      this. withdraw(c)
      a.deposit(c)
    }
  }

  val k= new Account("987611421V",5011,11650.50)
  val l= new Account("987611438V",8955,50000)
  k.transfer(l,500)


  println("\nQuestion3")
  println(k.balance)
  println(l.balance)



  //Question4

  println("\nQuestion4")

  val m= new Account("957611421V",5011,-10)
  val n= new Account("587611438V",8955,-50000)

  var  bank:List[Account]= List(k,l,m,n)

  val find=(n:String)=> bank.filter(x=>x.nic.equals(n))


  val overdraft= bank.filter(x=>x.balance<0)
  println("Overdraft accounts : " +overdraft)

  val total=bank.map(x=>x.balance).reduce((x,y)=>x+y)
  println("Total Balances of all accounts : " +total)

  bank.map(x=> if(x.balance>0) x.balance= x.balance *1.05 else x.balance= x.balance *1.1)
  println("Interests added: " +bank)


}
