package practice

object Exercise extends App {
  println(" ========== Exercise 1 ")
  /**
   * Exercise 1
   * Data model of a credit card
   */
  sealed trait CreditCardType
  case object Visa extends CreditCardType
  case object Master extends CreditCardType
  case object Discover extends CreditCardType
  println(" ========== Exercise 2 ")
  /**
   * Exercise 2
   * Data model of a Paint Application
   */
  sealed trait Paint
  case class Canvas(background:Color,shapes: Array[Shapes]) extends Paint
  case class Point(x:Int,y:Int) extends Paint
  case class Color() extends Paint

  sealed trait Shapes extends Paint
  case class Rectangle(startingPoint:Point,length:Int, breadth:Int, color:Color) extends Shapes
  case class Circle(startingPoint:Point,radius:Int,color:Color) extends Shapes
  case class FreeFormDrawing(points:Array[Point],color:Color) extends Shapes
  println(" ========== Exercise 4 ")
  /**
   * Exercise 4
   * Pattern match on an expression to print what type of value it returns
   */
  def printTypes(printMyType: Any): Unit =
    printMyType match {
      case i:Int => println("Integer.")
      case f:Float => println("Float.")
      case d:Double => println("Double.")
      case str:String => println("String.")
      case _ => println("Unknown Type.")
    }

  printTypes(10)
  printTypes("Hello World")
  printTypes(10.01)

  println(" ========== Exercise 5 ")
  /**
    * Exercise 5
    * Pattern matching with regular expression
    * Note : uncomment and implement
    */
  val date = raw"""(\d{4})-(\d{2})-(\d{2})""".r

  def getDateType(dateType: String): String = dateType match {
    case date(_*) => "It's a date!"
    case _ => "It's not a valid date"
  }
  println(getDateType("2004-01-20"))
  println(getDateType("204-01-20"))

  def getYear(dateType: String): String = dateType match {
    case date(year, month, day) => s"$year was a good year for PLs."
  }
  println(getYear("2004-01-20"))
  println(getYear("2020-01-20"))
  println(" ========== Exercise 6 ")
  /**
    * Exercise 6
    * Pattern match on an Option inside either
    */
  type Input = Either[String, Option[String]]

  def getStringFrom(input:Input) : String = input match {
    case Left(_) => "Got String"
    case Right(Some(value)) => "Got Option with String"
    case Right(None) => "Got Empty Option"
  }

  println(getStringFrom(Left("Good Morning")))
  println(getStringFrom(Right(Option("Good Morning"))))
  println(getStringFrom(Right(Option.empty)))
  println(" ========== Exercise 7 ")
  /**
    * Exercise 7
    * Use Pattern match on tuple for below result
    */

  def computeTuple(t:(Int, Int)): Option[Int] = t match {
    case (a,b) if (a == b)  => Some(a+b)
    case (a,b) if (a < b)   => Some(a)
    case _ => None
  }

  println(computeTuple((1,2)))
  println(computeTuple((4,4)))
  println(computeTuple((7,4)))

  println(" ========== Exercise 8 ")
  /**
    * Exercise 8
    *
    * Pattern match on multiple object containing a domain
    */
  case class Address(
                      street: String,
                      city: String,
                      postalCode: String,
                      state: String
                    )
  sealed trait PhoneType
  case object HomePhone extends PhoneType
  case object WorkPhone extends PhoneType
  case object Other extends PhoneType

  case class Person(
                     firstName: String,
                     lastName: String,
                     address: Address,
                     phoneType: PhoneType
                   )
  def checkIfPersonHasWorkPhone(person: Person): Boolean = person.phoneType match {
    case WorkPhone => true
    case _ => false
  }

  def checkIfPersonAreFromGivenCity(person: Person, city: String): Boolean = city match {
    case person.address.city => true
    case _ => false
  }

  var personFromChicago = new Person("Alex","Adams",
                  new Address("Adam Street","Chicago","62109","IL"),WorkPhone)
  var personFromBoston = new Person("Bob","Adams",
                  new Address("Adam Street","Boston","42109","MA"),HomePhone)

  println(personFromChicago + " has Work Phone: " + checkIfPersonHasWorkPhone(personFromChicago))
  println(personFromBoston + " has Work Phone: " + checkIfPersonHasWorkPhone(personFromBoston))
  println(personFromChicago + " is From Chicago: " + checkIfPersonAreFromGivenCity(personFromChicago,"Chicago"))
  println(personFromBoston + " is From Chicago: " + checkIfPersonAreFromGivenCity(personFromBoston,"Chicago"))

  sealed trait Errors
  case object InvalidAddress extends Errors
  case object InvalidPerson extends Errors
  //helper functions
  def emptyString: String => Boolean = ???

  def emptyStringCheck(str: String):Boolean = str match {
    case "" => true
    case null => true
    case _ => false
  }

  println(emptyStringCheck("Alex"));
  println(emptyStringCheck(""));
  println(emptyStringCheck(null));
  //Lets make some smart constructors
  def makeAddress(address: Address): Either[Errors, Address] = address match {
    case address if(emptyStringCheck(address.street))  => Left(InvalidAddress)
    case address if(emptyStringCheck(address.city))  => Left(InvalidAddress)
    case address if(emptyStringCheck(address.postalCode))  => Left(InvalidAddress)
    case address if(emptyStringCheck(address.state))  => Left(InvalidAddress)
    case _  => Right(address)
  }

  def makePerson(person: Person): Either[Errors, Person] = person match {
    case person if makeAddress(person.address).isLeft  => Left(InvalidPerson)
    case person if emptyStringCheck(person.firstName) => Left(InvalidPerson)
    case person if emptyStringCheck(person.lastName) => Left(InvalidPerson)
    case _ => Right(person)
  }

  //Testing, create a sample person and tryout the person smart constructor
  var addresValid = makeAddress(new Address("Adam Street","Boston","42109","MA"))
  var addresInvalidStreet = makeAddress(new Address("","Boston","42109","MA"))
  var addresInvalidCity = makeAddress(new Address("Adam Street",null,"42109","MA"))

  println("addresValid:"+addresValid)
  println("addresInvalidStreet:"+addresInvalidStreet)
  println("addresInvalidCity:"+addresInvalidCity)

  var validPerson = makePerson(new Person("Alex","Adams",
    new Address("Adam Street","Chicago","62109","IL"),WorkPhone))

  var personInvalidAddress = makePerson(new Person("Alex","Adams",
    new Address("","Chicago","62109","IL"),WorkPhone))

  var invalidPerson = makePerson(new Person("Alex",null,
    new Address("Adam Street","Chicago","62109","IL"),WorkPhone))

  println("validPerson:"+validPerson)
  println("personInvalidAddress:"+personInvalidAddress)
  println("invalidPerson:"+invalidPerson)

}
