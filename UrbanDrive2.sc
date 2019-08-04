case class User(firstName: String, lastName: String, custType: String)
case class Location(latitue: Double = 0.00, longitude: Double = 0.00)
case class Cab(typ: String, totalSeats: Int, seatsAvlbl: Int, currentLocation: Location, finalDestination: Location){
  def distanceFromHere(here: Location): Double = 0.00
  def timeToDrop(here: Location): Long = 0
  def isOnPromotion: Boolean = false

}
case class Source() extends Location
case class Destination() extends Location

case class CabRequest(source: Source, destination: Destination, service: String, seats: Int)

type Filter = List[Cab] => List[Cab]

/*  a non trivial process of fetching cabs from, say, in memory DB based on source
and destination
* */
def matchRoutes(source: Source, destination: Destination): Filter = {
  //retrieve cabs
  cabs: List[Cab] => cabs.filter(c => c.finalDestination == destination && isWithinRange(c.currentLocation, source))
    def isWithinRange(cabLoc:Location, myLoc: Location) = true
}

/* a simple filter to filter cabs based on service type - pool, premier, etc,
* */
def matchClass(typName: String): Filter = {
  cabs: List[Cab] => cabs.filter(_.typ.equalsIgnoreCase(typName))
}

def matchPromotionCabs: Filter = {
  cabs: List[Cab] => cabs.filter(_.isOnPromotion)
}

/*  a long laborious process of matching the best cabs based on multitude of factors
*  such as driver ratings, user ratings, co-passenger ratings, traffic conditions, etc
* */
def matchSeats(numOfSeats: Int): Filter = {
  cabs: List[Cab] => cabs.filter(_.seatsAvlbl == numOfSeats)
}

def matchDistanceToPickUp(here: Location, dis: Double): Filter = {
  cabs: List[Cab] => cabs.filter(_.distanceFromHere(here) == dis)
}

def matchTimeToDrop(here: Location, time: Int): Filter = {
  cabs: List[Cab] => cabs.filter(_.timeToDrop(here) <= time)
}

val source = new Source()
val destination = new Destination()
val location = Location()

def applyFirstLeftThenRight(f: Filter, g:Filter): Filter = {
  cabs: List[Cab] => g(f(cabs))
}

/* craft any number of composite functions and pass it to client */

object CabServiceAsSetOfFunctions{

  def boxOnWheels(start: Source, end: Destination) = List(matchRoutes(start, end))
  def matchConvinience:List[Filter]  = List[Filter](matchDistanceToPickUp(location, 4.00), matchTimeToDrop(location, 60))
  def getBasic(start:Source, end: Destination):List[Filter] = {
    List[Filter](matchRoutes(start, end), matchClass("basic"))
  }
  def getPool(start:Source, end: Destination)(seats: Int):List[Filter]  = List[Filter](matchRoutes(start, end), matchClass("pool"), matchSeats(seats))
  def getPremier(start:Source, end: Destination):List[Filter]  = List[Filter](matchRoutes(start, end), matchClass("premier"))
  def getPremierTravel(start:Source, end: Destination):List[Filter]  = getPremier(start, end) ++ List(matchClass("premier"))
  def getEconomyCommute(start:Source, end: Destination):List[Filter]  = getPool(start, end)(1) ++ matchConvinience
  def getPromotionalTrips(start:Source, end: Destination):List[Filter]  = getPool(start, end)(1) ++ List(matchPromotionCabs)

}

object SmartOccupancyManager{

  import CabServiceAsSetOfFunctions._

  val cabsFromDB = List[Cab](new Cab("pool",4,2, Location(),Location()),
    new Cab("go", 4, 4, Location(), Location()), new Cab("premier", 6, 6, Location(), Location()))

  def getCabs(start:Source, end: Destination, user: User, request: CabRequest): List[Cab] = {
    val matchFunction = request match {
      case (start, end, _, "SomeFancyNameForBasic") => getBasic(start, end)
      case (start, end, seats, "ShareMyCab") => getPool(start, end)(seats)
      case (start, end, _, "OffersOnWheels") => getPromotionalTrips(start, end)
      case (start, end, seats, "MyDailyCommute") => getEconomyCommute(start, end)
      case _ => boxOnWheels(request.source, request.destination)
    }
    matchFunction.reduce(_ andThen _)(cabsFromDB)
  }
}
