import CabService._
import OccupancyManager.cabsFromDB

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

/*  a non trivial process of fetching cabs from, say, in memory DB based on source
and destination
* */
def matchRoutes(source: Source, destination: Destination): List[Cab] => List[Cab] = {
  //retrieve cabs
  cabs: List[Cab] => cabs.filter(c => c.finalDestination == destination && isWithinRange(c.currentLocation, source))
    def isWithinRange(cabLoc:Location, myLoc: Location) = true
}

/* a simple filter to filter cabs based on service type - pool, premier, etc,
* */
def matchClass(typName: String): List[Cab] => List[Cab] = {
  cabs: List[Cab] => cabs.filter(_.typ.equalsIgnoreCase(typName))
}

def matchPromotionCabs: List[Cab] => List[Cab] = {
  cabs: List[Cab] => cabs.filter(_.isOnPromotion)
}

/*  a long laborious process of matching the best cabs based on multitude of factors
*  such as driver ratings, user ratings, co-passenger ratings, traffic conditions, etc
* */
def matchSeats(numOfSeats: Int): List[Cab] => List[Cab] = {
  cabs: List[Cab] => cabs.filter(_.seatsAvlbl == numOfSeats)
}

def matchDistanceToPickUp(here: Location, dis: Double): List[Cab] => List[Cab] = {
  cabs: List[Cab] => cabs.filter(_.distanceFromHere(here) == dis)
}

def matchTimeToDrop(here: Location, time: Int):List[Cab] => List[Cab]= {
  cabs: List[Cab] => cabs.filter(_.timeToDrop(here) <= time)
}

val source = new Source()
val destination = new Destination()
val location = Location()


//val matchRouteAndClass = matchRoutes(source, destination) andThen matchClass("pool")
//val fullMatch  = matchRoutes(source, destination) andThen matchClass("pool") andThen matchSeats(2) andThen matchDistanceToPickUp(location, 4.00) andThen matchTimeToDrop(location, 3600)
//val matchOccupancy = matchClass("pool") andThen matchSeats(2)
//val fullMatch1 = matchRoutes(source, destination) andThen matchOccupancy
//val fullMatch2 = matchRouteAndClass andThen matchSeats(2)

/* craft any number of composite functions and pass it to client */

object CabService{

  def boxOnWheels(start: Source, end: Destination) = matchRoutes(start, end)

  /* predfined recipe for implementing abstract notion of convinience; in practice this definition will created
  * throguh  a complex process depending on local conditions. For e.g. time to drop could be 30 mins
  * for Bangalore while 30 mins for San Fransisco. */
  def matchConvinience = matchDistanceToPickUp(location, 4.00) andThen matchTimeToDrop(location, 60)

  /* more abtstract constructs have been defined based on underlying ones; however the client is not aware of
* how these constructs are composed to generate final outcomes */
  def getBasic(start:Source, end: Destination) = matchRoutes(start, end) andThen matchClass("basic")
  def getPool(start:Source, end: Destination)(seats: Int) = matchRoutes(start, end) andThen matchClass("pool") andThen matchSeats(seats)
  def getPremier(start:Source, end: Destination) = matchRoutes(start, end) andThen matchClass("premier")

  /* more abtstract constructs defined by business needs*/
  def getPremierTravel(start:Source, end: Destination) = getPremier(start, end) andThen matchClass("premier")
  def getEconomyCommute(start:Source, end: Destination) = getPool(start, end)(1) andThen matchConvinience
  def getPromotionalTrips(start:Source, end: Destination) = getPool(start, end)(1) andThen matchPromotionCabs

}


object OccupancyManager{

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
    matchFunction(cabsFromDB)
  }

}