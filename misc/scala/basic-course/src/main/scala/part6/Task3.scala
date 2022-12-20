package part6

import scala.util.Try
import scala.collection.immutable.{Map}

object Task3 extends App {

  type Network = Map[String, Set[String]]
  type Location = String
  type Routes = Set[String]

  def add(network: Network, location: Location): Network =
    network + (location -> Set())
  def remove(network: Network, location: Location): Network = {
    val routes = network(location)
    val acc_ = network - location
    routes.foldLeft(acc_)((a, s) => a + (s -> (a(s) - location)))
  }
  def getRoutes(
      network: Network,
      pointA: Location,
      pointB: Location
  ): (Routes, Routes) = {
    require(pointA != pointB)
    val routesA = network(pointA)
    val routesB = network(pointB)
    (routesA, routesB)
  }
  def connect(network: Network, pointA: Location, pointB: Location): Network = {
    val (routesA, routesB) = getRoutes(network, pointA, pointB)
    network + (pointA -> (routesA + pointB)) + (pointB -> (routesB + pointA))
  }
  def disconnect(
      network: Network,
      pointA: Location,
      pointB: Location
  ): Network = {
    val (routesA, routesB) = getRoutes(network, pointA, pointB)
    network + (pointA -> (routesA - pointB)) + (pointB -> (routesB - pointA))
  }

  def nFlights(network: Network, location: Location): Int = network(
    location
  ).size
  def mostFlights(network: Network): Location = {
    val maxi = network.values.map(_.size).max
    network.keys.filter(network(_).size == maxi).head
  }
  def nLocationsWithNoFlights(network: Network): Int =
    network.count({ case (a, b) => b.isEmpty })

  def isConnected(network: Network, pointA: String, pointB: String): Boolean = {
    type Visited = Set[String]
    def visit(v: String, visited: Set[String]): Set[String] = {
      (network(v) -- visited).foldLeft(visited)((visited_, v_) => {
        if (!visited_.contains(v_)) visit(v_, visited_ + v_)
        else visited_
      })
    }
    visit(pointA, Set()).contains(pointB)
  }

  val empty: Network = Map[String, Set[String]]()
  val network = add(add(empty, "Moscow"), "StPetersburg")
  println(connect(network, "Moscow", "StPetersburg"))
  val moscToPeterNet = connect(network, "Moscow", "StPetersburg")
  println(disconnect(moscToPeterNet, "Moscow", "StPetersburg"))
  println(remove(moscToPeterNet, "Moscow"))
  val locations = add(add(add(empty, "Moscow"), "StPetersburg"), "Novosibirsk")
  val moscToNov = connect(locations, "Moscow", "Novosibirsk")
  val testNet = connect(moscToNov, "Moscow", "StPetersburg")
  println(nFlights(testNet, "Moscow"))
  println(mostFlights(testNet))
  println(nLocationsWithNoFlights(testNet))
  println(isConnected(testNet, "Novosibirsk", "StPetersburg"))
  println(isConnected(testNet, "Moscow", "Novosibirsk"))
  
  val network1: Map[String, Set[String]] = Map(
    "A" -> Set("B"),
    "B" -> Set("A", "C"),
    "C" -> Set("B"),
    "D" -> Set("F"),
    "F" -> Set("D")
  )
  println(network1)
  println(isConnected(network1, "A", "D")) // false
  println(isConnected(network1, "A", "C")) // true
}
