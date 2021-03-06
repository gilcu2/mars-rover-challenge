package spacey

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarsRoverTest extends AnyFlatSpec with Matchers {

	"Planet" can "define its size" in {
		val mars = Planet(5, 6)
		mars.coordinates.x.location should be(5)
		mars.coordinates.y.location should be(6)
	}

	"Rover" should "accept starting point (X,Y) of a rover and the direction (N,S,E,W) it is facing" in {
		val rover = Rover(12, 42, 'E')
		rover.coordinates.x.location should be(12)
		rover.coordinates.y.location should be(42)
		rover.direction should be('E')
	}

	it should "be able to move forward (f)" in {
		val rover = Rover(12, 42, 'E')
		val roverAfter = rover.sendCommands("f")
		roverAfter.coordinates.x.location should be(13)
		roverAfter.coordinates.y.location should be(42)
	}

	it should "be able to move backward (b)" in {
		val rover = Rover(12, 42, 'E')
		val roverAfter = rover.sendCommands("b")
		roverAfter.coordinates.x.location should be(11)
		roverAfter.coordinates.y.location should be(42)
	}

	it should "be able to turn left (l)" in {
		val rover = Rover(12, 42, 'N')
		val roverAfter = rover.sendCommands("l")
		roverAfter.direction should be('W')
	}

	  it should "be able to receive a character array of commands" in {
	    val rover = Rover(12, 42, 'E')
	    rover.sendCommands("flf")
	    rover.coordinates.x.location should be (13)
	    rover.coordinates.y.location should be (43)
	    rover.direction should be ('N')
	  }

	//  it should "wrap from one edge of the grid to another" in {
	//    val rover = Rover(1, 1, 'E', Planet(3, 3))
	//    rover.sendCommands("fff")
	//    rover.coordinates.x.location should be (1)
	//    rover.sendCommands("rfff")
	//    rover.coordinates.y.location should be (1)
	//  }
	//
	//  it should "report OK and array of commands if no obstacle was found" in {
	//    val rover = Rover(12, 42, 'E')
	//    rover.sendCommands("f") should be ("OK: f")
	//  }
	//
	//  it should "report NOK and array of commands that lead to an obstacle" in {
	//    val rover = Rover(1, 1, 'N', Planet(10, 10, List(Coordinates(1, 3))))
	//    rover.sendCommands("ff") should be ("NOK: f")
	//  }

}
