package spacey

case class CoordinateValue(location: Int)

case class Coordinates(x: CoordinateValue, y: CoordinateValue)

case class Matrix(x11:Int,x12:Int,x21:Int,x22:Int)

case class Planet(coordinates: Coordinates)

object Planet {

	def apply(x: Int, y: Int): Planet = Planet(Coordinates(CoordinateValue(x), CoordinateValue(y)))

}

case class Rover(coordinates: Coordinates, direction: Char) {

	def sendCommands(movement: String): Rover = {

		movement.foldLeft(this)((r: Rover, op: Char) => r.doOneCommand(op))
	}

	def doOneCommand(movement: Char): Rover = movement match {
		case 'f' => Rover(coordinates.x.location + 1, coordinates.y.location, direction)
		case 'b' => Rover(coordinates.x.location - 1, coordinates.y.location, direction)
		case 'l' => Rover(coordinates.x.location, coordinates.y.location, rotateLeft(direction))
		case _ => this
	}

	private def rotateLeft(direction: Char): Char = direction match {
		case 'N' => 'W'
		case _ => direction
	}

	private def directionToInternal
}

object Rover {

	def apply(x: Int, y: Int, direction: Char): Rover = Rover(
		Coordinates(CoordinateValue(x), CoordinateValue(y)), direction)

}