package exercise

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable

case class Point(x: Int, y: Int)
case class Box(topLeft: Point, bottomRight: Point) {
  def area: Int = {
    val width = bottomRight.x - topLeft.x + 1
    val height = bottomRight.y - topLeft.y + 1
    width * height
  }

  def overlaps(other: Box): Boolean = {
    val left1 = topLeft.x
    val right1 = bottomRight.x
    val top1 = topLeft.y
    val bottom1 = bottomRight.y

    val left2 = other.topLeft.x
    val right2 = other.bottomRight.x
    val top2 = other.topLeft.y
    val bottom2 = other.bottomRight.y

    !(right1 < left2 || left1 > right2 || bottom1 < top2 || top1 > bottom2)
  }
}

object BoundingBox {
  def findConnectedGroups(grid: Array[String]): List[List[Point]] = {
    val rows = grid.length
    val cols = grid(0).length
    val visited = Array.fill(rows, cols)(false)
    val groups = mutable.ListBuffer[List[Point]]()

    def isValid(x: Int, y: Int): Boolean =
      x >= 0 && x < rows && y >= 0 && y < cols

    @tailrec
    def dfs(
        pointsToVisit: List[Point],
        currentGroup: mutable.ListBuffer[Point]
    ): Unit = {
      pointsToVisit match {
        case Nil => () // Base case: there are no more points to process
        case Point(x, y) :: rest =>
          // Convert from 1-based to 0-based coordinates for grid access
          val gridX = x - 1
          val gridY = y - 1

          if (
            isValid(gridX, gridY) && !visited(gridX)(gridY) && grid(gridX)(
              gridY
            ) == '*'
          ) {
            visited(gridX)(gridY) = true
            currentGroup += Point(x, y)

            // Add adjacent points to visit (up, right, down, left)
            val newPoints = List(
              Point(x - 1, y),
              Point(x, y + 1),
              Point(x + 1, y),
              Point(x, y - 1)
            ) ++ rest

            dfs(newPoints, currentGroup)
          } else {
            dfs(rest, currentGroup)
          }
      }
    }

    for {
      i <- 0 until rows
      j <- 0 until cols
    } {
      if (grid(i)(j) == '*' && !visited(i)(j)) {
        val currentGroup = mutable.ListBuffer[Point]()
        dfs(
          List(Point(i + 1, j + 1)),
          currentGroup
        ) // Start with 1-based coordinates
        groups += currentGroup.toList
      }
    }

    groups.toList
  }

  def getBoundingBox(points: List[Point]): Box = {
    val minX = points.map(_.x).min
    val maxX = points.map(_.x).max
    val minY = points.map(_.y).min
    val maxY = points.map(_.y).max
    Box(Point(minX, minY), Point(maxX, maxY))
  }

  def findLargestNonOverlappingBox(boxes: List[Box]): Option[Box] = {
    boxes.sortBy(-_.area).find { box =>
      boxes.filter(_ != box).forall(!_.overlaps(box))
    }
  }

  def main(args: Array[String]): Unit = {
    // Read input from stdin
    val lines = Source.stdin.getLines().toArray

    if (lines.isEmpty) {
      System.exit(0)
    }

    // Find all connected groups of asterisks
    val groups = findConnectedGroups(lines)

    // Get bounding boxes for each group
    val boxes = groups.map(getBoundingBox)

    // Find the largest non-overlapping box
    findLargestNonOverlappingBox(boxes) match {
      case Some(box) =>
        println(
          s"(${box.topLeft.x},${box.topLeft.y})(${box.bottomRight.x},${box.bottomRight.y})"
        )
        System.exit(0)
      case None =>
        System.exit(0)
    }
  }
}
