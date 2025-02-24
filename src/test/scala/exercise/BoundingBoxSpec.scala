package exercise

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable

class BoundingBoxSpec extends AnyFlatSpec with Matchers {
  // Test findConnectedGroups
  "findConnectedGroups" should "identify single asterisk group" in {
    val input = Array(
      "---",
      "-*-",
      "---"
    )
    val result = BoundingBox.findConnectedGroups(input)
    result should have length 1
    result.head should contain only Point(2, 2)
  }

  it should "identify multiple separate groups" in {
    val input = Array(
      "*--*",
      "----",
      "*---"
    )
    val result = BoundingBox.findConnectedGroups(input)
    result should have length 3
    result should contain allOf (
      List(Point(1, 1)),
      List(Point(1, 4)),
      List(Point(3, 1))
    )
  }

  it should "identify connected vertical group" in {
    val input = Array(
      "---",
      "-*-",
      "-*-"
    )
    val result = BoundingBox.findConnectedGroups(input)
    result should have length 1
    result.head should contain allOf (Point(2, 2), Point(3, 2))
  }

  it should "identify connected horizontal group" in {
    val input = Array(
      "---",
      "-**-",
      "---"
    )
    val result = BoundingBox.findConnectedGroups(input)
    result should have length 1
    result.head should contain allOf (Point(2, 2), Point(2, 3))
  }

  // Test getBoundingBox
  "getBoundingBox" should "calculate correct box for single point" in {
    val points = List(Point(2, 2))
    val result = BoundingBox.getBoundingBox(points)
    result should be(Box(Point(2, 2), Point(2, 2)))
    result.area should be(1)
  }

  it should "calculate correct box for vertical line" in {
    val points = List(Point(1, 2), Point(2, 2), Point(3, 2))
    val result = BoundingBox.getBoundingBox(points)
    result should be(Box(Point(1, 2), Point(3, 2)))
    result.area should be(3)
  }

  it should "calculate correct box for rectangle" in {
    val points = List(Point(2, 2), Point(2, 3), Point(3, 2), Point(3, 3))
    val result = BoundingBox.getBoundingBox(points)
    result should be(Box(Point(2, 2), Point(3, 3)))
    result.area should be(4)
  }

  // Test findLargestNonOverlappingBox
  "findLargestNonOverlappingBox" should "return largest non-overlapping box" in {
    val boxes = List(
      Box(Point(1, 1), Point(2, 2)), // area = 4
      Box(Point(2, 2), Point(3, 3)), // area = 4, overlaps with first
      Box(Point(1, 4), Point(1, 4)) // area = 1
    )
    val result = BoundingBox.findLargestNonOverlappingBox(boxes)
    result should be(Some(Box(Point(1, 4), Point(1, 4))))
  }

  it should "return None for empty list" in {
    val result = BoundingBox.findLargestNonOverlappingBox(Nil)
    result should be(None)
  }

  it should "handle overlapping boxes correctly" in {
    val boxes = List(
      Box(Point(1, 1), Point(3, 3)), // area = 9
      Box(Point(2, 2), Point(4, 4)), // area = 9, overlaps with first
      Box(Point(1, 5), Point(2, 6)) // area = 4, no overlap
    )
    val result = BoundingBox.findLargestNonOverlappingBox(boxes)
    result should be(Some(Box(Point(1, 5), Point(2, 6))))
  }

  // Test the example from the problem statement
  "complete solution" should "handle the example input correctly" in {
    val input = Array(
      "**-------***",
      "-*--**--***-",
      "-----***--**",
      "-------***--"
    )
    val groups = BoundingBox.findConnectedGroups(input)
    val boxes = groups.map(BoundingBox.getBoundingBox)
    val result = BoundingBox.findLargestNonOverlappingBox(boxes)

    result should be(Some(Box(Point(1, 1), Point(2, 2))))
  }
}
