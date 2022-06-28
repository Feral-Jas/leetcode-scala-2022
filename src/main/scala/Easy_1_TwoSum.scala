/** Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target.
  *
  * You may assume that each input would have exactly one solution, and you may not use the same element twice.
  *
  * You can return the answer in any order.
  *
  * Example 1:
  *
  * Input: nums = [2,7,11,15], target = 9
  * Output: [0,1]
  * Explanation: Because nums[0] + nums[1] == 9, we return [0, 1].
  * Example 2:
  *
  * Input: nums = [3,2,4], target = 6
  * Output: [1,2]
  * Example 3:
  *
  * Input: nums = [3,3], target = 6
  * Output: [0,1]
  */
object Easy_1_TwoSum {
  def solution1(nums: Array[Int], target: Int): Array[Int] = {
    val zipWithIndex = nums.zipWithIndex
    zipWithIndex collect {
      case (x, index)
          if zipWithIndex.exists(y => y._1 == (target - x) && y._2 != index) =>
        index
    }
  }
  def solution2(nums: Array[Int], target: Int): Array[Int] = {
    val dict = scala.collection.mutable.Map[Int, Int]()
    (for {
      pair @ (k, v) <- nums.zipWithIndex if dict.contains(target - k) || {
        dict += pair; false
      }
      j <- dict.get(target - k)
    } yield Array(j, v)).flatten
  }
  def main(args: Array[String]): Unit = {
    val case1 = (Array(1, 2, 3, 4, 5) -> 9) -> Array(3, 4)
    val case2 = (Array(3, 2, 4) -> 6) -> Array(1, 2)
    val case3 = (Array(3, 3) -> 6) -> Array(0, 1)
    Map(case1, case2, case3).foreach { case ((arr, tar), expected) =>
      val res = solution2(arr, tar)
      assert(
        res sameElements expected,
        arr.mkString("Array(", ", ", ")") + "->" + tar + " " + res.mkString(
          "Array(",
          ", ",
          ")"
        ) + " should " + expected.mkString("Array(", ", ", ")")
      )
    }
  }
}
