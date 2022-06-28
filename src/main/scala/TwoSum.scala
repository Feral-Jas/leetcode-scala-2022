object TwoSum {
  def solution1(nums: Array[Int], target: Int): Array[Int] = {
    val zipWithIndex = nums.zipWithIndex
    zipWithIndex collect {
      case (x, index)
          if zipWithIndex.exists(y => y._1 == (target - x) && y._2 != index) =>
        index
    }
  }
  def solution2(nums: Array[Int], target: Int): Array[Int] = {
    ???
  }
  def main(args: Array[String]): Unit = {
    val case1 = (Array(1, 2, 3, 4, 5) -> 9) -> Array(3, 4)
    val case2 = (Array(3, 2, 4) -> 6) -> Array(1, 2)
    val case3 = (Array(3, 3, 5) -> 6) -> Array(0, 1)
    Map(case1, case2, case3).foreach { case ((arr, tar), expected) =>
      val res = solution1(arr, tar)
      assert(
        res sameElements expected,
        arr.mkString("Array(", ", ", ")") + "->" + tar + res.mkString(
          "Array(",
          ", ",
          ")"
        ) + " should " + expected.mkString("Array(", ", ", ")")
      )
    }
  }
}
