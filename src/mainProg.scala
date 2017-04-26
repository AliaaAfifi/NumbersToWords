import Numbers.NumbersToEnglishWords

/**
  * Created by aliaa on 20/04/17.
  */
object mainProg {
  def main(args: Array[String]): Unit = {
    val C1 = new NumbersToEnglishWords
    //for(i <- 549751219 to 999999999) {
      //println(C1 convertToWords i)
    //}


    println(C1 convertToWords 549751219.5)//val C1 = new NumbersToEnglishWords
    println(C1 convertToWords 110.25)
    println(C1 convertToWords 301.50)
    println(C1 convertToWords 1100)
    println(C1 convertToWords 1001)
    println(C1 convertToWords 9999999)
    println(C1 convertToWords 1)
    println(C1 convertToWords 0)
    println(C1 convertToWords 11)
    println(C1 convertToWords 999000000)
    println(C1 convertToWords 100000050)

  }
}