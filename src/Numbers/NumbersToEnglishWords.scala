package Numbers
import scala.math.BigDecimal
/**
  * Created by aliaa on 20/04/17.
  */
object NumbersToEnglishWords {
  private val tensNames = Array("", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
  private val numNames = Array("", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")
  private val specialNames = Array("", "thousand", "million")

  private def convertLessThanOneThousand(number: Int) = {
    //Splitting the three digits
    val unit: Int = number % 10
    val tens: Int = (number % 100) / 10
    val hundreds: Int = number / 100

    //Converting unit and tens into words
    val soFar = if (number % 100 < 20) {
      numNames(number % 100)
    } else {
      numNames(number % 10) match {
        case "" => tensNames(tens) + numNames(number % 10)
        case _ => tensNames(tens) + "-" + numNames(number % 10)
      }
    }

    //Concatenating and returning
    if (hundreds == 0)
      soFar
    else if (soFar.isEmpty)
      numNames(hundreds) + " hundred"
    else if (number % 100 >= 10)
      numNames(hundreds) + " hundred " + soFar
    else
      numNames(hundreds) + " hundred and " + soFar
  }

  private def convertBeforeDecimalPoint(I_Number: Int): String = I_Number match {
    case 0 => "zero"
    case _ => {
      val resultWord = convert(I_Number, 0).trim
      val commaCheck = resultWord takeRight (1)
      commaCheck match {
        case "," => (resultWord dropRight (1)).trim
        case _ => resultWord.trim
      }
    }

  }

  private def convertAfterDecimalPoint(D_Number: Double): String = D_Number match {
    case 0.25 => "twenty-five"
    case 0.5 => "fifty"
    case 0.75 => "seventy-five"
    case _ => ""
  }

  private def preConversion(theNum: BigDecimal): (Int, Double) = {
    //splitting the BigDecimal to two numbers (before point, and after point)
    val beforeDecimalPoint: Int = theNum.toInt
    val AfterDecimalPoint = theNum - beforeDecimalPoint
    (beforeDecimalPoint, AfterDecimalPoint.toDouble)
  }

  private def convert(theNum: Int, index: Int): String = {
    if (theNum == 0) {
      ""
    } else {
      val part = theNum % 1000
      val theWord = part match {
        case 0 => ""
        case _ => convertLessThanOneThousand(part) + " " + specialNames(index) + ", "
      }
      convert(theNum / 1000, index + 1) + theWord
    }
  }

  def convertToWords(D_Number: BigDecimal): String = {
    val (inPounds, inPiasters) = preConversion(D_Number)

    val pound: String = inPounds match {
      case 1 => " pound"
      case _ => " pounds"
    }

    if (inPiasters == 0.0) {
      convertBeforeDecimalPoint(inPounds) + pound
    } else {
      convertBeforeDecimalPoint(inPounds) + pound + ", " + convertAfterDecimalPoint(inPiasters) + " piasters"
    }
  }

  def main(args: Array[String]): Unit = {
    println(convertToWords(10))
    println(convertToWords(100))
    println(convertToWords(1000))
    println(convertToWords(1000000))
    println(convertToWords(9))
    println(convertToWords(99))
    println(convertToWords(999))
    println(convertToWords(9999))
    println(convertToWords(99999))
    println(convertToWords(999999))
    println(convertToWords(101))
    println(convertToWords(111))
    println(convertToWords(1001))
    println(convertToWords(1101))
    println(convertToWords(11001))
    println(convertToWords(1111))
    println(convertToWords(1011))
    println(convertToWords(111101))
  }
}
