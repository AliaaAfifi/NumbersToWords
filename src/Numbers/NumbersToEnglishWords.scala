package Numbers
import scala.math.BigDecimal
/**
  * Created by aliaa on 20/04/17.
  */
class NumbersToEnglishWords {
  private val tensNames = Array("", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
  private val numNames = Array("", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")

  private def convertLessThanOneThousand(number: Int) = {
    //Splitting the three digits
    val unit: Int = number % 10
    val tens: Int = (number % 100) / 10
    val hundreds: Int = number / 100

    //Converting unit and tens into words
    val soFar = (number % 100 < 20) match {
      case true => numNames(number % 100)
      case false => {
        numNames(number % 10) match {
          case "" => tensNames(tens) + numNames(number % 10)
          case _ => tensNames(tens) + "-" + numNames(number % 10)
        }
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
      //Padding the number to 9 digits
      val paddedNumber = f"${I_Number}%09d"

      //Splitting the number
      val Millions_inNumbers = paddedNumber.substring(0, 3)
      val HundredThousands_inNumbers = paddedNumber.substring(3, 6)
      val Thousands_inNumbers = paddedNumber.substring(6, 9)

      //Converting the parts into words
      val Millions_inWords = Millions_inNumbers match {
        case "000" => ""
        case _ => convertLessThanOneThousand(Integer.parseInt(Millions_inNumbers)) + " million, "
      }

      val HundredThousands_inWords = HundredThousands_inNumbers match {
        case "000" => ""
        case _ => convertLessThanOneThousand(Integer.parseInt(HundredThousands_inNumbers)) + " thousand, "
      }

      val Thousands_inWords = convertLessThanOneThousand(Integer.parseInt(Thousands_inNumbers.toString))

      //Conacatenating string of each part
      val resultWord = (Millions_inWords + HundredThousands_inWords + Thousands_inWords).trim

      //Dropping the last character(if it is comma), then returning the result word
      val commaCheck = resultWord takeRight(1)
      commaCheck match {
        case "," => resultWord dropRight(1)
        case _ => resultWord
      }
    }
  }

  private def convertAfterDecimalPoint(D_Number: Double): String = D_Number match {
    case 0.25 => "twenty-five"
    case 0.5 => "fifty"
    case _ => ""
  }

  private def preConversion(theNum: BigDecimal): (Int, Double) = {
    //splitting the BigDecimal to two numbers (before point, and after point)
    val beforeDecimalPoint: Int = theNum.toInt
    val AfterDecimalPoint = theNum - beforeDecimalPoint
    (beforeDecimalPoint, AfterDecimalPoint.toDouble)
  }

  def convertToWords(D_Number: BigDecimal): String = {
    val pairOfTwoNums = preConversion(D_Number)
    val inPounds = pairOfTwoNums._1
    val inPiasters = pairOfTwoNums._2

    val pound: String = inPounds match {
      case 0 => ""
      case 1 => " pound"
      case _ => " pounds"
    }

    if (inPiasters == 0.0) {
      convertBeforeDecimalPoint(inPounds) + pound
    } else {
      convertBeforeDecimalPoint(inPounds) + pound + ", " + convertAfterDecimalPoint(inPiasters) + " piasters"
    }
  }
}
