package snap

import snap.MatchType.MatchType

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object InputFunctions {

  // Get the number of players from the user
  @tailrec
  def askNumPlayers: Int = {
    print("Enter desired number of players (minimum 2, default 2): ")
    val lineIn: String = readLine()

    if (lineIn.isEmpty) {
      2
    } else {
      try {
        val convertedInt = lineIn.toInt
        if (convertedInt < 2) {
          throw new Exception("too low")
        } else {
          convertedInt
        }
      } catch {
        case _: Exception =>
          println(s"$lineIn is not an acceptable integer value")
          askNumPlayers
      }
    }
  }

  // Get the number of decks from the user
  @tailrec
  def askNumDecks: Int = {
    print("Enter desired number of decks (minimum 1): ")
    val lineIn: String = readLine()

    try {
      val convertedInt = lineIn.toInt
      if (convertedInt < 1) {
        throw new Exception("too low")
      } else {
        convertedInt
      }
    } catch {
      case _: Exception =>
        println(s"$lineIn is not an acceptable integer value")
        askNumDecks
    }
  }

  // Get the match type from the user
  @tailrec
  def askMatchType: MatchType = {
    print("Enter desired match type, Suit - s, Value - v, Both - b: ")
    val lineIn: String = readLine().toLowerCase

    try {
      lineIn match {
        case "s" | "suit"          => MatchType.Suit
        case "v" | "value" | "val" => MatchType.Val
        case "b" | "both"          => MatchType.Both
        case _                     => throw new Exception("too low")
      }
    } catch {
      case _: Exception =>
        println(s"$lineIn is not an acceptable value")
        askMatchType
    }
  }

}
