package snap

import snap.Suit.Suit

object Suit extends Enumeration {
  type Suit = Value

  val Club, Diamond, Heart, Spade = Value
}

case class Card(suit: Suit, value: Int) {
  // Not actually used, but would be if more info put to the output about state.
  def print: String = {
    val valueString = value match {
      case 1  => "Ace"
      case 2  => "Two"
      case 3  => "Three"
      case 4  => "Four"
      case 5  => "Five"
      case 6  => "Six"
      case 7  => "Seven"
      case 8  => "Eight"
      case 9  => "Nine"
      case 10 => "Ten"
      case 11 => "Jack"
      case 12 => "Queen"
      case 13 => "King"
      case _  => "Unknown"
    }

    val suitString = Suit match {
      case Suit.Club    => " of clubs"
      case Suit.Diamond => " of diamonds"
      case Suit.Heart   => " of hearts"
      case Suit.Spade   => " of spades"
    }

    valueString + suitString
  }
}
