package snap

import snap.MatchType.MatchType

import scala.annotation.tailrec
import scala.util.Random

object SnapGame {

  // Created a list of cards for a single deck. ordered.
  private def newDeck: Seq[Card] =
    (1 to 13).map(num => Card(Suit.Club, num)) ++
      (1 to 13).map(num => Card(Suit.Diamond, num)) ++
      (1 to 13).map(num => Card(Suit.Heart, num)) ++
      (1 to 13).map(num => Card(Suit.Spade, num))

  // shuffles the provided list of cards
  private def shuffleDeck(deck: Seq[Card]): Seq[Card] =
    deck.foldLeft(Seq.empty[Card])((shuffledDeck: Seq[Card], newCard: Card) =>
      if (shuffledDeck.isEmpty) {
        Seq(newCard)
      } else {
        val insertPoint = Random.between(0, shuffledDeck.length)
        val (startDeck, endDeck) = shuffledDeck.splitAt(insertPoint)
        (startDeck :+ newCard) ++ endDeck
      }
    )
  // Could have used the built in Random.shuffle(deck)
  // Could have used a hash map function

  // splits the provided list of cards according to the number of players
  // if the list doesn't split evenly, then the remainders will be distributed from the start of the player list
  private def splitCards(
      deck: Seq[Card],
      numberOfPlayers: Int
  ): Seq[Seq[Card]] = {
    val deckLength = deck.length
    val remainder = deckLength % numberOfPlayers
    val splitNum = (deckLength - remainder) / numberOfPlayers

    val (evenDeckPart, remainderDeckPart) = deck.splitAt(deckLength - remainder)
    val splitDecksWithoutRemainder = evenDeckPart.grouped(splitNum).toSeq

    val (needOneMore, goodAsIs) = splitDecksWithoutRemainder.splitAt(remainder)

    val hasOneMore = needOneMore.zip(remainderDeckPart).map {
      case (cardPile, remainderCard) => cardPile :+ remainderCard
    }

    hasOneMore ++ goodAsIs
  }

  // Set up the player object according to the number of players and decks
  def setupPlayers(playersNum: Int, deckNum: Int): Seq[Player] = {
    val deckPile = (0 until deckNum).flatMap(_ => newDeck)
    val shuffledDeckPile = shuffleDeck(deckPile)

    val splitDeck = splitCards(shuffledDeckPile, playersNum)

    splitDeck.zipWithIndex.map { case (cards, num) =>
      Player(num, cards, Seq.empty[Card])
    }
  }

  // Analyses the top cards of all players, and returns a set of the cards that can be snapped on
  // A bit over-engineered, this would not just return the latest card, but any that could be snapped.
  // I did this to be a bit more functional with it,
  // and I was testing it out with playing 1 card from each player at a time
  private def getSnappableCards(
      players: Seq[Player],
      matchType: MatchType
  ): Set[Card] = {
    val playerTopCards = players.flatMap(_.getTopCard)

    matchType match {
      case MatchType.Both =>
        playerTopCards
          .groupMapReduce(identity)(_ => 1)(_ + _)
          .filter(_._2 > 1)
          .keySet
      case MatchType.Suit =>
        val suitSet = playerTopCards
          .groupMapReduce(_.suit)(_ => 1)(_ + _)
          .filter(_._2 > 1)
          .keySet
        playerTopCards.filter(x => suitSet.contains(x.suit)).toSet
      case MatchType.Val =>
        val valueSet = playerTopCards
          .groupMapReduce(_.value)(_ => 1)(_ + _)
          .filter(_._2 > 1)
          .keySet
        playerTopCards.filter(x => valueSet.contains(x.value)).toSet
    }
  }

  // This plays a card out from the first player in the list.
  // Then analyses the snap situations, moves around cards if required
  // Then moves the first player to the end of the list
  private def playFirstPlayer(
      players: Seq[Player],
      matchType: MatchType
  ): Seq[Player] = {
    // Next work would be to refactor this function.
    val newPlayers = players.head.playCard +: players.tail

    val snappableCards = getSnappableCards(newPlayers, matchType)

    val cardsWon: Seq[(Player, Seq[Card])] =
      newPlayers.map(player => player.takePlayedStackIfMatch(snappableCards))

    val (playersAfterRemove, cardsToWinner) = cardsWon.unzip

    val allCardsToWinner = cardsToWinner.flatten

    val playerReactionPairs =
      playersAfterRemove.map(player => (player, player.getReactionRandomNumber))

    val (winningPlayer, _) = playerReactionPairs.minBy(_._2)

    val newWinningPlayer = winningPlayer.giveCards(allCardsToWinner)

    val indexOfWinner =
      playersAfterRemove.indexWhere(_.playerNum == newWinningPlayer.playerNum)

    val finalPlayerListThisRound =
      playersAfterRemove.updated(indexOfWinner, newWinningPlayer)

    finalPlayerListThisRound.tail :+ finalPlayerListThisRound.head
  }

  // Tail recursive function to play the game.
  // This was to avoid a while loop with a mutable object.
  @tailrec
  def playGameOfSnap(
      players: Seq[Player],
      matchType: MatchType
  ): Seq[Player] = {
    if (players.exists(_.hand.isEmpty)) {
      players
    } else {
      playGameOfSnap(playFirstPlayer(players, matchType), matchType)
    }
  }
}
