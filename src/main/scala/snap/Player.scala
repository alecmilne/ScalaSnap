package snap

import scala.util.Random

case class Player(
    playerNum: Int,
    hand: Seq[Card],
    private val playedStack: Seq[Card]
) {

  def playCard: Player = {
    val (cardToPlay, newHand) = hand.splitAt(1)
    val newStack = cardToPlay ++ playedStack

    this.copy(hand = newHand, playedStack = newStack)
  }

  def getTopCard: Option[Card] = playedStack.headOption

  def getReactionRandomNumber: Int = Random.nextInt

  def takePlayedStackIfMatch(checkCards: Set[Card]): (Player, Seq[Card]) = {
    val isMatching = getTopCard.exists(topCard => checkCards.contains(topCard))

    if (isMatching) {
      (this.copy(playedStack = Seq.empty[Card]), playedStack)
    } else {
      (this, Seq.empty[Card])
    }
  }

  def giveCards(newCards: Seq[Card]): Player =
    this.copy(hand = hand ++ newCards)
}
