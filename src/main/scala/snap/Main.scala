package snap

object Main {

  def main(args: Array[String]): Unit = {
    val playersNum = InputFunctions.askNumPlayers
    val deckNum = InputFunctions.askNumDecks
    val matchType = InputFunctions.askMatchType

    val players = SnapGame.setupPlayers(playersNum, deckNum)

    val finalPlayerState = SnapGame.playGameOfSnap(players, matchType)

    val winningPlayer = finalPlayerState.maxBy(_.hand.length)

    println(s"Winning player was player number ${winningPlayer.playerNum}")
  }
}
