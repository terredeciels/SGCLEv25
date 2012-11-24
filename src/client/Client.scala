package client

import main.Fen
import main.Ui
import generateur.GPosition
import scala.collection.JavaConversions._
import generateur.Generateur

object Client {

  def main(args: Array[String]) {
    //val position = new GPosition
    //    val fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    //    position.init(fen)
    //    position.pseudocoups = new Generateur(position).fCoupsLegaux
    //    System.out.println(position)
    //    System.exit(0)

    val command = new Array[String](3)
    command(0) = "-cli"
    // command(1) = "F:/ProgmEchecsNotes/shirov.pgn" //814
    // command(1) = "F:/ProgmEchecsNotes/paulsen.pgn"; //322
    command(1) = "F:/ProgmEchecsNotes/Berliner.pgn"; //64

    // command(1) = "F:/ProgmEchecsNotes/Berliner10parties.pgn"; //10
    //command(1) = "F:/ProgmEchecsNotes/ashley.pgn"; //414
    //    command[1] = "F:/ProgmEchecsNotes/bird.pgn";//353
    //        command[1] = "F:/ProgmEchecsNotes/Tartakower.pgn";//1290
    // command(1) = "F:/ProgmEchecsNotes/Capablanca.pgn"; //597
    //  command(1) = "F:/ProgmEchecsNotes/Boleslavsky.pgn"; //651
    //       command(1) = "F:/ProgmEchecsNotes/Soltis.pgn";//370
    // command(1) = "F:/ProgmEchecsNotes/Motylev.pgn";//1169

    Ui.main(command)
    for (fen <- Fen.getFenList) { // scala.collection.JavaConversions._
      val gposition = new GPosition(fen)
      gposition.fCoupsLegaux
      System.out.print(gposition)
    }
  }
}