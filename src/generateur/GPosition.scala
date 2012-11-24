package generateur

import chesspresso._
import collection.mutable.ArrayBuffer
import chesspresso.position.Position
import chesspresso.move.Move

class GPosition(var fen: String) extends TModele {
  
  var allcp_moves: Array[Short] = _
  var coupslegaux: ArrayBuffer[GCoups] = _
  
  var droitPetitRoqueBlanc = false
  var droitGrandRoqueNoir = false
  var droitGrandRoqueBlanc = false
  var droitPetitRoqueNoir = false
  var caseEP = 0
  var traits = 0
  
  var etats = new Array[Int](NB_CELLULES)
  var cp_etats = new Array[Int](NB_CASES)

  def fCoupsLegaux = {   
    val position = new Position(fen)
    
    for (caseO <- CP_CASES)
      cp_etats(caseO) = position.getStone(caseO)
      
    allcp_moves = position.getAllMoves
    traits = position.getToPlay
    val roques = position.getCastles
    droitPetitRoqueNoir = (8 & roques) == 8
    droitGrandRoqueNoir = (4 & roques) == 4
    droitPetitRoqueBlanc = (2 & roques) == 2
    droitGrandRoqueBlanc = (1 & roques) == 1
    
    for (caseO <- 0 to NB_CELLULES - 1)
      etats(caseO) = OUT
      
    val itetats = cp_etats.iterator
    var indice = 0
    while (itetats.hasNext) {
      etats(CASES(indice)) = itetats.next.asInstanceOf[java.lang.Integer]
      indice += 1
    }
    if (position.getToPlay == Chess.WHITE)
      traits = BLANC
    else traits = NOIR
    if (position.getSqiEP == PAS_DE_CASE)
      caseEP = -1
    else caseEP = CASES(position.getSqiEP)

    coupslegaux = new Generateur(this).fCoupsLegaux
  }

  def copie = {
    val position = new GPosition(fen)
    System.arraycopy(etats, 0, position.etats, 0, NB_CELLULES)
    position
  }
  override def toString = {

    val diffStringList = toStringListGCoups --= toStringListCPCoups
    if (!diffStringList.isEmpty) {
      fen + '\n' + "Coups ChessPresso:" + "\n" + toStringListCPCoups
      +'\n' + "Coups GCLE:" + "\n" + toStringListGCoups + "\n" + "Diff:" + "\n" + diffStringList + "\n"
    } else {""}
  }

  def toStringListGCoups = {
    val result = new ArrayBuffer[String]
    for (coups <- coupslegaux) result += coups.getString
    result.sorted
  }
  def toStringListCPCoups = {
    val result = new ArrayBuffer[String]
    for (move <- allcp_moves) result += Move.getString(move)
    result.sorted
    result
  }

}