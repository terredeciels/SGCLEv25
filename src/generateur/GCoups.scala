package generateur

import generateur.TypeCoups._

class GCoups(var piece: Int, var caseO: Int, var caseX: Int, var pieceprise: Int,
  var typedecoups: TypeCoups, var piecePromotion: Int) extends TModele {

  def this(piece: Int, caseO: Int, caseX: Int, pieceprise: Int, typedecoups: TypeCoups) =
    this(piece, caseO, caseX, pieceprise, typedecoups, 0)

  def getString: String = {
    if (piece == ROI && caseO == e1 && caseX == g1 || piece == ROI && caseO == e8 && caseX == g8) "O-O"
    else if (piece == ROI && caseO == e1 && caseX == c1 || piece == ROI && caseO == e8 && caseX == c8) "O-O-O"
    else if (typedecoups == EnPassant) getString(caseO) + "x" + getString(caseX)
    else if (typedecoups == Promotion) {
      if (pieceprise != 0)
        getString(caseO) + "x" + getString(caseX) + STRING_PIECE(math.abs(piecePromotion))
      else
        getString(caseO) + "-" + getString(caseX) + STRING_PIECE(math.abs(piecePromotion))
    } else {
      if (typedecoups == Prise)
        getString(caseO) + "x" + getString(caseX)
      else
        getString(caseO) + "-" + getString(caseX)
    }
  }

  def getString(caseC: CASE): String = STRING_CASES(INDICECASES(caseC))

}