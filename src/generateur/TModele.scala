package generateur

import generateur.TypeCoups._
import collection.mutable.ArrayBuffer

trait TModele {
  type BOOL = Boolean
  type COULEUR = Int
  type CASE = Int
  type DIRECTION = Int
  type PIECE = Int
  type TYPEPIECE = Int
  type ETAT = Int
  type ETATS = Array[ETAT]
  type COUPS = GCoups
  type TYPEDECOUPS = TypeCoups
  type POSITION = GPosition
  type LISTE[T] = ArrayBuffer[T]
  type LISTEDECOUPS = LISTE[COUPS]
  type DIRECTIONPIECE = Array[DIRECTION]
  type CFonct = Contexte => Contexte

  abstract class Contexte {
    var directionsPiece: DIRECTIONPIECE
    var direction: DIRECTION
    var typeDeCoups: TypeCoups
    var cO: CASE
    var entier: Int
    var booleen: Boolean
  }

  class Roundable(f: CFonct) {
    def o(g: CFonct) = new CFonct {
      def apply(c: Contexte) = f(g(c))
    }
  }
  class Puis(g: CFonct) {
    def puis(f: CFonct) = new CFonct {
      def apply(c: Contexte) = f(g(c))
    }
  }
  class SiInfix(g: CFonct) {
    def SI(f: CFonct) = new CFonct {
      def apply(c: Contexte) = if (f(c).booleen) g(c) else c
    }
  }
  class TantQueInfix(g: CFonct) {
    def TANTQUE(f: CFonct) = new CFonct {
      def apply(c: Contexte) = {
        while (f(c).booleen) g(c)
        c
      }
    }
  }

  implicit def toRoundable(f: CFonct) = new Roundable(f)
  implicit def toPuis(f: CFonct) = new Puis(f)
  implicit def toTantQueInfix(f: CFonct) = new TantQueInfix(f)
  implicit def toSiInfix(f: CFonct) = new SiInfix(f)

  val PAS_DE_CASE = -1
  val fen_initiale: String = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  val BLANC = -1
  val NOIR = 1
  val a1 = 26
  val h1 = 33
  val a8 = 110
  val h8 = 117
  val e1 = 30
  val f1 = 31
  val g1 = 32
  val d1 = 29
  val c1 = 28
  val b1 = 27
  val e8 = 114
  val f8 = 115
  val g8 = 116
  val d8 = 113
  val c8 = 112
  val b8 = 111
  val a7 = 98
  val h7 = 105
  val a2 = 38
  val h2 = 45
  val CP_CASES = Array(
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
    30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57,
    58, 59, 60, 61, 62, 63)
  val CASES = Array(26, 27, 28, 29, 30, 31, 32, 33,
    38, 39, 40, 41, 42, 43, 44, 45,
    50, 51, 52, 53, 54, 55, 56, 57,
    62, 63, 64, 65, 66, 67, 68, 69,
    74, 75, 76, 77, 78, 79, 80, 81,
    86, 87, 88, 89, 90, 91, 92, 93,
    98, 99, 100, 101, 102, 103, 104, 105,
    110, 111, 112, 113, 114, 115, 116, 117)
  val INDICECASES = Array(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, 2, 3, 4, 5, 6, 7, -1, -1, -1, -1, 8, 9, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, 16, 17, 18, 19, 20, 21, 22, 23, -1, -1, -1, -1, 24, 25, 26, 27, 28, 29, 30, 31, -1, -1, -1, -1, 32, 33, 34, 35, 36, 37, 38, 39, -1, -1, -1, -1, 40, 41, 42, 43, 44, 45, 46, 47, -1, -1, -1, -1, 48, 49, 50, 51, 52, 53, 54, 55, -1, -1, -1, -1, 56, 57, 58, 59, 60, 61, 62, 63, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
  val STRING_CASES = Array("a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1",
    "a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2",
    "a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3",
    "a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4",
    "a5", "b5", "c5", "d5", "e5", "f5", "g5", "h5",
    "a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6",
    "a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7",
    "a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8")
  val STRING_PIECE = Array("", "N", "B", "R", "Q")
  val ROI = 6
  val CAVALIER = 1
  val TOUR = 3
  val FOU = 2
  val DAME = 4
  val PION = 5
  val NB_CASES = 64
  val NB_CELLULES = 144
  val VIDE = 0
  val OUT = 9
  val nord = +12
  val est = -1
  val sud = -12
  val ouest = +1
  val nordest = nord + est
  val nordouest = nord + ouest
  val sudest = sud + est
  val sudouest = sud + ouest
  val O = {}
  val DIR_CAVALIER = Array(2 * nord + est, 2 * nord + ouest, 2 * est + nord, 2 * est + sud, 2 * sud + est, 2 * sud + ouest, 2 * ouest + nord, 2 * ouest + sud)
  val DIR_DAME = Array(nord, nordest, est, sudest, sud, sudouest, ouest, nordouest)
  val DIR_FOU = Array(nordest, sudest, sudouest, nordouest)
  val DIR_ROI = Array(nord, nordest, est, sudest, sud, sudouest, ouest, nordouest)
  val DIR_TOUR = Array(nord, est, sud, ouest)
}
