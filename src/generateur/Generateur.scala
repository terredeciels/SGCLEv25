package generateur

import generateur.TypeCoups._

class Generateur(val position: GPosition, val couleur: Int) extends TModele {

  var positionSimuler: POSITION = _
  var coups: COUPS = _
  var pseudoCoupsPositionSimuler: LISTEDECOUPS = _
  var coupsMettantEnEchec: LISTEDECOUPS = _
  var coupsAttaqueRoque: LISTEDECOUPS = _
  var pCoups: LISTEDECOUPS = _
  var pseudoCoups = new LISTEDECOUPS
  var recherchePionAttaqueRoque = false
  var x: Contexte = new Contexte {
    var entier = 0
    var booleen = false
    var typeDeCoups = Deplacement
    var cO = 0
    var direction = 0
    var directionsPiece: DIRECTIONPIECE = _
  }

  def this(position: GPosition) = this(position, position.traits)

  val DIRECTIONS = Map[Int, DIRECTIONPIECE](CAVALIER -> DIR_CAVALIER, FOU -> DIR_FOU, TOUR -> DIR_TOUR,
    DAME -> DIR_DAME, ROI -> DIR_ROI)
  val GLISSEMENT = Map[Int, Contexte => Contexte](ROI -> fPseudoCoupsNonGlissant, CAVALIER -> fPseudoCoupsNonGlissant,
    TOUR -> fPseudoCoupsGlissant, FOU -> fPseudoCoupsGlissant, DAME -> fPseudoCoupsGlissant)

  def fDirections(code: TYPEPIECE): DIRECTIONPIECE = DIRECTIONS.get(code).get
  def fGlissement(code: TYPEPIECE): CFonct = GLISSEMENT.get(code).get
  def etats = position.etats
  def NordSudSelonCouleur: DIRECTION = if (couleur == BLANC) nord else sud
  def AjouterCoups = new CFonct {
    def apply(x: Contexte) = {
      pseudoCoups += new GCoups(etats(x.cO), x.cO, x.entier, etats(x.entier), x.typeDeCoups)
      x
    }
  }
  def AjouterCoupsDeplacement = new CFonct {
    def apply(x: Contexte) = {
      pseudoCoups += new GCoups(etats(x.cO), x.cO, x.entier, etats(x.entier), Deplacement)
      x
    }
  }
  def AjouterCoupsPrise = new CFonct {
    def apply(x: Contexte) = {
      pseudoCoups += new GCoups(etats(x.cO), x.cO, x.entier, etats(x.entier), Prise)
      x
    }
  }
  def CaseEstVide = new CFonct {
    def apply(x: Contexte) = {
      x.booleen = etats(x.entier) == VIDE
      x
    }
  }
  def CaseSuivanteDansDirection = new CFonct {
    def apply(x: Contexte) = {
      x.entier = x.entier + x.direction
      x
    }
  }
  def PieceAdverse = new CFonct {
    def apply(x: Contexte) = {
      x.booleen = etats(x.entier) != OUT && etats(x.entier) * couleur < 0
      x
    }
  }
  def couleurPiece(c: CASE) = if (etats(c) < 0) BLANC else NOIR
  def couleurPiece(c: CASE, etats: ETATS) = if (etats(c) < 0) BLANC else NOIR
  def existe(c: CASE) = etats(c) != OUT
  def rangFinal(c: CASE) = c >= a1 && c <= h1 && couleur == NOIR || c >= a8 && c <= h8 && couleur == BLANC
  def rangInitial(c: CASE) = c >= a7 && c <= h7 && couleur == NOIR || c >= a2 && c <= h2 && couleur == BLANC
  def pieceQuiALeTrait(c: CASE) = !(etats(c) == VIDE) && couleurPiece(c) == couleur
  def typeDePiece(c: CASE) = math.abs(etats(c))
  def typeDePiece(c: CASE, etats: ETATS) = math.abs(etats(c))
  def ajouterCoups(cO: CASE, cX: CASE, typedecoups: TypeCoups) =
    pseudoCoups += new GCoups(etats(cO), cO, cX, etats(cX), typedecoups)
  def ajouterCoups(cO: CASE, cX: CASE, typedecoups: TypeCoups, typepiecepromue: TYPEPIECE) =
    pseudoCoups += new GCoups(etats(cO), cO, cX, etats(cX), typedecoups, typepiecepromue)
  def ajouterCoupsEnPassant(sudEstOuOuest: DIRECTION, caseEP: CASE) {
    pCoups += new GCoups(PION, caseEP + sudEstOuOuest, caseEP, 0, TypeCoups.EnPassant)
  }
  def AjouterLeCoups = new CFonct {
    def apply(x: Contexte) = {
      coupsMettantEnEchec += coups
      x
    }
  }
  def pieceAdverse(c: CASE) = existe(c) && etats(c) * couleur < 0
  def fPion(c: CASE, couleur: COULEUR) = typeDePiece(c) == PION && couleurPiece(c) == couleur
  def fPseudoCoups(position: POSITION, couleur: COULEUR): LISTEDECOUPS = new Generateur(position, couleur).fPseudoCoups
  def fPseudoCoupsAttaque(position: POSITION, couleur: COULEUR): LISTEDECOUPS = new Generateur(position, couleur).fPseudoCoupsAttaque
  def fCoupsLegaux = {
    pCoups = fPseudoCoups(position, couleur)
    ajouterLesRoquesPossibles
    ajouterLesPionEnPassantPossibles
    pCoups --= fSuprimerEchecs(pCoups)
    pCoups
  }
  def fSuprimerEchecs(listeDePseudoCoups: LISTEDECOUPS): LISTEDECOUPS = {
    coupsMettantEnEchec = new LISTEDECOUPS
    listeDePseudoCoups.foreach {
      coups =>
        positionSimuler = fSimulerPosition(position, coups, couleur)
        pseudoCoupsPositionSimuler = fPseudoCoups(positionSimuler, -couleur)
        this.coups = coups
        (AjouterLeCoups SI RoiEstEnEchec)(x)
    }
    coupsMettantEnEchec
  }
  def RoiEstEnEchec = new CFonct {
    def apply(x: Contexte) = {
      val etats = positionSimuler.etats
      val caseRoi = CASES.find(cO => typeDePiece(cO, etats) == ROI && couleurPiece(cO, etats) == couleur).get
      x.booleen = !pseudoCoupsPositionSimuler.forall(coups => coups.caseX != caseRoi)
      x
    }
  }
  def fSimulerPosition(position: POSITION, coups: COUPS, couleur: COULEUR) = {
    val positionSimuler = position.copie
    val cO = coups.caseO
    val cX = coups.caseX
    val etats = positionSimuler.etats
    coups.typedecoups match {
      case TypeCoups.Deplacement | TypeCoups.Prise =>
        etats(cX) = etats(cO)
        etats(cO) = VIDE
      case TypeCoups.EnPassant =>
        etats(cX) = etats(cO)
        etats(cO) = VIDE
        etats(cX + nord * couleur) = VIDE
      case TypeCoups.Promotion =>
        etats(cX) = coups.piecePromotion
        etats(cO) = VIDE
      case TypeCoups.Roque => //TODO match typeDeCoups : Roque
      case _ =>
    }
    positionSimuler
  }
  def ajouterLesRoquesPossibles = {
    coupsAttaqueRoque = fPseudoCoupsAttaque(position, -couleur)
    if (couleur == BLANC) {
      if (droitPetitRoque(position.droitPetitRoqueBlanc, f1, g1, e1)) pCoups += new GCoups(ROI, e1, g1, 0, TypeCoups.Roque)
      if (droitGrandRoque(position.droitGrandRoqueBlanc, d1, c1, b1, e1)) pCoups += new GCoups(ROI, e1, c1, 0, TypeCoups.Roque)
    } else {
      if (droitPetitRoque(position.droitPetitRoqueNoir, f8, g8, e8)) pCoups += new GCoups(ROI, e8, g8, 0, TypeCoups.Roque)
      if (droitGrandRoque(position.droitGrandRoqueNoir, d8, c8, b8, e8)) pCoups += new GCoups(ROI, e8, c8, 0, TypeCoups.Roque)
    }
  }
  def droitPetitRoque(droit: BOOL, cx1: CASE, cx2: CASE, cx3: CASE) = droit && etats(cx1) == VIDE && etats(cx2) == VIDE && attaqueRoque(cx3, cx1, cx2)
  def droitGrandRoque(droit: BOOL, cx1: CASE, cx2: CASE, cx3: CASE, cx4: CASE) = droit && etats(cx1) == VIDE && etats(cx2) == VIDE && etats(cx3) == VIDE && attaqueRoque(cx4, cx1, cx2)
  def attaqueRoque(cx: CASE, cx1: CASE, cx2: CASE): BOOL = coupsAttaqueRoque.forall(coups => coups.caseX != cx && coups.caseX != cx1 && coups.caseX != cx2)
  def ajouterLesPionEnPassantPossibles() {
    val caseEP = position.caseEP
    if (caseEP != PAS_DE_CASE) Set(est, ouest)
      .foreach(estOuOuest =>
        if (fPion(caseEP + nord * couleur + estOuOuest, couleur)) ajouterCoupsEnPassant(nord * couleur + estOuOuest, caseEP))
  }
  def fPseudoCoupsAttaque = {
    recherchePionAttaqueRoque = true
    fPseudoCoups
    pseudoCoups
  }
  def fPseudoCoups = {
    CASES.filter(pieceQuiALeTrait).foreach(caseO =>
      typeDePiece(caseO) match {
        case PION =>
          x.cO = caseO
          pseudoCoupsPion(caseO, recherchePionAttaqueRoque)
        case _ =>
          x.cO = caseO
          x.directionsPiece = DIRECTIONS(typeDePiece(caseO))
          fGlissement(typeDePiece(caseO))(x)
      })
    pseudoCoups
  }
  def fPseudoCoupsGlissant(x: Contexte) = {
    x.directionsPiece.foreach {
      direction =>
        {
          x.direction = direction
          x.entier = x.cO + direction
          (AjouterCoupsDeplacement puis CaseSuivanteDansDirection TANTQUE CaseEstVide)(x)
          (AjouterCoupsPrise SI PieceAdverse)(x)
        }
    }
    x
  }
  def fPseudoCoupsNonGlissant(x: Contexte) = {
    for (direction <- x.directionsPiece) {
      x.entier = x.cO + direction
      ((AjouterCoupsDeplacement SI CaseEstVide) puis (AjouterCoupsPrise SI PieceAdverse))(x)
    }
    x
  }
  def pseudoCoupsPion(cO: CASE, recherchePionAttaqueRoque: BOOL) {
    x.entier = cO + NordSudSelonCouleur
    if (CaseEstVide(x).booleen) {
      if (rangFinal(x.entier))
        pseudoCoupsPromotion(cO, x.entier)
      else
        ajouterCoups(cO, x.entier, TypeCoups.Deplacement)
      if (rangInitial(cO)) {
        x.entier = cO + 2 * NordSudSelonCouleur
        if (CaseEstVide(x).booleen)
          ajouterCoups(cO, x.entier, TypeCoups.Deplacement)
      }
    }

    if (!recherchePionAttaqueRoque)
      Set(est, ouest).foreach(estOuOuest => diagonalePionPrise(cO, estOuOuest))
    else
      Set(est, ouest).foreach(estOuOuest => diagonalePionAttaqueRoque(cO, estOuOuest))

  }
  def diagonalePionAttaqueRoque(cO: CASE, estOuOuest: DIRECTION) {
    val cX = cO + NordSudSelonCouleur + estOuOuest
    if (existe(cX)) ajouterCoups(cO, cX, TypeCoups.Attaque)
  }
  def diagonalePionPrise(cO: Int, estOuOuest: DIRECTION) {
    val cX = cO + NordSudSelonCouleur + estOuOuest
    if (pieceAdverse(cX))
      if (rangFinal(cX)) pseudoCoupsPromotion(cO, cX)
      else ajouterCoups(cO, cX, TypeCoups.Prise)
  }
  def pseudoCoupsPromotion(cO: CASE, cX: CASE) = Set(FOU, CAVALIER, DAME, TOUR).foreach(piece => ajouterCoups(cO, cX, TypeCoups.Promotion, couleur * piece))
}