
package sugar.liability

sealed trait LiabilityOutcome
case object Voluntary extends LiabilityOutcome
case object Mandatory extends LiabilityOutcome

case class LiabilityModel (
  litresProduced: Int,
  imports: Boolean,
  copacksForOthers: Boolean,
  copackedByOtherUk: Int = 0
) {

  def outcome: Set[LiabilityOutcome] = this match {
    case LiabilityModel(prod,_,_,copack) if prod + copack >= 1000000 => Set(Mandatory)
    case LiabilityModel(_,false,false,0) => Set.empty      
    case LiabilityModel(_,false,false,_) => Set(Voluntary)
    case LiabilityModel(_,_,_,0) => Set(Mandatory)            
    case _ => Set(Voluntary, Mandatory)
  }

  def outcome2: Set[LiabilityOutcome] = {
    val totalProd = litresProduced + copackedByOtherUk
    Set(
      Some(Mandatory).filter{_ => totalProd >= 1000000 || copacksForOthers || imports},
      Some(Voluntary).filter{_ => copackedByOtherUk != 0 && litresProduced + copackedByOtherUk < 1000000 }
    ).flatten
  }
}
