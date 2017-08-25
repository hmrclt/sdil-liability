package sugar


package object liability {

  implicit class RichSetOutcome(s: Set[LiabilityOutcome]) {
    def toOption: Option[LiabilityOutcome] = s match {
      case both if (both contains Mandatory) & (both contains Voluntary) => Some(Mandatory)
      case x => x.headOption
    }
  }
}

