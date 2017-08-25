package sugar.liability

import collection.mutable.Stack
import org.scalatest._

class LiabilityToolSpec extends FlatSpec with Matchers {

  val scenarios = Map(
    ("NFSG",LiabilityModel(0,false,false,800000)) -> Set(Voluntary),
    ("2",LiabilityModel(300000,false,false,0)) -> Set(),
    ("Folkingtons",LiabilityModel(0,true,false,500000)) -> Set(Voluntary, Mandatory),
    ("Cott",LiabilityModel(5000000,false,true,0)) -> Set(Mandatory),
    ("Refresco-Gerber",LiabilityModel(0,false,true,0)) -> Set(Mandatory),
    ("Lidl",LiabilityModel(0,true,false,10000000)) -> Set(Mandatory),
    ("Red Bull",LiabilityModel(0,true,false,0)) -> Set(Mandatory),
    ("Radnor Hills",LiabilityModel(950000,false,true,0)) -> Set(Mandatory),
    ("Belvoir",LiabilityModel(900000,false,false,0)) -> Set(),
    ("Fentiman's",LiabilityModel(0,false,false,10000000)) -> Set(Mandatory),
    ("Hypothetical 2", LiabilityModel(	50000,	false,	false, 40000	 )) -> Set(Voluntary),
    ("Hypothetical 3", LiabilityModel(	50000,	true,	false, 0	 )) -> Set(Mandatory),
    ("Hypothetical 4", LiabilityModel(	50000,	true,	false, 40000	 )) -> Set(Voluntary,Mandatory),
    ("Hypothetical 5", LiabilityModel(	50000,	false,	true,  0	 )) -> Set(Mandatory),
    ("Hypothetical 6", LiabilityModel(	50000,	false,	true,  40000	 )) -> Set(Voluntary,Mandatory),
    ("Hypothetical 7", LiabilityModel(	50000,	true,	true,  0	 )) -> Set(Mandatory),
    ("Hypothetical 8", LiabilityModel(	50000,	true,	true,  40000	 )) -> Set(Voluntary,Mandatory),
    ("Hypothetical 9", LiabilityModel(	1000000,	false,	false, 0	 )) -> Set(Mandatory),
    ("Hypothetical 10", LiabilityModel(	500000,	false,	false, 500000	 )) -> Set(Mandatory),
    ("Hypothetical 11", LiabilityModel(	500000,	true,	false, 500000	 )) -> Set(Mandatory),
    ("Hypothetical 12", LiabilityModel(	500000,	false,	true,  500000	 )) -> Set(Mandatory),
    ("Hypothetical 13", LiabilityModel(	500000,	true,	true,  500000	 )) -> Set(Mandatory),
    ("Hypothetical 14", LiabilityModel(	0,	true,	false, 0	 )) -> Set(Mandatory),
    ("Hypothetical 15", LiabilityModel(	0,	false,	true,  0	 )) -> Set(Mandatory),
    ("Hypothetical 16", LiabilityModel(	0,	true,	true,  0	 )) -> Set(Mandatory),
    ("Hypothetical 17",	LiabilityModel(	0,	false,	false,	500000	)) -> Set(Voluntary),
    ("Hypothetical 18",	LiabilityModel(	0,	true,	false,	500000	)) -> Set(Voluntary,Mandatory),
    ("Hypothetical 19",	LiabilityModel(	0,	false,	true,	500000	)) -> Set(Voluntary,Mandatory),
    ("Hypothetical 20",	LiabilityModel(	0,	true,	true,	500000	)) -> Set(Voluntary,Mandatory)
  )

  // https://docs.google.com/spreadsheets/d/16oIqsl2RFS_ozSVLGuWXG_AL_qJuhi6e6mfBeEZ8V4g/edit#gid=1133728034
  val matrix: Seq[(String,String,String,String,Option[LiabilityOutcome])] = Seq(
    ("AvR Matrix #1",  "--X", "---X", "---X", None),
    ("AvR Matrix #2",  "--X", "---X", "X---", None),
    ("AvR Matrix #3",  "--X", "---X", "-X--", Some(Voluntary)),
    ("AvR Matrix #4",  "--X", "---X", "--X-", Some(Mandatory)),
    ("AvR Matrix #5",  "--X", "X---", "---X", Some(Mandatory)),
    ("AvR Matrix #6",  "--X", "X---", "X---", Some(Mandatory)),
    ("AvR Matrix #7",  "--X", "X---", "-X--", Some(Mandatory)),
    ("AvR Matrix #8",  "--X", "X---", "--X-", Some(Mandatory)),
    ("AvR Matrix #9",  "--X", "-X--", "---X", Some(Mandatory)),
    ("AvR Matrix #10", "--X", "-X--", "X---", Some(Mandatory)),
    ("AvR Matrix #11", "--X", "-X--", "-X--", Some(Mandatory)),
    ("AvR Matrix #12", "--X", "-X--", "--X-", Some(Mandatory)),
    ("AvR Matrix #13", "--X", "--X-", "---X", Some(Mandatory)),
    ("AvR Matrix #14", "--X", "--X-", "X---", Some(Mandatory)),
    ("AvR Matrix #15", "--X", "--X-", "-X--", Some(Mandatory)),
    ("AvR Matrix #16", "--X", "--X-", "--X-", Some(Mandatory)),
    ("AvR Matrix #17", "-X-", "---X", "---X", Some(Mandatory)),
    ("AvR Matrix #18", "-X-", "---X", "X---", Some(Mandatory)),
    ("AvR Matrix #19", "-X-", "---X", "-X--", Some(Mandatory)),
    ("AvR Matrix #20", "-X-", "---X", "--X-", Some(Mandatory)),
    ("AvR Matrix #21", "-X-", "X---", "---X", Some(Mandatory)),
    ("AvR Matrix #22", "-X-", "X---", "X---", Some(Mandatory)),
    ("AvR Matrix #23", "-X-", "X---", "-X--", Some(Mandatory)),
    ("AvR Matrix #24", "-X-", "X---", "--X-", Some(Mandatory)),
    ("AvR Matrix #25", "-X-", "-X--", "---X", Some(Mandatory)),
    ("AvR Matrix #26", "-X-", "-X--", "X---", Some(Mandatory)),
    ("AvR Matrix #27", "-X-", "-X--", "-X--", Some(Mandatory)),
    ("AvR Matrix #28", "-X-", "-X--", "--X-", Some(Mandatory)),
    ("AvR Matrix #29", "-X-", "--X-", "---X", Some(Mandatory)),
    ("AvR Matrix #30", "-X-", "--X-", "X---", Some(Mandatory)),
    ("AvR Matrix #31", "-X-", "--X-", "-X--", Some(Mandatory)),
    ("AvR Matrix #32", "-X-", "--X-", "--X-", Some(Mandatory)),
    ("AvR Matrix #33", "X--", "---X", "---X", Some(Mandatory)),
    ("AvR Matrix #34", "X--", "---X", "X---", Some(Mandatory)),
    ("AvR Matrix #35", "X--", "---X", "-X--", Some(Mandatory)),
    ("AvR Matrix #36", "X--", "---X", "--X-", Some(Mandatory)),
    ("AvR Matrix #37", "X--", "X---", "---X", Some(Mandatory)),
    ("AvR Matrix #38", "X--", "X---", "X---", Some(Mandatory)),
    ("AvR Matrix #39", "X--", "X---", "-X--", Some(Mandatory)),
    ("AvR Matrix #40", "X--", "X---", "--X-", Some(Mandatory)),
    ("AvR Matrix #41", "X--", "-X--", "---X", Some(Mandatory)),
    ("AvR Matrix #42", "X--", "-X--", "X---", Some(Mandatory)),
    ("AvR Matrix #43", "X--", "-X--", "-X--", Some(Mandatory)),
    ("AvR Matrix #44", "X--", "-X--", "--X-", Some(Mandatory)),
    ("AvR Matrix #45", "X--", "--X-", "---X", Some(Mandatory)),
    ("AvR Matrix #46", "X--", "--X-", "X---", Some(Mandatory)),
    ("AvR Matrix #47", "X--", "--X-", "-X--", Some(Mandatory)),
    ("AvR Matrix #48", "X--", "--X-", "--X-", Some(Mandatory))
  )

  def mappedMatrix: Seq[((String, LiabilityModel), Option[LiabilityOutcome])] =
    matrix.map{ case (name, i, c, p, desired) =>

      val (litresProduced, copackedByOtherUk) = p match {
        case "---X" => (0,0)
        case "X---" => (300000,0)
        case "-X--" => (300000,300000)
        case "--X-" => (1000001,0)
      }

      val copacksForOthers = c != "---X"
      val imports = i != "--X"
      (name, LiabilityModel(litresProduced, imports, copacksForOthers, copackedByOtherUk)) -> desired
    }

  for {
    ((name,lm),expected) <- scenarios
  } {
    s"$lm ($name)" should s"be ${expected.map(_.toString).mkString("+")}" in {
      (lm.outcome) should be (expected)
    }
  }

  for {
    ((name,lm),expected) <- mappedMatrix
  } {
    s"$lm ($name)" should s"be ${expected.map(_.toString).mkString("+")}" in {
      (lm.outcome.toOption) should be (expected)
    }
  }

}
