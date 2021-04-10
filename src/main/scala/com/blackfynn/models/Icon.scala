package com.blackfynn.concepts.models
import enumeratum._
sealed abstract class Icon extends EnumEntry {
  val asset: String
}
object Icon extends Enum[Icon] with CirceEnum[Icon] {
  val values = findValues
  case object Acquisition extends Icon {
    val asset = "icon-acquistion.svg"
  }
  case object Analysis extends Icon {
    val asset = "icon-analysis.svg"
  }
  case object AnatomicalLocation extends Icon {
    val asset = "icon-location.svg"
  }
  case object AnimalSpecimen extends Icon {
    val asset = "icon-mouse.svg"
  }
  case object BaseModel extends Icon {
    val asset = "icon-base_model.svg"
  }
  case object ChemicalInSolution extends Icon {
    val asset = "icon-erlenmeyer.svg"
  }
  case object CorollaryExperiment extends Icon {
    val asset = "icon-graph.svg"
  }
  case object Environment extends Icon {
    val asset = "icon-environment.svg"
  }
  case object Experiment extends Icon {
    val asset = "icon-test_tube.svg"
  }
  case object Extraction extends Icon {
    val asset = "icon-extraction.svg"
  }
  case object HumanSpecimen extends Icon {
    val asset = "icon-team.svg"
  }
  case object Measurement extends Icon {
    val asset = "icon-ruler.svg"
  }
  case object Modulation extends Icon {
    val asset = "icon-modulation.svg"
  }
  case object Organization extends Icon {
    val asset = "icon-organization.svg"
  }
  case object Preset extends Icon {
    val asset = "icon-preset.svg"
  }
  case object Procedure extends Icon {
    val asset = "icon-procedure.svg"
  }
  case object Protocol extends Icon {
    val asset = "icon-clinical-trial.svg"
  }
  case object Researcher extends Icon {
    val asset = "icon-person.svg"
  }
  case object Resource extends Icon {
    val asset = "icon-document.svg"
  }
  case object Scaffold extends Icon {
    val asset = "icon-scaffold.svg"
  }
  case object TissueSample extends Icon {
    val asset = "icon-petri_dish.svg"
  }
}
