package models

import models.Job.*

case class Job(cuts: List[Cut],
               codec: Option[String],
               preset: Option[String],
               crf: Option[Int],
               bitRateV: Option[String],
               bitRateA: Option[String],
               soundVolumeOptimize: Boolean = false)

object Job {

  case class Cut(from: String,
                 to: String)
  
}
