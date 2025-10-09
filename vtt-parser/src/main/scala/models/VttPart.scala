package models

import java.time.LocalTime

final case class VttPart(no: Long,
                         from: LocalTime,
                         to: LocalTime,
                         text: String) {
  val isMetaMessage: Boolean = text.matches("^\\[.*")
}
