package models

import models.Speaker.*

case class Speaker(name: String,
                   styles: List[Style])

object Speaker {
  case class Style(name: String,
                   id: Int)
}
