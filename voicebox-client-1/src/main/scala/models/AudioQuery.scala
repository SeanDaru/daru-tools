package models

import io.circe.Decoder
import models.AudioQuery.*

case class AudioQuery(accent_phrases: List[AccentPhrase],
                      speedScale: Double = 1.0,
                      pitchScale: Double = 1.0,
                      intonationScale: Double = 1.0,
                      volumeScale: Double = 1.0,
                      prePhonemeLength: Double = 0.5,
                      postPhonemeLength: Double = 0.5,
                      pauseLength: Option[Double] = None,
                      pauseLengthScale: Double = 1.0,
                      outputSamplingRate: Double = 24000,
                      outputStereo: Boolean = false,
                      kana: Option[String] = None)


object AudioQuery {

  case class AccentPhrase(moras: List[Mora],
                          accent: Int,
                          pause_mora: Option[Mora] = None,
                          is_interrogative: Boolean = false)

  case class Mora(text: String,
                  consonant: Option[String],
                  consonant_length: Option[Double],
                  vowel: String,
                  vowel_length: Double,
                  pitch: Double)

  given Decoder[Mora] = d => {
    for {
      v1 <- d.downField("text").as[String]
      v2 <- d.downField("consonant").as[Option[String]]
      v3 <- d.downField("consonant_length").as[Option[Double]]
      v4 <- d.downField("vowel").as[String]
      v5 <- d.downField("vowel_length").as[Double]
      v6 <- d.downField("pitch").as[Double]
    } yield Mora(v1, v2, v3, v4, v5, v6)
  }

}
