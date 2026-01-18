//> using dep com.lihaoyi::os-lib:0.11.6
//> using dep org.typelevel::cats-core:2.13.0
//> using dep io.circe::circe-core:0.14.15
//> using dep io.circe::circe-parser:0.14.15
//> using dep io.circe::circe-generic:0.14.15
//> using dep io.circe::circe-literal:0.14.15
//> using dep org.http4s::http4s-ember-client:1.0.0-M46
//> using dep org.http4s::http4s-dsl:1.0.0-M46
//> using dep org.http4s::http4s-circe:1.0.0-M46
//> using dep org.typelevel::log4cats-slf4j:2.7.1
//> using dep org.slf4j:slf4j-simple:2.0.17
//> using dep org.scala-lang::toolkit:0.8.0

import HttpService.*
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import io.circe.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import models.AudioQuery.{AccentPhrase, Mora}
import models.{AudioQuery, Speaker}
import os.Path

object App {

  type IsAccent = Boolean
  type SpeakerId = String

  val basePath = "http://localhost:50021"

  @main
  def main(args: String*): Unit = {
    args(0) match {
      case "speakers" | "s" => showSpeakers(getSpeakers())
      case "exportAudioQuery" | "aq" => exportAudioQuery()
      case "exportAudio" | "a" => exportAudio()
      case "exportAudioQueryAndAudio" | "aqa" => exportAudio(exportAudioQuery().some)
      case _ => println("No Action Selected.")
    }
  }

  def showSpeakers(ss: List[Speaker]) = {
    ss.foreach { sp =>
      println(s"${sp.name}:")
      sp.styles.foreach { st =>
        println(s"  ${st.name}:${st.id}")
      }
    }
  }

  def getSpeakers(): List[Speaker] = {
    HttpService.getJson[List[Speaker]](s"$basePath/speakers".toUriUnsafe).unsafeRunSync()
  }

  def exportAudioQuery(): Path = {
    val input: Path = os.pwd / "input" / "text.tsv"
    val outputDir: Path = os.pwd / "output"
    val output: Path = outputDir / "aq.tsv"
    os.makeDir.all(outputDir)

    if (os.exists(input)) {
      val lines = os.read.lines(input).map(_.split("\t")).tail
      val aqs = lines.map(l => getQuery(l(0), l(1)))
      val tsv = (aqs zip lines).flatMap((aq, l) =>
        convertAudioQueryToList(aq, l(0))
      ).map(_.mkString("\t")).mkString("\n")
      os.write.over(output, tsv)
    } else println(s"${input}を配置してください。")

    output
  }

  val queryHeader = "=AudioQuery="
  val morasHeader = "=Moras="
  val pauseMorasHeader = "=PauseMoras="

  def convertAudioQueryToList(aq: AudioQuery,
                              speakerId: SpeakerId): List[List[String]] = {
    val colLength = 8

    def normalizeD(d: Double): String = {
      BigDecimal(d).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble.toString
    }

    def normalizeOpD(opd: Option[Double]): String = {
      opd.fold("")(normalizeD)
    }

    def normalizeOpS(ops: Option[String]): String = {
      ops.getOrElse("")
    }

    val nD = normalizeD
    val nOD = normalizeOpD
    val nOS = normalizeOpS

    val phraseHeader: List[String] = List(queryHeader, "スピーカーID", "速度", "音程", "抑揚", "音量") // "空秒前", "空秒後", "停止長", "停止長率", "サンプリングレート", "ステレオ"),
    val phraseRow: List[String] = List("", speakerId, nD(aq.speedScale), nD(aq.pitchScale), nD(aq.intonationScale), nD(aq.volumeScale))
    val pauseMoraHeader: List[String] = List(pauseMorasHeader, "文字", "母音", "母音長")
    val moraHeader: List[String] = List(morasHeader, "文字", "アクセント", "子音", "子音長", "母音", "母音長", "音程")

    def mkMoraRow(m: Mora,
                  isAccent: Boolean): List[String] = {
      List("", m.text, if (isAccent) "!" else "", nOS(m.consonant), nOD(m.consonant_length), m.vowel, nD(m.vowel_length), nD(m.pitch))
    }

    def mkPauseMoraRow(p: AccentPhrase): List[String] = {
      p.pause_mora.fold(Nil) { m =>
        List("", m.text, m.vowel, nD(m.vowel_length))
      }
    }

    val rows: List[List[String]] =
      phraseHeader ::
        phraseRow ::
        aq.accent_phrases.flatMap { p =>
          moraHeader :: p.moras.zipWithIndex.map((m, i) => mkMoraRow(m, i == p.accent - 1)) :::
            List(pauseMoraHeader, mkPauseMoraRow(p))
        }

    rows.map { r =>
      r ::: List.fill(colLength - r.length)("")
    }
  }

  def covertListToAudioQuery(data: List[List[String]]): List[(AudioQuery, SpeakerId)] = {
    def parseOpD(s: String): Option[Double] = {
      if (s.isEmpty) None else s.toDoubleOption
    }

    def parseMora(row: List[String]): Mora = {
      Mora(
        text = row(1),
        consonant = if (row(3).isEmpty) None else Some(row(3)),
        consonant_length = parseOpD(row(4)),
        vowel = row(5),
        vowel_length = row(6).toDouble,
        pitch = row(7).toDouble
      )
    }

    def parsePauseMora(row: List[String]): Option[Mora] = {
      if(row.lift(1).exists(_.nonEmpty)) {
        Mora(
          text = row(1),
          consonant = None,
          consonant_length = None,
          vowel = row(2),
          vowel_length = row(3).toDouble,
          pitch = 0.0
        ).some
      } else None
    }
    
    val lastRowIdx = data.length - 1
    val dataWithIdx = data.zipWithIndex
    val aqHeaderWithIdxes = dataWithIdx.filter(r => r._1.headOption.contains(queryHeader))

    aqHeaderWithIdxes.zipWithIndex.map { case ((_, startAqIdx), i) =>
      val aqRow = data(startAqIdx + 1)
      val endAqIdx = aqHeaderWithIdxes.lift(i + 1) match {
        case Some(next) => next._2 - 1
        case None => lastRowIdx
      }

      val morasRangedRows = dataWithIdx.slice(startAqIdx + 2, startAqIdx + 2 + endAqIdx - (startAqIdx + 1))
      val morasHeaderIdxes = morasRangedRows.filter(row => row._1.headOption.contains(morasHeader))
      val morasGroups: List[(List[(Mora, IsAccent)], Option[Mora])] = morasHeaderIdxes.zipWithIndex.map { case ((_, startIdx), j) =>
        val endIdx = morasHeaderIdxes.lift(j + 1) match {
          case Some(next) => next._2 - 3
          case None => endAqIdx - 2 
        }
        
        (startIdx + 1 to endIdx).map { idx =>
          val mora = parseMora(data(idx))
          val isAccent = data(idx)(2).nonEmpty
          (mora, isAccent)
        }.toList -> parsePauseMora(data(endIdx + 2))
      }
      
      val phrases = morasGroups.map { (ms, pm) =>
        AccentPhrase(
          moras = ms.map(_._1),
          accent = Math.max(ms.indexWhere(_._2), 0),
          pause_mora = pm,
        )
      }

      val speakerId = aqRow(1)

      AudioQuery(
        accent_phrases = phrases,
        speedScale = aqRow(2).toDouble,
        pitchScale = aqRow(3).toDouble,
        intonationScale = aqRow(4).toDouble,
        volumeScale = aqRow(5).toDouble,
      ) -> speakerId
    }
  }

  def getQuery(speakerId: String,
               text: String): AudioQuery = {
    val uri = s"$basePath/audio_query".toUriUnsafe
      .withQueryParams(Map("text" -> text, "speaker" -> speakerId))
    HttpService.postJson[String, AudioQuery](uri, "").unsafeRunSync()
  }

  def exportAudio(input: Option[Path] = None): Unit = {
    val fixedInput: Path = input.getOrElse(os.pwd / "input" / "aq.tsv")
    val outputDir: Path = os.pwd / "output"
    os.makeDir.all(outputDir)

    if (os.exists(fixedInput)) {
      val lines = os.read.lines(fixedInput).map(_.split("\t").toList).toList
      val aqs = covertListToAudioQuery(lines)
      aqs.zipWithIndex.foreach { case ((aq, speakerId), i) =>
        val uri = s"$basePath/synthesis".toUriUnsafe
          .withQueryParams(Map("speaker" -> speakerId))
        val wav = HttpService.postJsonGetBytes(uri, aq).unsafeRunSync()
        os.write.over(outputDir / s"$i.json", aq.asJson.noSpaces)
        os.write.over(outputDir / s"$i.wav", wav)
      }
    } else println(s"${fixedInput}を配置してください。")
  }


}
