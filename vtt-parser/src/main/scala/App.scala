//> using dep com.lihaoyi::os-lib:0.11.5

import models.*
import os.Path

import java.time.LocalTime
import scala.annotation.tailrec


object App {

  @main
  def main(args: String*): Unit = {
    val inputDir: Path = os.pwd / "input"
    val inputExt = "vtt"
    val outputDir: Path = os.pwd / "output"
    val outputExt = "txt"

    val inputs = os.list(inputDir).filter(f => os.isFile(f) && f.ext == inputExt)
    inputs.foreach { vtt =>
      println(s"Processing ${vtt}...")
      val outputFile = outputDir / s"${vtt.baseName}.$outputExt"
      os.makeDir.all(outputDir)
      parse(os.read.lines(vtt).toList) match {
        case Left(e) =>
          println(e)
        case Right(vps) =>
          val ss = toSentences(vps.filterNot(_.isMetaMessage))
          os.write.over(outputFile, ss.mkString("\n"))
          println("Done output sentences.")
          // TODO: その他色々な出力方式で出力する？
      }
    }
  }

  def parse(lines: List[String]): Either[String, List[VttPart]] = {
    val left = Left("Invalid Data.")

    @tailrec
    def go(ls: List[String],
           acc: List[VttPart]): Either[String, List[VttPart]] = {
      ls match {
        case ls if ls.length < 3 => Right(acc.reverse)
        case n :: s :: m :: tail =>
          (for {
            no <- n.toLongOption
            (from, to) <- parseSpan(s)
          } yield VttPart(no, from, to, m)) match {
            case Some(vp) => go(tail, vp :: acc)
            case None =>
              println(s"""E: Invalid Parsing Data($n, $s, $m).""")
              left
          }
        case _ =>
          println("""E: Invalid Parsing Data(No Match).""")
          left
      }
    }

    if (!lines.headOption.contains("WEBVTT")) {
      println("""E: None WEBVTT.""")
      left
    } else go(lines.tail.filter(_.nonEmpty), Nil)
  }

  def parseSpan(s: String): Option[(LocalTime, LocalTime)] = {
    s.split(" --> ").toList match {
      case f :: t :: Nil =>
        for {
          _f <- parseLT(f)
          _t <- parseLT(t)
        } yield ((_f, _t))
      case _ => None
    }
  }

  def parseLT(s: String): Option[LocalTime] = {
    s.split("[:.]").toList match {
      case h :: m :: s :: ms :: Nil =>
        for {
          _h <- h.toIntOption
          _m <- m.toIntOption
          _s <- s.toIntOption
          _ms <- ms.toIntOption
        } yield LocalTime.of(_h, _m, _s, _ms * 1000)
      case _ => None
    }
  }

  def toSentences(vps: List[VttPart]): List[String] = {
    vps.map(_.text)
      .mkString(" ")
      .split("(?<=[.\\]])")
      .view
      .map(_.trim)
      .filter(_.length >= 2)
      .toList
  }

}
