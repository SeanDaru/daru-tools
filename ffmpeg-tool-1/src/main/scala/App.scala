//> using dep com.lihaoyi::os-lib:0.11.6
//> using dep org.typelevel::cats-core:2.13.0
//> using dep io.circe::circe-core:0.14.15
//> using dep io.circe::circe-generic:0.14.15
//> using dep io.circe::circe-literal:0.14.15

import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.*
import io.circe.generic.auto.*
import io.circe.jawn.decode
import models.Job
import os.Path

object App {

  @main
  def main(args: String*): Unit = {
    val inputDir: Path = os.pwd / "input"
    val inputExts = List("mov")
    val outputDir: Path = os.pwd / "output"
    val outputExt = "mp4"
    os.makeDir.all(outputDir)

    val inInputs = os.list(inputDir)
    val inputs = inInputs.filter(f => os.isFile(f) && inputExts.contains(f.ext))
      .flatMap { f =>
        val json = inputDir / s"${f.baseName}.${f.ext}.json"
        if (os.isFile(json)) (f, json).some
        else none
      }

    inputs.foreach { (video, json) =>
      println(s"Processing ${video}...")

      val jsonS = os.read.lines(json).mkString
      decode[Job](jsonS).fold(
        e => println(s"設定が不正です.\n$e"),
        s => {
          // カット処理
          val cuts = NonEmptyList.fromList(s.cuts).fold(NonEmptyList.one(video)) {
            _.zipWithIndex.map { (c, i) =>
              val output = outputDir / s"${video.baseName}.$i.mov"
              val cmd = os.proc(
                "ffmpeg",
                "-i", video,
                "-c", "copy",
                "-ss", c.from,
                "-to", c.to,
                output)
              cmd.call()
              output
            }
          }
          // リストファイル書き出し
          val list = outputDir / s"${video.baseName}.list"
          os.write.over(list, cuts.map(c => s"file '${c.toString}''").mkString_("\n"))
          //  音量レベル確認
          lazy val minVol = {
            os.proc(
                "ffmpeg", "-i", video, "-vn", "-af", "volumedetect", "-f", "null", "-")
              .call()
              .err.text()
              .split("\n").find(_.contains("max_volume"))
              .map(_.replaceFirst(".*(\\d+\\.\\d+) dB.*", "$1"))
              .flatMap(_.toDoubleOption)
              .getOrElse(0.0)
          }
          // マージ + 圧縮 + 音量調整
          val output = outputDir / s"${video.baseName}.$outputExt"
          val cmd = os.proc(
            "ffmpeg",
            "-safe", "0",
            "-f", "concat",
            "-i", list,
            s.codec.map(v => List("-c:v", v)),
            s.preset.map(v => List("-preset", v)),
            s.crf.map(v => List("-crf", v.toString)),
            s.bitRateV.map(b => List("-b:v", b)),
            s.bitRateA.map(b => List("-b:a", b)),
            if (s.soundVolumeOptimize) List("-af", f"volume=${-minVol}%.1fdB")
            else List(),
            output)
          cmd.call()
          // 後片付け
          cuts.map(os.remove)
          os.remove(list)
        }
      )
    }
  }

}
