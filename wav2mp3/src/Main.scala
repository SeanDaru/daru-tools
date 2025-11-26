//> using dep com.lihaoyi::os-lib:0.11.4
//> using dep ws.schild:jave-core:3.5.0
//> using dep ws.schild:jave-all-deps:3.5.0

import os.Path
import ws.schild.jave.encode.*
import ws.schild.jave.{Encoder, MultimediaObject}

@main
def main(args: String*): Unit = {
  val inputDir: Path = os.pwd / "input"
  val inputExt = "wav"
  val outputDir: Path = os.pwd / "output"
  val outputExt = "mp3"

  val inputs = os.list(inputDir).filter(f => os.isFile(f) && f.ext == inputExt)
  inputs.foreach { wav =>
    val outputFile = outputDir / s"${wav.baseName}.$outputExt"
    os.makeDir.all(outputDir)

    val audioAttributes = new AudioAttributes()
    audioAttributes.setCodec("libmp3lame")
    audioAttributes.setBitRate(256000)
    audioAttributes.setChannels(1)
    audioAttributes.setSamplingRate(44100)

    val encodingAttributes = new EncodingAttributes()
    encodingAttributes.setAudioAttributes(audioAttributes)

    try {
      val encoder = new Encoder()
      encoder.encode(
        new MultimediaObject(wav.toNIO.toFile),
        outputFile.toNIO.toFile,
        encodingAttributes
      )
      println(s"Converted ${wav.baseName} to MP3")
    } catch {
      case e: Exception =>
        System.err.println(s"Error converting ${wav.baseName}: ${e.getMessage}")
    }
  }
}
