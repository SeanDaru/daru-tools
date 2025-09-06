//> using dep com.lihaoyi::os-lib:0.11.5
//> using dep org.apache.pdfbox:pdfbox:3.0.5

import org.apache.pdfbox.Loader
import org.apache.pdfbox.cos.*
import org.apache.pdfbox.pdmodel.{PDDocument, PDPage, PDResources}

import scala.jdk.CollectionConverters.*
import scala.util.Using
import java.util.logging.{Logger, Level}

object App {
  
  // PDFBoxの警告を抑制
  Logger.getLogger("org.apache.fontbox").setLevel(Level.SEVERE)
  Logger.getLogger("org.apache.pdfbox").setLevel(Level.SEVERE)
  
  @main
  def main(): Unit = {
    
    val inputDir = os.pwd / "input"
    val inputExt = "pdf"
    val outputDir = os.pwd / "output"
    val producer = "D's"
    os.makeDir.all(outputDir)

    val inputs = os.list(inputDir).filter(f => os.isFile(f) && f.ext == inputExt)
    inputs.foreach { pdf =>
      Using.resource(Loader.loadPDF(pdf.toIO)) { doc =>
        changeProducer(doc, producer)
        doc.getPages.forEach { p =>
          rmReportsWaterMark(p)
          rmReportsResources(p)
        }
        val filename = s"${pdf.baseName}.removed.pdf"
        val path = outputDir / filename
        doc.save(path.toIO)
      }
    }
  }

  private def changeProducer(doc: PDDocument,
                             newProducer: String): Unit = {
    doc.getDocumentInformation.setProducer("D's System.")
  }

  private def rmReportsWaterMark(p: PDPage): Unit = {
    val search = ".*FIELD REPORTS.*".r.regex
    val streams = p.getContentStreams.asScala.toList
    val filtered = streams.filterNot { s =>
      val txt = new String(s.toByteArray, "UTF-8")
      txt.matches(search)
    }
    p.setContents(filtered.asJava)
  }

  private def rmReportsResources(p: PDPage): Unit = {
    val nowR = p.getResources
    val newR = PDResources()
    nowR.getColorSpaceNames.forEach(n => newR.put(n, nowR.getColorSpace(n)))
    nowR.getPatternNames.forEach(n => newR.put(n, nowR.getPattern(n)))
    nowR.getShadingNames.forEach(n => newR.put(n, nowR.getShading(n)))
    nowR.getXObjectNames.forEach(n => newR.put(n, nowR.getXObject(n)))
    nowR.getPropertiesNames.forEach(n => newR.put(n, nowR.getProperties(n)))
    nowR.getExtGStateNames.forEach(n =>
      if (n.getName == "gs999") println("Skipped gs999")
      else newR.put(n, nowR.getExtGState(n))
    )
    nowR.getFontNames.forEach(n =>
      if (n.getName == "F999") println("Skipped F999")
      else newR.put(n, nowR.getFont(n))
    )
    p.setResources(newR)
  }

}
