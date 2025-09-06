//> using dep com.lihaoyi::os-lib:0.11.5

object App {

  private val reports = "/usr/local/bin/reports"
  private val resourceDir = os.pwd / "resource"
  private val outputDir = os.pwd / "output"
  
  @main
  def main(): Unit = {
    os.makeDir.all(outputDir)
    render(sampleJson1, "sample1")
  }
  
  val sampleJson1: String = {
    // language=json
    s"""{
       |  "settings": {
       |    "template-root": "$resourceDir"
       |  },
       |  "resources": {
       |    "font": {
       |      "/RictyDiminished": {
       |        "src": "RictyDiminished-Regular.ttf",
       |        "embed": true,
       |        "subset": true
       |      }
       |    }
       |  },
       |  "template": "sample1.template.pdf",
       |  "context": {
       |    "text1": "Hello",
       |    "text2": {
       |      "value": "Reports.",
       |      "color": "cmyk(100,0,0,0)"
       |    },
       |    "table": [
       |      ["Table.0.0", "Table.0.1"], [
       |        "Table.1.0", {
       |          "value": "Table.1.1",
       |          "color": "cmyk(0,100,0,0)"
       |        }
       |      ]
       |    ]
       |  }
       |}
       |""".stripMargin
  }

  private def render(json: String,
                     name: String): Unit = {
    val j = os.temp(contents = json)
    os.proc(reports, "render", "-", outputDir / s"$name.pdf").call(stdin = j)
    os.remove(j)
  }

}
