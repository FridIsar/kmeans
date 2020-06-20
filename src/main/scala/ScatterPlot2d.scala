import java.awt.Color

import smile.data.{Attribute, AttributeDataset, NominalAttribute, NumericAttribute}
import smile.plot.plot
import smile.read

object ScatterPlot2d extends App {
  val attributes = new Array[Attribute](4)

  attributes(0) = new NumericAttribute("sepal length in cm")
  attributes(1) = new NumericAttribute("sepal width in cm")
  attributes(2) = new NumericAttribute("petal length in cm")
  attributes(3) = new NumericAttribute("petal width in cm")

  val label = new NominalAttribute("class")

  val dataFileUri = this.getClass.getClassLoader.getResource("iris.data").toURI.getPath
  val data: AttributeDataset = read.csv(dataFileUri, attributes = attributes, response = Some((label, 4)))

  val x2 = data.x().map(_.slice(0, 2))  // Takes first two columns
  val window = plot(x2, data.labels(), Array('*', '+', 'o'), Array(Color.RED, Color.BLUE, Color.CYAN))

  window.canvas.setAxisLabels(attributes.map(_.getName).slice(0, 2): _*)
}
