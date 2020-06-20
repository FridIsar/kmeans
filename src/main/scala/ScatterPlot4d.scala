import java.awt.Color

import smile.data.{Attribute, AttributeDataset, NominalAttribute, NumericAttribute}
import smile.plot.plot
import smile.read

object ScatterPlot4d extends App {
  val attributes = new Array[Attribute](4)

  attributes(0) = new NumericAttribute("sepal length in cm")
  attributes(1) = new NumericAttribute("sepal width in cm")
  attributes(2) = new NumericAttribute("petal length in cm")
  attributes(3) = new NumericAttribute("petal width in cm")

  val label = new NominalAttribute("class")

  val dataFileUri = this.getClass.getClassLoader.getResource("iris.data").toURI.getPath
  val data: AttributeDataset = read.csv(dataFileUri, attributes = attributes, response = Some((label, 4)))

  // This plots all the attributes pairs.
  plot(data, '*', Array(Color.RED, Color.BLUE, Color.CYAN))
}
