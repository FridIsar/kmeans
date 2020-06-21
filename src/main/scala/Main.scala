import java.awt.Color

import smile.data.{Attribute, AttributeDataset, NominalAttribute, NumericAttribute}
import smile.plot.plot
import smile.read

import scala.collection.mutable.ListBuffer

object Main {
  def moyenne(data : Array[Array[Double]], nbCar : Int) : Double = {
    var result : Double = 0
    for (i <- 0 to 149) {
      result += data(i)(nbCar)
    }
    result / 150
  }

  def variance(data : Array[Array[Double]], nbCar : Int) : Double = {
    var result : Double = 0
    var moy : Double = moyenne(data, nbCar)
    for (i <- 0 to 149) {
      result += scala.math.pow(data(i)(nbCar)-moy, 2)
    }
    result / 150
  }

  def ecartType(data : Array[Array[Double]], nbCar : Int) : Double = {
    scala.math.sqrt(variance(data, nbCar))
  }

  def covariance(data : Array[Array[Double]], x : Int, y : Int) : Double = {
    var result : Double = 0
    var avgX : Double = moyenne(data, x)
    var avgY : Double = moyenne(data, y)
    for (i <- 0 to 149) {
      result += ((data(i)(x)-avgX) * (data(i)(y)-avgY))
    }
    result / 150
  }

  def correlation(data : Array[Array[Double]], x : Int, y : Int) : Double = {
    covariance(data, x, y) / (ecartType(data,x)*ecartType(data,y))
  }

  def distance(data : Array[Array[Double]], var1 : Int, var2 : Int, lineA : Int,  lineB : Int): Double =  {
    var xA = data(lineA)(var1);
    var yA = data(lineA)(var2);
    var xB = data(lineB)(var1);
    var yB = data(lineB)(var2);
    scala.math.sqrt(scala.math.pow((xB-xA),2)+scala.math.pow((yB-yA), 2));  //distance
  }

  def kmeans(data : Array[Array[Double]], var1 : Int, var2 : Int) : Int = {
    var randone = scala.util.Random.nextInt(150);
    var randtwo = randone
    while (randone == randtwo)  {
      randtwo = scala.util.Random.nextInt(150);
    }
    var groupone = new ListBuffer[Int];
    var grouptwo = new ListBuffer[Int];

    var i = 0;
    while (i < 150) {
      var distone = distance(data,var1,var2,randone,i);
      var disttwo = distance(data,var1,var2,randtwo,i);
      if (distone > disttwo)  {
        print(i + "est supérieur\n")
        groupone.append(i)
      }
      else  {
        print("l'inverse\n")
        grouptwo.append(i);
      }
      i+=1
    }
    print("Listbuffers "+groupone+"\n deux "+grouptwo)
  1
  }

  def main(args: Array[String]): Unit = {
    val attributes = new Array[Attribute](4)

    attributes(0) = new NumericAttribute("sepal length in cm")
    attributes(1) = new NumericAttribute("sepal width in cm")
    attributes(2) = new NumericAttribute("petal length in cm")
    attributes(3) = new NumericAttribute("petal width in cm")

    val label = new NominalAttribute("class")

    val dataFileUri = this.getClass.getClassLoader.getResource("iris.data").toURI.getPath
    val data: AttributeDataset = read.csv(dataFileUri, attributes = attributes, response = Some((label, 4)))
    print(data.x().map(_.slice(0, 2)))
    val x2 = data.x().map(_.slice(0, 2))  // Takes first two columns
    //val window = plot(x2, data.labels(), Array('*', '+', 'o'), Array(Color.RED, Color.BLUE, Color.CYAN)) //CREATION
    //setosa rouge, versicolor bleu, virginica cyan

    print("Moyenne : "+moyenne(x2, 0)) //0 slength 1 swidth 2 plength 3 pwidth
    print(" Variance : "+variance(x2, 0))
    print(" Ecart-type : "+ecartType(x2, 0))
    print(" Covariance : "+covariance(x2, 0, 1))
    print(" Coefficient de correlation : "+correlation(x2, 0, 1))

    kmeans(x2, 0, 1)

    //print(" Distance : "+distance(x2, 0,1,53,119))
    //plot(data, '*', Array(Color.RED, Color.BLUE, Color.CYAN))
    //window.canvas.setAxisLabels(attributes.map(_.getName).slice(0, 2): _*) modif post creation
  }
}