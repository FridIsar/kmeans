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

  def distance(xA : Double, yA : Double, xB : Double,  yB : Double): Double =  {
    scala.math.sqrt(scala.math.pow((xB-xA),2)+scala.math.pow((yB-yA), 2));  //distance
  }

  def centrer(data : Array[Array[Double]], var1 : Int, var2 : Int, group : ListBuffer[Int]) : Array[Double] = {
    var xTotal = 0.0;
    var yTotal = 0.0;
    var totalLength = group.length
    for (i <- 0 to totalLength) {
      xTotal += data(i)(var1);
      yTotal += data(i)(var2);
    }
    Array(xTotal/totalLength, yTotal/totalLength);
  }

  def kmeans(data : Array[Array[Double]], var1 : Int, var2 : Int) : Int = {
    //initialisation
    var totalTours = 0;
    var hasChanged = true
    var randone = scala.util.Random.nextInt(150);
    var randtwo = randone
    while (randone == randtwo)  {
      randtwo = scala.util.Random.nextInt(150);
    }
    var groupone = new ListBuffer[Int];
    var grouptwo = new ListBuffer[Int];
    var prevcentreone = new Array[Double](2);
    var prevcentretwo = new Array[Double](2);
    var centreone = Array(data(randone)(var1),data(randone)(var2))  //coordonnées xy du centre
    var centretwo = Array(data(randtwo)(var1),data(randtwo)(var2))
    while (hasChanged)  {
      totalTours +=1;
      var i = 0;
      while (i < 150) {
        var distone = distance(centreone(0), centreone(1),data(i)(var1),data(i)(var2))
        var disttwo = distance(centretwo(0), centretwo(1),data(i)(var1),data(i)(var2))
        if (distone > disttwo)  {
          groupone.append(i)
        }
        else  {
          grouptwo.append(i);
        }
        i+=1
      }
      centreone = centrer(data,var1,var2,groupone);
      centretwo = centrer(data,var1,var2,grouptwo);
      if (prevcentreone.equals(centreone) && prevcentretwo.equals(centretwo)) {
        hasChanged = false;
      }
      prevcentreone = centreone.clone()
      prevcentretwo = centretwo.clone()
      groupone = new ListBuffer[Int];
      grouptwo = new ListBuffer[Int];
    }
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
    print("\nend")
    //print(" Distance : "+distance(x2, 0,1,53,119))
    //plot(data, '*', Array(Color.RED, Color.BLUE, Color.CYAN))
    //window.canvas.setAxisLabels(attributes.map(_.getName).slice(0, 2): _*) modif post creation
  }
}