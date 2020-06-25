import java.awt.Color

import smile.data.{Attribute, NominalAttribute, NumericAttribute}
import smile.plot.plot

import scala.collection.mutable.ListBuffer
import scala.io.Source


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
    var totalLength = group.length-1 //MOINS 1?
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
    var randthree = randtwo
    while (randthree == randtwo || randthree == randone)  {
      randthree = scala.util.Random.nextInt(150);
    }
    var groupone = new ListBuffer[Int];
    var grouptwo = new ListBuffer[Int];
    var groupthree = new ListBuffer[Int];
    var prevcentreone = new Array[Double](2);
    var prevcentretwo = new Array[Double](2);
    var prevcentrethree = new Array[Double](2);
    var centreone = Array(data(randone)(var1),data(randone)(var2))  //coordonnées xy du centre
    var centretwo = Array(data(randtwo)(var1),data(randtwo)(var2))
    var centrethree = Array(data(randthree)(var1),data(randthree)(var2))
    while (hasChanged)  {
      totalTours +=1;
      var i = 0;
      while (i < 150) {
        var distone = distance(centreone(0), centreone(1),data(i)(var1),data(i)(var2))
        var disttwo = distance(centretwo(0), centretwo(1),data(i)(var1),data(i)(var2))
        var distthree = distance(centrethree(0), centrethree(1),data(i)(var1),data(i)(var2))
        if (distone < disttwo && distone < distthree)  {
            groupone.append(i)
        }
        else  {
          if (disttwo < distone && disttwo < distthree)  {
            grouptwo.append(i)
          }
          else  {
            groupthree.append(i)
          }
        }
        i+=1
      }
      centreone = centrer(data,var1,var2,groupone);
      centretwo = centrer(data,var1,var2,grouptwo);
      centrethree = centrer(data,var1,var2,groupthree);
      print("\nx1 "+centreone(0)+" y1 "+centreone(1)+"\nx2 "+centretwo(0)+" y2 "+
        centretwo(1)+"\nx3 "+centrethree(0)+" y3 "+centrethree(1)+"\n")
      Thread.sleep(1000)
      if (prevcentreone(0) == centreone(0) && prevcentreone(1) == centreone(1) &&
        prevcentretwo(0) == centretwo(0) && prevcentretwo(1) == centretwo(1) &&
        prevcentrethree(0) == centrethree(0) && prevcentrethree(1) == centrethree(1)) {
        hasChanged = false;
      }
      if (totalTours % 2 == 0) { //un tour sur 2 pour eviter la boucle infinie
        prevcentreone = centreone.clone()
        prevcentretwo = centretwo.clone()
        prevcentrethree = centrethree.clone()
      }
      if(hasChanged)  {
        groupone = new ListBuffer[Int];
        grouptwo = new ListBuffer[Int];
        groupthree = new ListBuffer[Int];
      }
    }

    var resultData = new Array[Array[Double]](153)
    var resultChars = new Array[Int](153)
    var k = 0;
    for (i <- 0 to groupone.length-1) {
      resultData(k) = Array(data(groupone(i))(var1),data(groupone(i))(var2))
      resultChars(k) = 0;
      k+=1;
    }
    for (i <- 0 to grouptwo.length-1) {
      resultData(k) = Array(data(grouptwo(i))(var1),data(grouptwo(i))(var2))
      resultChars(k) = 1;
      k+=1;
    }
    for (i <- 0 to groupthree.length-1) {
      resultData(k) = Array(data(groupthree(i))(var1),data(groupthree(i))(var2))
      resultChars(k) = 2;
      k+=1;
    }
    resultData(k) = centreone;
    resultChars(k) = 3;
    k+=1;
    resultData(k) = centretwo;
    resultChars(k) = 4;
    k+=1;
    resultData(k) = centrethree;
    resultChars(k) = 5;
    print("K IS "+k)
    val window = plot(resultData, resultChars, Array('o', 'o', 'o', '*','*','*'), Array(Color.RED, Color.BLUE, Color.CYAN,Color.RED, Color.BLUE, Color.CYAN))
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
    val lines = Source.fromFile(dataFileUri).getLines.toArray;
    var data : Array[Array[Double]] = Array.ofDim[Double](150, 4);

    for (i <- 0 until lines.length-1) {
      var lin = lines(i).split(",");
      for (j <- 0 until 3)  {
        data(i)(j) = lin(j).toDouble;
      }
    }

    print("Moyenne : "+moyenne(data, 0)) //0 slength 1 swidth 2 plength 3 pwidth
    print(" Variance : "+variance(data, 0))
    print(" Ecart-type : "+ecartType(data, 0))
    print(" Covariance : "+covariance(data, 0, 1))
    print(" Coefficient de correlation : "+correlation(data, 1, 3))

    kmeans(data, 0, 1)
    print("\n end")
  }
}