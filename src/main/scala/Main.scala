import java.awt.Color

import smile.plot.plot

import scala.collection.mutable.ListBuffer
import scala.io.Source


object Main {
  var data = new Array[Array[Double]](150)

  def setData(fileName : String): Int =  {
    val dataFileUri = this.getClass.getClassLoader.getResource(fileName).toURI.getPath
    val lines = Source.fromFile(dataFileUri).getLines.toArray
    data = Array.ofDim[Double](150, 4)
    for (i <- 0 until lines.length-1) {
      val lin = lines(i).split(",")
      for (j <- 0 until 4)  {
        data(i)(j) = lin(j).toDouble
      }
    }
    0
  }

  def moyenne(nbCar : Int) : Double = {
    var result : Double = 0
    for (i <- 0 to 149) {
      result += data(i)(nbCar)
    }
    result / 150
  }

  def variance(nbCar : Int) : Double = {
    var result : Double = 0
    val moy : Double = moyenne(nbCar)
    for (i <- 0 to 149) {
      result += scala.math.pow(data(i)(nbCar)-moy, 2)
    }
    result / 150
  }

  def ecartType(nbCar : Int) : Double = {
    scala.math.sqrt(variance(nbCar))
  }

  def covariance(x : Int, y : Int) : Double = {
    var result : Double = 0
    val avgX : Double = moyenne(x)
    val avgY : Double = moyenne(y)
    for (i <- 0 to 149) {
      result += ((data(i)(x)-avgX) * (data(i)(y)-avgY))
    }
    result / 150
  }

  def correlation(x : Int, y : Int) : Double = {
    covariance(x, y) / (ecartType(x)*ecartType(y))
  }

  def distance(xA : Double, yA : Double, xB : Double,  yB : Double): Double =  {
    scala.math.sqrt(scala.math.pow(xB-xA,2)+scala.math.pow(yB-yA, 2));  //distance
  }

  def centrer(nbCar1 : Int, nbCar2 : Int, group : ListBuffer[Int]) : Array[Double] = {
    var xTotal = 0.0
    var yTotal = 0.0
    val totalLength = group.length
    for (i <- 0 to totalLength) {
      xTotal += data(i)(nbCar1)
      yTotal += data(i)(nbCar2)
    }
    Array(xTotal/totalLength, yTotal/totalLength)
  }

  def kMeans(nbCar1 : Int, nbCar2 : Int) : Int = {
    //initialisation
    var totalTours = 0
    var hasChanged = true
    val randone = scala.util.Random.nextInt(150)
    var randtwo = randone
    while (randone == randtwo)  {
      randtwo = scala.util.Random.nextInt(150)
    }
    var randthree = randtwo
    while (randthree == randtwo || randthree == randone)  {
      randthree = scala.util.Random.nextInt(150)
    }
    var groupOne = new ListBuffer[Int]
    var groupTwo = new ListBuffer[Int]
    var groupThree = new ListBuffer[Int]
    var prevCentreOne = new Array[Double](2)
    var prevCentreTwo = new Array[Double](2)
    var prevCentreThree = new Array[Double](2)
    var centreOne = Array(data(randone)(nbCar1),data(randone)(nbCar2))  //coordonnées xy du centre
    var centreTwo = Array(data(randtwo)(nbCar1),data(randtwo)(nbCar2))
    var centreThree = Array(data(randthree)(nbCar1),data(randthree)(nbCar2))

    while (hasChanged)  {
      totalTours +=1
      var i = 0
      while (i < 150) {
        val distone = distance(centreOne(0), centreOne(1),data(i)(nbCar1),data(i)(nbCar2))
        val disttwo = distance(centreTwo(0), centreTwo(1),data(i)(nbCar1),data(i)(nbCar2))
        val distthree = distance(centreThree(0), centreThree(1),data(i)(nbCar1),data(i)(nbCar2))
        if (distone < disttwo && distone < distthree)  {
            groupOne.append(i)
        }
        else  {
          if (disttwo < distone && disttwo < distthree)  {
            groupTwo.append(i)
          }
          else  {
            groupThree.append(i)
          }
        }
        i+=1
      }
      centreOne = centrer(nbCar1,nbCar2,groupOne)
      centreTwo = centrer(nbCar1,nbCar2,groupTwo)
      centreThree = centrer(nbCar1,nbCar2,groupThree)
      print("\nx1 "+centreOne(0)+" y1 "+centreOne(1)+"\nx2 "+centreTwo(0)+" y2 "+
        centreTwo(1)+"\nx3 "+centreThree(0)+" y3 "+centreThree(1)+"\n")
      Thread.sleep(1000)
      if (prevCentreOne(0) == centreOne(0) && prevCentreOne(1) == centreOne(1) &&
        prevCentreTwo(0) == centreTwo(0) && prevCentreTwo(1) == centreTwo(1) &&
        prevCentreThree(0) == centreThree(0) && prevCentreThree(1) == centreThree(1)) {
        hasChanged = false
      }
        prevCentreOne = Array(centreOne(0), centreOne(1))
        prevCentreTwo = Array(centreTwo(0), centreTwo(1))
        prevCentreThree = Array(centreThree(0), centreThree(1))
      if(hasChanged)  {
        groupOne = new ListBuffer[Int]
        groupTwo = new ListBuffer[Int]
        groupThree = new ListBuffer[Int]
      }
    }

    val resultData = new Array[Array[Double]](153)
    val resultChars = new Array[Int](153)
    var k = 0
    for (i <- groupOne.indices) {
      resultData(k) = Array(data(groupOne(i))(nbCar1),data(groupOne(i))(nbCar2))
      resultChars(k) = 0
      k+=1
    }
    for (i <- groupTwo.indices) {
      resultData(k) = Array(data(groupTwo(i))(nbCar1),data(groupTwo(i))(nbCar2))
      resultChars(k) = 1
      k+=1
    }
    for (i <- groupThree.indices) {
      resultData(k) = Array(data(groupThree(i))(nbCar1),data(groupThree(i))(nbCar2))
      resultChars(k) = 2
      k+=1
    }
    resultData(k) = centreOne
    resultChars(k) = 3
    k+=1
    resultData(k) = centreTwo
    resultChars(k) = 4
    k+=1
    resultData(k) = centreThree
    resultChars(k) = 5
    print("K IS "+k)
    val window = plot(resultData, resultChars, Array('o', 'o', 'o', 'o','o','o'), Array(Color.RED, Color.BLUE, Color.CYAN,Color.RED, Color.BLUE, Color.CYAN))
    window.canvas.setAxisLabels("x","y")
    1
  }

  def main(args: Array[String]): Unit = {

    setData("iris.data")

    //0 slength 1 swidth 2 plength 3 pwidth

    print("Moyenne : "+moyenne( 0))
    print(" Variance : "+variance( 0))
    print(" Ecart-type : "+ecartType( 0))
    print(" Covariance : "+covariance(0, 1))
    print(" Coefficient de correlation : "+correlation(0, 3))

    kMeans( 0, 3)
  }
}