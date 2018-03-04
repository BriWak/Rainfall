package app

object Rain extends App{

  def mean(town: String, strng: String): Double = {
    val getTown = s"($town:.+)".r.findAllIn(strng).mkString
    val getRain = "[0-9.]+".r.findAllIn(getTown).map(_.toDouble).toList

    getTown.length match {
      case 0 => -1
      case _ => getRain.sum / getRain.length
    }
  }

  def variance(town: String, strng: String): Double = {
    val getTown = s"($town:.+)".r.findAllIn(strng).mkString
    val getRain = "[0-9.]+".r.findAllIn(getTown).map(_.toDouble).toList
    val meanest = getRain.sum / getRain.length
    val diff = getRain.map(x => x - meanest)
    val varest = diff.map(x => x * x)

    getTown.length match {
      case 0 => -1
      case _ => varest.sum / varest.length
    }
  }

}
