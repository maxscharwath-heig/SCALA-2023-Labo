// SCALA - Labo 4
// Nicolas Crausaz & Maxime Scharwath

package Data

import scala.concurrent.duration._
import scala.concurrent.Future

trait ProductService:
  type BrandName = String
  type ProductName = String

  def getPrice(product: ProductName, brand: Option[BrandName]): Double
  def getDefaultBrand(product: ProductName): BrandName
  def startPreparation(product: ProductName, brand: Option[BrandName]): Future[Unit]

class ProductImpl extends ProductService:
  // Available beers and their prices
  private val beers = Map(
    "boxer"      -> (1.0, Duration(1, SECONDS)),
    "farmer"     -> (1.0, Duration(1.5, SECONDS)),
    "wittekop"   -> (2.0, Duration(1.5, SECONDS)),
    "punkipa"    -> (3.0, Duration(2, SECONDS)),
    "jackhammer" -> (3.0, Duration(2, SECONDS)),
    "tenebreuse" -> (4.0, Duration(3, SECONDS))
  )

  // Available croissants and their prices
  private val croissants = Map(
    "maison"  -> (2.0, Duration(1, SECONDS)),
    "cailler" -> (2.0, Duration(2, SECONDS))
  )

  def getPrice(product: ProductName, brand: Option[BrandName]): Double = {
    if (product.isEmpty()) {
      throw new IllegalArgumentException("Product name cannot be empty")
    }

    val brnd = if brand.isEmpty then getDefaultBrand(product) else brand.get
    product match {
      case "biere" => beers.getOrElse(brnd, throw new IllegalArgumentException("Unknown beer brand"))._1
      case "croissant" => croissants.getOrElse(brnd, throw new IllegalArgumentException("Unknown croissant brand"))._1
      case _ => throw new IllegalArgumentException("Unknown product")
    }
  }

  def getDefaultBrand(product: ProductName): BrandName = {
    product match {
      case "biere"     => "boxer"
      case "croissant" => "maison"
      case _           => throw new IllegalArgumentException("Unknown product")
    }
  }

  def startPreparation(product: ProductName, brand: Option[BrandName]): Future[Unit] = {
    val brnd = if brand.isEmpty then getDefaultBrand(product) else brand.get

    val duration = product match {
      case "biere" => beers.getOrElse(brnd, throw new IllegalArgumentException("Unknown beer brand"))._2
      case "croissant" => croissants.getOrElse(brnd, throw new IllegalArgumentException("Unknown croissant brand"))._2
      case _ => throw new IllegalArgumentException("Unknown product")
    }

    Utils.FutureOps.randomSchedule(duration, Duration(0.5, SECONDS), 0.5)
  }
end ProductImpl
