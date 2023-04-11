package Data

trait ProductService:
  type BrandName = String
  type ProductName = String

  def getPrice(product: ProductName, brand: BrandName): Double
  def getDefaultBrand(product: ProductName): BrandName

class ProductImpl extends ProductService:
  def getPrice(product: ProductName, brand: String): Double = {
    product match {
      case "biere" =>
        brand match {
          case "boxer"      => 1.0
          case "farmer"     => 1.0
          case "wittekop"   => 2.0
          case "punkipa"    => 3.0
          case "jackhammer" => 3.0
          case "tenebreuse" => 4.0
          case _            => 0.0 // throw
        }
      case "croissant" =>
        brand match {
          case "maison"  => 2.0
          case "cailler" => 2.0
          case _         => 0.0 // throw
        }
      case _ => 0.0 // throw
    }
  }
  def getDefaultBrand(product: ProductName): BrandName = {
    product match {
      case "biere"     => "boxer"
      case "croissant" => "maison"
      case _           => ""
    }
  }
end ProductImpl
