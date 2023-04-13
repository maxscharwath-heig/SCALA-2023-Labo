package Data

trait ProductService:
  type BrandName = String
  type ProductName = String

  def getPrice(product: ProductName, brand: BrandName): Double
  def getDefaultBrand(product: ProductName): BrandName

class ProductImpl extends ProductService:
  // Available beers and their prices
  private val beers = Map(
    "boxer"      -> 1.0,
    "farmer"     -> 1.0,
    "wittekop"   -> 2.0,
    "punkipa"    -> 3.0,
    "jackhammer" -> 3.0,
    "tenebreuse" -> 4.0
  )

  // Available croissants and their prices
  private val croissants = Map(
    "maison"  -> 2.0,
    "cailler" -> 2.0
  )

  def getPrice(product: ProductName, brand: BrandName): Double = {
    if (product.isEmpty()) {
      throw new IllegalArgumentException("Product name cannot be empty")
    }

    val brnd = if brand.isEmpty() then getDefaultBrand(product) else brand
    product match {
      case "biere" => beers.getOrElse(brnd, throw new IllegalArgumentException("Unknown beer brand"))
      case "croissant" => croissants.getOrElse(brnd, throw new IllegalArgumentException("Unknown croissant brand"))
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
end ProductImpl
