package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    val bSquared = Var(b() * b())
    val minus4ac = Var(-4 * a() * c())
    Var(bSquared() + minus4ac())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Var {
      delta() match {
        case d if d < 0 => Set()
        case d if d == 0 => Set(-b() / (2 * a()))
        case _ =>
          val minusB = Var(-b())
          val twoA = Var(2 * a())
          val sqrtDelta = Var(Math.sqrt(delta()))
          Set((minusB() - sqrtDelta()) / twoA(), (minusB() + sqrtDelta()) / twoA())
      }
    }
  }
}
