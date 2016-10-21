package monad

import org.scalatest.{FlatSpec, Matchers}

class MonadFeatureTest extends FlatSpec with Matchers {
  it should "have instances for Option" in {
    val optionMonad = implicitly[Monad[Option]]

    val anOption: Option[Int] = optionMonad.unit(1)

    anOption shouldBe Some(1)

    optionMonad.flatMap(optionMonad.unit(2))(a => Some(a + 1)) shouldBe Some(3)
    optionMonad.map(anOption)(_ + 1) shouldBe Some(2)
    optionMonad.join(Some(anOption)) shouldBe anOption
  }
}
