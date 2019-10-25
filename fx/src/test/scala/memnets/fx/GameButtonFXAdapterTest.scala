package memnets.fx

import memnets.core._
import org.junit.Test
import org.scalatest.MustMatchers
import org.scalatestplus.junit.JUnitSuite

class GameButtonFXAdapterTest extends JUnitSuite with MustMatchers {
  @Test def press: Unit = {
    var built = false
    val bldr = ModelBuilder() { implicit model =>
      built = true
      val btns = GameButtons(2)
      val btn = btns(0)

      val fx = new GameButtonFXAdapter(btn)
      var pressCount = 0
      var releaseCount = 0
      fx.pressed ==> { sel =>
        if (sel)
          pressCount += 1
        else
          releaseCount += 1
      }

      assert(btn.pressed === false)
      assert(pressCount === 0)
      assert(releaseCount === 0)

      btn.pressed = true

      assert(pressCount === 1)
      assert(releaseCount === 0)

      btn.pressed = false

      assert(pressCount === 1)
      assert(releaseCount === 1)

      // switch to fx
      fx.pressed.value = true

      assert(pressCount === 2)
      assert(releaseCount === 1)

      fx.pressed.value = false

      assert(pressCount === 2)
      assert(releaseCount === 2)
    }

    bldr.build()

    assert(built === true)
  }
}
