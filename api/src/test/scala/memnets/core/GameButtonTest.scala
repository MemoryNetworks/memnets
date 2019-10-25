package memnets.core

import memnets.core.impl.ModelImpl
import org.junit.Test
import org.scalatest.MustMatchers
import org.scalatestplus.junit.JUnitSuite

class GameButtonTest extends JUnitSuite with MustMatchers {
  @Test def pressed: Unit = {

    val bldr = ModelBuilder() { model =>
      }
    implicit val model = new ModelImpl(bldr, ModelConfig())

    val btns = GameButtons(2)
    val btn = btns(0)

    var pressCount = 0
    var releaseCount = 0
    btn.onPressed { sel =>
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

    btn.pressed = true

    assert(pressCount === 2)
    assert(releaseCount === 1)
  }
}
