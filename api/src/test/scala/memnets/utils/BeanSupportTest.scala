package memnets.utils

import org.junit.Test
import org.scalatest.MustMatchers
import org.scalatestplus.junit.JUnitSuite

class BeanSupportTest extends JUnitSuite with MustMatchers {
  @Test def dirty: Unit = {

    object Subject extends BeanSupport with DirtySupport

    assert(Subject.dirty === false)

    var dirtyCount = 0
    val sub = Subject.onDirty = {
      dirtyCount += 1
    }

    // initialize call
    assert(dirtyCount === 1)

    Subject.dirty = true

    assert(dirtyCount === 2)

    Subject.dirty = false
    assert(dirtyCount === 2)

    Subject.dirty = true
    assert(dirtyCount === 3)

    sub.cancel()

    Subject.dirty = false
    Subject.dirty = true
    assert(dirtyCount === 3)
  }
}
