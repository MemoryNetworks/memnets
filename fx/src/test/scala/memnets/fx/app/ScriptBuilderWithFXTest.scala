package memnets.fx.app

import javax.script.ScriptException
import memnets.core.ModelConfig
import memnets.model._
import memnets.utils._
import org.junit._
import org.scalatest.MustMatchers
import org.scalatestplus.junit.JUnitSuite

/**
 * NOTE: cannot have following line in this test (but ok in editor)
 * config.useDouble = false
 * b/c it changes model
 */
class ScriptBuilderWithFXTest extends JUnitSuite with MustMatchers with Logging {
  // need scala compiler in global libs
  // this line will break test b/c swaps models...
  val badScript =
    """
      |  val swta = SoftWTAZZZ(;
      |
    """.stripMargin
  @Test def scriptBuilder: Unit = {
    val script = "/test-script.mnet".readStream()
    val scrtBldr = new ScriptBuilderWithFX()
    logger.debug("offset: " + scrtBldr.headerOffSet)
    val cfg = ModelConfig()
    var builder = scrtBldr { script }
    logger.debug("b4 initModel")
    val model = builder.build(cfg)
    logger.debug("after initModel")
    val nSize = model.system.variables.length
    model.name = "bogus"
    assert(nSize > 0)
    logger.debug("b4 2nd initModel")
    val model2 = builder.build(cfg)
    assert(model2.name !== model.name)
    assert(model2.system.variables.length === nSize)
    logger.debug("after 2nd initModel")

    // now test multiple calls
    logger.debug("2nd compile")
    builder = scrtBldr { script }
    builder.build(cfg)

    logger.debug("bad script")
    builder = scrtBldr { badScript }
    assertThrows[ScriptException](builder.build(cfg))

    logger.debug("3rd compile")
    builder = scrtBldr { script }
    builder.build(cfg)

    logger.debug("bad script 2")
    builder = scrtBldr { badScript }
    assertThrows[ScriptException](builder.build(cfg))

    logger.debug("4th compile")
    builder = scrtBldr { script }
    builder.build(cfg)

    logger.debug("test row, col")
    var hadError = false
    try {
      builder = scrtBldr { "val y = Y2()" }
      builder.build(cfg)
    } catch {
      case se: ScriptException =>
        hadError = true
        val (row, col) = scrtBldr.scriptErrorToPos(se)
        assert(row === 0)
        assert(col === 8)
    }
    assert(hadError)

    hadError = false
    try {
      builder = scrtBldr {
        """val y = Y()
          |
          |val y2 = Y2
          |""".stripMargin
      }
      builder.build(cfg)
    } catch {
      case se: ScriptException =>
        hadError = true
        val (row, col) = scrtBldr.scriptErrorToPos(se)
        assert(row === 2)
        assert(col === 9)
    }
    assert(hadError)

  }
}
