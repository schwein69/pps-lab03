package u03

import org.junit.*
import org.junit.Assert.*

class BooleanLambdaTest extends LambdaTest:
  import Lambda.*

  @Test
  def testTrue() =
    assertTrue(toBoolean(True))

  @Test
  def testFalse() =
    assertFalse(toBoolean(False))

  @Test
  def testNot() =
    assertTrue(toBoolean(Not(False)))
    assertFalse(toBoolean(Not(True)))

  @Test
  def testOr() =
    assertFalse(toBoolean(Or(False)(False)))
    assertTrue(toBoolean(Or(False)(True)))
    assertTrue(toBoolean(Or(True)(False)))
    assertTrue(toBoolean(Or(True)(True)))

  @Test
  def testAnd() =
    assertFalse(toBoolean(And(False)(False)))
    assertFalse(toBoolean(And(False)(True)))
    assertFalse(toBoolean(And(True)(False)))
    assertTrue(toBoolean(And(True)(True)))
end BooleanLambdaTest
