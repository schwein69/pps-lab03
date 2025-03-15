package u03

object Lambda:

  // Define a type L that extends type L => L itself
  // Just one of the many magic tricks we will see of Scala
  trait L extends (L => L)

  val True: L = x => y => x
  val False: L = x => y => y
  val Not: L = b => b(False)(True)
  val And: L = b1 => b2 => b1(b2)(False)
  val Or: L = b1 => b2 => b1(True)(b2)

  val N0: L = s => z => z
  val N1: L = s => z => s(z)
  val N2: L = s => z => s(s(z))
  val N3: L = s => z => s(s(s(z)))
  val N4: L = s => z => s(s(s(s(z))))
  val IsZero: L = n => n(x => False)(True)
  val Succ: L = n => s => z => s(n(s)(z))
  val Plus: L = n => m => s => z => n(s(m(s)(z)))
  val Pred: L = n => n(g => h => h(g(Succ)))(u => N0)(u => u)
  val Subtract: L = n => m => m(Pred)(n)
end Lambda
