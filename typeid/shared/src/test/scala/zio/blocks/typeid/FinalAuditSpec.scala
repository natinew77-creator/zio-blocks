package zio.blocks.typeid

import zio.test._
import zio.test.Assertion._

object FinalAuditSpec extends ZIOSpecDefault {
  def spec = suite("FinalAuditSpec")(
    test("Identity Parity Check: TypeId.of[List[Int]]") {
      val typeId       = TypeId.of[List[Int]]
      val stringOutput = typeId.toString
      // We expect normalized output. Scala 2 and 3 should ideally match,
      // but we need to see the exact string to confirm parity.
      // This test prints it for manual verification if needed, but asserts on structure.
      println(s"TypeId.of[List[Int]] output: $stringOutput")
      assert(typeId.show)(equalTo("scala.collection.immutable.List")) &&
      assert(typeId.typeParams.headOption.map(_.variance))(isSome(equalTo(Variance.Covariant))) &&
      assert(TypeId.of[List[Int]].asInstanceOf[Any])(not(equalTo(TypeId.of[List[String]].asInstanceOf[Any])))
    },
    test("Canonicalization Verification: TypeId.of[Runnable with Serializable] vs Serializable with Runnable") {
      val t1 = TypeId.of[Runnable with java.io.Serializable]
      val t2 = TypeId.of[java.io.Serializable with Runnable]

      // HashCode must be equal for map keys
      println(s"HashCode t1: ${t1.hashCode}, t2: ${t2.hashCode}")
      assert(t1.hashCode)(equalTo(t2.hashCode)) &&
      assert(t1.asInstanceOf[Any])(equalTo(t2.asInstanceOf[Any]))
    },
    test("Contravariance Logic check") {
      val t1 = TypeId.of[Any => Int]
      val t2 = TypeId.of[Int => Int]
      // Function1[-A, +B]
      // Any => Int <: Int => Int should be TRUE because Int <: Any (contravariant input)

      assert(t1.isSubtypeOf(t2))(isTrue) &&
      assert(t2.isSubtypeOf(t1))(isFalse)
    },
    test("Recursive Generic Identity Check: Map[String, List[Int]] vs Map[String, List[String]]") {
      val t1 = TypeId.of[Map[String, List[Int]]]
      val t2 = TypeId.of[Map[String, List[String]]]

      // These must NOT be equal. The args must be captured deep in the structure.
      // Map is applied to (String, List[Int])
      // List[Int] must be distinct from List[String]

      assert(t1.asInstanceOf[Any])(not(equalTo(t2.asInstanceOf[Any]))) &&
      assert(t1.args)(hasSize(equalTo(2))) // Map[K, V]
    }
  )
}
