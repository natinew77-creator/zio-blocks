package zio.blocks.schema

import zio.blocks.schema._

case class Repro(x: Int = 1)
object Repro {
  implicit val schema: Schema[Repro] = Schema.derived[Repro]
}
