package zio.blocks.schema

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.quoted._

private[schema] object CommonMacroOps {
  def fail(msg: String)(using q: Quotes): Nothing = {
    import q.reflect._

    report.errorAndAbort(msg, Position.ofMacroExpansion)
  }

  def typeArgs(using q: Quotes)(tpe: q.reflect.TypeRepr): List[q.reflect.TypeRepr] = {
    import q.reflect._

    tpe match {
      case AppliedType(_, args) => args.map(_.dealias)
      case _                    => Nil
    }
  }

  def isGenericTuple(using q: Quotes)(tpe: q.reflect.TypeRepr): Boolean = {
    import q.reflect._

    tpe <:< TypeRepr.of[Tuple] && !defn.isTupleClass(tpe.typeSymbol)
  }

  // Borrowed from an amazing work of Aleksander Rainko:
  // https://github.com/arainko/ducktape/blob/8d779f0303c23fd45815d3574467ffc321a8db2b/ducktape/src/main/scala/io/github/arainko/ducktape/internal/Structure.scala#L253-L270
  def genericTupleTypeArgs(using q: Quotes)(tpe: q.reflect.TypeRepr): List[q.reflect.TypeRepr] = {
    import q.reflect._

    def loop(tpr: TypeRepr): List[TypeRepr] = {
      val dealiased = tpr.dealias
      dealiased match {
        case AppliedType(base, List(head, tail))
            if base.typeSymbol == defn.TupleClass(2) || base.typeSymbol.name == "*:" =>
          // This is a *: (tuple cons) - extract head and recurse on tail
          head.dealias :: loop(tail)
        case AppliedType(base, args) if defn.isTupleClass(base.typeSymbol) =>
          // This is a TupleN class - return all type args
          args.map(_.dealias)
        case _ if dealiased =:= TypeRepr.of[EmptyTuple] =>
          // EmptyTuple - no more elements
          Nil
        case _ =>
          // Unknown structure - return empty
          Nil
      }
    }

    loop(tpe)
  }

  // Borrowed from an amazing work of Aleksander Rainko:
  // https://github.com/arainko/ducktape/blob/8d779f0303c23fd45815d3574467ffc321a8db2b/ducktape/src/main/scala/io/github/arainko/ducktape/internal/Structure.scala#L277-L295
  def normalizeGenericTuple(using q: Quotes)(typeArgs: List[q.reflect.TypeRepr]): q.reflect.TypeRepr = {
    import q.reflect._

    val size = typeArgs.size
    if (size > 0 && size <= 22) defn.TupleClass(size).typeRef.appliedTo(typeArgs)
    else {
      typeArgs.foldRight(TypeRepr.of[EmptyTuple]) {
        val tupleCons = TypeRepr.of[*:]
        (curr, acc) => tupleCons.appliedTo(List(curr, acc))
      }
    }
  }

  def isUnion(using q: Quotes)(tpe: q.reflect.TypeRepr): Boolean = {
    import q.reflect._

    tpe match {
      case _: OrType => true
      case _         => false
    }
  }

  def allUnionTypes(using q: Quotes)(tpe: q.reflect.TypeRepr): List[q.reflect.TypeRepr] = {
    import q.reflect._

    val seen  = new mutable.HashSet[TypeRepr]
    val types = new ListBuffer[TypeRepr]

    def loop(tpe: TypeRepr): Unit = tpe.dealias match {
      case OrType(left, right) => loop(left); loop(right)
      case dealiased           => if (seen.add(dealiased)) types.addOne(dealiased)
    }

    loop(tpe)
    types.toList
  }

  def directSubTypes(using q: Quotes)(tpe: q.reflect.TypeRepr): List[q.reflect.TypeRepr] = {
    import q.reflect._

    val tpeTypeSymbol = tpe.typeSymbol
    val subTypes      = tpeTypeSymbol.children.map { symbol =>
      if (symbol.isType) {
        val subtype = symbol.typeRef
        subtype.memberType(symbol.primaryConstructor) match {
          case _: MethodType                                              => subtype
          case PolyType(names, _, MethodType(_, _, AppliedType(base, _))) =>
            base.appliedTo(names.map {
              val binding = typeArgs(subtype.baseType(tpeTypeSymbol))
                .zip(typeArgs(tpe))
                .foldLeft(Map.empty[String, TypeRepr]) { case (binding, (childTypeArg, parentTypeArg)) =>
                  val childTypeSymbol = childTypeArg.typeSymbol
                  if (childTypeSymbol.isTypeParam) binding.updated(childTypeSymbol.name, parentTypeArg)
                  else binding
                }
              name =>
                binding.getOrElse(
                  name,
                  fail(s"Type parameter '$name' of '$symbol' can't be deduced from type arguments of '${tpe.show}'.")
                )
            })
          case _ => fail(s"Cannot resolve free type parameters for ADT cases with base '${tpe.show}'.")
        }
      } else Ref(symbol).tpe
    }
    if (tpe <:< TypeRepr.of[Option[?]]) subTypes.sortBy(_.typeSymbol.fullName)
    else subTypes
  }
}
