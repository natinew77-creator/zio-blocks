package zio.blocks.typeid

object Subtyping {
  
  final case class Context(
    assumptions: Set[(TypeRepr, TypeRepr)] = Set.empty,
    depth: Int = 0,
    maxDepth: Int = 100
  ) {
    def assume(sub: TypeRepr, sup: TypeRepr): Context =
      copy(assumptions = assumptions + ((sub, sup)))
    
    def isAssumed(sub: TypeRepr, sup: TypeRepr): Boolean =
      assumptions.contains((sub, sup))
    
    def deeper: Context = copy(depth = depth + 1)
    def tooDeep: Boolean = depth >= maxDepth
  }
  
  def isSubtype(sub: TypeRepr, sup: TypeRepr)(implicit ctx: Context = Context()): Boolean = {
    if (ctx.tooDeep) return false // Fail safe
    
    if (ctx.isAssumed(sub, sup)) return true
    if (sub == sup) return true
    
    val subNorm = sub.dealias
    val supNorm = sup.dealias
    
    if (subNorm == supNorm) return true
    
    val newCtx = ctx.assume(sub, sup).deeper
    
    (subNorm, supNorm) match {
      case (TypeRepr.NothingType, _) => true
      case (_, TypeRepr.AnyType) => true
      case (_, TypeRepr.AnyKindType) => true // Assuming AnyKind is top
      
      // Intersection on RHS: A <: B & C => A <: B && A <: C
      case (_, TypeRepr.Intersection(ts)) =>
        ts.forall(t => isSubtype(subNorm, t)(newCtx))
        
      // Union on LHS: A | B <: C => A <: C && B <: C
      case (TypeRepr.Union(ts), _) =>
        ts.forall(t => isSubtype(t, supNorm)(newCtx))
        
      // Union on RHS: A <: B | C => A <: B || A <: C
      // This is weak checking. Real subtyping is more complex (A could be part B part C).
      // But for nominal types, simple check is often enough.
      // Standard Scala subtyping allows A <: B | C if A <: B OR A <: C.
      case (_, TypeRepr.Union(ts)) =>
        ts.exists(t => isSubtype(subNorm, t)(newCtx))
        
      // Intersection on LHS: A & B <: C => A <: C || B <: C
      // Actually A & B <: C if either A <: C or B <: C gives enough info.
      case (TypeRepr.Intersection(ts), _) =>
        ts.exists(t => isSubtype(t, supNorm)(newCtx))
        
      // Applied Types (Generics)
      case (TypeRepr.AppliedType(t1, args1), TypeRepr.AppliedType(t2, args2)) =>
        if (isSubtype(t1, t2)(newCtx)) {
           // Check args with variance
           // We need the variances of t1/t2.
           // Assuming t1 == t2 for now or compatible constructors.
           // Get TypeId from t1
           t1 match {
             case TypeRepr.Ref(id, _) => checkArgs(id.typeParams, args1, args2)(newCtx)
             case _ => args1 == args2 // fallback to invariance if unknown
           }
        } else false
      
      // Nominal References
      case (TypeRepr.Ref(id1, args1), TypeRepr.Ref(id2, args2)) =>
        val (base1, fullArgs1) = (id1.copy(args = Nil), id1.args ++ args1)
        val (base2, fullArgs2) = (id2.copy(args = Nil), id2.args ++ args2)
        
        if (base1 == base2) {
           checkArgs(base1.typeParams, fullArgs1, fullArgs2)(newCtx)
        } else {
           // Check parents
           id1.parents.exists(p => isSubtype(p, supNorm)(newCtx))
        }
        
      // Default fallback
      case _ => false
    }
  }
  
  def isEquivalent(a: TypeRepr, b: TypeRepr)(implicit ctx: Context = Context()): Boolean =
    isSubtype(a, b) && isSubtype(b, a)

  private def checkArgs(params: List[TypeParam], args1: List[TypeRepr], args2: List[TypeRepr])(implicit ctx: Context): Boolean = {
    if (args1.size != args2.size) return false
    params.zip(args1.zip(args2)).forall { case (param, (a1, a2)) =>
      (a1, a2) match {
        // Case: List[_] <: List[_]
        case (TypeRepr.Wildcard(b1), TypeRepr.Wildcard(b2)) =>
           (for { u1 <- b1.upper; u2 <- b2.upper } yield isSubtype(u1, u2)).getOrElse(true) &&
           (for { l1 <- b1.lower; l2 <- b2.lower } yield isSubtype(l2, l1)).getOrElse(true)

        // Case: List[Int] <: List[_]
        // Effectively: Does Int conform to the bounds of _?
        // Note: Wildcard ignores variance of the parameter position because it explicitly sets bounds.
        case (t, TypeRepr.Wildcard(bounds)) =>
          bounds.lower.forall(l => isSubtype(l, t)) && bounds.upper.forall(u => isSubtype(t, u))

        // Case: List[_] <: List[Int] (Usually false unless bounds are tight)
        case (TypeRepr.Wildcard(bounds), t) =>
           // ? <: Int if ? is tighter than Int? 
           // Generally implies bounds.upper <: Int (and logic for lower)
           bounds.upper.exists(u => isSubtype(u, t))

        case _ =>
          param.variance match {
            case Variance.Invariant => isEquivalent(a1, a2)
            case Variance.Covariant => isSubtype(a1, a2)
            case Variance.Contravariant => isSubtype(a2, a1)
          }
      }
    }
  }
}
