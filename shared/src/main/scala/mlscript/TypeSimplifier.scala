package mlscript

import mlscript.utils._
import mlscript.utils.shorthands._

import scala.collection.mutable.{Map => MutMap}

trait TypeSimplifier { self: Typer =>
  
  abstract class SimplifyPipeline {
    def debugOutput(msg: => Str): Unit
    
    def apply(st: ST)(implicit ctx: Ctx): ST = {
      def unifyTypeVariable(ty: SimpleType)(implicit ctx: Ctx): SimpleType = trace(s"UTY: ${ty}"){
        val tvMap: MutMap[TV, ST] = MutMap.empty
        def process(ty: SimpleType): SimpleType = {
          ty match {
            // For each type variable that's not mapped. Find all the type variables and concrete types
            // It's been unified with. Create a new type variable all the unified type variables to it.
            // Update the set of unified concrete types of the new type variable with the found concrete types
            case tv: TypeVariable =>
              tvMap.getOrElse(tv, {
                // find unified type variables and concrete types
                val (tvs, sts) = tv.unifiedWith.partitionMap {
                  case tv: TypeVariable => L(tv)
                  case st => R(st)
                }

                debugOutput(s"Unifying: ${tv} with ${tvs} and ${sts}")

                // If the type variable is unified with multiple type variables
                // create a new type variable and map all of them to it
                val nv = if (tvs.nonEmpty) {
                  val nv = freshVar(tv.prov, tv.nameHint)(tv.level)
                  debugOutput(s"Mapped: ${(tv :: tvs).mkString(", ")} to ${nv}")
                  (tv :: tvs).foreach(tvMap.put(_, nv))
                  nv
                } else {
                  tvMap.put(tv, tv)
                  tv
                }

                nv.uniConcreteTypes = sts.map(process).toSet
                debugOutput(s"Mapped: ${sts} to ${nv.uniConcreteTypes}")
                nv
              })
            case t @ TypeBounds(lb, ub) => TypeBounds(process(lb), process(ub))(t.prov)
            case t @ FunctionType(l, r) => FunctionType(process(l), process(r))(t.prov)
            case t @ ComposedType(p, l, r) => ComposedType(p, process(l), process(r))(t.prov)
            case t @ RecordType(fs) => RecordType(fs.mapValues(process))(t.prov)
            case t @ TupleType(fs) => TupleType(fs.mapValues(process))(t.prov)
            case e @ ExtrType(_) => e
            case p @ ProvType(und) => ProvType(process(und))(p.prov)
            case _: TraitTag => ty
            case tr @ TypeRef(d, ts) => TypeRef(d, ts.map(process))(tr.prov)
          }
        }
        process(ty)
      }(res => s"STY: $res")

      // if type variable is unified with only one concrete type replace it with the concrete type
      def simplifyTypeVariable(ty: SimpleType)(implicit ctx: Ctx): SimpleType = {
        val tvMap: MutMap[TV, ST] = MutMap.empty
        def process(ty: SimpleType): SimpleType = trace(s"S: $ty ${ty.getClass.getSimpleName}"){
          ty match {
            case tv: TV => tvMap.getOrElseUpdate(tv, {
              // temporarily map type to prevent infinite recursion
              // this value will be overwritten by the value returned by this function
              tvMap += ((tv, tv))
              tv.uniConcreteTypes = tv.uniConcreteTypes.map(process)

              tv.uniConcreteTypes.toSeq match {
                case Seq(ty) =>
                  println(s"Simplified: $tv to $ty")
                  ty
                case _ => tv
              }
            })
            case t @ TypeBounds(lb, ub) => TypeBounds(process(lb), process(ub))(t.prov)
            case t @ FunctionType(l, r) => FunctionType(process(l), process(r))(t.prov)
            case t @ ComposedType(p, l, r) => ComposedType(p, process(l), process(r))(t.prov)
            case t @ RecordType(fs) => RecordType(fs.mapValues(process))(t.prov)
            case t @ TupleType(fs) => TupleType(fs.mapValues(process))(t.prov)
            case e @ ExtrType(_) => e
            case p @ ProvType(und) => ProvType(process(und))(p.prov)
            case _: TraitTag => ty
            case tr @ TypeRef(d, ts) => TypeRef(d, ts.map(process))(tr.prov)
          }
        }(res => s"S: $res")

        process(ty)
      }

      var cur = st
      cur = unifyTypeVariable(st)
      cur = simplifyTypeVariable(cur)
      debugOutput(s"⬤ Unified: ${cur}")
      debugOutput(s" where: ${cur.showUnified}")

      cur
    }
  }
}
