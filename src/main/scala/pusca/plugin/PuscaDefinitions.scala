package pusca.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.symtab.Flags._
import scala.annotation.tailrec
import pusca.Internal._

/** Common functions used in the pusca plugin. */
trait PuscaDefinitions {
  val global: Global
  import global._

  /** The pusca annotations. */
  protected object Annotation {
    def apply(annotation: Symbol): AnnotationInfo = AnnotationInfo(annotation.tpe, Nil, Nil)

    val pure = definitions.getClass("pusca.pure")
    val impure = definitions.getClass("pusca.impure")
    val impureIf = definitions.getClass("pusca.impureIf")
    val impureIfReturnType = definitions.getClass("pusca.impureIfReturnType")
    val declarePure = definitions.getClass("pusca.declarePure")
    val allForMethod = pure :: impure :: impureIf :: impureIfReturnType :: declarePure :: Nil

    val sideEffectFree = definitions.getClass("pusca.sideEffectFree")
    val allForTypes = sideEffectFree :: Nil

    //TODO
    //    val returnedInfere = definitions.getClass("pusca.Internal.returnedInfere")
    //    val internalForTypes = returnedInfere :: Nil
  }
  protected object PuscaMethods {
    lazy val puscaPackage = definitions.getModule("pusca")
    lazy val puscaInternalObject = definitions.getMember(puscaPackage, "Internal")
    lazy val purityOfMethod = definitions.getMember(puscaInternalObject, "purityOf")
  }

  /** A type that does not allow a side effect */
  object SideEffectFreeType {
    def unapply(tpe: Type) = {
      if (isSideEffectFree(tpe)) Some(tpe)
      else None
    }
    def isSideEffectFree(tpe: Type) = tpe.hasAnnotation(Annotation.sideEffectFree) || tpe.dealias.hasAnnotation(Annotation.sideEffectFree)
  }

  object Utils {
    /**
     * The return type of the method.
     *  i.e. for "def a(i: Int): String" it returns String
     */
    @tailrec def returnTypeOf(methodType: Type): Type = methodType match {
      case PolyType(_, rt)      ⇒ returnTypeOf(rt)
      case MethodType(_, r)     ⇒ r
      case NullaryMethodType(r) ⇒ r
      case t                    ⇒ t
    }
    /** Return type of the method. See returnTypeOf(Type). */
    def returnTypeOf(t: MethodSymbol): Type = returnTypeOf(t.tpe)

    /** Changes the purity annotation on the given method */
    def changePurityAnnotation(to: Symbol)(on: DefDef) {
      on.tpe = tpeWithPurityAnnotation(to)(on.tpe)
      setPurityAnnotationOnSymbol(to)(on.symbol)
    }
    private def setPurityAnnotationOnSymbol(to: Symbol)(on: Symbol) {
      val na = Annotation(to) :: on.annotations.filterNot(Annotation.allForMethod.contains)
      on.setAnnotations(na)
    }
    private def tpeWithPurityAnnotation(to: Symbol)(on: Type) = {
      val na = Annotation(to) :: on.annotations.filterNot(Annotation.allForMethod.contains)
      on.withAnnotations(na)
    }
  }

  /** Usage: <code>MethodPurity.of(myMethod)</code> */
  object MethodPurity {
    /**
     * Determines the declared purity of a method/class/function.
     * Does not take the body of the method into account, whether the body conforms to the declaration is checked elsewhere.
     */
    def of(s: MethodSymbol): Purity = handler(s)

    private lazy val handler = puscaHandler.orElse(legacyHandler).orElse(impureHandler)

    /** Handler for pusca-compiled code */
    private val puscaHandler: PartialFunction[MethodSymbol, Purity] = {
      case s if s.hasAnnotation(Annotation.pure)        ⇒ AlwaysPure
      case s if s.hasAnnotation(Annotation.declarePure) ⇒ AlwaysPure
      case s if s.hasAnnotation(Annotation.impure)      ⇒ AlwaysImpure
      case s if s.hasAnnotation(Annotation.impureIf) ⇒
        val impureIfs = s.annotations.find(_.atp.typeSymbol == Annotation.impureIf) match {
          case Some(AnnotationInfo(_, args, _)) ⇒ args.collect { case SymbolApply(arg) ⇒ arg }
          case None                             ⇒ Nil
        }
        ImpureDependingOn(impureIfs.map(scala.Symbol(_)).toSet)
      case s if s.hasAnnotation(Annotation.impureIfReturnType) ⇒
        val rt = Utils.returnTypeOf(s)
        rt match {
          case SideEffectFreeType(_)                     ⇒ AlwaysPure
          case t if t.typeSymbol.isTypeParameterOrSkolem ⇒ ImpureDependingOn(Set(scala.Symbol(rt.typeSymbol.name.toString)))
          case _                                         ⇒ AlwaysImpure
        }
    }
    /** Handler for code that was not compiled by pusca */
    private lazy val legacyHandler: PartialFunction[MethodSymbol, Purity] = {
      case s ⇒ lookupLegacy(s).getOrElse(AlwaysImpure)
    }
    private def lookupLegacy(s: Symbol): Option[Purity] = {
      legacyMap.get(s.fullName) match {
        case p @ Some(purity)                    ⇒ p
        case None if s.isRoot || s.isRootPackage ⇒ lookupLegacy(s.owner)
        case _                                   ⇒ None
      }
    }

    /** Handler that considers everything as impure */
    private val impureHandler: PartialFunction[MethodSymbol, Purity] = {
      case _ ⇒ AlwaysImpure
    }

    private[this] object SymbolApply {
      private val applyName = stringToTermName("apply")
      private val symbolName = stringToTermName("Symbol")
      private val scalaName = stringToTermName("scala")
      def unapply(t: Tree) = t match {
        case Apply(Select(Select(Ident(scalaName), symbolName), applyName), Literal(arg @ Constant(_)) :: Nil) if arg.tag == StringTag ⇒
          Some(arg.stringValue.intern)
        case _ ⇒ None
      }
    }

    val puscaConf = "pusca.conf"

    lazy val legacyMap: Map[String, Purity] = {
      /** load config from the classpath of the compiler itself (embedded config) */
      def compilerClasspathConfig = {
        import scala.collection.JavaConversions._
        this.getClass.getClassLoader.getResources("/" + puscaConf).flatMap(ConfigLoader.loadFromUrl)
      }
      /** load configs from the classpath of the compilation */
      def classpathConfig =
        global.classPath.asURLs.flatMap { url ⇒
          val ef = url.toExternalForm
          val nef = (if (ef.endsWith(".jar")) "jar:" + ef + "!" else ef) + "/" + puscaConf
          val nu = new java.net.URL(nef)
          ConfigLoader.loadFromUrl(nu)
        }
      /** load configs from the source path */
      def sourceConfig = global.classPath.sourcepaths.flatMap { path ⇒
        println("@@ sourcepath " + path) //TODO
        val file = path.fileNamed(puscaConf)
        if (file.exists) Some(ConfigLoader.load(file.input))
        else None
      }

      def configToEntry(c: Config) = {
        val value = c.value.toLowerCase match {
          case "pure"   ⇒ Some(AlwaysPure)
          case "impure" ⇒ Some(AlwaysImpure)
          //          case "impureifreturntype" ⇒          //TODO how?
          //TODO rest
          case _        ⇒ None
        }
        value.map { v ⇒
          val key = c.key.mkString(".")
          (key, v)
        }
      }

      (compilerClasspathConfig ++ classpathConfig ++ sourceConfig).foldRight(Map.empty[String, Purity]) { (e, r) ⇒
        r ++ e.flatMap(configToEntry)
      }
    }
  }

  /**
   * Usage:
   *    <code>TreePurity.of(tree)</code>
   *  or
   *    <code>TreePurity.impuresIn(tree)</code>
   */
  object TreePurity {
    case class PurityInfo(pos: Position, purity: Purity, desc: String)

    /** purity of a block of code */
    def of(tree: Tree) = {
      impuresIn(tree).foldLeft[Purity](AlwaysPure) { (s, e) ⇒
        s match {
          case AlwaysPure   ⇒ e.purity
          case DeclaredPure ⇒ e.purity
          case AlwaysImpure ⇒ AlwaysImpure
          case s @ ImpureDependingOn(tps) ⇒ e.purity match {
            case AlwaysPure                 ⇒ s
            case DeclaredPure               ⇒ s
            case AlwaysImpure               ⇒ AlwaysImpure
            case ImpureDependingOn(moreTps) ⇒ ImpureDependingOn(tps ++ moreTps)
          }
        }
      }
    }

    /**
     * Gets the values used for the type parameters of a method.
     * Example:
     *  <code>
     *  trait Example[B] {
     *      def method1[A](a: A): B
     *  }
     *  new Example[String].method1(10)
     *  </code>
     * it returns: A -> Int, B -> String
     */
    private def resolveTypeParams(a: Apply): TypeMap = a.fun match {
      case TypeApply(fun, targs) ⇒
        val pl = fun.symbol.typeParams.map(tp ⇒ scala.Symbol(tp.name.toString)).toList
        methodOwnerTypeParams(fun) ++ pl.zip(targs.map(_.tpe)).toMap
      case f ⇒ methodOwnerTypeParams(f)
    }
    private type TypeMap = Map[scala.Symbol, Type]
    private def methodOwnerTypeParams(t: Tree): TypeMap = {
      def typeParams(tpe: Type, lookingFor: Symbol): TypeMap = {
        def tpsFor(o: Symbol, s: Symbol): TypeMap = {
          s.typeParams.map(s ⇒ (scala.Symbol(s.name.toString), s.tpe.asSeenFrom(tpe, o))).toMap
        }
        lookingFor.ownerChain.view.filter(_.typeParams.nonEmpty).foldLeft(Map[scala.Symbol, Type]())((s, e) ⇒ tpsFor(e, e) ++ s)
      }
      t match {
        case s @ Select(o, _) ⇒ typeParams(o.tpe, s.symbol)
        case i: Ident         ⇒ typeParams(i.tpe, i.symbol)
        case _                ⇒ Map()
      }
    }

    /** applies the type map to the PurityInfo, changing its purity accordingly */
    def adjustPurityInfo(pi: PurityInfo, typeMap: TypeMap) = pi.purity match {
      case ImpureDependingOn(ss) ⇒
        val (skolems, concrete) = ss.map(typeMap.apply).partition(_.typeSymbol.isTypeParameterOrSkolem)
        val np = {
          if (concrete.find(tp ⇒ !SideEffectFreeType.isSideEffectFree(tp)).isDefined) AlwaysImpure
          else if (skolems.nonEmpty) ImpureDependingOn(skolems.map(tp ⇒ scala.Symbol(tp.typeSymbol.name.toString)).toSet)
          else AlwaysPure
        }
        pi.copy(purity = np)
      case _ ⇒ pi
    }

    /** all not 'always pure' calls inside a block of code */
    def impuresIn(tree: Tree): List[PurityInfo] = {
      val root = tree
      val rootSymbol = tree.symbol
      def purityOf(of: Symbol) = of match {
        case ms: MethodSymbol ⇒ MethodPurity.of(ms)
        case _                ⇒ AlwaysPure
      }
      def descOf(s: Symbol) = s match {
        case s if s.isSetter      ⇒ "write to non-local var " + s.name.toString.dropRight(4)
        case s if s.isGetter      ⇒ "access to non-local var " + s.name
        case s if s.isConstructor ⇒ "impure method call to " + s.owner.name + ".<init>"
        case s                    ⇒ "impure method call to " + s.name
      }
      def handleSeq(ts: Seq[Tree], soFar: List[PurityInfo]): List[PurityInfo] = ts.foldLeft(soFar)((sf, e) ⇒ handle(e, sf))
      def handle(tree: Tree, soFar: List[PurityInfo]): List[PurityInfo] = {
        tree match {
          case a @ Apply(fun, args) ⇒
            val nr = purityOf(fun.symbol) match {
              case AlwaysPure ⇒ Nil
              case p ⇒
                val pi = PurityInfo(a.pos, p, descOf(fun.symbol))
                adjustPurityInfo(pi, resolveTypeParams(a)) :: Nil
            }
            handleSeq(a.args, nr ::: soFar)

          case a @ Assign(s @ Select(_, _), _) if s.symbol.isModuleVar ⇒ //implementation detail of object (assigns a xxx$module var)
            soFar
          case a @ Assign(lhs, rhs) if (!lhs.symbol.ownerChain.contains(rootSymbol)) ⇒ // assign to var outside the scope of this method
            PurityInfo(a.pos, AlwaysImpure, "write to non-local var " + lhs.symbol.name) :: soFar

          case s: Select if s.symbol.isModuleVar ⇒ //implementation detail of object (reads a xxx$module var)
            soFar
          case s: Select if s.symbol.isSetter ⇒ //assign to var via setter
            PurityInfo(s.pos, AlwaysImpure, "write to non-local var " + s.name.toString.dropRight(4)) :: soFar
          case s: Select if s.symbol.isMutable && !s.symbol.ownerChain.contains(rootSymbol) ⇒ // read of var outside the scope of this method (private[this])
            PurityInfo(s.pos, AlwaysImpure, "access to non-local var " + s.name) :: soFar
          case s: Select if s.symbol.isGetter && !s.symbol.isStable ⇒ // read of var via accessor
            PurityInfo(s.pos, AlwaysImpure, "access to non-local var " + s.name) :: soFar
          case s @ Select(own, _) if s.symbol.isGetter ⇒ handle(own, soFar)
          case s @ Select(own, sel) ⇒
            val nr = purityOf(s.symbol) match {
              case AlwaysPure ⇒ Nil
              case p ⇒
                val pi = PurityInfo(s.pos, p, descOf(s.symbol))
                adjustPurityInfo(pi, methodOwnerTypeParams(s)) :: Nil
            }
            nr ::: soFar

          case i: Ident if i.symbol.isMutable && !i.symbol.ownerChain.contains(rootSymbol) ⇒ // read of var defined in an outer function
            PurityInfo(i.pos, AlwaysImpure, "access to non-local var " + i.name) :: soFar

          //don't recurse into sub-defs
          case d: DefDef   ⇒ soFar
          case c: ClassDef ⇒ soFar
          case f: Function ⇒ soFar

          case other       ⇒ handleSeq(other.children, soFar)
        }
      }
      handle(tree, Nil).reverse
    }
  }
}