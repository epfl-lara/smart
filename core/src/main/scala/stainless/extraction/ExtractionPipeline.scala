/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction

trait ExtractionPipeline { self =>
  val s: extraction.Trees
  val t: ast.Trees

  implicit val context: inox.Context
  protected implicit def printerOpts: s.PrinterOptions = s.PrinterOptions.fromContext(context)
  // `targetPrinterOpts` isn't implicit to avoid ambiguous references
  protected def targetPrinterOpts: t.PrinterOptions = t.PrinterOptions.fromContext(context)

  def extract(symbols: s.Symbols): t.Symbols

  // make a String representation for a table of Symbols `s`, only keeping 
  // functions and classes whose names appear in `objs`
  def symbolsToString(tt: ast.Trees)(s: tt.Symbols, objs: Set[String]): String = {
    val printerOpts = tt.PrinterOptions.fromContext(context)
    def objectsToString(m: Iterable[(Identifier,tt.Definition)]): String = 
      m.collect { 
        case (id,d) if objs.isEmpty || objs.contains(id.name) => 
          d.asString(printerOpts)
      }.mkString("\n\n")

    val functions = objectsToString(s.functions)
    val sorts = objectsToString(s.sorts)
    val classes = 
      if (tt == oo.trees) 
        // we do the casts in both directions since the compiler doesn't 
        // recognize that tt == oo.trees
        objectsToString(s.asInstanceOf[oo.trees.Symbols]
                         .classes
                         .asInstanceOf[Iterable[(Identifier,tt.Definition)]])
      else
        ""

    def wrapWith(header: String, s: String) = {
      if (s.isEmpty) ""
      else 
        "-------------" + header + "-------------\n" +
        s + "\n\n"
    }

    wrapWith("Functions", functions) ++
    wrapWith("Sorts", sorts) ++
    wrapWith("Classes", classes)
  }

  // `extractWithDebug` is a wrapper around `extract` which outputs trees for debugging
  // and which outputs position checks
  def extractWithDebug(symbols: s.Symbols): t.Symbols = {
    implicit val debugSection = inox.ast.DebugSectionTrees
    val phases = context.options.findOption(optDebugPhases)
    val objs = context.options.findOption(optDebugObjects).getOrElse(Seq()).toSet
    val debug: Boolean = 
      debugTransformation && 
      (phases.isEmpty || (phases.isDefined && phases.get.exists(phaseName.contains _)))

    context.reporter.synchronized {
      val symbolsToPrint = symbolsToString(s)(symbols, objs)
      if (debug && !symbolsToPrint.isEmpty) {
        context.reporter.debug("\n\n\n\nSymbols before " + phaseName + "\n")
        context.reporter.debug(symbolsToPrint)
      }

      // start the position checker before extracting the symbols, if the option if on
      if (debug && self.context.reporter.debugSections.contains(utils.DebugSectionPositions)) {
        val posChecker = utils.PositionChecker(self.phaseName)(self.s)(self.context)(true)
        symbols.functions.values.toSeq.foreach(posChecker.traverse)
      }

      val res = extract(symbols)
      val resToPrint = symbolsToString(t)(res, objs)

      if (debug && (!symbolsToPrint.isEmpty || !resToPrint.isEmpty)) {
        context.reporter.debug("\n\nSymbols after " + phaseName +  "\n")
        context.reporter.debug(resToPrint)
        context.reporter.debug("\n\n")
      }

      if (debug && self.context.reporter.debugSections.contains(utils.DebugSectionPositions)) {
        val posChecker = utils.PositionChecker(self.phaseName)(self.t)(self.context)(false)
        res.functions.values.toSeq.foreach(posChecker.traverse)
      }
      res
    }
  }

  def invalidate(id: Identifier): Unit

  def andThen(that: ExtractionPipeline { val s: self.t.type }): ExtractionPipeline {
    val s: self.s.type
    val t: that.t.type
  } = new ExtractionPipeline {
    override val s: self.s.type = self.s
    override val t: that.t.type = that.t
    override val context = self.context

    override def extract(symbols: s.Symbols): t.Symbols = {
      that.extractWithDebug(self.extractWithDebug(symbols))
    }

    override def invalidate(id: Identifier): Unit = {
      self.invalidate(id)
      that.invalidate(id)
    }
  }
}

object ExtractionPipeline {
  def apply(transformer: ast.TreeTransformer { val s: Trees; val t: ast.Trees })
           (implicit ctx: inox.Context): ExtractionPipeline {
    val s: transformer.s.type
    val t: transformer.t.type
  } = new ExtractionPipeline { self =>
    override val s: transformer.s.type = transformer.s
    override val t: transformer.t.type = transformer.t
    override val context = ctx

    override def extract(symbols: s.Symbols): t.Symbols =
      symbols.transform(transformer.asInstanceOf[ast.TreeTransformer {
        val s: self.s.type
        val t: self.t.type
      }])

    override def invalidate(id: Identifier): Unit = ()
  }

  def apply(transformer: inox.ast.SymbolTransformer { val s: Trees; val t: ast.Trees })
           (implicit ctx: inox.Context): ExtractionPipeline {
    val s: transformer.s.type
    val t: transformer.t.type
  } = new ExtractionPipeline {
    override val s: transformer.s.type = transformer.s
    override val t: transformer.t.type = transformer.t
    override val context = ctx

    override def extract(symbols: s.Symbols): t.Symbols = transformer.transform(symbols)
    override def invalidate(id: Identifier): Unit = ()
  }
}

trait CachingPhase extends ExtractionPipeline with ExtractionCaches { self =>
  override val debugTransformation = true

  protected type FunctionResult
  protected val funCache: ExtractionCache[s.FunDef, FunctionResult]

  protected type SortResult
  protected val sortCache: ExtractionCache[s.ADTSort, SortResult]

  protected type TransformerContext
  protected def getContext(symbols: s.Symbols): TransformerContext

  protected def extractFunction(context: TransformerContext, fd: s.FunDef): FunctionResult
  protected def registerFunctions(symbols: t.Symbols, functions: Seq[FunctionResult]): t.Symbols

  protected def extractSort(context: TransformerContext, sort: s.ADTSort): SortResult
  protected def registerSorts(symbols: t.Symbols, sorts: Seq[SortResult]): t.Symbols

  override final def extract(symbols: s.Symbols): t.Symbols = {
    val context = getContext(symbols)
    extractSymbols(context, symbols)
  }

  protected def extractSymbols(context: TransformerContext, symbols: s.Symbols): t.Symbols = {
    val functions = symbols.functions.values.map { fd =>
      // funCache.cached(fd, symbols)(extractFunction(context, fd))
      extractFunction(context, fd)
    }.toSeq

    val sorts = symbols.sorts.values.map { sort =>
      sortCache.cached(sort, symbols)(extractSort(context, sort))
    }.toSeq

    registerSorts(registerFunctions(t.NoSymbols, functions), sorts)
  }
}

trait SimpleSorts extends CachingPhase {
  override protected type SortResult = t.ADTSort
  override protected def registerSorts(symbols: t.Symbols, sorts: Seq[t.ADTSort]): t.Symbols = symbols.withSorts(sorts)
}

trait SimplyCachedSorts extends CachingPhase {
  override protected final val sortCache: ExtractionCache[s.ADTSort, SortResult] = new SimpleCache[s.ADTSort, SortResult]
}

trait DependentlyCachedSorts extends CachingPhase {
  override protected final val sortCache: ExtractionCache[s.ADTSort, SortResult] = new DependencyCache[s.ADTSort, SortResult]
}

trait IdentitySorts extends SimpleSorts with SimplyCachedSorts { self =>
  private[this] final object identity extends ast.TreeTransformer {
    override val s: self.s.type = self.s
    override val t: self.t.type = self.t
  }

  override protected def extractSort(context: TransformerContext, sort: s.ADTSort): t.ADTSort = identity.transform(sort)
}

trait SimpleFunctions extends CachingPhase {
  override protected type FunctionResult = t.FunDef
  override protected def registerFunctions(symbols: t.Symbols, functions: Seq[t.FunDef]): t.Symbols = symbols.withFunctions(functions)
}

trait SimplyCachedFunctions extends CachingPhase {
  override protected final val funCache: ExtractionCache[s.FunDef, FunctionResult] = new SimpleCache[s.FunDef, FunctionResult]
}

trait DependentlyCachedFunctions extends CachingPhase {
  override protected final val funCache: ExtractionCache[s.FunDef, FunctionResult] = new DependencyCache[s.FunDef, FunctionResult]
}

trait IdentityFunctions extends SimpleFunctions with SimplyCachedFunctions { self =>
  private[this] final object identity extends ast.TreeTransformer {
    override val s: self.s.type = self.s
    override val t: self.t.type = self.t
  }

  override protected def extractFunction(context: TransformerContext, fd: s.FunDef): t.FunDef = identity.transform(fd)
}

trait SimplePhase extends ExtractionPipeline with SimpleSorts with SimpleFunctions { self =>
  override protected type TransformerContext <: ast.TreeTransformer {
    val s: self.s.type
    val t: self.t.type
  }

  override protected def extractFunction(context: TransformerContext, fd: s.FunDef): t.FunDef = context.transform(fd)
  override protected def extractSort(context: TransformerContext, sort: s.ADTSort): t.ADTSort = context.transform(sort)
}
