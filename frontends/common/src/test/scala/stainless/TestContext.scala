/* Copyright 2009-2019 EPFL, Lausanne */

package stainless

import inox.{ DebugSection, Options, OptionValue }
import stainless.verification.optVCCache
import stainless.frontend.optSmartContracts

object TestContext {

  /**
   * Create a context for testing purposes.
   *
   * Unless explicitely present in [[options]], the returned
   * context is set to use no VC cache and no smart contracts.
   */
  def apply(options: Options): inox.Context = {
    def setOptionFalse(opts: Options, option: inox.FlagOptionDef): Options = {
      if ((opts findOption option).isDefined) opts
      else opts + OptionValue(option)(false)
    }
    val newOptions1 = setOptionFalse(options, optVCCache)
    val newOptions2 = setOptionFalse(newOptions1, optSmartContracts)
    inox.TestContext(newOptions2)
  }

  /**
   * Use for debug purposes.
   *
   * The returned context has a DefaultReporter.
   **/
  def debug(sections: Set[DebugSection], options: Options): inox.Context = {
    val reporter = new stainless.DefaultReporter(sections)
    val ctx = apply(options)
    inox.Context(reporter, ctx.interruptManager, ctx.options, ctx.timers)
  }

  def debug(sections: Set[DebugSection]): inox.Context = debug(sections, Options.empty)

  def empty: inox.Context = apply(Options.empty)

}


