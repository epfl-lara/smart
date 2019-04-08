/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts

import inox.utils.Graphs._

trait DependencyGraph extends methods.DependencyGraph {
  protected val trees: smartcontracts.Trees
  import trees._

  override protected def computeDependencyGraph: DiGraph[Identifier, SimpleEdge[Identifier]] = {
    var g = super.computeDependencyGraph

    for (cd <- symbols.classes.values if cd.isContract) {
      for (fid <- cd.methods(symbols) if (fid.name == "invariant" || fid.name == "evolution"))
        g += SimpleEdge(cd.id, fid)
      for (fid <- cd.methods(symbols) if (fid.name.contains("addressOf") || fid.name == "environmentInvariant"))
        g += SimpleEdge(cd.id, fid)
    }

    g
  }
}
