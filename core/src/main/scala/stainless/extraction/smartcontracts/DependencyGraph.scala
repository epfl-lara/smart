/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts

import inox.utils.Graphs._

trait DependencyGraph extends methods.DependencyGraph {
  import trees._

  override protected def computeDependencyGraph: DiGraph[Identifier, SimpleEdge[Identifier]] = {
    var g = super.computeDependencyGraph

    g
  }
}
