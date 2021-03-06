/*§
  ===========================================================================
  GraphsJ - Scenarios
  ===========================================================================
  Copyright (C) 2009-2016 Gianluca Costa
  ===========================================================================
  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
  ===========================================================================
*/

package info.gianlucacosta.graphsj.scenarios.sst

import info.gianlucacosta.eighthbridge.graphs.point2point.visual.VisualGraph
import info.gianlucacosta.eighthbridge.graphs.point2point.{ArcBinding, TopologyCacheDirectedGraph}

case class PrimGraph(
                      vertexes: Set[PrimVertex] = Set(),
                      links: Set[PrimLink] = Set(),
                      bindings: Set[ArcBinding] = Set()
                    )
  extends VisualGraph[PrimVertex, PrimLink, PrimGraph]
    with TopologyCacheDirectedGraph[PrimVertex, PrimLink, PrimGraph] {
  override protected def graphCopy(vertexes: Set[PrimVertex], links: Set[PrimLink], bindings: Set[ArcBinding]): PrimGraph =
    copy(
      vertexes = vertexes,
      links = links,
      bindings = bindings
    )
}
