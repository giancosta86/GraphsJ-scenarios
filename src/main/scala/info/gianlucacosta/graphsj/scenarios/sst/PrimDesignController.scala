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

import info.gianlucacosta.eighthbridge.fx.canvas.basic.editing.{VertexNamingController, WeightLinkController}
import info.gianlucacosta.eighthbridge.graphs.point2point.visual.VisualGraph

import scalafx.geometry.Point2D

class PrimDesignController[G <: VisualGraph[PrimVertex, PrimLink, G]] extends VertexNamingController[PrimVertex, PrimLink, G] with WeightLinkController[PrimVertex, PrimLink, G] {
  override protected def instantiateVertex(center: Point2D, vertexName: String): PrimVertex =
    new PrimVertex(center = center, name = vertexName)


  override def createLink(graph: G, sourceVertex: PrimVertex, targetVertex: PrimVertex): Option[G] = {
    val link = new PrimLink(
      weight = 0
    )

    Some(
      graph.addLink(sourceVertex, targetVertex, link)
    )
  }
}
