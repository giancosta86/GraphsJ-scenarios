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

package info.gianlucacosta.graphsj.scenarios.planbricks

import info.gianlucacosta.eighthbridge.fx.canvas.basic.BasicController
import info.gianlucacosta.eighthbridge.fx.canvas.{GraphCanvas, VertexNode}

import scalafx.geometry.{Dimension2D, Point2D}

trait PopController extends BasicController[StepVertex, PopLink, PopGraph] {
  override def createVertexNode(graphCanvas: GraphCanvas[StepVertex, PopLink, PopGraph], vertex: StepVertex): VertexNode[StepVertex, PopLink, PopGraph] =
    new StepVertexNode(graphCanvas, vertex.step)


  override def createLinkInternalPoint(graph: PopGraph, link: PopLink, newInternalPoints: List[Point2D], internalPoint: Point2D): Option[PopGraph] = {
    val newLink =
      link.visualCopy(internalPoints = newInternalPoints)

    Some(
      graph.replaceLink(newLink)
    )
  }


  override def deleteLinkInternalPoint(graph: PopGraph, link: PopLink, newInternalPoints: List[Point2D], internalPoint: Point2D): Option[PopGraph] = {
    val newLink =
      link.visualCopy(internalPoints = newInternalPoints)

    Some(
      graph.replaceLink(newLink)
    )
  }


  override def minCanvasDimension: Dimension2D =
    new Dimension2D(
      2000,
      2000
    )
}
