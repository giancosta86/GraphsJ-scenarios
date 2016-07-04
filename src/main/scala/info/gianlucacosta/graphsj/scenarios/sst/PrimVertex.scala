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

import java.util.UUID

import info.gianlucacosta.eighthbridge.fx.canvas.basic.BasicVertex
import info.gianlucacosta.eighthbridge.graphs.point2point.specific.Named
import info.gianlucacosta.helios.mathutils.Numbers

import scalafx.geometry.{Dimension2D, Point2D}


object PrimVertex {
  val FontDimension = new Dimension2D(12, 19)
}

case class PrimVertex(
                       center: Point2D,
                       styleClasses: List[String] = List(),
                       name: String = "",
                       bestVertex: Option[PrimVertex] = None,
                       distanceFromBestVertex: Option[Double] = None,
                       @transient selected: Boolean = false,
                       id: UUID = UUID.randomUUID()
                     ) extends BasicVertex[PrimVertex] with Named[PrimVertex] {


  override def dimension: Dimension2D =
    BasicVertex.estimateVertexSize(text, PrimVertex.FontDimension, 10)


  override val text: String =
    if (bestVertex.nonEmpty)
      s"${name} {${bestVertex.get.name}, ${Numbers.smartString(distanceFromBestVertex.get)}}"
    else
      name


  override def visualCopy(center: Point2D, selected: Boolean): PrimVertex =
    copy(
      center = center,
      selected = selected
    )


  override def nameCopy(name: String): PrimVertex =
    copy(name = name)
}
