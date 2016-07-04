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

import info.gianlucacosta.eighthbridge.fx.canvas.basic.{BasicLink, LinkHandleRadius}
import info.gianlucacosta.eighthbridge.graphs.point2point.specific.Weighted
import info.gianlucacosta.helios.mathutils.Numbers

import scalafx.geometry.Point2D


case class PrimLink(weight: Double,
                    styleClasses: List[String] = List(),
                    internalPoints: List[Point2D] = Nil,
                    @transient selected: Boolean = false,
                    labelCenter: Option[Point2D] = None,
                    id: UUID = UUID.randomUUID()
                   ) extends BasicLink[PrimLink] with Weighted[PrimLink] {

  val minWeight = 0.0
  val maxWeight = Double.MaxValue

  checkWeight()


  override val handleRadius: LinkHandleRadius = LinkHandleRadius()

  override val text: String = Numbers.smartString(weight)


  override def visualCopy(internalPoints: List[Point2D], selected: Boolean, labelCenter: Option[Point2D]): PrimLink = {
    copy(
      internalPoints = internalPoints,
      selected = selected,
      labelCenter = labelCenter
    )
  }


  override def weightCopy(weight: Double): PrimLink =
    copy(weight = weight)
}