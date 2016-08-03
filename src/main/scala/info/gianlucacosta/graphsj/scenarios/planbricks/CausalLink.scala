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

import java.util.UUID

import info.gianlucacosta.lambdaprism.logic.basic.formulas.Literal
import info.gianlucacosta.lambdaprism.logic.basic.matching.Environment

import scalafx.geometry.Point2D


case class CausalLink(
                       effect: Literal,
                       precondition: Literal,
                       matchEnvironment: Environment,
                       threats: Set[StepVertex] = Set(),
                       selected: Boolean = false,
                       internalPoints: List[Point2D] = List(),
                       labelCenter: Option[Point2D] = None,
                       id: UUID = UUID.randomUUID()
                     ) extends PopLink {
  override def text: String = {
    val threatsString =
      if (threats.nonEmpty)
        s"\n\nTHREATS:\n${threats.mkString("\n")}"
      else
        ""

    s"${effect} --> ${precondition}${threatsString}"
  }


  override def visualCopy(internalPoints: List[Point2D], selected: Boolean, labelCenter: Option[Point2D]): CausalLink =
    copy(
      internalPoints = internalPoints,
      selected = selected,
      labelCenter = labelCenter
    )


  override def styleClasses: List[String] =
    if (threats.isEmpty)
      List("causalLink")
    else
      List(
        "causalLink",
        "threatened"
      )
}
