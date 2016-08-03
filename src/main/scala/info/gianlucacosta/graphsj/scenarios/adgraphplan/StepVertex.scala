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

package info.gianlucacosta.graphsj.scenarios.adgraphplan

import java.util.UUID

import info.gianlucacosta.lambdaprism.planning.problem.Step

import scalafx.geometry.{Dimension2D, Point2D}


object StepVertex {
  def formatStep(step: Step, mutexes: Set[Step]): String = {
    val mutexString: String =
      if (mutexes.nonEmpty)
        s"\n\n${ConstructionVertex.MutexString}\n\n${mutexes.mkString("\n")}"
      else
        ""
    s"${step}" + mutexString
  }
}


case class StepVertex(
                       step: Step,

                       mutexes: Set[Step],

                       dimension: Dimension2D,

                       center: Point2D,

                       selected: Boolean = false,

                       id: UUID = UUID.randomUUID()
                     ) extends ConstructionVertex {


  override def text: String =
    StepVertex.formatStep(step, mutexes)


  override val styleClasses: List[String] =
    List("stepVertex")


  override def visualCopy(center: Point2D, selected: Boolean): StepVertex =
    copy(
      center = center,
      selected = selected
    )
}
