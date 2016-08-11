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

package info.gianlucacosta.graphsj.scenarios.basicdecisiontree

import java.util.UUID

import scalafx.geometry.Point2D

case object InstructionsVertex extends DecisionTreeVertex {
  override def text: String =
    "This graph cannot be edited.\n\nPress the Run button\nto see the intermediate results\nand the decision tree."


  override def center: Point2D =
    new Point2D(420, 200)

  override def visualCopy(center: Point2D, selected: Boolean): DecisionTreeVertex =
    InstructionsVertex

  override def styleClasses: List[String] =
    List(
      "instructionsVertex"
    )

  override def selected: Boolean =
    false

  override val id: UUID =
    UUID.randomUUID()
}
