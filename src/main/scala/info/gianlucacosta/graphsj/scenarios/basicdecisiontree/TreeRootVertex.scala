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

import info.gianlucacosta.lambdaprism.classification.basic.{Attribute, DecisionTree}

import scalafx.geometry.Point2D


object TreeRootVertex {
  def formatAttribute(attribute: Attribute): String =
    attribute
}

case class TreeRootVertex(
                           decisionTree: DecisionTree,
                           center: Point2D,
                           selected: Boolean = false,
                           id: UUID = UUID.randomUUID()
                         ) extends DecisionTreeVertex {

  override def text: String =
    TreeRootVertex.formatAttribute(decisionTree.attribute)


  override def visualCopy(center: Point2D, selected: Boolean): TreeRootVertex =
    copy(
      center = center,
      selected = selected
    )

  override def styleClasses: List[String] =
    List()
}
