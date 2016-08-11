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

import info.gianlucacosta.lambdaprism.classification.basic.DecisionTreeLeaf

import scalafx.geometry.Point2D

object TreeLeafVertex {
  def formatLeaf(treeLeaf: DecisionTreeLeaf): String =
    treeLeaf.toString
}


case class TreeLeafVertex(
                           treeLeaf: DecisionTreeLeaf,
                           center: Point2D,
                           selected: Boolean = false,
                           id: UUID = UUID.randomUUID()
                         ) extends DecisionTreeVertex {

  override def text: String =
    TreeLeafVertex.formatLeaf(treeLeaf)


  override def visualCopy(center: Point2D, selected: Boolean): TreeLeafVertex =
    copy(
      center = center,
      selected = selected
    )

  override def styleClasses: List[String] =
    List()
}
