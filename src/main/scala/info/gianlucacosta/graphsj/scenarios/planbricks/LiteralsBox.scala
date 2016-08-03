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

import info.gianlucacosta.lambdaprism.logic.basic.formulas.Literal

import scala.collection.JavaConversions._
import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.scene.layout.FlowPane
import scalafx.scene.text.Text


private abstract class LiteralsBox(
                                    literals: List[Literal],
                                    header: String,
                                    dedicatedStyleClass: String
                                  ) extends FlowPane {
  padding =
    Insets(5)

  vgap =
    4

  hgap =
    8

  prefWrapLength =
    420


  styleClass.add(dedicatedStyleClass)


  initLabels()


  def initLabels(): Unit = {
    if (literals.nonEmpty) {
      val headerLabel =
        new Text(header) {
          styleClass.add("literalsHeader")
        }

      children.add(headerLabel)

      val literalLabels =
        literals
          .map(literal => {
            new Text(literal.toString)
          })

      literalLabels.foreach(literalLabel =>
        children.add(literalLabel)
      )
    }
  }


  def render(): Unit = {
    literals.zipWithIndex.foreach {
      case (literal, literalIndex) =>
        val literalStyleClassOption =
          getLiteralStyleClass(literal)

        val literalNode =
          children(1 + literalIndex)

        literalNode.styleClass.clear()

        literalStyleClassOption.foreach(literalStyleClass => {
          literalNode.styleClass.add(
            literalStyleClass
          )
        })
    }
  }

  def getLiteralStyleClass(literal: Literal): Option[String]
}