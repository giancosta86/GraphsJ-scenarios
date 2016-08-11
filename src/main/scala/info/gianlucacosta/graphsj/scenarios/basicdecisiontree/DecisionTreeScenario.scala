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

import info.gianlucacosta.eighthbridge.fx.canvas.GraphCanvasController
import info.gianlucacosta.eighthbridge.fx.canvas.basic.{BasicStyles, DragDropController, ReadOnlyController}
import info.gianlucacosta.graphsj.{Algorithm, Scenario}
import info.gianlucacosta.helios.desktop.DesktopUtils
import info.gianlucacosta.lambdaprism.classification.basic.{ClassificationProblem, ClassificationProblemDialog}

import scalafx.geometry.Dimension2D

object DecisionTreeScenario {
  val Name =
    "Basic Decision Tree"

  val Stylesheets: List[String] =
    List(
      BasicStyles.resourceUrl.toExternalForm,
      getClass.getResource("BasicDecisionTree.css").toExternalForm
    )

  private val WebsiteUrl =
    "https://github.com/giancosta86/GraphsJ-scenarios"
}


class DecisionTreeScenario(private var problem: ClassificationProblem) extends Scenario[DecisionTreeVertex, DecisionTreeLink, DecisionTreeGraph] {
  override val name: String =
    DecisionTreeScenario.Name


  override def createAlgorithm(): Algorithm[DecisionTreeVertex, DecisionTreeLink, DecisionTreeGraph] =
    new DecisionTreeAlgorithm(problem)


  override def createDesignController(): GraphCanvasController[DecisionTreeVertex, DecisionTreeLink, DecisionTreeGraph] =
    new ReadOnlyController[DecisionTreeVertex, DecisionTreeLink, DecisionTreeGraph](true)


  override def runStepsBeforePausing: Int =
    0


  override def stylesheets: List[String] =
    DecisionTreeScenario.Stylesheets


  override def createRuntimeController(): GraphCanvasController[DecisionTreeVertex, DecisionTreeLink, DecisionTreeGraph] =
    new DragDropController[DecisionTreeVertex, DecisionTreeLink, DecisionTreeGraph](true) {
      override def minCanvasDimension: Dimension2D =
        new Dimension2D(1500, 1500)
    }


  override def showHelp(): Unit =
    DesktopUtils.openBrowser(DecisionTreeScenario.WebsiteUrl)


  override def createDesignGraph(): DecisionTreeGraph =
    new DecisionTreeGraph


  override def showSettings(designGraph: DecisionTreeGraph): Option[DecisionTreeGraph] = {
    val newProblemOption = ClassificationProblemDialog.askForProblem(
      Some(problem)
    )

    newProblemOption.map(problem => {
      this.problem =
        problem

      new DecisionTreeGraph
    })
  }
}
