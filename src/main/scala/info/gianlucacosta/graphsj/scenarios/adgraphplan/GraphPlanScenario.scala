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

import info.gianlucacosta.eighthbridge.fx.canvas.GraphCanvasController
import info.gianlucacosta.eighthbridge.fx.canvas.basic.{BasicStyles, DragDropController}
import info.gianlucacosta.graphsj.{Algorithm, Scenario}
import info.gianlucacosta.helios.desktop.DesktopUtils
import info.gianlucacosta.lambdaprism.planning.problem.Problem
import info.gianlucacosta.lambdaprism.planning.problem.ProblemValidators._
import info.gianlucacosta.lambdaprism.planning.problem.dialog.ProblemDialog

object GraphPlanScenario {
  val ProblemValidator =
    CompositeValidator(
      PropositionalValidator,
      StrictClosedWorldValidator,
      HasMainActionsValidator
    )


  private val WebsiteUrl =
    "https://github.com/giancosta86/GraphsJ-scenarios"


  val Stylesheets =
    List(
      BasicStyles.resourceUrl.toExternalForm,
      getClass.getResource("GraphPlan.css").toExternalForm
    )
}


class GraphPlanScenario(private var problem: Problem)
  extends Scenario[ConstructionVertex, ConstructionLink, ConstructionGraph] {
  override val name: String =
    "GraphPlan variant based on Add and Delete arcs"


  override def createAlgorithm(): Algorithm[ConstructionVertex, ConstructionLink, ConstructionGraph] =
    new GraphPlanAlgorithm(problem)


  override def createDesignController(): GraphCanvasController[ConstructionVertex, ConstructionLink, ConstructionGraph] =
    new DragDropController[ConstructionVertex, ConstructionLink, ConstructionGraph](false)


  override def createRuntimeController(): GraphCanvasController[ConstructionVertex, ConstructionLink, ConstructionGraph] =
    new DragDropController[ConstructionVertex, ConstructionLink, ConstructionGraph](false)


  override val stylesheets: List[String] =
    GraphPlanScenario.Stylesheets


  override def showHelp(): Unit = {
    DesktopUtils.openBrowser(GraphPlanScenario.WebsiteUrl)
  }


  override def createDesignGraph(): ConstructionGraph =
    ConstructionGraph(problem)


  override def showSettings(designGraph: ConstructionGraph): Option[ConstructionGraph] = {
    val newProblemOption = ProblemDialog.askForProblem(
      Some(problem),
      GraphPlanScenario.ProblemValidator
    )

    newProblemOption.map(problem => {
      this.problem = problem

      ConstructionGraph(problem)
    })
  }

  override def runStepsBeforePausing: Int =
    15
}
