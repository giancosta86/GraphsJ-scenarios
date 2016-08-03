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

import info.gianlucacosta.eighthbridge.fx.canvas.GraphCanvasController
import info.gianlucacosta.eighthbridge.fx.canvas.basic.BasicStyles
import info.gianlucacosta.graphsj.{Algorithm, Scenario}
import info.gianlucacosta.helios.desktop.DesktopUtils
import info.gianlucacosta.helios.fx.dialogs.Alerts
import info.gianlucacosta.lambdaprism.planning.problem.Problem
import info.gianlucacosta.lambdaprism.planning.problem.ProblemValidators._
import info.gianlucacosta.lambdaprism.planning.problem.dialog.ProblemDialog


object PopScenario {
  val Name =
    "Partial Order Planning (POP)"

  val ProblemValidator =
    CompositeValidator(
      PropositionalValidator,

      HasMainActionsValidator
    )

  private val WebsiteUrl =
    "https://github.com/giancosta86/GraphsJ-scenarios"
}


class PopScenario(private var problem: Problem) extends Scenario[StepVertex, PopLink, PopGraph] {
  override val name: String =
    PopScenario.Name


  override def createAlgorithm(): Algorithm[StepVertex, PopLink, PopGraph] =
    new PopAlgorithm()


  override def createDesignController(): GraphCanvasController[StepVertex, PopLink, PopGraph] =
    new PopDesignController(problem)


  override def createRuntimeController(): GraphCanvasController[StepVertex, PopLink, PopGraph] =
    new PopRuntimeController


  override def runStepsBeforePausing: Int =
    0


  override def stylesheets: List[String] =
    List(
      BasicStyles.resourceUrl.toExternalForm,
      getClass.getResource("POP.css").toExternalForm
    )


  override def showHelp(): Unit =
    DesktopUtils.openBrowser(PopScenario.WebsiteUrl)


  override def createDesignGraph(): PopGraph =
    PopGraph(problem)


  override def showSettings(designGraph: PopGraph): Option[PopGraph] = {
    if (
      designGraph.vertexes.size == 2 &&
        designGraph.temporalLinks.size == 1 &&
        designGraph.causalLinks.isEmpty
    ) {
      val newProblemOption =
        ProblemDialog.askForProblem(
          Some(problem),
          PopScenario.ProblemValidator
        )

      newProblemOption.map(problem => {
        this.problem =
          problem

        PopGraph(problem)
      })
    } else {
      Alerts.showWarning(
        "The problem can be edited only after deleting every custom step and every causal link"
      )

      None
    }
  }
}
