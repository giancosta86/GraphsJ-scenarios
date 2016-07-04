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

import info.gianlucacosta.graphsj.ScenarioFactory
import info.gianlucacosta.lambdaprism.planning.problem.dialog.ProblemDialog

class GraphPlanFactory extends ScenarioFactory[ConstructionVertex, ConstructionLink, ConstructionGraph] {
  override val scenarioName: String =
    "Add/Delete GraphPlan"

  override def createScenario: Option[GraphPlanScenario] = {
    val problemOption = ProblemDialog.askForProblem(
      None,
      GraphPlanScenario.ProblemValidator
    )

    problemOption.map(problem =>
      new GraphPlanScenario(problem)
    )
  }

}
