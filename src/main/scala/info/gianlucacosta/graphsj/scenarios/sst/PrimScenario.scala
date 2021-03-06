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

package info.gianlucacosta.graphsj.scenarios.sst

import info.gianlucacosta.eighthbridge.fx.canvas.GraphCanvasController
import info.gianlucacosta.eighthbridge.fx.canvas.basic.{BasicStyles, DragDropController}
import info.gianlucacosta.graphsj.{Algorithm, Scenario}
import info.gianlucacosta.helios.desktop.DesktopUtils
import info.gianlucacosta.helios.fx.dialogs.Alerts


object PrimScenario {
  val Name: String =
    "Prim's Shortest Spanning Tree (SST)"

  private val WebsiteUrl =
    "https://github.com/giancosta86/GraphsJ-scenarios"
}


class PrimScenario extends Scenario[PrimVertex, PrimLink, PrimGraph] {
  override val name: String =
    PrimScenario.Name


  override def showHelp(): Unit = {
    DesktopUtils.openBrowser(PrimScenario.WebsiteUrl)
  }


  override def showSettings(designGraph: PrimGraph): Option[PrimGraph] = {
    Alerts.showInfo("No settings available for this scenario.")

    None
  }


  override def createAlgorithm(): Algorithm[PrimVertex, PrimLink, PrimGraph] =
    new PrimAlgorithm[PrimGraph]


  override def createDesignController(): GraphCanvasController[PrimVertex, PrimLink, PrimGraph] =
    new PrimDesignController[PrimGraph]


  override def createRuntimeController(): GraphCanvasController[PrimVertex, PrimLink, PrimGraph] =
    new DragDropController[PrimVertex, PrimLink, PrimGraph](false)


  override def createDesignGraph(): PrimGraph =
    new PrimGraph


  override def runStepsBeforePausing: Int =
    0

  override def stylesheets: List[String] =
    List(
      BasicStyles.resourceUrl.toExternalForm,
      getClass.getResource("PrimStyles.css").toExternalForm
    )
}
