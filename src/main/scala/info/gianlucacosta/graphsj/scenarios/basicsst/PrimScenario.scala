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

package info.gianlucacosta.graphsj.scenarios.basicsst

import info.gianlucacosta.eighthbridge.fx.canvas.GraphCanvasController
import info.gianlucacosta.eighthbridge.graphs.point2point.visual.{DefaultVisualGraph, VisualGraph}
import info.gianlucacosta.eighthbridge.util.DesktopUtils
import info.gianlucacosta.eighthbridge.util.fx.dialogs.Alerts
import info.gianlucacosta.graphsj.{Algorithm, Scenario}

import scalafx.geometry.Dimension2D


class PrimScenario extends Scenario {
  private val WebsiteUrl = "https://github.com/giancosta86/GraphsJ-Algorithms"

  override val name: String = "Prim's Shortest Spanning Tree (Basic version)"


  override def showHelp(): Unit = {
    DesktopUtils.openBrowser(WebsiteUrl)
  }


  override def showSettings(): Unit = {
    Alerts.showInfo("This basic implementation of the scenario does not support settings")
  }


  override def createAlgorithm(): Algorithm =
    new PrimAlgorithm


  override def createDesignController(): GraphCanvasController =
    new PrimDesignController


  override def createDesignGraph(): VisualGraph =
    new DefaultVisualGraph(
      false,
      new Dimension2D(700, 500)
    )
}
