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

import info.gianlucacosta.eighthbridge.fx.canvas.basic.BasicVertexNode
import info.gianlucacosta.eighthbridge.fx.canvas.basic.BasicVertexNode.DimensionQuery
import info.gianlucacosta.eighthbridge.graphs.point2point.ArcBinding
import info.gianlucacosta.eighthbridge.graphs.point2point.visual.VisualGraph
import info.gianlucacosta.lambdaprism.classification.basic.{DecisionTree, DecisionTreeLeaf, Value}

import scalafx.geometry.{Dimension2D, Point2D}


object DecisionTreeGraph {
  def apply(decisionTree: DecisionTree): DecisionTreeGraph = {
    val rootVertexDimension: Dimension2D =
      BasicVertexNode.getDimensions(
        DecisionTreeScenario.Stylesheets,
        List(
          DimensionQuery(
            TreeRootVertex.formatAttribute(
              decisionTree.attribute
            )
          )
        )
      ).head


    val treeLeavesList: List[DecisionTreeLeaf] =
      decisionTree
        .leaves
        .values
        .toList


    val leafQueries: List[DimensionQuery] =
      treeLeavesList.map(leaf =>
        DimensionQuery(
          TreeLeafVertex.formatLeaf(leaf)
        )
      )


    val leafVertexDimensions: Map[Value, Dimension2D] =
      treeLeavesList.map(_.attributeValue).zip(
        BasicVertexNode.getDimensions(
          DecisionTreeScenario.Stylesheets,
          leafQueries
        )
      )
        .toMap


    val rootNodeCenterY =
      30 +
        rootVertexDimension.height / 2


    val interLeafPadding: Double =
      90


    val totalLeafLevelWidth =
      leafVertexDimensions
        .values
        .map(_.width)
        .sum +
        interLeafPadding * (leafVertexDimensions.size - 1)


    val rootCenter =
      new Point2D(
        interLeafPadding + totalLeafLevelWidth / 2,

        rootNodeCenterY
      )


    val maxLeafHeight =
      leafVertexDimensions
        .values
        .map(_.height)
        .max


    val interLevelGap =
      160


    val leafCenterY =
      rootNodeCenterY +
        rootVertexDimension.height / 2 +
        interLevelGap +
        maxLeafHeight / 2


    val sortedAttributeValues: List[Value] =
      decisionTree
        .problem
        .getView(decisionTree.attribute)
        .attributeValues
        .toList
        .sorted


    val leafCenters: Map[Value, Point2D] =
      sortedAttributeValues.foldLeft(0.0, List[(Value, Point2D)]()) {
        case ((lastRightSide, cumulatedCenters), attributeValue) =>
          val leafWidth =
            leafVertexDimensions(attributeValue).width

          val leafCenter =
            new Point2D(
              lastRightSide +
                interLeafPadding +
                leafWidth / 2,

              leafCenterY
            )

          val leafRightSide: Double =
            leafCenter.x +
              leafWidth / 2

          (
            leafRightSide,
            (attributeValue -> leafCenter) :: cumulatedCenters
            )
      }._2.toMap


    val rootVertex: TreeRootVertex =
      new TreeRootVertex(
        decisionTree,
        rootCenter
      )


    val leafVertexes: Set[TreeLeafVertex] =
      leafCenters.iterator.map {
        case (attributeValue, leafCenter) =>
          new TreeLeafVertex(
            decisionTree.leaves(attributeValue),
            leafCenter
          )
      }.toSet


    val vertexes: Set[DecisionTreeVertex] =
      leafVertexes ++ Set(rootVertex)


    val baseGraph: DecisionTreeGraph =
      new DecisionTreeGraph(
        vertexes = vertexes,
        Set(),
        Set()
      )


    leafVertexes.foldLeft(baseGraph)((partialGraph, leafVertex) => {
      val link =
        new DecisionTreeLink(
          leafVertex.treeLeaf
        )

      partialGraph
        .addLink(
          rootVertex,
          leafVertex,
          link
        )
    })
  }
}


case class DecisionTreeGraph private(
                                      vertexes: Set[DecisionTreeVertex],
                                      links: Set[DecisionTreeLink],
                                      bindings: Set[ArcBinding]
                                    ) extends VisualGraph[DecisionTreeVertex, DecisionTreeLink, DecisionTreeGraph] {
  def this() = this(
    Set(
      InstructionsVertex
    ),
    Set(),
    Set()
  )


  override protected def graphCopy(vertexes: Set[DecisionTreeVertex], links: Set[DecisionTreeLink], bindings: Set[ArcBinding]): DecisionTreeGraph =
    copy(
      vertexes = vertexes,
      links = links,
      bindings = bindings
    )
}
