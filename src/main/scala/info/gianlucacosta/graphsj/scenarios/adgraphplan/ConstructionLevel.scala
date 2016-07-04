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

import scalafx.geometry.{Dimension2D, Point2D}

trait ConstructionLevel[TItem, TVertex <: ConstructionVertex] {
  def items: List[TItem]


  private val mutexMap: Map[TItem, Set[TItem]] =
    createMutexMap()


  protected def createMutexMap(): Map[TItem, Set[TItem]]


  private val estimatedWidths: Map[TItem, Double] =
    items
      .map(item =>
        item -> estimateVertexWidth(item)
      )
      .toMap


  private def estimateVertexWidth(item: TItem): Double = {
    val mutexStrings =
      mutexMap
        .getOrElse(item, Set())
        .map(_.toString)

    val lines =
      Set(
        item.toString,
        ConstructionVertex.MutexString
      ) ++ mutexStrings

    26 + 10 * lines
      .map(_.length)
      .max
  }


  private val estimatedHeights: Map[TItem, Double] =
    items
      .map(item =>
        item -> estimateVertexHeight(item)
      )
      .toMap


  private def estimateVertexHeight(item: TItem): Double = {
    val mutexes =
      mutexMap.getOrElse(item, Set())

    val numLines =
      if (mutexes.nonEmpty)
        1 + 3 + mutexes.size
      else
        1

    23 + 18 * numLines
  }


  val width: Double =
    if (items.isEmpty)
      0
    else
      estimatedWidths.values.max


  protected val horizontalSpacing: Double =
    250


  protected val verticalSpacing: Double =
    80


  val top: Double =
    20


  private val vertexMap: Map[TItem, TVertex] =
    if (items.isEmpty)
      Map()
    else {
      val firstPair =
        items.head -> createVertex(
          items.head,
          top
        )

      items.tail.foldLeft(List(firstPair)) {
        case (cumulatedPairs, item) =>
          val (previousItem, previousVertex) =
            cumulatedPairs.head

          val vertex = createVertex(
            item,
            previousVertex.center.y + estimatedHeights(previousItem) / 2 + verticalSpacing
          )

          val currentPair =
            item -> vertex


          currentPair :: cumulatedPairs
      }.toMap
    }


  def left: Double


  def height: Double =
    items
      .lastOption
      .map(lastItem => {
        val lastVertex =
          vertexMap(lastItem)

        val lastHeight =
          estimatedHeights(lastItem)

        lastVertex.center.y + lastHeight / 2 - top
      })
      .getOrElse(0)


  private def createVertex(item: TItem, currentTop: Double): TVertex = {
    val center = new Point2D(
      left + width / 2,
      currentTop + estimatedHeights(item) / 2
    )


    val size =
      new Dimension2D(
        estimatedWidths(item),
        estimatedHeights(item)
      )

    doCreateVertex(
      item,
      mutexMap.getOrElse(item, Set()),
      size,
      center
    )
  }


  protected def doCreateVertex(item: TItem, mutexes: Set[TItem], size: Dimension2D, center: Point2D): TVertex


  def getVertex(item: TItem): TVertex =
    vertexMap(item)


  def vertexes: Iterable[TVertex] =
    vertexMap.values


  def inMutex(anItem: TItem, anotherItem: TItem): Boolean =
    mutexMap.getOrElse(anItem, Set()).contains(anotherItem)


  def duplicates(otherLevel: ConstructionLevel[TItem, TVertex]): Boolean = {
    items == otherLevel.items &&
      mutexMap == otherLevel.mutexMap
  }
}
