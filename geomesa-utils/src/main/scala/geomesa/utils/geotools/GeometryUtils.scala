/*
 * Copyright 2013 Commonwealth Computer Research, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package geomesa.utils.geotools

import com.vividsolutions.jts.geom.{Coordinate, Envelope, Point, Polygon}
import org.geotools.geometry.jts.{JTS, JTSFactoryFinder}
import org.geotools.referencing.GeodeticCalculator
import org.geotools.referencing.crs.DefaultGeographicCRS

/**
 * The object provides convenience methods for common operations on geometries.
 */
object GeometryUtils {

  val geoCalc = new GeodeticCalculator(DefaultGeographicCRS.WGS84)
  val geoFactory = JTSFactoryFinder.getGeometryFactory

  /**
   * Returns a bounding box which circumscribes a buffered circle around a point
   *
   * @param startingPoint the Point to buffer around
   * @param distance the buffered distance in meters
   * @return A Polygon which bounds the buffered point
   */
  def bufferPoint(startPoint: Point, distance: Double): Polygon = {
    geoCalc.setStartingGeographicPoint(startPoint.getX, startPoint.getY)

    // Convert meters to dec degrees based on widest point in dec degrees of circle
    geoCalc.setDirection(90, distance)
    val right = geoCalc.getDestinationGeographicPoint
    val distanceDegrees = startPoint.distance(geoFactory.createPoint(new Coordinate(right.getX, right.getY)))

    // Walk circle bounds for bounding box
    geoCalc.setDirection(0, distance)
    val top = geoCalc.getDestinationGeographicPoint
    geoCalc.setDirection(180, distance)
    val bottom = geoCalc.getDestinationGeographicPoint
    geoCalc.setStartingGeographicPoint(top)
    geoCalc.setDirection(90, distance)
    val topRight = geoCalc.getDestinationGeographicPoint
    geoCalc.setDirection(-90, distance)
    val topLeft = geoCalc.getDestinationGeographicPoint
    geoCalc.setStartingGeographicPoint(bottom)
    geoCalc.setDirection(90, distance)
    val bottomRight = geoCalc.getDestinationGeographicPoint
    geoCalc.setDirection(-90, distance)
    val bottomLeft = geoCalc.getDestinationGeographicPoint

    val env = (new Envelope(startPoint.getCoordinate))
    env.expandToInclude(topRight.getX, topRight.getY)
    env.expandToInclude(topLeft.getX, topLeft.getY)
    env.expandToInclude(bottomRight.getX, bottomRight.getY)
    env.expandToInclude(bottomLeft.getX, bottomLeft.getY)
    JTS.toGeometry(env)
  }

  /** Convert meters to dec degrees based on widest point in dec degrees of circle */
  def distanceDegrees(startPoint: Point, meters: Double) = {
    startPoint.distance(farthestPoint(startPoint, meters))
  }

  /** Farthest point based on widest point in dec degrees of circle */
  def farthestPoint(startPoint: Point, meters: Double) = {
    val calc = new GeodeticCalculator()
    calc.setStartingGeographicPoint(startPoint.getX, startPoint.getY)
    calc.setDirection(90, meters)
    val dest2D = calc.getDestinationGeographicPoint
    geoFactory.createPoint(new Coordinate(dest2D.getX, dest2D.getY))
  }

}
