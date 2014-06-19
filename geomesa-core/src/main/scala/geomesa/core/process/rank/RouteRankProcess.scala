package geomesa.core.process.rank

import com.vividsolutions.jts.geom.{Geometry, LineString}
import geomesa.core.data.AccumuloFeatureCollection
import geomesa.core.process.query.QueryProcess
import geomesa.utils.geotools.Conversions._
import org.apache.log4j.Logger
import org.geotools.data.simple.SimpleFeatureCollection
import org.geotools.data.store.ReTypingFeatureCollection
import org.geotools.factory.CommonFactoryFinder
import org.geotools.geometry.jts.{JTS, ReferencedEnvelope}
import org.geotools.process.factory.{DescribeParameter, DescribeProcess, DescribeResult}
import org.geotools.referencing.CRS
import org.geotools.referencing.crs.DefaultGeographicCRS

import scala.util.Try

/**
 * Created with IntelliJ IDEA.
 * User: kevin
 * Date: 6/18/14
 * Time: 5:37 PM
 */
@DescribeProcess(
title = "Geomesa-enabled Ranking of Feature Groups in Proximity to Route",
description = "Performs a proximity search on a Geomesa feature collection using another feature collection as input." +
  " Then groups the features according to a key and computes ranking metrics thats measures the prominence of " +
  "each key within the search region. The computed metrics measure the frequency of each feature group within the " +
  "search region, relative frequency in the surrounding area, the spatial diversity of the feature within the " +
  "region, and evidence of motion through the search region."
)
class RouteRankProcess {

  private val log = Logger.getLogger(classOf[RouteRankProcess])

  @DescribeResult(description = "Ranking metrics for each key value")
  def execute(
               @DescribeParameter(
                 name = "inputFeatures",
                 description = "This must be a single line string for now")
               inputFeatures: SimpleFeatureCollection,

               @DescribeParameter(
                 name = "dataFeatures",
                 description = "The data set to query for matching features")
               dataFeatures: SimpleFeatureCollection,

               @DescribeParameter(
                 name = "bufferDistance",
                 description = "Buffer size in meters")
               bufferDistance: java.lang.Double,

               @DescribeParameter(
                 name = "keyField",
                 description = "The name of the key attribute to group by")
               keyField: String

               ): Map[String, RankingValues] = {

    log.info("Attempting Geomesa Route Rank on collection type " + dataFeatures.getClass.getName)

    if (!dataFeatures.isInstanceOf[AccumuloFeatureCollection]) {
      log.warn("The provided data feature collection type may not support geomesa proximity search: " + dataFeatures.getClass.getName)
    }
    if (dataFeatures.isInstanceOf[ReTypingFeatureCollection]) {
      log.warn("WARNING: layer name in geoserver must match feature type name in geomesa")
    }

    val route = extractRoute(inputFeatures)
    route match {
      case Some(r) =>
        val spec = new SfSpec(keyField, "geomesa_index_start_time")
        val routeShape = r.route.bufferMeters(bufferDistance)
        val boxShape = boundingSquare(routeShape)
        val ff = CommonFactoryFinder.getFilterFactory2
        val boxFilter = ff.intersects(ff.property("geomesa_index_geometry"), ff.literal(boxShape))
        val routeFilter = ff.intersects(ff.property("geomesa_index_geometry"), ff.literal(routeShape))
        val qp = new QueryProcess
        val routeSearchResults = qp.execute(dataFeatures, routeFilter)
        val boxSearchResults = qp.execute(dataFeatures, boxFilter)
        val routeFeatures = new SimpleFeatureWithDateTimeAndKeyCollection(routeSearchResults, spec)
        val boxFeatures = new SimpleFeatureWithDateTimeAndKeyCollection(boxSearchResults, spec)
        val routeAndFeatures = new RouteAndSurroundingFeatures(r, boxFeatures, routeFeatures)
        routeAndFeatures.rank(boxShape.getEnvelopeInternal, List(routeShape))
      case _ =>
        log.warn("WARNING: input feature to rank process must be a single LineString")
        Map[String, RankingValues]()
    }
  }

  def extractRoute(inputFeatures: SimpleFeatureCollection): Option[Route] = {
    if (inputFeatures.size() == 1) {
      val routeTry = for {
        ls <- Try(inputFeatures.features().take(1).next().getDefaultGeometry.asInstanceOf[LineString])
        ls4326 <- Try(if (ls.getSRID == 4326) ls else {
          val sourceCRS = inputFeatures.getSchema.getCoordinateReferenceSystem
          val transform = CRS.findMathTransform(sourceCRS, DefaultGeographicCRS.WGS84, true)
          JTS.transform(ls, transform).asInstanceOf[LineString]
        })
        route = new Route(ls4326)
      } yield route
      routeTry.toOption
    }
    else None
  }

  def boundingSquare(bufferedRouteGeometry: Geometry) = {
    val env1 = bufferedRouteGeometry.getEnvelopeInternal
    val diffLat = env1.getMaxY - env1.getMinY
    val diffLon = env1.getMaxX - env1.getMinX
    val centerLat = (env1.getMaxY + env1.getMinY) / 2.0
    val centerLon = (env1.getMaxX + env1.getMinX) / 2.0
    val delta = (if (diffLat > diffLon) diffLat else diffLon) / 2.0
    val env2 = new ReferencedEnvelope(centerLon - delta, centerLon + delta, centerLat - delta, centerLat + delta,
      DefaultGeographicCRS.WGS84)
    JTS.toGeometry(env2)
  }


}