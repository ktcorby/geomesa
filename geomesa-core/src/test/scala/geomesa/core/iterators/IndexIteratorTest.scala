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

package geomesa.core.iterators

import collection.JavaConversions._
import com.vividsolutions.jts.geom.{Polygon, Geometry}
import geomesa.core.data._
import geomesa.core.index._
import geomesa.utils.text.WKTUtils
import org.apache.accumulo.core.client.mock.MockInstance
import org.apache.accumulo.core.client.security.tokens.PasswordToken
import org.geotools.data.simple.SimpleFeatureStore
import org.geotools.data.{Query, DataUtilities}
import org.geotools.factory.Hints
import org.geotools.feature.simple.SimpleFeatureBuilder
import org.geotools.filter.text.ecql.ECQL
import org.joda.time.{Duration, Interval, DateTime}
import org.junit.runner.RunWith
import org.opengis.feature.simple.SimpleFeature
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class IndexIteratorTest extends SpatioTemporalIntersectingIteratorTest {

  import geomesa.utils.geotools.Conversions._

  object IITest {

    // utility function that can encode multiple types of geometry
    def createSimpleFeature(id: String, wkt: String, dt: DateTime = null): SimpleFeature = {
      val geomType: String = wkt.split( """\(""").head
      val geometry: Geometry = WKTUtils.read(wkt)
      val entry = SimpleFeatureBuilder.build(TestData.featureType,
        List(null, null, null, null, geometry, dt.toDate, dt.toDate), s"|data|$id")
      entry.setAttribute(geomType, id)
      entry.setAttribute("attr2", "2nd" + id)
      entry.getUserData.put(Hints.USE_PROVIDED_FID, java.lang.Boolean.TRUE)
      entry.getUserData.put(Hints.PROVIDED_FID, entry.toString)
      entry
    }

    def convertToSimpleFeatures(entries: List[TestData.Entry] = TestData.fullData): List[SimpleFeature] = {
      entries.map { entry =>
        createSimpleFeature(entry.id, entry.wkt, entry.dt)
      }
    }

    def setupMockFeatureSource(entries: List[TestData.Entry]): SimpleFeatureStore = {
      val mockInstance = new MockInstance("dummy")
      val c = mockInstance.getConnector("user", new PasswordToken("pass".getBytes))
      if (c.tableOperations.exists(TEST_TABLE)) c.tableOperations.delete(TEST_TABLE)

      val dsf = new AccumuloDataStoreFactory

      import AccumuloDataStoreFactory.params._

      val ds = dsf.createDataStore(
        Map(
          zookeepersParam.key -> "dummy",
          instanceIdParam.key -> "dummy",
          userParam.key -> "user",
          passwordParam.key -> "pass",
          authsParam.key -> "S,USA",
          tableNameParam.key -> "test_table",
          mockParam.key -> "true",
          featureEncParam.key -> "avro",
          idxSchemaParam.key -> TestData.schemaEncoding
        ))

      ds.createSchema(TestData.featureType)
      val fs = ds.getFeatureSource(TestData.featureName).asInstanceOf[SimpleFeatureStore]
      val dataFeatures = convertToSimpleFeatures(entries)
      val featureCollection = DataUtilities.collection(dataFeatures)
      fs.addFeatures(featureCollection)
      fs.getTransaction.commit()
      fs
    }
  }

  override def runMockAccumuloTest(label: String,
                                   entries: List[TestData.Entry] = TestData.fullData,
                                   ecqlFilter: Option[String] = None,
                                   numExpectedDataIn: Int = 113,
                                   dtFilter: Interval = null,
                                   overrideGeometry: Boolean = false,
                                   doPrint: Boolean = true): Int = {

    // create the query polygon
    val polygon: Polygon = overrideGeometry match {
      case true => IndexSchema.everywhere
      case false => WKTUtils.read(TestData.wktQuery).asInstanceOf[Polygon]
    }

    //create the Feature Source
    val fs = IITest.setupMockFeatureSource(entries)

    val gf = s"WITHIN(geomesa_index_geometry, ${polygon.toText})"
    val dt: Option[String] = Option(dtFilter).map(int =>
      s"(geomesa_index_start_time between '${int.getStart}' AND '${int.getEnd}')"
    )
    def red(f: String, og: Option[String]) = og match {
      case Some(g) => s"$f AND $g"
      case None => f
    }

    val tfString = red(red(gf, dt), ecqlFilter)
    val tf = ECQL.toFilter(tfString)


    // select a few attributes to trigger the IndexIterator
    // Note that since we are re-running all the tests from the IntersectingIteratorTest,
    // some of the tests may actually use the IntersectingIterator
    val outputAttributes = Array("geomesa_index_geometry")
    //val q = new Query(TestData.featureType.getTypeName, tf)
    val q = new Query(TestData.featureType.getTypeName, tf, outputAttributes)
    val sfCollection = fs.getFeatures(q)
    sfCollection.features().count(x => true)
  }
}
