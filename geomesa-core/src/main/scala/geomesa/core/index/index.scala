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

package geomesa.core

import org.apache.accumulo.core.data.{Value, Key}
import org.geotools.data.DataUtilities
import org.geotools.factory.Hints.{IntegerKey, ClassKey}
import org.geotools.filter.identity.FeatureIdImpl
import org.geotools.geometry.jts.ReferencedEnvelope
import org.joda.time.DateTime
import org.opengis.feature.simple.SimpleFeatureType
import org.opengis.filter.identity.FeatureId

/**
 * These are package-wide constants.
 */
package object index {
  val MIN_DATE = new DateTime(Long.MinValue)
  val MAX_DATE = new DateTime(Long.MaxValue)

  val SF_PROPERTY_GEOMETRY   = "geomesa_index_geometry"
  val SF_PROPERTY_START_TIME = "geomesa_index_start_time"
  val SF_PROPERTY_END_TIME   = "geomesa_index_end_time"

  def getDtgFieldName(sft: SimpleFeatureType) = Option(sft.getUserData.get(SF_PROPERTY_START_TIME)).map{_.toString}
  // wrapping function in option to protect against incorrect values in SF_PROPERTY_START_TIME
  def getDtgDescriptor(sft: SimpleFeatureType) = getDtgFieldName(sft).flatMap{name => Option(sft.getDescriptor(name))}
  val spec = "geomesa_index_geometry:Geometry:srid=4326,geomesa_index_start_time:Date,geomesa_index_end_time:Date"
  val indexSFT = DataUtilities.createType("geomesa-idx", spec)

  implicit def string2id(s: String): FeatureId = new FeatureIdImpl(s)

  type KeyValuePair = (Key, Value)

  object QueryHints {
    val DENSITY_KEY = new ClassKey(classOf[java.lang.Boolean])
    val WIDTH_KEY   = new IntegerKey(256)
    val HEIGHT_KEY  = new IntegerKey(256)
    val BBOX_KEY    = new ClassKey(classOf[ReferencedEnvelope])
  }
}

