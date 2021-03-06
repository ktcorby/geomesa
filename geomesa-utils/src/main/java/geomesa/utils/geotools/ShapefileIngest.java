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

package geomesa.utils.geotools;

import org.geotools.data.DataStore;

/**
 * Utility class to help bridge the Java and Scala code for external consumers
 * of the API.
 */
public class ShapefileIngest {
    public static DataStore ingestShapefile(String shapefileName,
                                       DataStore dataStore,
                                       String featureName) {
        // invoke the Scala code
        return geomesa.utils.geotools.GeneralShapefileIngest$.MODULE$.shpToDataStore(
                shapefileName, dataStore, featureName);
    }
}
