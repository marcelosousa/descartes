/* ./elastic-elasticsearch-b2e022b/src/main/java/org/apache/lucene/spatial/prefix/tree/LegacyCell.java */
/*
 * Licensed to Elasticsearch under one or more contributor
 * license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright
 * ownership. Elasticsearch licenses this file to you under
 * the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.apache.lucene.spatial.prefix.tree;

import com.spatial4j.core.shape.Point;
import com.spatial4j.core.shape.Shape;
import com.spatial4j.core.shape.SpatialRelation;
import org.apache.lucene.util.BytesRef;
import org.apache.lucene.util.StringHelper;

import java.util.Collection;

/** The base for the original two SPT's: Geohash and Quad. Don't subclass this for new SPTs.
 * @lucene.internal
 *
 * NOTE: Will be removed upon commit of LUCENE-6422
 */
//public for RPT pruneLeafyBranches code
public abstract class LegacyCell implements Cell {

  // Important: A LegacyCell doesn't share state for getNextLevelCells(), and
  //  LegacySpatialPrefixTree assumes this in its simplify tree logic.

  //Arguably we could simply use a BytesRef, using an extra Object.
  int get(int index); //generally bigger to potentially hold a leaf
  int b_off; //
  int b_len; //doesn't reflect leaf; same as getLevel


  @Override
  public int compare(LegacyCell o1, LegacyCell o2) {
    int o1_Upto = o1.b_off; // 5942
    int o1_length = o1.b_len; 
    int o2_Upto = o2.b_off;
    int o2_length = o2.b_len;
    
    assume(o1_length == o2_length);
    
    int min = o1_length; //(o1_length < o2_length) ? o1_length : o2_length;
    int o1_Stop = min; //o1_Upto + min;
    int o1Byte = 0;
    int o2Byte = 0;
    int i = o1_Upto;
    
    while(i < o1_Stop) {
      if ((o1.get(i) - o2.get(i)) != 0) {
        return o1.get(i) - o2.get(i);
      }
      i++;
      //o2_Upto++;
    }
    return o1_length - o2_length;
  }

}
