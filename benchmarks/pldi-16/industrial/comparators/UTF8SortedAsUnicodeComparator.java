/* ./elastic-elasticsearch-b2e022b/src/main/java/org/elasticsearch/common/text/UTF8SortedAsUnicodeComparator.java */
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
package org.elasticsearch.common.text;

import org.elasticsearch.common.bytes.BytesReference;

import java.util.Comparator;

// LUCENE 4 UPGRADE: Is this the right way of comparing bytesreferences inside Text instances?
// Copied from Lucene's BytesRef comparator
public class UTF8SortedAsUnicodeComparator implements Comparator<BytesReference> {

    boolean hasArray();
    int arrayOffset();
    int length;
    int get(int index);
    
    @Override
    public int compare(BytesReference o1, BytesReference o2) {
        if (o1.hasArray() && o2.hasArray()) {
            int aUpto = o1.arrayOffset();
            int bUpto = o2.arrayOffset();
            assume(aUpto == 0);
            int aStop = aUpto + (o1.length < o2.length ? o1.length : o2.length);  
            int aByte;
            int bByte;
            int diff;

            while (aUpto < aStop) {
                aByte = nondet(o1.get(aUpto)); 
                bByte = nondet(o2.get(bUpto)); 
                                                
                diff = aByte - bByte;
                if (diff != 0) {
                    return diff;
                }
                
                aUpto++;
                bUpto++;
            }
            // One is a prefix of the other, or, they are equal:
            return o1.length - o2.length;
        } else {
            int aUpto = 0;
            int bUpto = 0;

            int aStop = aUpto + (o1.length < o2.length ? o1.length : o2.length);  
            int aByte;
            int bByte;
            int diff;
            
            while (aUpto < aStop) {
                aByte = nondet(o1.get(aUpto));                 
                bByte = nondet(o2.get(bUpto)); 
                
                diff = aByte - bByte;
                if (diff != 0) {
                    return diff;
                }
                
                aUpto++;
                bUpto++;
            }

            // One is a prefix of the other, or, they are equal:
            return o1.length - o2.length;
        }
    }
}
