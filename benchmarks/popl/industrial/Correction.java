/* ./elastic-elasticsearch-b2e022b/src/main/java/org/elasticsearch/search/suggest/phrase/Correction.java */
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
package org.elasticsearch.search.suggest.phrase;

import org.apache.lucene.util.BytesRef;
import org.apache.lucene.util.BytesRefBuilder;
import org.elasticsearch.search.suggest.SuggestUtils;
import org.elasticsearch.search.suggest.phrase.DirectCandidateGenerator.Candidate;

import java.util.Arrays;

//TODO public for tests
public final class Correction implements Comparator<Correction> {

    //public static final Correction[] EMPTY = new Correction[0];
    public double score;
    
    // public final Candidate[] candidates; 
    int candidatesLength;
    int getCandidate(int index);

    /** Lower scores sorts first; if scores are equal,
     *  than later terms (zzz) sort first .*/
    @Override
    public int compare(Correction o1, Correction o2) {
        // inlined function
        if (o1.score == o2.score){
            int limit = o1.candidatesLength < o2.candidatesLength ? o1.candidatesLength : o2.candidatesLength; 
            int i=0;
            int cmp;
            while(i < limit){
                cmp = Int.compare(o1.getCandidate(i), o2.getCandidate(i));
                if (cmp != 0){
                    return -cmp;
                }
                i++;
            }
            return o1.candidatesLength - o2.candidatesLength;
        } else {
            return o1.score == o2.score ? 0 : o1.score > o2.score ? 1 : -1; //Double.compare(o1.score, o2.score);
        }        
    }
}
