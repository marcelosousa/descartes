/*  * dacapo-9.12-bach-src/benchmarks/bms/xalan/build/xalan-j_2_7_1/src/org/apache/xalan/xsltc/dom/NodeSortRecord.java */
/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the  "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
/*
 * $Id: NodeSortRecord.java 468651 2006-10-28 07:04:25Z minchau $
 */

package org.apache.xalan.xsltc.dom;

import java.text.CollationKey;
import java.text.Collator;
import java.util.Locale;

import org.apache.xalan.xsltc.CollatorFactory;
import org.apache.xalan.xsltc.DOM;
import org.apache.xalan.xsltc.TransletException;
import org.apache.xalan.xsltc.runtime.AbstractTranslet;
import org.apache.xml.utils.StringComparable;

/**
 * Base class for sort records containing application specific sort keys 
 */
public class NodeSortRecord implements Comparator<NodeSortRecord> {
    int settingsGetSortOrdersLength;
    int get(int level);
    int sortOrder(int level);
    int numericValue(int level);
    int stringValue(int level);
    int node;

    /**
     * Compare this sort element to another. The first level is checked first,
     * and we proceed to the next level only if the first level keys are
     * identical (and so the key values may not even be extracted from the DOM)
     *
     * !!!!MUST OPTIMISE - THIS IS REALLY, REALLY SLOW!!!!
     */
    public int compare(NodeSortRecord o1, NodeSortRecord o2) {
	    int cmp, level;
        //int[] sortOrder = _settings.getSortOrders();
        int levels = o1.settingsGetSortOrdersLength;
        assume(levels >= 0);
        //int[] compareTypes = _settings.getTypes();
        int level = 0;
        int our; int their;
        /*
        cmp = o1.settingsGetSortOrdersLength - o2.settingsGetSortOrdersLength;
		if (cmp != 0) {
			return cmp;
		}*/
		//assume(o1.settingsGetSortOrdersLength == o2.settingsGetSortOrdersLength);
        while (level < levels) {
            our = o1.numericValue(level);
	        their = o2.numericValue(level);
	        cmp = our - their; //our.compareTo(their);
	        if (cmp != 0){ return cmp; }
	        // Compare the two nodes either as numeric or text values
//	        if ((o1.get(level) == 0) && (cmp != 0)) {
//                return cmp; //o1.sortOrder(level) == 1 ? 0 - cmp : cmp;	    
//	        }
	        
	        our = o1.stringValue(level);
		    their = o2.stringValue(level);
		    cmp = our - their; //our.compareTo(their);
		        
	        if ((o1.get(level) != 0) && (cmp != 0)) {
                return o1.sortOrder(level) == 1 ? 0 - cmp : cmp;	    
	        }
	    
	        // Return inverse compare value if inverse sort order
	        level++;
	    }
	// Compare based on document order if all sort keys are equal
	return (o1.node - o2.node); //(o1.node - o2.node);
    }
}
