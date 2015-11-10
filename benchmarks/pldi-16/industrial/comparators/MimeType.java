/* ./spring-projects-spring-framework-7891c0d/spring-core/src/main/java/org/springframework/util/MimeType.java */
/*
 * Copyright 2002-2014 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.util;

import java.io.Serializable;
import java.nio.charset.Charset;
import java.util.BitSet;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeSet;

/**
 * Represents a MIME Type, as originally defined in RFC 2046 and subsequently used in
 * other Internet protocols including HTTP. This class however does not contain support
 * the q-parameters used in HTTP content negotiation. Those can be found in the sub-class
 * {@code org.springframework.http.MediaType} in the {@code spring-web} module.
 *
 * <p>Consists of a {@linkplain #getType() type} and a {@linkplain #getSubtype() subtype}.
 * Also has functionality to parse media types from a string using
 * {@link #valueOf(String)}. For more parsing options see {@link MimeTypeUtils}.
 *
 * @author Arjen Poutsma
 * @author Juergen Hoeller
 * @author Rossen Stoyanchev
 * @since 4.0
 * @see MimeTypeUtils
 */
public class MimeType implements Comparator<MimeType>, Serializable {

    int getType();
    int getSubtype();
    int getParametersSize();
    int get(int index);
    int getValue(int key);

	/**
	 * Compares this {@code MediaType} to another alphabetically.
	 * @param other media type to compare to
	 * @see MimeTypeUtils#sortBySpecificity(List)
	 */
	@Override
	public int compare(MimeType o1, MimeType o2) {	  
		int comp = String.compareIgnoreCase(o1.getType(), o2.getType()); 
		if (comp != 0) {
			return comp;
		}
		comp = String.compareIgnoreCase(o1.getSubtype(), o2.getSubtype());
		if (comp != 0) {
			return comp;
		}
		comp = o1.getParametersSize() - o2.getParametersSize();
		if (comp != 0) {
			return comp;
		}
				
		int o1Attribute;
		int o2Attribute;
		int o1Value;
		int o2Value;
		
		int i=0;
		while (i < o1.getParametersSize()) {		    
			o1Attribute = o1.get(i);
			o2Attribute = o2.get(i);
			
			comp = o1Attribute - o2Attribute;
			if (comp != 0) {
				return comp;
			}
			
			o1Value = o1.getValue(o1Attribute);
			o2Value = o2.getValue(o2Attribute);

			comp = o1Value - o2Value;
			if (comp != 0) {
				return comp;
			}
			i++;
		}
		return 0; 
	}


}
