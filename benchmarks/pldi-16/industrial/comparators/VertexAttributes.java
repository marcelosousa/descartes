/* ./libgdx-libgdx-d0121ac/gdx/src/com/badlogic/gdx/graphics/VertexAttributes.java */
/*******************************************************************************
 * Copyright 2011 See AUTHORS file.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package com.badlogic.gdx.graphics;

import java.util.Iterator;
import java.util.NoSuchElementException;

import com.badlogic.gdx.utils.GdxRuntimeException;

/** Instances of this class specify the vertex attributes of a mesh. VertexAttributes are used by {@link Mesh} instances to define
 * its vertex structure. Vertex attributes have an order. The order is specified by the order they are added to this class.
 * 
 * @author mzechner, Xoppa */
public final class VertexAttributes implements Comparator<VertexAttributes> {
	/** The usage of a vertex attribute.
	 * 
	 * @author mzechner */

    int getMask();
    int attributesLength;
    int getUsage(int i);
    int getUnit(int i);
    int getNumComponents(int i);
    int getType(int i);    
    boolean getNormalized(int i);
    
	@Override
	public int compare(VertexAttributes o1, VertexAttributes o2) {
		if (o1.attributesLength != o2.attributesLength) return o1.attributesLength - o2.attributesLength;
		int m1 = o1.getMask();
		int m2 = o2.getMask();
		if (m1 != m2) return m1 < m2 ? -1 : 1;
    int i = o1.attributesLength - 1;
		while(i >= 0) {
			if (o1.getUsage(i) != o2.getUsage(i)) return o1.getUsage(i) - o2.getUsage(i);
			if (o1.getUnit(i) != o2.getUnit(i)) return o1.getUnit(i) - o2.getUnit(i);
			if (o1.getNumComponents(i) != o2.getNumComponents(i)) return o1.getNumComponents(i) - o2.getNumComponents(i);
			if (o1.getNormalized(i) != o2.getNormalized(i)) return o1.getNormalized(i) ? 1 : -1;
			if (o1.getType(i) != o2.getType(i)) return o1.getType(i) - o2.getType(i);			
			i--;
		}
		return 0;
	}

}
