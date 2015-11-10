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
public final class VertexAttributes implements Iterable<VertexAttribute>, Comparable<VertexAttributes> {
	@Override
	int isVertexAttributes;
	int attributesLength;
	int reference;
	int get(int i);
	
	public boolean equals(VertexAttributes o1, VertexAttributes o2) {
		if (o1.attributesLength != o2.attributesLength) {
		  return false;
	  }
		for (int i = 0; i < o1.attributesLength; i++) {
		  if (equals(o1.get(i), o2.get(i)) == 0) {
		    return false;
		  }
		}
		return true;
	}
}
