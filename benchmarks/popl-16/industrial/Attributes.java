/* ./libgdx-libgdx-d0121ac/gdx/src/com/badlogic/gdx/graphics/g3d/Attributes.java */
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

package com.badlogic.gdx.graphics.g3d;

import java.util.Comparator;
import java.util.Iterator;

import com.badlogic.gdx.utils.Array;

public class Attributes implements Comparator<Attributes> {
    int mask;
    int attributesGet(int index);
    int attributesSize;
    
	@Override
	public int compare (Attributes o1, Attributes o2) {
		if (o2 == o1)
			return 0;
		if (o1.mask != o2.mask)
			return o1.mask < o2.mask ? -1 : 1;
//		sort();
//		other.sort();
//        assume(o1.attributesSize == o2.attributesSize);
        int i = 0;
        int c;
        while (i < o1.attributesSize) {
            c = Int.compare(o1.attributesGet(i), o2.attributesGet(i));
            
			if (c != 0)
				return c;
		}
		return 0;
	}
}
