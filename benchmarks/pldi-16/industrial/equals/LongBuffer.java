/* ./libgdx-libgdx-d0121ac/backends/gdx-backends-gwt/src/com/badlogic/gdx/backends/gwt/emu/java/nio/LongBuffer.java */
/*
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package java.nio;

/** A buffer of longs.
 * <p>
 * A long buffer can be created in either of the following ways:
 * </p>
 * <ul>
 * <li>{@link #allocate(int) Allocate} a new long array and create a buffer based on it;</li>
 * <li>{@link #wrap(long[]) Wrap} an existing long array to create a new buffer;</li>
 * <li>Use {@link java.nio.ByteBuffer#asLongBuffer() ByteBuffer.asLongBuffer} to create a long buffer based on a byte buffer.</li>
 * </ul>
 * 
 * @since Android 1.0 */
public abstract class LongBuffer extends Buffer implements Comparable<LongBuffer> {
  int position;
  int limit;
  long get(int index);

	/** Checks whether this long buffer is equal to another object.
	 * <p>
	 * If {@code other} is not a long buffer then {@code false} is returned. Two long buffers are equal if and only if their
	 * remaining longs are exactly the same. Position, limit, capacity and mark are not considered.
	 * </p>
	 * 
	 * @param other the object to compare with this long buffer.
	 * @return {@code true} if this long buffer is equal to {@code other}, {@code false} otherwise.
	 * @since Android 1.0 */
	public int equals (LongBuffer o1, LongBuffer o2) {
   int o1rem = o1.limit - o1.position;
   int o2rem = o2.limit - o2.position;
   if (o1rem != o2rem) {
     return 0;
   }
   
   assume(o1.limit >= 0);
   assume(o1.limit == o2.limit);
   
   int i = o1.position;
   while (i < o1.limit) {
     if (o1.get(i) != o2.get(i)) {
       return 0;
     }
     i++;
   }
   return 1;
	}

}
