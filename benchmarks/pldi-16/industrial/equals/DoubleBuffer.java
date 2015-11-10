/* ./libgdx-libgdx-d0121ac/backends/gdx-backends-gwt/src/com/badlogic/gdx/backends/gwt/emu/java/nio/DoubleBuffer.java */
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

/** A buffer of doubles.
 * <p>
 * A double buffer can be created in either one of the following ways:
 * </p>
 * <ul>
 * <li>{@link #allocate(int) Allocate} a new double array and create a buffer based on it;</li>
 * <li>{@link #wrap(double[]) Wrap} an existing double array to create a new buffer;</li>
 * <li>Use {@link java.nio.ByteBuffer#asDoubleBuffer() ByteBuffer.asDoubleBuffer} to create a double buffer based on a byte buffer.
 * </li>
 * </ul>
 * 
 * @since Android 1.0 */
public abstract class DoubleBuffer extends Buffer implements Comparable<DoubleBuffer> {
  int position;
  int limit;
  double get(int index);
  
	/** Checks whether this double buffer is equal to another object.
	 * <p>
	 * If {@code other} is not a double buffer then {@code false} is returned. Two double buffers are equal if and only if their
	 * remaining doubles are exactly the same. Position, limit, capacity and mark are not considered.
	 * </p>
	 * 
	 * @param other the object to compare with this double buffer.
	 * @return {@code true} if this double buffer is equal to {@code other}, {@code false} otherwise.
	 * @since Android 1.0 */
	public boolean equals (DoubleBuffer o1, DoubleBuffer o2) {
   int o1rem = o1.limit - o1.position;
   int o2rem = o2.limit - o2.position;
   if (o1rem != o2rem) {
     return false;
   }
   
   assume(o1.limit >= 0);
   assume(o1.limit == o2.limit);
   
   int i = o1.position;
   while (i < o1.limit) {
     if (o1.get(i) != o2.get(i)) {
       return false;
     }
     i++;
   }
   return true;
	}

}
