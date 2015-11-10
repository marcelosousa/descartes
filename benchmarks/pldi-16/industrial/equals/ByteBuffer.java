/* ./libgdx-libgdx-d0121ac/backends/gdx-backends-gwt/src/com/badlogic/gdx/backends/gwt/emu/java/nio/ByteBuffer.java */
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

import com.google.gwt.corp.compatibility.Endianness;
import com.google.gwt.corp.compatibility.StringToByteBuffer;

/** A buffer for bytes.
 * <p>
 * A byte buffer can be created in either one of the following ways:
 * </p>
 * <ul>
 * <li>{@link #allocate(int) Allocate} a new byte array and create a buffer based on it;</li>
 * <li>{@link #allocateDirect(int) Allocate} a memory block and create a direct buffer based on it;</li>
 * <li>{@link #wrap(byte[]) Wrap} an existing byte array to create a new buffer.</li>
 * </ul>
 * @since Android 1.0 */
public abstract class ByteBuffer extends Buffer implements Comparable<ByteBuffer>, StringToByteBuffer {
  int position;
  int limit;
  Byte get(int index);
  
	/** Checks whether this byte buffer is equal to another object.
	 * <p>
	 * If {@code other} is not a byte buffer then {@code false} is returned. Two byte buffers are equal if and only if their
	 * remaining bytes are exactly the same. Position, limit, capacity and mark are not considered.
	 * </p>
	 * 
	 * @param other the object to compare with this byte buffer.
	 * @return {@code true} if this byte buffer is equal to {@code other}, {@code false} otherwise.
	 * @since Android 1.0 */
	public boolean equals (ByteBuffer o1, ByteBuffer o2) {
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
