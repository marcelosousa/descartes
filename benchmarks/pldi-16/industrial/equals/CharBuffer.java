/* ./libgdx-libgdx-d0121ac/backends/gdx-backends-gwt/src/com/badlogic/gdx/backends/gwt/emu/java/nio/CharBuffer.java */
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

import java.io.IOException;

/** A buffer of chars.
 * <p>
 * A char buffer can be created in either one of the following ways:
 * </p>
 * <ul>
 * <li>{@link #allocate(int) Allocate} a new char array and create a buffer based on it;</li>
 * <li>{@link #wrap(char[]) Wrap} an existing char array to create a new buffer;</li>
 * <li>{@link #wrap(CharSequence) Wrap} an existing char sequence to create a new buffer;</li>
 * <li>Use {@link java.nio.ByteBuffer#asCharBuffer() ByteBuffer.asCharBuffer} to create a char buffer based on a byte buffer.</li>
 * </ul>
 * 
 * @since Android 1.0 */
public abstract class CharBuffer extends Buffer implements Comparable<CharBuffer>, CharSequence, Appendable {// , Readable {
  int position;
  int limit;
  char get(int index);

	/** Checks whether this char buffer is equal to another object.
	 * <p>
	 * If {@code other} is not a char buffer then {@code false} is returned. Two char buffers are equal if and only if their
	 * remaining chars are exactly the same. Position, limit, capacity and mark are not considered.
	 * </p>
	 * 
	 * @param other the object to compare with this char buffer.
	 * @return {@code true} if this char buffer is equal to {@code other}, {@code false} otherwise.
	 * @since Android 1.0 */
 	public boolean equals (CharBuffer o1, CharBuffer o2) {
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
