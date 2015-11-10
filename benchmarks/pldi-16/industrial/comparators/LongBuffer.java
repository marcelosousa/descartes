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
  
  int remaining();
  int position;
  long get(int pos);

	/** Compare the remaining longs of this buffer to another long buffer's remaining longs.
	 * 
	 * @param otherBuffer another long buffer.
	 * @return a negative value if this is less than {@code otherBuffer}; 0 if this equals to {@code otherBuffer}; a positive value
	 *         if this is greater than {@code otherBuffer}
	 * @exception ClassCastException if {@code otherBuffer} is not a long buffer.
	 * @since Android 1.0 */
	public int compare (LongBuffer o1, LongBuffer o2) {
		int compareRemaining = (o1.remaining() < o2.remaining()) ? o1.remaining() : o2.remaining();
		int thisPos = o1.position;
		int otherPos = o2.position;
		// BEGIN android-changed
		long thisLong, otherLong;
		while (compareRemaining > 0) {
			thisLong = o1.get(thisPos);
			otherLong = o2.get(otherPos);
			if (thisLong != otherLong) {
				return thisLong < otherLong ? -1 : 1;
			}
			thisPos++;
			otherPos++;
			compareRemaining--;
		}
		// END android-changed
		return o1.remaining() - o2.remaining();
	}

}
