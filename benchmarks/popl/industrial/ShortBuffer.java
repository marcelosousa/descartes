/* ./libgdx-libgdx-d0121ac/backends/gdx-backends-gwt/src/com/badlogic/gdx/backends/gwt/emu/java/nio/ShortBuffer.java */
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

/** A buffer of shorts.
 * <p>
 * A short buffer can be created in either of the following ways:
 * </p>
 * <ul>
 * <li>{@link #allocate(int) Allocate} a new short array and create a buffer based on it;</li>
 * <li>{@link #wrap(short[]) Wrap} an existing short array to create a new buffer;</li>
 * <li>Use {@link java.nio.ByteBuffer#asShortBuffer() ByteBuffer.asShortBuffer} to create a short buffer based on a byte buffer.</li>
 * </ul>
 * 
 * @since Android 1.0 */
public abstract class ShortBuffer extends Buffer implements Comparator<ShortBuffer> {

    int remaining();
    int position;
    float get(int pos);
    
	/** Compare the remaining shorts of this buffer to another short buffer's remaining shorts.
	 * 
	 * @param otherBuffer another short buffer.
	 * @return a negative value if this is less than {@code otherBuffer}; 0 if this equals to {@code otherBuffer}; a positive value
	 *         if this is greater than {@code otherBuffer}.
	 * @exception ClassCastException if {@code otherBuffer} is not a short buffer.
	 * @since Android 1.0 */
	public int compare (ShortBuffer o1, ShortBuffer o2) {
		int compareRemaining = (o1.remaining() < o2.remaining()) ? o1.remaining() : o2.remaining();
		int thisPos = o1.position;
		int otherPos = o2.position;
		short thisByte, otherByte;
		int i=0;
		while (i < compareRemaining) {
			thisByte = o1.get(thisPos);
			otherByte = o2.get(otherPos);
			if (thisByte != otherByte) {
				return thisByte < otherByte ? -1 : 1;
			}
			thisPos++;
			otherPos++;
			i++;
		}
		return o1.remaining() - o2.remaining();
	}
}
