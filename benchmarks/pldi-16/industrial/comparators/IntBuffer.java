/* ./libgdx-libgdx-d0121ac/backends/gdx-backends-gwt/src/com/badlogic/gdx/backends/gwt/emu/java/nio/IntBuffer.java */
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

/** A buffer of ints.
 * <p>
 * A int buffer can be created in either of the following ways:
 * </p>
 * <ul>
 * <li>{@link #allocate(int) Allocate} a new int array and create a buffer based on it;</li>
 * <li>{@link #wrap(int[]) Wrap} an existing int array to create a new buffer;</li>
 * <li>Use {@link java.nio.ByteBuffer#asIntBuffer() ByteBuffer.asIntBuffer} to create a int buffer based on a byte buffer.</li>
 * </ul>
 * 
 * @since Android 1.0 */
public abstract class IntBuffer extends Buffer implements Comparator<IntBuffer> {

    int remaining();
    int position;
    int get(int pos);
    
	/** Compares the remaining ints of this buffer to another int buffer's remaining ints.
	 * 
	 * @param otherBuffer another int buffer.
	 * @return a negative value if this is less than {@code other}; 0 if this equals to {@code other}; a positive value if this is
	 *         greater than {@code other}.
	 * @exception ClassCastException if {@code other} is not an int buffer.
	 * @since Android 1.0 */
	public int compare (IntBuffer o1, IntBuffer o2) {
		int compareRemaining = (o1.remaining() < o2.remaining()) ? o1.remaining() : o2.remaining();
		int thisPos = o1.position;
		int otherPos = o2.position;
		// BEGIN android-changed
		int thisInt, otherInt;
		while (compareRemaining > 0) {
			thisInt = o1.get(thisPos);
			otherInt = o2.get(otherPos);
			if (thisInt != otherInt) {
				return thisInt < otherInt ? -1 : 1;
			}
			thisPos++;
			otherPos++;
			compareRemaining--;
		}
		// END android-changed
		return o1.remaining() - o2.remaining();
	}

}
