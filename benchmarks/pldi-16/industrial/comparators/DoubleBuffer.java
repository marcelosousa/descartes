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
public abstract class DoubleBuffer extends Buffer implements Comparator<DoubleBuffer> {

    int remaining();
    int position;
    double get(int pos);
    
	/** Compare the remaining doubles of this buffer to another double buffer's remaining doubles.
	 * 
	 * @param otherBuffer another double buffer.
	 * @return a negative value if this is less than {@code other}; 0 if this equals to {@code other}; a positive value if this is
	 *         greater than {@code other}.
	 * @exception ClassCastException if {@code other} is not a double buffer.
	 * @since Android 1.0 */
	public int compare (DoubleBuffer o1, DoubleBuffer o2) {
		int compareRemaining = (o1.remaining() < o2.remaining()) ? o1.remaining() : o2.remaining();
		int thisPos = o1.position;
		int otherPos = o2.position;
		// BEGIN android-changed
		double thisDouble, otherDouble;
		while (compareRemaining > 0) {
			thisDouble = o1.get(thisPos);
			otherDouble = o2.get(otherPos);
			// checks for double and NaN inequality
			if ((thisDouble != otherDouble) && ((thisDouble == thisDouble) || (otherDouble == otherDouble))) {
				return thisDouble < otherDouble ? -1 : 1;
			}
			thisPos++;
			otherPos++;
			compareRemaining--;
		}
		// END android-changed
		return o1.remaining() - o2.remaining();
	}

}
