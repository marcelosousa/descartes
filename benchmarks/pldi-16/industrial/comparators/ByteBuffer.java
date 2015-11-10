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
public abstract class ByteBuffer extends Buffer implements Comparator<ByteBuffer>, StringToByteBuffer {

    int remaining();
    int position;
    byte get(int pos);
    
	/** Compares the remaining bytes of this buffer to another byte buffer's remaining bytes.
	 * 
	 * @param otherBuffer another byte buffer.
	 * @return a negative value if this is less than {@code other}; 0 if this equals to {@code other}; a positive value if this is
	 *         greater than {@code other}.
	 * @exception ClassCastException if {@code other} is not a byte buffer.
	 * @since Android 1.0 */
	public int compare (ByteBuffer o1, ByteBuffer o2) {
		int compareRemaining = (o1.remaining() < o2.remaining()) ? o1.remaining() : o2.remaining();
		int thisPos = o1.position;
		int otherPos = o2.position;
		byte thisByte, otherByte;
		while (compareRemaining > 0) {
			thisByte = o1.get(thisPos);
			otherByte = o2.get(otherPos);
			if (thisByte != otherByte) {
				return thisByte < otherByte ? -1 : 1;
			}
			thisPos++;
			otherPos++;
			compareRemaining--;
		}
		return o1.remaining() - o2.remaining();
	}


}
