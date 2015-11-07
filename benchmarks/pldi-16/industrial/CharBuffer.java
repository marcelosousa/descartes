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
public abstract class CharBuffer extends Buffer implements Comparator<CharBuffer>, CharSequence, Appendable {// , Readable {
    int remaining();
    int position;
    char get(int position);
    
	/** Compare the remaining chars of this buffer to another char buffer's remaining chars.
	 * 
	 * @param otherBuffer another char buffer.
	 * @return a negative value if this is less than {@code otherBuffer}; 0 if this equals to {@code otherBuffer}; a positive value
	 *         if this is greater than {@code otherBuffer}.
	 * @exception ClassCastException if {@code otherBuffer} is not a char buffer.
	 * @since Android 1.0 */
	public int compare (CharBuffer o1, CharBuffer o2) {
		int compareRemaining = (o1.remaining() < o2.remaining()) ? o1.remaining() : o2.remaining();
		int thisPos = o1.position;
		int otherPos = o2.position;
		char thisByte, otherByte;
		int i = 0;		
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
