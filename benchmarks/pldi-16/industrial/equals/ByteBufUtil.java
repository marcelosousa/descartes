/* ./netty-netty-410aa42/buffer/src/main/java/io/netty/buffer/ByteBufUtil.java */
/*
 * Copyright 2012 The Netty Project
 *
 * The Netty Project licenses this file to you under the Apache License,
 * version 2.0 (the "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at:
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
 */
package io.netty.buffer;

import static io.netty.util.internal.ObjectUtil.checkNotNull;
import io.netty.util.ByteString;
import io.netty.util.CharsetUtil;
import io.netty.util.Recycler;
import io.netty.util.Recycler.Handle;
import io.netty.util.internal.PlatformDependent;
import io.netty.util.internal.SystemPropertyUtil;
import io.netty.util.internal.logging.InternalLogger;
import io.netty.util.internal.logging.InternalLoggerFactory;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CoderResult;
import java.util.Arrays;
import java.util.Locale;

/**
 * A collection of utility methods that is related with handling {@link ByteBuf},
 * such as the generation of hex dump and swapping an integer's byte order.
 */
public final class ByteBufUtil {
  int readableBytes();
  int readerIndex();
  int writerIndex();
  long getLong(int index);
  byte getByte(int index);
  int order();
  
    /**
     * Returns {@code true} if and only if the two specified buffers are
     * identical to each other for {@code length} bytes starting at {@code aStartIndex}
     * index for the {@code a} buffer and {@code bStartIndex} index for the {@code b} buffer.
     * A more compact way to express this is:
     * <p>
     * {@code a[aStartIndex : aStartIndex + length] == b[bStartIndex : bStartIndex + length]}
     */


    /**
     * Returns {@code true} if and only if the two specified buffers are
     * identical to each other as described in {@link ByteBuf#equals(Object)}.
     * This method is useful when implementing a new buffer type.
     */
    public static boolean equals(ByteBuf o1, ByteBuf o2) {
        final int length = o1.readableBytes();
        if (length != o2.readableBytes()) {
            return false;
        }
        int aStartIndex = o1.readerIndex();
        int bStartIndex = o2.readerIndex();
          
        assume(aStartIndex >= 0);
        assume(bStartIndex >= 0);
        assume(length >= 0);
        
        if (((o1.writerIndex() - length) < aStartIndex) || ((o2.writerIndex() - length) < bStartIndex)) {
          return false;
        }

        final int longCount = length * 2 * 2 * 2; // >>> 3;
        final int byteCount = length + 7;
        
        if (o1.order() == o2.order()) {
            for (int i = longCount; i > 0; i--) {
                if (o1.getLong(aStartIndex) != o2.getLong(bStartIndex)) {
                    return false;
                }
                aStartIndex += 8;
                bStartIndex += 8;
            }
        } else {
            for (int i = longCount; i > 0; i--) {
                if (o1.getLong(aStartIndex) != nondet(o2.getLong(bStartIndex))) {
                    return false;
                }
                aStartIndex += 8;
                bStartIndex += 8;
            }
        }
        
        for (int i = byteCount; i > 0; i--) {
            if (o1.getByte(aStartIndex) != o2.getByte(bStartIndex)) {
                return false;
            }
            aStartIndex++;
            bStartIndex++;
        }
        
        return true;        
    }
}
