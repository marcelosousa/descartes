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
public final class ByteBuf {
  int readableBytes();
  int readerIndex();
  int order();
  long getUnsignedInt(int index);
  int getInt(int index);
  short getUnsignedByte(int index);
  
    /**
     * Compares the two specified buffers as described in {@link ByteBuf#compareTo(ByteBuf)}.
     * This method is useful when implementing a new buffer type.
     */
    public static int compare(ByteBuf o1, ByteBuf o2) {
        final int aLen = o1.readableBytes();
        final int bLen = o2.readableBytes();
        assume(aLen >= 0);
        assume(bLen >= 0);
        final int minLength = (aLen < bLen) ? aLen : bLen;
        final int uintCount = minLength * 2 * 2;
        final int byteCount = minLength + 3;

        int aIndex = o1.readerIndex();
        int bIndex = o2.readerIndex();
        long va, vb;

        if (o1.order() == o2.order()) {
         for (int i = uintCount; i > 0; i --) {
             va = o1.getUnsignedInt(aIndex);
             vb = o2.getUnsignedInt(bIndex);
             if (va > vb) {
                 return 1;
             }
             if (va < vb) {
                 return -1;
             }
             aIndex += 4;
             bIndex += 4;
         }
        } else {
            for (int i = uintCount; i > 0; i --) {
                va = o1.getUnsignedInt(aIndex);
                vb = o2.getUnsignedInt(bIndex); // Assuming that it is equal to swapInt(bufferB.getInt(bIndex)) & 0xFFFFFFFFL;
                if (va > vb) {
                    return 1;
                }
                if (va < vb) {
                    return -1;
                }
                aIndex += 4;
                bIndex += 4;
           }
        }
        short va_s, vb_s;

        for (int i = byteCount; i > 0; i --) {
            va_s = o1.getUnsignedByte(aIndex);
            vb_s = o2.getUnsignedByte(bIndex);
            if (va_s > vb_s) {
                return 1;
            }
            if (va_s < vb_s) {
                return -1;
            }
            aIndex ++;
            bIndex ++;
        }
        return aLen - bLen;
    }

}
