/*
 * Copyright 1999-2012 Alibaba Group.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.alibaba.dubbo.remoting.buffer;

import java.nio.ByteBuffer;

/**
 * @author <a href="mailto:gang.lvg@alibaba-inc.com">kimi</a>
 */
public final class ChannelBuffers {
  int readableBytes();
  int readerIndex();
  byte getByte(int index);
  
    public static boolean equals(ChannelBuffer o1, ChannelBuffer o2) {
        final int aLen = o1.readableBytes();
        if (aLen != o2.readableBytes()) {
            return false;
        }

        final int byteCount = aLen + 7;

        int aIndex = o1.readerIndex();
        int bIndex = o2.readerIndex();

        for (int i = byteCount; i > 0; i--) {
            if (o1.getByte(aIndex) != o2.getByte(bIndex)) {
                return false;
            }
            aIndex++;
            bIndex++;
        }

        return true;
    }


}
