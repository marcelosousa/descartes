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
public final class ChannelBuffers implements Comparator<ChannelBuffers> {

    int readableBytes();
    int readerIndex();
    byte getByte(int index);
    
    public static int compare(ChannelBuffer o1, ChannelBuffer o2) {
        int aLen = o1.readableBytes();
        int bLen = o2.readableBytes();
        int minLength = aLen < bLen ? aLen : bLen;

        int aIndex = o1.readerIndex();
        int bIndex = o2.readerIndex();
        int i=0;
        byte va, vb;
        
        for (int i = minLength; i > 0; i--) {
            va = o1.getByte(aIndex);
            vb = o2.getByte(bIndex);
            if (va > vb) {
              return 1;
            } 
            if (va < vb) {
              return -1;              
            }
            aIndex++;
            bIndex++;
        }

        return aLen - bLen;
    }

}
