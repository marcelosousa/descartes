/* ./netty-netty-410aa42/codec-http/src/main/java/io/netty/handler/codec/http/multipart/CaseIgnoringComparator.java */
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
package io.netty.handler.codec.http.multipart;

import java.io.Serializable;
import java.util.Comparator;

final class CaseIgnoringComparator implements Comparator<CharSequence>, Serializable {
    /*
    private static final long serialVersionUID = 4582133183775373862L;

    static final CaseIgnoringComparator INSTANCE = new CaseIgnoringComparator();

    private CaseIgnoringComparator() {
    }
    */
    int length();
    char charAt(int i);
    
    @Override
    public int compare(CharSequence o1, CharSequence o2) {
        int o1Length = o1.length();
        int o2Length = o2.length();
        int min = o1Length < o2Length ? o1Length : o2Length; 
        int i = 0;
        while (i < min) {
            char c1 = o1.charAt(i);
            char c2 = o2.charAt(i);
            if (c1 != c2) {
                c1 = Character.toUpperCase(c1);
                c2 = Character.toUpperCase(c2);
                if (c1 != c2) {
                    c1 = Character.toLowerCase(c1);
                    c2 = Character.toLowerCase(c2);
                    if (c1 != c2) {
                        return c1 - c2;
                    }
                }
            }
            i++;
        }
        return o1Length - o2Length;
    }

    private Object readResolve() {
        return INSTANCE;
    }
}
