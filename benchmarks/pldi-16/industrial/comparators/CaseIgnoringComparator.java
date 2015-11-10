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
        char c1;
        char c2;
        char c1U;
        char c2U;
        char c1L;
        char c2L;
        for (int i = 0; i < min; i++) {
            c1 = o1.charAt(i);
            c2 = o2.charAt(i);
            c1U = Character.toUpperCase(c1); 
            c2U = Character.toUpperCase(c2); 
            c1L = Character.toLowerCase(c1U); 
            c2L = Character.toLowerCase(c2U); 
            if ((c1 != c2) && (c1U != c2U) && (c1L != c2L)){
              return c1L - c2L;
            }

        }
        return o1Length - o2Length;
    }

    private Object readResolve() {
        return INSTANCE;
    }
}
