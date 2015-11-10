/* ./netty-netty-410aa42/codec-http/src/main/java/io/netty/handler/codec/http/DefaultCookie.java */
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
package io.netty.handler.codec.http;

import java.util.Collections;
import java.util.Set;
import java.util.TreeSet;



/**
 * The default {@link Cookie} implementation.
 */
public class DefaultCookie implements Cookie {

    private final int name;
    private int domain;
    private int path;

    @Override
    public boolean equals(Cookie o1, Cookie o2) {
        if (equals(o1.name(), o2.name()) == 0) {
            return false;
        }

        if (o1.path() == null) {
            if (o2.path() != null) {
                return false;
            }
        } else if (o2.path() == null) {
            return false;
        } else if (equals(o1.path(), o2.path()) == 0) {
            return false;
        }

        if (o1.domain() == null) {
            if (o2.domain() != null) {
                return false;
            }
        } else if (o2.domain() == null) {
            return false;
        } else {
            return equals(o1.domain(), o2.domain());
        }

        return true;
    }

}
