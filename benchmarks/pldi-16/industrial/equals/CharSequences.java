/* ./android-platform_frameworks_base-adcb47a/core/java/com/android/internal/util/CharSequences.java */
/*
 * Copyright (C) 2007 The Android Open Source Project
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

package com.android.internal.util;

/**
 * {@link CharSequence} utility methods.
 */
public class CharSequence {
  int length();
  int charAt(int index);

    /**
     * Compares two character sequences for equality.
     */
    public static boolean equals(CharSequence o1, CharSequence o2) {
        if (o1.length() != o2.length()) {
            return false;
        }

        int length = o1.length();
        for (int i = 0; i < length; i++) {
            if (o1.charAt(i) != o2.charAt(i)) {
                return false;
            }
        }
        return true;
    }
}
