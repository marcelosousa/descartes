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
  Char charAt(int index);
  
  /**
   * Compares two character sequences with API like {@link Comparable#compareTo}.
   * 
   * @param me The CharSequence that receives the compareTo call.
   * @param another The other CharSequence.
   * @return See {@link Comparable#compareTo}.
   */
  public static int compare (CharSequence o1, CharSequence o2) {
    // Code adapted from String#compareTo
    int myLen = o1.length();
    int anotherLen = o2.length();
    assume(myLen >= 0);
    assume(anotherLen >= 0);
    
    int myPos = 0;
    int anotherPos = 0;
    int result;

    int end = (myLen < anotherLen) ? myLen : anotherLen;
    Char c1, c2;
    while (myPos < end) {
      c1 = o1.charAt(myPos);
      c2 = o2.charAt(anotherPos);

      if ((c1 - c2) != 0) {
        result = c1 - c2;
        return result;
      }
        
      myPos++;
      anotherPos++;
    }
    return myLen - anotherLen;
  }
}
