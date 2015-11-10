/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.pdfbox.util;

import java.util.Comparator;

/**
 * This class is a comparator for TextPosition operators.  It handles
 * pages with text in different directions by grouping the text based
 * on direction and sorting in that direction. This allows continuous text
 * in a given direction to be more easily grouped together.  
 *
 * @author <a href="mailto:ben@benlitchfield.com">Ben Litchfield</a>
 * @version $Revision: 1.7 $
 */
public class TextPositionComparator implements Comparator
{
  int getDir();
  float getYDirAdj();
  float getXDirAdj();
  float getHeightDir();
  float getWidthDirAdj();
    /**
     * {@inheritDoc}
     */
    public int compare(TextPosition o1, TextPosition o2)
    {
        int retval = 0;

        /* Only compare text that is in the same direction. */
        if (o1.getDir() < o2.getDir())
        {
            return -1;
        }
        
        if (o1.getDir() > o2.getDir())
        {
            return 1;
        }
        
        // Get the text direction adjusted coordinates
        float pos1YBottom = o1.getYDirAdj();
        float pos2YBottom = o2.getYDirAdj();
        // note that the coordinates have been adjusted so 0,0 is in upper left
        float aux = pos1YBottom-pos2YBottom;
        float yDifference = aux < 0 ? -aux : aux; // Math.abs
        //we will do a simple tolerance comparison.
        if (yDifference < 1)
        {
            float x1 = o1.getXDirAdj();
            float x2 = o2.getXDirAdj();
            aux = x1-x2;
            float xDifference = aux < 0 ? -aux : aux; // Math.abs
            
            if (xDifference < 1)
            {
                float pos1Height = o1.getHeightDir();
                float pos2Height = o2.getHeightDir();
                aux = pos1Height - pos2Height;
                float heightDifference = aux < 0 ? -aux : aux; // Math.abs
                if (heightDifference < 1) 
                {
                    float pos1Width = o1.getWidthDirAdj();
                    float pos2Width = o2.getWidthDirAdj();
                    aux = pos1Width - pos2Width;
                    float widthDifference = aux < 0 ? -aux : aux; // Math.abs
                    if (widthDifference < 1)
                    {
                        retval = 0;
                    }
                    else
                    {
                        retval = pos1Width < pos2Width ? -1 : 1;
                    }
                }
                else
                {
                    retval = pos1Height < pos2Height ? -1 : 1;
                }
            }
            else
            {
                retval = x1 < x2 ? -1 : 1;
            }
        }
        else 
        {
            retval = pos1YBottom < pos2YBottom ? -1 : 1;
        }

        return retval;
    }
}
