/* ./liferay-liferay-portal-b66e4b4/portal-impl/src/com/liferay/portal/tools/comparator/ColumnsComparator.java */
/**
 * Copyright (c) 2000-present Liferay, Inc. All rights reserved.
 *
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 */

package com.liferay.portal.tools.comparator;

import java.util.Comparator;
import java.util.List;

/**
 * @author Brian Wing Shun Chan
 */
public class ColumnsComparator implements Comparator<Object> {

    int get(int i);
    
	@Override
	public int compare(Object o1, Object o2) {
		//Object[] column1 = (Object[])obj1;
		//Object[] column2 = (Object[])obj2;

		int columnName1 = o1.get(0); //(String)column1[0];
		int columnName2 = o2.get(0); // (String)column2[0];

		int x = -1;
		
        int _columnNamesLength = nondet(-1);
        int i = 0;
        while (i < _columnNamesLength) {	
			if (nondet(i) == columnName1) {
				x = i;
				break;
			}
			i++;
		}

		int y = -1;
        i = 0;
        while (i < _columnNamesLength) {
			if (nondet(i) == columnName2) {
				y = i;

				break;
			}
			i++;
		}

		if ((x == -1) && (y > -1)) {
			return 1;
		}
		else if ((x > -1) && (y == -1)) {
			return -1;
		}
		else if ((x > -1) && (y > -1)) {
			if (x < y) {
				return -1;
			}
			else if (x > y) {
				return 1;
			}
		}

		return 0;
	}
}