/* ./liferay-liferay-portal-b66e4b4/portal-service/src/com/liferay/portal/kernel/util/MethodComparator.java */
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

package com.liferay.portal.kernel.util;

import java.lang.reflect.Method;

import java.util.Comparator;

/**
 * @author Shuyang Zhou
 * @author Brian Wing Shun Chan
 */
public class MethodComparator implements Comparator<Method> {

    int getName();
    int getParameterTypesLength();
    int getParameterTypeName(int index);
    
	@Override
	public int compare(Method o1, Method o2) {
		int name1 = o1.getName();
		int name2 = o2.getName();

		int value = name1 - name2;

		if (value != 0) {
			return value;
		}

		int parameterTypes1Length = o1.getParameterTypesLength();
		int parameterTypes2Length = o2.getParameterTypesLength();
        assume(parameterTypes1Length > 0);
        assume(parameterTypes2Length > 0);
		int i = 0;
        int parameterTypeName1;
        int parameterTypeName2;
        
		while ((i < parameterTypes1Length) &&
			   (i < parameterTypes2Length)) {

			parameterTypeName1 = o1.getParameterTypeName(i);
			parameterTypeName2 = o2.getParameterTypeName(i);

			value = parameterTypeName1 - parameterTypeName2;

			if (value != 0) {
				return value;
			}

			i++;
		}

        return parameterTypes1Length - parameterTypes2Length;
/*		if (i < (parameterTypes1Length -1)) {
			return -1;
		}
		else {
			return 1;
		}*/
	}

}