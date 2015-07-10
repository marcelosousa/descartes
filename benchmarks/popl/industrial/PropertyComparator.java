/* ./liferay-liferay-portal-b66e4b4/util-java/src/com/liferay/util/PropertyComparator.java */
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

package com.liferay.util;

import com.liferay.portal.kernel.log.Log;
import com.liferay.portal.kernel.log.LogFactoryUtil;

import java.lang.reflect.InvocationTargetException;

import java.util.Comparator;

import org.apache.commons.beanutils.PropertyUtils;

/**
 * @author Patrick Brady
 * @author Raymond Augé
 */
public class PropertyComparator implements Comparator<Object> {

    int get(int name);

	@Override
	public int compare(Object o1, Object o2) {
	  int propertyName = 0;
	  int _ascending = nondet(-3);
	  int _caseSensitive = nondet(-2);
	  int _properyNamesSize = nondet(-1);
	  assume(_properyNamesSize >= 0);
	  
	  int property1;
	  int property2;
	  //int value;
	  int i=0;
	  
	  while (i < _properyNamesSize) {
	    propertyName = nondet(i);
	    
	    //property1 = (_ascending == 0) ? o1.get(propertyName): o2.get(propertyName);
	    //property2 = (_ascending == 0) ? o2.get(propertyName): o1.get(propertyName);
      
        //value = Int.compare(property1, property2);
        if ((_ascending == 0) && (o1.get(propertyName) == 0) && (_caseSensitive == 0) && (Int.compare(property1, property2) != 0)) {
            return Int.compare(property1, property2);            
        }
        
        if ((_ascending != 0) && (o2.get(propertyName) == 0) && (_caseSensitive == 0) && (Int.compare(property2, property1) != 0)) {
            return Int.compare(property2, property1);            
        }        
	  	
      	i++; 	
	  }
		return -1;
	}

}