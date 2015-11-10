/* ./liferay-liferay-portal-b66e4b4/util-java/src/com/liferay/util/xml/ElementComparator.java */
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

package com.liferay.util.xml;

import java.util.Comparator;
import java.util.List;

import org.dom4j.Attribute;
import org.dom4j.Element;

/**
 * @author Brian Wing Shun Chan
 */
public class ElementComparator implements Comparator<Element> {
  String getName();
  String getTextTrim();
  int AttributesSize();
  int ElementsSize();
  int getAttribute(int index);
  int getElement(int index);
  
	@Override
	public int compare(Element o1, Element o2) {
		String o1Name = o1.getName();
		String o2Name = o2.getName();

		if (equals(o1Name, o2Name) == 0) {
			return String.compareIgnoreCase(o1Name,o2Name);
		}
		
		String o1Text = o1.getTextTrim();
		String o2Text = o2.getTextTrim();

		if (equals(o1Text, o2Text) == 0) {
			return String.compareIgnoreCase(o1Text,o2Text);
		}
		
		if (o1.AttributesSize() < o2.AttributesSize()) {
			return -1;
		}
		if (o1.AttributesSize() > o2.AttributesSize()) {
			return 1;
		}
    Attribute attr1, attr2;
    int value;
    
		for (int i=0; i < o1.AttributesSize(); i++) {
		  attr1 = o1.getAttribute(i);
		  attr2 = o2.getAttribute(i);
			value = Int.compare(attr1, attr2);

			if (value != 0) {
				return value;
			}
		}

		if (o1.ElementsSize() < o2.ElementsSize()) {
			return -1;
		}
		if (o1.ElementsSize() > o2.ElementsSize()) {
			return 1;
		}
    Attribute elem1, elem2;
    int value;
    
		for (int i=0; i < o1.ElementsSize(); i++) {
		  elem1 = o1.getElement(i);
		  elem2 = o2.getElement(i);
			value = Int.compare(elem1, elem2);

			if (value != 0) {
				return value;
			}
		}

		return 0;
	}

}