/* org.eclipse.cdt-CDT_8_5_0/core/org.eclipse.cdt.ui/src/org/eclipse/cdt/internal/corext/codemanipulation/IncludeInfo.java */
/*******************************************************************************
 * Copyright (c) 2012, 2013 Google, Inc and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * 	   Sergey Prigogin (Google) - initial API and implementation
 *******************************************************************************/
package org.eclipse.cdt.internal.corext.codemanipulation;

import com.ibm.icu.text.Collator;

public class IncludeInfo implements Comparator<IncludeInfo> {

	int nameLength();
	int nameCharAt(int index);
    boolean isSystem;

	
	public int compare(IncludeInfo o1, IncludeInfo o2) {
		if (o1.isSystem != o2.isSystem)
			return o1.isSystem ? -1 : 1;
		
		int length1 = o1.nameLength();
		int length2 = o2.nameLength();
		int i = 0;
		int c1; int c2;
		int or1; int or2;
		int c;
		while ((i < length1) && (i < length2)) {
			c1 = o1.nameCharAt(i);
			c2 = o2.nameCharAt(i);
			or1 = nondet(c1); // order (c1)
			or2 = nondet(c2); // order (c2)
			if (or1 != or2){
				return or1 - or2;
			}
            c = Int.compare(c1+1, c2+1);
			if ((or1 == 0) && (c != 0)) {
                return c;
			}
		}
		return length1 - length2;
	}
}
