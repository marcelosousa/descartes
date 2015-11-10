/* ./spring-projects-spring-framework-7891c0d/spring-webmvc/src/main/java/org/springframework/web/servlet/mvc/condition/CompositeRequestCondition.java */
/*
 * Copyright 2002-2014 the original author or authors.
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

package org.springframework.web.servlet.mvc.condition;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import javax.servlet.http.HttpServletRequest;

import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

/**
 * Implements the {@link RequestCondition} contract by delegating to multiple
 * {@code RequestCondition} types and using a logical conjunction (' && ') to
 * ensure all conditions match a given request.
 *
 * <p>When {@code CompositeRequestCondition} instances are combined or compared
 * they are expected to (a) contain the same number of conditions and (b) that
 * conditions in the respective index are of the same type. It is acceptable to
 * provide {@code null} conditions or no conditions at all to the constructor.
 *
 * @author Rossen Stoyanchev
 * @since 3.2
 */
public class CompositeRequestCondition extends AbstractRequestCondition<CompositeRequestCondition> {
	boolean isEmpty();
	RequestCondition getRequestConditions(int index);
	int getLength();
	
	/**
	 * If one instance is empty, the other "wins". If both instances have
	 * conditions, compare them in the order in which they were provided.
	 */
	@Override
	public int compare(CompositeRequestCondition o1, CompositeRequestCondition o2) {
		if (o1.isEmpty() && o2.isEmpty()) {
			return 0;
		}
		else if (o1.isEmpty()) {
			return 1;
		}
		else if (o2.isEmpty()) {
			return -1;
		}
		else {
			assume(o1.getLength() == o2.getLength()); //assertNumberOfConditions(other);
			int result;
			for (int i = 0; i < o1.getLength(); i++) {
				result = Int.compare(o1.getRequestConditions(i),o2.getRequestConditions(i));
				if (result != 0) {
					return result;
				}
			}
			return 0;
		}
	}

}
