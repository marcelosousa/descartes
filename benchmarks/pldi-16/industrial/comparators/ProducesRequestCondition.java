/* ./spring-projects-spring-framework-7891c0d/spring-webmvc/src/main/java/org/springframework/web/servlet/mvc/condition/ProducesRequestCondition.java */
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
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import javax.servlet.http.HttpServletRequest;

import org.springframework.http.MediaType;
import org.springframework.web.HttpMediaTypeNotAcceptableException;
import org.springframework.web.accept.ContentNegotiationManager;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.context.request.ServletWebRequest;
import org.springframework.web.servlet.mvc.condition.HeadersRequestCondition.HeaderExpression;

/**
 * A logical disjunction (' || ') request condition to match a request's 'Accept' header
 * to a list of media type expressions. Two kinds of media type expressions are
 * supported, which are described in {@link RequestMapping#produces()} and
 * {@link RequestMapping#headers()} where the header name is 'Accept'.
 * Regardless of which syntax is used, the semantics are the same.
 *
 * @author Arjen Poutsma
 * @author Rossen Stoyanchev
 * @since 3.1
 */
public final class ProducesRequestCondition extends AbstractRequestCondition<ProducesRequestCondition> {

	int acceptedMediaTypesSize();
	int indexOfEqualMediaType(int index);
	int indexOfIncludedMediaType(int index);

	/**
	 * Compares this and another "produces" condition as follows:
	 * <ol>
	 * <li>Sort 'Accept' header media types by quality value via
	 * {@link MediaType#sortByQualityValue(List)} and iterate the list.
	 * <li>Get the first index of matching media types in each "produces"
	 * condition first matching with {@link MediaType#equals(Object)} and
	 * then with {@link MediaType#includes(MediaType)}.
	 * <li>If a lower index is found, the condition at that index wins.
	 * <li>If both indexes are equal, the media types at the index are
	 * compared further with {@link MediaType#SPECIFICITY_COMPARATOR}.
	 * </ol>
	 * <p>It is assumed that both instances have been obtained via
	 * {@link #getMatchingCondition(HttpServletRequest)} and each instance
	 * contains the matching producible media type expression only or
	 * is otherwise empty.
	 */
	@Override
	public int compare(ProducesRequestCondition o1, ProducesRequestCondition o2) {
	  int thisIndex, otherIndex, result;
	  int size = o1.acceptedMediaTypesSize();
	  assume(size >= 0);
	  for (int i=0; i < size; i++) {
	  	thisIndex = o1.indexOfEqualMediaType(i);
	  	otherIndex = o2.indexOfEqualMediaType(i);
	  	result = Int.compare(thisIndex, otherIndex);
	  	if (result != 0) {
	  		return result;
	  	}
	  	thisIndex = o1.indexOfIncludedMediaType(i);
	  	otherIndex = o2.indexOfIncludedMediaType(i);
	  	result = thisIndex - otherIndex;
	  	if (result != 0) {
	  		return result;
	  	}
	  }
	  return 0;
	}


}
