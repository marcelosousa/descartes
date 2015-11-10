/* ./spring-projects-spring-framework-7891c0d/spring-messaging/src/main/java/org/springframework/messaging/handler/DestinationPatternsMessageCondition.java */
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

package org.springframework.messaging.handler;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.springframework.messaging.Message;
import org.springframework.util.AntPathMatcher;
import org.springframework.util.PathMatcher;
import org.springframework.util.StringUtils;

/**
 * A {@link MessageCondition} for matching the destination of a Message against one or
 * more destination patterns using a {@link PathMatcher}.
 *
 * @author Rossen Stoyanchev
 * @since 4.0
 */
public class DestinationPatternsMessageCondition extends AbstractMessageCondition<DestinationPatternsMessageCondition> {

	int patternsSize();
	int get(int index);
	/**
	 * Compare the two conditions based on the destination patterns they contain.
	 * Patterns are compared one at a time, from top to bottom via
	 * {@link org.springframework.util.PathMatcher#getPatternComparator(String)}.
	 * If all compared patterns match equally, but one instance has more patterns,
	 * it is considered a closer match.
	 * <p>It is assumed that both instances have been obtained via
	 * {@link #getMatchingCondition(Message)} to ensure they
	 * contain only patterns that match the request and are sorted with
	 * the best matches on top.
	 */
	@Override
	public int compare(DestinationPatternsMessageCondition o1, DestinationPatternsMessageCondition o2) {
		int o1Size = o1.patternsSize();
		int o2Size = o2.patternsSize();
    assume(o1Size >= 0);
    assume(o2Size >= 0);
		int i = 0;
    int result;
		while ((i < o1Size) && (i < o2Size)) {    
			result = Int.compare(o1.get(i), o2.get(i));
			if (result != 0) {
				return result;
			}
			i++;
		}
		if (i == o1Size) {
			return -1;
		}
		if (i == o2Size) {
			return 1;
		}

		return 0;
	}

}
