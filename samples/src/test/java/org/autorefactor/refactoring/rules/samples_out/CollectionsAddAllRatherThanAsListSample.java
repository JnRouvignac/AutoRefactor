/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.   See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program under LICENSE-GNUGPL.   If not, see
 * <http://www.gnu.org/licenses/>.
 *
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution under LICENSE-ECLIPSE, and is
 * available at http://www.eclipse.org/legal/epl-v10.html
 */
package org.autorefactor.refactoring.rules.samples_out;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;

public class CollectionsAddAllRatherThanAsListSample {
    public void useCollections(List<String> texts, String[] additionalTexts) {
        // Keep this comment
        Collections.addAll(texts, additionalTexts);
    }

    public boolean useCollectionsAndReturnBoolean(List<String> texts, String[] additionalTexts) {
        // Keep this comment
        return Collections.addAll(texts, additionalTexts);
    }

    public void useCollectionsOnInteger(List<Integer> numbers, Integer[] additionalNumbers) {
        // Keep this comment
        Collections.addAll(numbers, additionalNumbers);
    }

    public void useCollectionsOnSet(Set<String> texts, String[] additionalTexts) {
        // Keep this comment
        Collections.addAll(texts, additionalTexts);
    }

    public void useCollectionsOnVarArg(List<Integer> numbers, Integer additionalNumber1, Integer additionalNumber2, Integer additionalNumber3) {
        // Keep this comment
        Collections.addAll(numbers, additionalNumber1, additionalNumber2, additionalNumber3);
    }

    public List<Integer> doNotUseCollectionsOnVariable(List<Integer> numbers, Integer[] additionalNumbers) {
        List<Integer> additionalList= Arrays.asList(additionalNumbers);
        numbers.addAll(additionalList);
        return additionalList;
    }

    public void doNotUseCollectionsForRemoving(List<Integer> numbers, Integer[] additionalNumbers) {
        numbers.removeAll(Arrays.asList(additionalNumbers));
    }

    public class DoNotRefactorCollectionImplementation<E> extends ArrayList<E> {
        private static final long serialVersionUID= 8837962990422334107L;

        @Override
        public boolean add(E additionalText) {
            addAll(Arrays.asList(additionalText, additionalText));
            return true;
        }
    }

    public class DoNotRefactorThisCollectionImplementation<E> extends ArrayList<E> {
        private static final long serialVersionUID= 8837962990422334107L;

        @Override
        public boolean add(E additionalText) {
            this.addAll(Arrays.asList(additionalText, additionalText));
            return true;
        }
    }
}
