/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program under LICENSE-GNUGPL.  If not, see
 * <http://www.gnu.org/licenses/>.
 *
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution under LICENSE-ECLIPSE, and is
 * available at http://www.eclipse.org/legal/epl-v10.html
 */
package org.autorefactor.ui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.ConcurrentModificationException;
import java.util.Iterator;

/**
 * Growable single threaded extension of the {@link ArrayList} whose
 * {@link Iterator} will not fail with {@link ConcurrentModificationException}.
 * The GrowableArrayList iterator can reloop over the initial list of elements
 * when calling {@link GrowableListIterator#reloop()}.
 */
final class GrowableArrayList<T> extends ArrayList<T> {

	public final class GrowableListIterator implements Iterator<T> {

		private int nextIndex = 0;

		public boolean hasNext() {
			return this.nextIndex < size();
		}

		public T next() {
			final T elem = get(this.nextIndex);
			this.nextIndex++;
			return elem;
		}

		public void remove() {
			throw new UnsupportedOperationException();
		}

		/**
		 * Appends once each of the already executed refactorings to the
		 * end of the parent GrowableArrayList. It does not reappend the
		 * refactorings that remain to be executed.
		 */
		public void reloop() {
			if (initialSize > 0) {
				final int nbAlreadyReadded = size() % initialSize;
				final int nbToReAdd = this.nextIndex % initialSize;
				// append the refactorings to reapply
				if (nbAlreadyReadded < nbToReAdd) {
					addAll(subList(nbAlreadyReadded, nbToReAdd));
				} else {
					// need to add the end of the initial list...
					addAll(subList(nbAlreadyReadded, initialSize));
					// ...before adding from the start of the initial list
					addAll(subList(0, nbToReAdd));
				}
			}
		}
	}

	/**
	 * The number of elements provided on creation of this List implementation.
	 */
	private final int initialSize;

	public GrowableArrayList(T... elements) {
		if (elements != null) {
			addAll(Arrays.asList(elements));
			initialSize = elements.length;
		} else {
			initialSize = 0;
		}
	}

	@Override
	public GrowableListIterator iterator() {
		return new GrowableListIterator();
	}

}
