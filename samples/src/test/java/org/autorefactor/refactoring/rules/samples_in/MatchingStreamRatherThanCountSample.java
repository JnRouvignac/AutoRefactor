/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016-2017 Fabrice Tiercelin - Annoying remaining loop variable occurrence
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
package org.autorefactor.refactoring.rules.samples_in;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Spliterator;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.function.ToDoubleFunction;
import java.util.function.ToIntFunction;
import java.util.function.ToLongFunction;
import java.util.stream.Collector;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.stream.LongStream;
import java.util.stream.Stream;

public class MatchingStreamRatherThanCountSample {
    public void replaceChecksOnSize(Stream<Integer> stream) {
        // Keep this comment
        System.out.println(stream.filter(i -> i > 0).count() > 0);
        System.out.println(stream.filter(i -> i > 0).count() == 0);
        System.out.println(stream.filter(i -> i > 0).count() != 0);
        System.out.println(stream.filter(i -> i > 0).count() <= 0);
        System.out.println(stream.filter(i -> i > 0).count() < 1);
        System.out.println(stream.filter(i -> i > 0).count() >= 1);

        System.out.println(0 < stream.filter(i -> i > 0).count());
        System.out.println(0 == stream.filter(i -> i > 0).count());
        System.out.println(0 != stream.filter(i -> i > 0).count());
        System.out.println(0 >= stream.filter(i -> i > 0).count());
        System.out.println(1 > stream.filter(i -> i > 0).count());
        System.out.println(1 <= stream.filter(i -> i > 0).count());
    }

    public boolean replaceChecksOnPrimitiveStream(IntStream stream) {
        // Keep this comment
        return stream.filter(i -> i > 0).count() > 0;
    }

    public void replaceChecksOnSizeWithBody(DoubleStream stream) {
        // Keep this comment
        System.out.println(stream.filter(i -> {return i > 0;}).count() > 0);
        System.out.println(stream.filter(i -> {return i > 0;}).count() == 0);
        System.out.println(stream.filter(i -> {return i > 0;}).count() != 0);
        System.out.println(stream.filter(i -> {return i > 0;}).count() <= 0);
        System.out.println(stream.filter(i -> {return i > 0;}).count() < 1);
        System.out.println(stream.filter(i -> {return i > 0;}).count() >= 1);

        System.out.println(0 < stream.filter(i -> {return i > 0;}).count());
        System.out.println(0 == stream.filter(i -> {return i > 0;}).count());
        System.out.println(0 != stream.filter(i -> {return i > 0;}).count());
        System.out.println(0 >= stream.filter(i -> {return i > 0;}).count());
        System.out.println(1 > stream.filter(i -> {return i > 0;}).count());
        System.out.println(1 <= stream.filter(i -> {return i > 0;}).count());
    }

    public void doNotRefactorChecksOtherThanEmptiness(Stream<Integer> stream) {
        System.out.println(stream.filter(i -> i > 0).count() >= 0);
        System.out.println(stream.filter(i -> i > 0).count() < 0);
        System.out.println(stream.filter(i -> i > 0).count() == 1);
        System.out.println(stream.filter(i -> i > 0).count() != 1);
        System.out.println(stream.filter(i -> i > 0).count() > 1);
        System.out.println(stream.filter(i -> i > 0).count() <= 1);
        System.out.println(stream.filter(i -> i > 0).count() >= 2);

        System.out.println(0 <= stream.filter(i -> i > 0).count());
        System.out.println(0 > stream.filter(i -> i > 0).count());
        System.out.println(1 == stream.filter(i -> i > 0).count());
        System.out.println(1 != stream.filter(i -> i > 0).count());
        System.out.println(1 < stream.filter(i -> i > 0).count());
        System.out.println(1 >= stream.filter(i -> i > 0).count());
        System.out.println(2 <= stream.filter(i -> i > 0).count());
    }

    public void doNotRefactorChecksOtherThanEmptinessWithBody(DoubleStream stream) {
        System.out.println(stream.filter(i -> {return i > 0;}).count() >= 0);
        System.out.println(stream.filter(i -> {return i > 0;}).count() < 0);
        System.out.println(stream.filter(i -> {return i > 0;}).count() == 1);
        System.out.println(stream.filter(i -> {return i > 0;}).count() != 1);
        System.out.println(stream.filter(i -> {return i > 0;}).count() > 1);
        System.out.println(stream.filter(i -> {return i > 0;}).count() <= 1);
        System.out.println(stream.filter(i -> {return i > 0;}).count() >= 2);

        System.out.println(0 <= stream.filter(i -> {return i > 0;}).count());
        System.out.println(0 > stream.filter(i -> {return i > 0;}).count());
        System.out.println(1 == stream.filter(i -> {return i > 0;}).count());
        System.out.println(1 != stream.filter(i -> {return i > 0;}).count());
        System.out.println(1 < stream.filter(i -> {return i > 0;}).count());
        System.out.println(1 >= stream.filter(i -> {return i > 0;}).count());
        System.out.println(2 <= stream.filter(i -> {return i > 0;}).count());
    }

    public boolean doNotRefactorActiveChecks(LongStream stream, List<Long> numbers) {
        return stream.filter(i -> numbers.remove(i)).count() > 0;
    }

    public boolean doNotRefactorMethodOtherThanFilter(LongStream stream) {
        return stream.skip(1_000).count() > 0;
    }

    public boolean doNotRefactorMethodOtherThanCount(LongStream stream) {
        return stream.filter(i -> i > 0).sum() > 0;
    }

    public class DoNotRefactorCollectionImplementation implements Stream<Date> {
        @Override
        public boolean anyMatch(Predicate<? super Date> predicate) {
            return filter(predicate).count() > 0;
        }

        @Override
        public boolean noneMatch(Predicate<? super Date> predicate) {
            return this.filter(predicate).count() == 0;
        }

        @Override
        public boolean allMatch(Predicate<? super Date> predicate) {
            return false;
        }

        @Override
        public Iterator<Date> iterator() {
            return null;
        }

        @Override
        public Spliterator<Date> spliterator() {
            return null;
        }

        @Override
        public boolean isParallel() {
            return false;
        }

        @Override
        public Stream<Date> sequential() {
            return null;
        }

        @Override
        public Stream<Date> parallel() {
            return null;
        }

        @Override
        public Stream<Date> unordered() {
            return null;
        }

        @Override
        public Stream<Date> onClose(Runnable closeHandler) {
            return null;
        }

        @Override
        public void close() {

        }

        @Override
        public Stream<Date> filter(Predicate<? super Date> predicate) {
            return null;
        }

        @Override
        public <R> Stream<R> map(Function<? super Date, ? extends R> mapper) {
            return null;
        }

        @Override
        public IntStream mapToInt(ToIntFunction<? super Date> mapper) {
            return null;
        }

        @Override
        public LongStream mapToLong(ToLongFunction<? super Date> mapper) {
            return null;
        }

        @Override
        public DoubleStream mapToDouble(ToDoubleFunction<? super Date> mapper) {
            return null;
        }

        @Override
        public <R> Stream<R> flatMap(Function<? super Date, ? extends Stream<? extends R>> mapper) {
            return null;
        }

        @Override
        public IntStream flatMapToInt(Function<? super Date, ? extends IntStream> mapper) {
            return null;
        }

        @Override
        public LongStream flatMapToLong(Function<? super Date, ? extends LongStream> mapper) {
            return null;
        }

        @Override
        public DoubleStream flatMapToDouble(Function<? super Date, ? extends DoubleStream> mapper) {
            return null;
        }

        @Override
        public Stream<Date> distinct() {
            return null;
        }

        @Override
        public Stream<Date> sorted() {
            return null;
        }

        @Override
        public Stream<Date> sorted(Comparator<? super Date> comparator) {
            return null;
        }

        @Override
        public Stream<Date> peek(Consumer<? super Date> action) {
            return null;
        }

        @Override
        public Stream<Date> limit(long maxSize) {
            return null;
        }

        @Override
        public Stream<Date> skip(long n) {
            return null;
        }

        @Override
        public void forEach(Consumer<? super Date> action) {

        }

        @Override
        public void forEachOrdered(Consumer<? super Date> action) {

        }

        @Override
        public Object[] toArray() {
            return null;
        }

        @Override
        public <A> A[] toArray(IntFunction<A[]> generator) {
            return null;
        }

        @Override
        public Date reduce(Date identity, BinaryOperator<Date> accumulator) {
            return null;
        }

        @Override
        public Optional<Date> reduce(BinaryOperator<Date> accumulator) {
            return null;
        }

        @Override
        public <U> U reduce(U identity, BiFunction<U, ? super Date, U> accumulator, BinaryOperator<U> combiner) {
            return null;
        }

        @Override
        public <R> R collect(Supplier<R> supplier, BiConsumer<R, ? super Date> accumulator, BiConsumer<R, R> combiner) {
            return null;
        }

        @Override
        public <R, A> R collect(Collector<? super Date, A, R> collector) {
            return null;
        }

        @Override
        public Optional<Date> min(Comparator<? super Date> comparator) {
            return null;
        }

        @Override
        public Optional<Date> max(Comparator<? super Date> comparator) {
            return null;
        }

        @Override
        public long count() {
            return 0;
        }

        @Override
        public Optional<Date> findFirst() {
            return null;
        }

        @Override
        public Optional<Date> findAny() {
            return null;
        }
    }
}
