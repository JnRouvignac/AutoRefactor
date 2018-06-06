/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Initial API and implementation
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
package org.autorefactor.refactoring.rules.samples_out;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;
import java.util.Spliterator;
import java.util.Stack;
import java.util.Vector;

public class GenericListRatherThanRawListSample {

    public void replaceLinkedListInstanceCreation(List<String> sourceList) {
        // Keep this comment
        boolean isFooContained = new LinkedList<String>(sourceList).contains("foo");
        // Keep this comment too
        int size = new LinkedList<String>(sourceList).size();
    }

    public void replaceArrayListInstanceCreation(List<String> sourceList) {
        // Keep this comment
        boolean isFooContained = new ArrayList<String>(sourceList).contains("foo");
        // Keep this comment too
        int size = new ArrayList<String>(sourceList).size();
    }

    public void replaceFullyQualifiedLinkedList(Date aDate, List<String> sourceList) {
        // Keep this comment
        boolean isFooContained = new java.util.LinkedList<Date>().contains(aDate);
        // Keep this comment too
        int size = new java.util.LinkedList<String>(sourceList).size();
    }

    public void replaceFullyQualifiedArrayList(Date aDate, List<String> sourceList) {
        // Keep this comment
        boolean isFooContained = new java.util.ArrayList<Date>().contains(aDate);
        // Keep this comment too
        int size = new java.util.ArrayList<String>(sourceList).size();
    }

    public void replaceLinkedListVariableDeclaration(List<LinkedList<String>> sourceList) {
        // Keep this comment
        java.util.LinkedList<LinkedList<String>> list = new java.util.LinkedList<LinkedList<String>>(sourceList);
    }

    public void replaceArrayListVariableDeclaration(List<LinkedList<String>> sourceList) {
        // Keep this comment
        java.util.ArrayList<LinkedList<String>> list = new java.util.ArrayList<LinkedList<String>>(sourceList);
    }

    public void replaceInterface(List<String> sourceList) {
        // Keep this comment
        List<String> list = new java.util.LinkedList<String>(sourceList);
    }

    public void replaceLinkedListVariableUse() {
        // Keep this comment
        java.util.LinkedList<String> list = new java.util.LinkedList<String>();
        // Keep this comment too
        list.add("bar");
    }

    public void replaceArrayListVariableUse() {
        // Keep this comment
        java.util.ArrayList<String> list = new java.util.ArrayList<String>();
        // Keep this comment too
        list.add("bar");
    }

    public void refactorListVariableUse() {
        // Keep this comment
        List<String> list = new java.util.LinkedList<String>();
        // Keep this comment too
        list.remove("bar");
    }

    public void refactorWithMethod() {
        // Keep this comment
        java.util.LinkedList<Observable> list = new java.util.LinkedList<Observable>();
        // Keep this comment too
        list.add(new Observable());
    }

    public void refactorListWithMethod() {
        // Keep this comment
        List<Observable> list = new java.util.LinkedList<Observable>();
        // Keep this comment too
        list.toArray(new Observable[0]);
    }

    public void doNotGenericizeListWithoutClearTypes() {
        List list = new java.util.LinkedList();
        list.remove(0);
    }

    public void doNotGenericizeListWithConflictingTypes() {
        List list = new java.util.LinkedList();
        list.add("1");
        list.add(1);
    }

    public void replaceLinkedListWithLoop(Date[] dates) {
        // Keep this comment
        java.util.LinkedList<Date> list = new java.util.LinkedList<Date>();
        for (Date date : dates) {
            list.add(date);
        }

        list.removeFirst();
    }

    public void replaceArrayListWithLoop(Date[] dates) {
        // Keep this comment
        java.util.ArrayList<Date> list = new java.util.ArrayList<Date>();
        for (Date date : dates) {
            list.add(date);
        }

        list.remove(0);
    }

    public String refactorListWithLoop(Date[] dates) {
        // Keep this comment
        List<Date> list = new java.util.LinkedList<Date>();
        for (Date date : dates) {
            list.add(date);
        }

        return list.toString();
    }

    public void replaceLinkedListWithModifier() {
        // Keep this comment
        final java.util.LinkedList<String> list = new java.util.LinkedList<String>();
        list.add("bar");
    }

    public void replaceArrayListWithModifier() {
        // Keep this comment
        final java.util.ArrayList<String> list = new java.util.ArrayList<String>();
        list.add("bar");
    }

    public void refactorListWithModifier() {
        // Keep this comment
        final List<String> list = new java.util.LinkedList<String>();
        list.add("bar");
    }

    public void replaceLinkedListWithParameter() {
        // Keep this comment
        java.util.LinkedList<String> list = new java.util.LinkedList<String>(new java.util.LinkedList<String>());
        list.add("bar");
    }

    public void replaceArrayListWithParameter() {
        // Keep this comment
        java.util.ArrayList<String> list = new java.util.ArrayList<String>(new java.util.LinkedList<String>());
        list.add("bar");
    }

    public void refactorListWithParameter() {
        // Keep this comment
        List<String> list = new java.util.LinkedList<String>(new java.util.LinkedList<String>());
        list.add("bar");
    }

    public boolean replaceReassignedLinkedList() {
        // Keep this comment
        java.util.LinkedList<String> list1 = new java.util.LinkedList<String>();
        list1.add("FOO");

        // Keep this comment too
        java.util.LinkedList<String> list2 = list1;
        list2.add("BAR");

        return list2.isEmpty();
    }

    public boolean replaceReassignedArrayList() {
        // Keep this comment
        java.util.ArrayList<String> list1 = new java.util.ArrayList<String>();
        list1.add("FOO");

        // Keep this comment too
        java.util.ArrayList<String> list2 = list1;
        list2.add("BAR");

        return list2.isEmpty();
    }

    public boolean replaceReassignedList() {
        // Keep this comment
        java.util.LinkedList<String> list1 = new java.util.LinkedList<String>();
        list1.add("FOO");

        // Keep this comment too
        List<String> list2 = list1;
        list2.add("BAR");

        return list2.isEmpty();
    }

    public void doNotReplaceLinkedListParameter(LinkedList aLinkedList) {
        LinkedList list = aLinkedList;
        list.add("bar");
    }

    public void doNotReplaceLinkedListPassedToAMethod() {
        String text = String.valueOf(new LinkedList());
    }

    public LinkedList doNotReplaceReturnedLinkedList() {
        return new LinkedList();
    }

    public void doNotReplaceReassignedVariable() {
        LinkedList list = new LinkedList();
        list = new LinkedList();
    }

    public void genericizeAdd(int index, String element) {
        LinkedList<String> list = new LinkedList<String>();
        list.add(index, element);
    }

    public boolean genericizeAddAll(int index, Collection<Byte> c) {
        LinkedList<Byte> list = new LinkedList<Byte>();
        return list.addAll(index, c);
    }

    public void genericizeAddFirst(Boolean e) {
        LinkedList<Boolean> list = new LinkedList<Boolean>();
        list.addFirst(e);
    }

    public void genericizeAddLast(Short e) {
        LinkedList<Short> list = new LinkedList<Short>();
        list.addLast(e);
    }

    public Iterator<Float> genericizeDescendingIterator() {
        LinkedList<Float> list = new LinkedList<Float>();
        return list.descendingIterator();
    }

    public Double genericizeElement() {
        LinkedList<Double> list = new LinkedList<Double>();
        return (Double) list.element();
    }

    public Long genericizeGetFirst() {
        LinkedList<Long> list = new LinkedList<Long>();
        return (Long) list.getFirst();
    }

    public Date genericizeGetLast() {
        LinkedList<Date> list = new LinkedList<Date>();
        return (Date) list.getLast();
    }

    public ListIterator<Number> genericizeListIterator(int index) {
        LinkedList<Number> list = new LinkedList<Number>();
        return list.listIterator(index);
    }

    public boolean genericizeOffer(BigDecimal e) {
        LinkedList<BigDecimal> list = new LinkedList<BigDecimal>();
        return list.offer(e);
    }

    public boolean genericizeOfferFirst(BigInteger e) {
        LinkedList<BigInteger> list = new LinkedList<BigInteger>();
        return list.offerFirst(e);
    }

    public boolean genericizeOfferLast(Observer e) {
        LinkedList<Observer> list = new LinkedList<Observer>();
        return list.offerLast(e);
    }

    public Observable genericizePeek() {
        LinkedList<Observable> list = new LinkedList<Observable>();
        return (Observable) list.peek();
    }

    public Map genericizePeekFirst() {
        LinkedList<Map> list = new LinkedList<Map>();
        return (Map) list.peekFirst();
    }

    public Character genericizePeekLast() {
        LinkedList<Character> list = new LinkedList<Character>();
        return (Character) list.peekLast();
    }

    public byte[] genericizePoll() {
        LinkedList<byte[]> list = new LinkedList<byte[]>();
        return (byte[]) list.poll();
    }

    public short[] genericizePollFirst() {
        LinkedList<short[]> list = new LinkedList<short[]>();
        return (short[]) list.pollFirst();
    }

    public int[] genericizePollLast() {
        LinkedList<int[]> list = new LinkedList<int[]>();
        return (int[]) list.pollLast();
    }

    public long[] genericizePop() {
        LinkedList<long[]> list = new LinkedList<long[]>();
        return (long[]) list.pop();
    }

    public void genericizePush(String[] e) {
        LinkedList<String[]> list = new LinkedList<String[]>();
        list.push(e);
    }

    public double[] genericizeRemove() {
        LinkedList<double[]> list = new LinkedList<double[]>();
        return (double[]) list.remove();
    }

    public float[] genericizeRemove(int index) {
        LinkedList<float[]> list = new LinkedList<float[]>();
        return (float[]) list.remove(index);
    }

    public Character[] genericizeRemoveFirst() {
        LinkedList<Character[]> list = new LinkedList<Character[]>();
        return (Character[]) list.removeFirst();
    }

    public boolean genericizeRemoveFirstOccurrence(Observer[] o) {
        LinkedList<Observer[]> list = new LinkedList<Observer[]>();
        return list.removeFirstOccurrence(o);
    }

    public Observable[] genericizeRemoveLast() {
        LinkedList<Observable[]> list = new LinkedList<Observable[]>();
        return (Observable[]) list.removeLast();
    }

    public boolean genericizeRemoveLastOccurrence(Date[] o) {
        LinkedList<Date[]> list = new LinkedList<Date[]>();
        return list.removeLastOccurrence(o);
    }

    public Object genericizeSet(int index, Map<Integer, String> element) {
        LinkedList<Map<Integer, String>> list = new LinkedList<Map<Integer, String>>();
        return list.set(index, element);
    }

    public Object genericizeSetOnList(int index, String[] element) {
        List<String[]> list = new LinkedList<String[]>();
        return list.set(index, element);
    }

    public Iterator<float[]> genericizeIterator() {
        LinkedList<float[]> list = new LinkedList<float[]>();
        return list.iterator();
    }

    public ListIterator<boolean[]> genericizeListIterator() {
        LinkedList<boolean[]> list = new LinkedList<boolean[]>();
        return list.listIterator();
    }

    public Spliterator<char[]> genericizeSpliterator() {
        LinkedList<char[]> list = new LinkedList<char[]>();
        return list.spliterator();
    }

    public void genericizeEnsureCapacity() {
        ArrayList<String> list = new ArrayList<String>();
        list.add("foo");
        list.ensureCapacity(10);
    }

    public void genericizeForEach() {
        ArrayList<String> list = new ArrayList<String>();
        list.add("foo");
        list.forEach(o -> o.notifyAll());
    }

    public boolean genericizeRemoveIf() {
        ArrayList<String> list = new ArrayList<String>();
        list.add("foo");
        return list.removeIf(o -> o.equals("bar"));
    }

    public void genericizeSort() {
        ArrayList<String> list = new ArrayList<String>();
        list.add("foo");
        list.sort(Collections.reverseOrder());
    }

    public void genericizeTrimToSize() {
        Vector<String> list = new Vector<String>();
        list.add("foo");
        list.trimToSize();
    }

    public void genericizeVectorEnsureCapacity() {
        Vector<String> list = new Vector<String>();
        list.add("foo");
        list.ensureCapacity(0);
    }

    public void genericizeSetSize() {
        Vector<String> list = new Vector<String>();
        list.add("foo");
        list.setSize(0);
    }

    public int genericizeCapacity() {
        Vector<String> list = new Vector<String>();
        list.add("foo");
        return list.capacity();
    }

    public void genericizeRemoveElementAt() {
        Vector<String> list = new Vector<String>();
        list.add("foo");
        list.removeElementAt(0);
    }

    public void genericizeRemoveAllElements() {
        Vector<String> list = new Vector<String>();
        list.add("foo");
        list.removeAllElements();
    }

    public int genericizeIndexOf() {
        Vector<String> list = new Vector<String>();
        return list.indexOf("foo", 10);
    }

    public int genericizeLastIndexOf() {
        Vector<String> list = new Vector<String>();
        return list.lastIndexOf("foo", 10);
    }

    public void genericizeSetElementAt() {
        Vector<String> list = new Vector<String>();
        list.setElementAt("foo", 10);
    }

    public void genericizeInsertElementAt() {
        Vector<String> list = new Vector<String>();
        list.insertElementAt("foo", 10);
    }

    public void genericizeAddElement() {
        Vector<String> list = new Vector<String>();
        list.addElement("foo");
    }

    public void genericizeRemoveElement() {
        Vector<String> list = new Vector<String>();
        list.removeElement("foo");
    }

    public void genericizeCopyInto() {
        Vector<String> list = new Vector<String>();
        list.copyInto(new String[0]);
    }

    public String genericizeElementAt() {
        Vector<String> list = new Vector<String>();
        list.add("foo");
        return (String) list.elementAt(0);
    }

    public String genericizeFirstElement() {
        Vector<String> list = new Vector<String>();
        list.add("foo");
        return (String) list.firstElement();
    }

    public String genericizeLastElement() {
        Vector<String> list = new Vector<String>();
        list.add("foo");
        return (String) list.lastElement();
    }

    public Enumeration<String> genericizeElements() {
        Vector<String> list = new Vector<String>();
        return list.elements();
    }

    public boolean genericizeEmpty() {
        Stack<String> list = new Stack<String>();
        list.add("foo");
        return list.empty();
    }

    public String genericizePush() {
        Stack<String> list = new Stack<String>();
        return (String) list.push("foo");
    }

    public int genericizeSearch() {
        Stack<String> list = new Stack<String>();
        return list.search("foo");
    }

    public String genericizeStackPop() {
        Stack<String> list = new Stack<String>();
        list.add("foo");
        return (String) list.pop();
    }

    public String genericizeStackPeek() {
        Stack<String> list = new Stack<String>();
        list.add("foo");
        return (String) list.peek();
    }

    public void replaceLinkedListWithRunnable() {
        // Keep this comment
        final java.util.LinkedList<String> list = new java.util.LinkedList<String>();
        list.set(0, "foo");
        new Runnable() {

            @Override
            public void run() {
                final java.util.LinkedList<String> localList = new java.util.LinkedList<String>();
                localList.add("foo");
            }
        };
    }

    public void replaceListWithRunnable() {
        // Keep this comment
        final List<String> list = new java.util.LinkedList<String>();
        list.set(0, "foo");
        new Runnable() {

            @Override
            public void run() {
                final List<String> localList = new java.util.LinkedList<String>();
                localList.add("foo");
            }
        };
    }

    public void doNotReplaceField() {
        Comparator c = new Comparator() {

            private boolean doNotRefactorTheExpression = new LinkedList().contains("foo");

            private LinkedList doNotReplaceField = new LinkedList();

            @Override
            public int compare(Object arg0, Object arg1) {
                return doNotReplaceField.contains(arg0) || !doNotReplaceField.contains(arg0) ? 1 : -1;
            }
        };
    }
}
