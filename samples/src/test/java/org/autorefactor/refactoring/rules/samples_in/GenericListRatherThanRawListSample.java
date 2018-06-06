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
package org.autorefactor.refactoring.rules.samples_in;

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
        boolean isFooContained = new LinkedList(sourceList).contains("foo");
        // Keep this comment too
        int size = new LinkedList(sourceList).size();
    }

    public void replaceArrayListInstanceCreation(List<String> sourceList) {
        // Keep this comment
        boolean isFooContained = new ArrayList(sourceList).contains("foo");
        // Keep this comment too
        int size = new ArrayList(sourceList).size();
    }

    public void replaceFullyQualifiedLinkedList(Date aDate, List<String> sourceList) {
        // Keep this comment
        boolean isFooContained = new java.util.LinkedList().contains(aDate);
        // Keep this comment too
        int size = new java.util.LinkedList(sourceList).size();
    }

    public void replaceFullyQualifiedArrayList(Date aDate, List<String> sourceList) {
        // Keep this comment
        boolean isFooContained = new java.util.ArrayList().contains(aDate);
        // Keep this comment too
        int size = new java.util.ArrayList(sourceList).size();
    }

    public void replaceLinkedListVariableDeclaration(List<LinkedList<String>> sourceList) {
        // Keep this comment
        java.util.LinkedList list = new java.util.LinkedList(sourceList);
    }

    public void replaceArrayListVariableDeclaration(List<LinkedList<String>> sourceList) {
        // Keep this comment
        java.util.ArrayList list = new java.util.ArrayList(sourceList);
    }

    public void replaceInterface(List<String> sourceList) {
        // Keep this comment
        List list = new java.util.LinkedList(sourceList);
    }

    public void replaceLinkedListVariableUse() {
        // Keep this comment
        java.util.LinkedList list = new java.util.LinkedList();
        // Keep this comment too
        list.add("bar");
    }

    public void replaceArrayListVariableUse() {
        // Keep this comment
        java.util.ArrayList list = new java.util.ArrayList();
        // Keep this comment too
        list.add("bar");
    }

    public void refactorListVariableUse() {
        // Keep this comment
        List list = new java.util.LinkedList();
        // Keep this comment too
        list.remove("bar");
    }

    public void refactorWithMethod() {
        // Keep this comment
        java.util.LinkedList list = new java.util.LinkedList();
        // Keep this comment too
        list.add(new Observable());
    }

    public void refactorListWithMethod() {
        // Keep this comment
        List list = new java.util.LinkedList();
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
        java.util.LinkedList list = new java.util.LinkedList();
        for (Date date : dates) {
            list.add(date);
        }

        list.removeFirst();
    }

    public void replaceArrayListWithLoop(Date[] dates) {
        // Keep this comment
        java.util.ArrayList list = new java.util.ArrayList();
        for (Date date : dates) {
            list.add(date);
        }

        list.remove(0);
    }

    public String refactorListWithLoop(Date[] dates) {
        // Keep this comment
        List list = new java.util.LinkedList();
        for (Date date : dates) {
            list.add(date);
        }

        return list.toString();
    }

    public void replaceLinkedListWithModifier() {
        // Keep this comment
        final java.util.LinkedList list = new java.util.LinkedList();
        list.add("bar");
    }

    public void replaceArrayListWithModifier() {
        // Keep this comment
        final java.util.ArrayList list = new java.util.ArrayList();
        list.add("bar");
    }

    public void refactorListWithModifier() {
        // Keep this comment
        final List list = new java.util.LinkedList();
        list.add("bar");
    }

    public void replaceLinkedListWithParameter() {
        // Keep this comment
        java.util.LinkedList list = new java.util.LinkedList(new java.util.LinkedList<String>());
        list.add("bar");
    }

    public void replaceArrayListWithParameter() {
        // Keep this comment
        java.util.ArrayList list = new java.util.ArrayList(new java.util.LinkedList<String>());
        list.add("bar");
    }

    public void refactorListWithParameter() {
        // Keep this comment
        List list = new java.util.LinkedList(new java.util.LinkedList<String>());
        list.add("bar");
    }

    public boolean replaceReassignedLinkedList() {
        // Keep this comment
        java.util.LinkedList list1 = new java.util.LinkedList();
        list1.add("FOO");

        // Keep this comment too
        java.util.LinkedList list2 = list1;
        list2.add("BAR");

        return list2.isEmpty();
    }

    public boolean replaceReassignedArrayList() {
        // Keep this comment
        java.util.ArrayList list1 = new java.util.ArrayList();
        list1.add("FOO");

        // Keep this comment too
        java.util.ArrayList list2 = list1;
        list2.add("BAR");

        return list2.isEmpty();
    }

    public boolean replaceReassignedList() {
        // Keep this comment
        java.util.LinkedList list1 = new java.util.LinkedList();
        list1.add("FOO");

        // Keep this comment too
        List list2 = list1;
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
        LinkedList list = new LinkedList();
        list.add(index, element);
    }

    public boolean genericizeAddAll(int index, Collection<Byte> c) {
        LinkedList list = new LinkedList();
        return list.addAll(index, c);
    }

    public void genericizeAddFirst(Boolean e) {
        LinkedList list = new LinkedList();
        list.addFirst(e);
    }

    public void genericizeAddLast(Short e) {
        LinkedList list = new LinkedList();
        list.addLast(e);
    }

    public Iterator<Float> genericizeDescendingIterator() {
        LinkedList list = new LinkedList();
        return list.descendingIterator();
    }

    public Double genericizeElement() {
        LinkedList list = new LinkedList();
        return (Double) list.element();
    }

    public Long genericizeGetFirst() {
        LinkedList list = new LinkedList();
        return (Long) list.getFirst();
    }

    public Date genericizeGetLast() {
        LinkedList list = new LinkedList();
        return (Date) list.getLast();
    }

    public ListIterator<Number> genericizeListIterator(int index) {
        LinkedList list = new LinkedList();
        return list.listIterator(index);
    }

    public boolean genericizeOffer(BigDecimal e) {
        LinkedList list = new LinkedList();
        return list.offer(e);
    }

    public boolean genericizeOfferFirst(BigInteger e) {
        LinkedList list = new LinkedList();
        return list.offerFirst(e);
    }

    public boolean genericizeOfferLast(Observer e) {
        LinkedList list = new LinkedList();
        return list.offerLast(e);
    }

    public Observable genericizePeek() {
        LinkedList list = new LinkedList();
        return (Observable) list.peek();
    }

    public Map genericizePeekFirst() {
        LinkedList list = new LinkedList();
        return (Map) list.peekFirst();
    }

    public Character genericizePeekLast() {
        LinkedList list = new LinkedList();
        return (Character) list.peekLast();
    }

    public byte[] genericizePoll() {
        LinkedList list = new LinkedList();
        return (byte[]) list.poll();
    }

    public short[] genericizePollFirst() {
        LinkedList list = new LinkedList();
        return (short[]) list.pollFirst();
    }

    public int[] genericizePollLast() {
        LinkedList list = new LinkedList();
        return (int[]) list.pollLast();
    }

    public long[] genericizePop() {
        LinkedList list = new LinkedList();
        return (long[]) list.pop();
    }

    public void genericizePush(String[] e) {
        LinkedList list = new LinkedList();
        list.push(e);
    }

    public double[] genericizeRemove() {
        LinkedList list = new LinkedList();
        return (double[]) list.remove();
    }

    public float[] genericizeRemove(int index) {
        LinkedList list = new LinkedList();
        return (float[]) list.remove(index);
    }

    public Character[] genericizeRemoveFirst() {
        LinkedList list = new LinkedList();
        return (Character[]) list.removeFirst();
    }

    public boolean genericizeRemoveFirstOccurrence(Observer[] o) {
        LinkedList list = new LinkedList();
        return list.removeFirstOccurrence(o);
    }

    public Observable[] genericizeRemoveLast() {
        LinkedList list = new LinkedList();
        return (Observable[]) list.removeLast();
    }

    public boolean genericizeRemoveLastOccurrence(Date[] o) {
        LinkedList list = new LinkedList();
        return list.removeLastOccurrence(o);
    }

    public Object genericizeSet(int index, Map<Integer, String> element) {
        LinkedList list = new LinkedList();
        return list.set(index, element);
    }

    public Object genericizeSetOnList(int index, String[] element) {
        List list = new LinkedList();
        return list.set(index, element);
    }

    public Iterator<float[]> genericizeIterator() {
        LinkedList list = new LinkedList();
        return list.iterator();
    }

    public ListIterator<boolean[]> genericizeListIterator() {
        LinkedList list = new LinkedList();
        return list.listIterator();
    }

    public Spliterator<char[]> genericizeSpliterator() {
        LinkedList list = new LinkedList();
        return list.spliterator();
    }

    public void genericizeEnsureCapacity() {
        ArrayList list = new ArrayList();
        list.add("foo");
        list.ensureCapacity(10);
    }

    public void genericizeForEach() {
        ArrayList list = new ArrayList();
        list.add("foo");
        list.forEach(o -> o.notifyAll());
    }

    public boolean genericizeRemoveIf() {
        ArrayList list = new ArrayList();
        list.add("foo");
        return list.removeIf(o -> o.equals("bar"));
    }

    public void genericizeSort() {
        ArrayList list = new ArrayList();
        list.add("foo");
        list.sort(Collections.reverseOrder());
    }

    public void genericizeTrimToSize() {
        Vector list = new Vector();
        list.add("foo");
        list.trimToSize();
    }

    public void genericizeVectorEnsureCapacity() {
        Vector list = new Vector();
        list.add("foo");
        list.ensureCapacity(0);
    }

    public void genericizeSetSize() {
        Vector list = new Vector();
        list.add("foo");
        list.setSize(0);
    }

    public int genericizeCapacity() {
        Vector list = new Vector();
        list.add("foo");
        return list.capacity();
    }

    public void genericizeRemoveElementAt() {
        Vector list = new Vector();
        list.add("foo");
        list.removeElementAt(0);
    }

    public void genericizeRemoveAllElements() {
        Vector list = new Vector();
        list.add("foo");
        list.removeAllElements();
    }

    public int genericizeIndexOf() {
        Vector list = new Vector();
        return list.indexOf("foo", 10);
    }

    public int genericizeLastIndexOf() {
        Vector list = new Vector();
        return list.lastIndexOf("foo", 10);
    }

    public void genericizeSetElementAt() {
        Vector list = new Vector();
        list.setElementAt("foo", 10);
    }

    public void genericizeInsertElementAt() {
        Vector list = new Vector();
        list.insertElementAt("foo", 10);
    }

    public void genericizeAddElement() {
        Vector list = new Vector();
        list.addElement("foo");
    }

    public void genericizeRemoveElement() {
        Vector list = new Vector();
        list.removeElement("foo");
    }

    public void genericizeCopyInto() {
        Vector list = new Vector();
        list.copyInto(new String[0]);
    }

    public String genericizeElementAt() {
        Vector list = new Vector();
        list.add("foo");
        return (String) list.elementAt(0);
    }

    public String genericizeFirstElement() {
        Vector list = new Vector();
        list.add("foo");
        return (String) list.firstElement();
    }

    public String genericizeLastElement() {
        Vector list = new Vector();
        list.add("foo");
        return (String) list.lastElement();
    }

    public Enumeration<String> genericizeElements() {
        Vector list = new Vector();
        return list.elements();
    }

    public boolean genericizeEmpty() {
        Stack list = new Stack();
        list.add("foo");
        return list.empty();
    }

    public String genericizePush() {
        Stack list = new Stack();
        return (String) list.push("foo");
    }

    public int genericizeSearch() {
        Stack list = new Stack();
        return list.search("foo");
    }

    public String genericizeStackPop() {
        Stack list = new Stack();
        list.add("foo");
        return (String) list.pop();
    }

    public String genericizeStackPeek() {
        Stack list = new Stack();
        list.add("foo");
        return (String) list.peek();
    }

    public void replaceLinkedListWithRunnable() {
        // Keep this comment
        final java.util.LinkedList list = new java.util.LinkedList();
        list.set(0, "foo");
        new Runnable() {

            @Override
            public void run() {
                final java.util.LinkedList localList = new java.util.LinkedList();
                localList.add("foo");
            }
        };
    }

    public void replaceListWithRunnable() {
        // Keep this comment
        final List list = new java.util.LinkedList();
        list.set(0, "foo");
        new Runnable() {

            @Override
            public void run() {
                final List localList = new java.util.LinkedList();
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
