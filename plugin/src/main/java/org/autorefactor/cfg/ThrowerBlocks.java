package org.autorefactor.cfg;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;

/**
 * Holds all the basic blocks that can potentially throw.
 */
class ThrowerBlocks {

    private final Map<CFGBasicBlock, Set<ITypeBinding>> potentialThrowingBlocks =
            new HashMap<CFGBasicBlock, Set<ITypeBinding>>();
    private final Map<CFGEdgeBuilder, Set<ITypeBinding>> potentialThrowingEdges =
            new HashMap<CFGEdgeBuilder, Set<ITypeBinding>>();

    public void addThrow(CFGBasicBlock basicBlock, ITypeBinding... exceptionTypes) {
        if (exceptionTypes != null && exceptionTypes.length > 0) {
            potentialThrowingBlocks.put(basicBlock, newSet(exceptionTypes));
        }
    }

    public void addThrow(CFGEdgeBuilder liveEdge, Set<ITypeBinding> exceptionTypes) {
        if (exceptionTypes != null && !exceptionTypes.isEmpty()) {
            potentialThrowingEdges.put(liveEdge, exceptionTypes);
        }
    }

    public void addThrow(Expression e, ITypeBinding... newExceptions) {
        // TODO JNR remove
    }

    private HashSet<ITypeBinding> newSet(ITypeBinding... exceptionTypes) {
        return new HashSet<ITypeBinding>(Arrays.asList(exceptionTypes));
    }

    public List<CFGBasicBlock> selectBlocksThrowing(ITypeBinding exceptionTypeToFind) {
        final List<CFGBasicBlock> results = new LinkedList<CFGBasicBlock>();
        for (Entry<CFGBasicBlock, Set<ITypeBinding>> entry : potentialThrowingBlocks.entrySet()) {
            final Set<ITypeBinding> thrownTypes = entry.getValue();
            if (exceptionTypeToFind == null || thrownTypes.contains(exceptionTypeToFind)) {
                results.add(entry.getKey());
            }
        }
        return results;
    }

    public Map<CFGBasicBlock, Set<ITypeBinding>> selectBlocksThrowingOtherThan(
            Set<ITypeBinding> exceptionTypesToReject) {
        Map<CFGBasicBlock, Set<ITypeBinding>> results = new HashMap<CFGBasicBlock, Set<ITypeBinding>>();
        for (Entry<CFGBasicBlock, Set<ITypeBinding>> entry : potentialThrowingBlocks.entrySet()) {
            final Set<ITypeBinding> bindings = getNonMatching(
                    exceptionTypesToReject, entry.getValue());
            if (!bindings.isEmpty()) {
                results.put(entry.getKey(), bindings);
            }
        }
        return results;
    }

    public List<CFGEdgeBuilder> selectEdgesThrowing(ITypeBinding exceptionTypeToFind) {
        final List<CFGEdgeBuilder> results = new LinkedList<CFGEdgeBuilder>();
        for (Entry<CFGEdgeBuilder, Set<ITypeBinding>> entry : potentialThrowingEdges.entrySet()) {
            final Set<ITypeBinding> thrownTypes = entry.getValue();
            if (exceptionTypeToFind == null || thrownTypes.contains(exceptionTypeToFind)) {
                results.add(entry.getKey());
            }
        }
        return results;
    }

    private Set<ITypeBinding> getNonMatching(Set<ITypeBinding> exceptionTypes,
            Set<ITypeBinding> thrownTypes) {
        final Set<ITypeBinding> results = new HashSet<ITypeBinding>();
        for (ITypeBinding thrownType : thrownTypes) {
            if (!exceptionTypes.contains(thrownType)) {
                results.add(thrownType);
            }
        }
        return results;
    }

    @Override
    public String toString() {
        return "potentialThrowingBlocks=" + potentialThrowingBlocks
                + " potentialThrowingEdges=" + potentialThrowingEdges;
    }

}
